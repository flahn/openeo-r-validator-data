library(httr)
library(magrittr)
library(gdalUtils)
library(tibble)
library(dplyr)
library(lubridate)
library(stringr)
library(jsonlite)

downloadCopernicusData = function(url) {
    cat("Downloading '",url,"'...\n")
    
    tmp_file = tempfile(tmpdir = getwd())
    
    
    response = GET(url = url,
                   authenticate(Sys.getenv("COPERNICUS_USER"),
                                Sys.getenv("COPERNICUS_PWD")),
                   write_disk(tmp_file,overwrite = TRUE),progress())
    
    fname <- str_match(headers(response)$`content-disposition`, "\"(.*)\"")[2]
    # rename
    file.rename(tmp_file, fname)
    cat("[DONE]\n")
}

readJP2 = function(folder) {
    jp2s = list.files(folder,pattern="jp2$",recursive = TRUE)
    
    (tibble(files=jp2s) %>% 
            mutate(filename=basename(files),
                   temp=strsplit(filename,split = "\\_|\\."),
                   timestamp=ymd_hms(unlist(strsplit(files,"/|\\_"))[5]),
                   granule=unlist(strsplit(files,"/|\\_"))[3],
                   processing_level=unlist(strsplit(files,"/|\\_"))[2]) %>% 
            rowwise() %>% 
            mutate(type = temp[1],
                   band = ifelse(type != "MSK", temp[3], temp[2]),
                   resolution = ifelse(type != "MSK", temp[4], temp[3])) %>% 
            mutate(resolution = ifelse(resolution == "jp2",NA,as.integer(sub(pattern="m",replacement = "",x = resolution)))) %>%
            ungroup() %>% 
            group_by(band) %>% 
            arrange(resolution) %>% 
            summarise(file=first(files),
                      timestamp=first(timestamp),
                      resolution=first(resolution),
                      granule = first(granule),
                      processing_level=first(processing_level),
                      type=first(type)) %>%
            mutate(file = paste0(folder,"/",file)))[c(2,3,4,5,6,7,8,9,13,10,11,12,1,14,15,16,17,18,19),] %>%
        mutate(band_index = 1:19)
}

createSpatialAggregates = function(.data) {
    .data %>% filter(grepl(x=band,pattern="^B.*")) %>% group_by(band,timestamp) %>%
        do(vrt = (.) %>% (function(data){
            t = format(data$timestamp[1],format="%Y%m%dT%H%M%S")
            filename = paste0(getwd(),"/",t,"_",data$band[1],"_",data$resolution[1],"m.vrt")
            
            gdalbuildvrt(data$file,filename,srcnodata = 0)
            
            return(filename)
        }),
        processing_level = first(.$processing_level),
        resolution = first(.$resolution),
        granule = first(.$granule),
        band_index = first(.$band_index)
        ) %>% 
        mutate(vrt = unlist(vrt),
               processing_level = unlist(processing_level),
               resolution = unlist(resolution),
               granule = unlist(granule),
               band_index = unlist(band_index)) %>%
        ungroup()
}

createGranuleAggregates = function(.data) {
    .data %>% group_by(timestamp) %>%
        do(granule_vrt = (.) %>% (function(data){
            t = format(data$timestamp[1],format="%Y%m%dT%H%M%S")
            filename = paste0(getwd(),"/",t,".vrt")
            
            data %>% arrange(band_index) %>% dplyr::select(vrt) %>% unlist() %>% gdalbuildvrt(filename,separate=TRUE)
            
            return(filename)
        }),
        timestamp = first(.$timestamp)) %>%
        mutate(timestamp=unlist(timestamp),
               granule_vrt = unlist(granule_vrt))
}

writeLookup = function(lookuptable,granules) {
    lookuptable %>% left_join(y=granules,by=c(timestamp="timestamp")) %>% 
        rename(file_access=granule_vrt,filename=vrt) %>% 
        mutate(file_access=basename(file_access),filename=basename(filename)) %>% 
        dplyr::select(file_access,timestamp,band_index,filename,-processing_level,-resolution,-granule) %>%
        group_by(timestamp) %>%
        write.csv2("lookup.csv")
}

prepareSentinel2Collection = function(name,title,description,epsg,uuids,tmpfolder,dst_folder) {
    if (!dir.exists(tmpfolder)) dir.create(tmpfolder, recursive = TRUE)
    if (!dir.exists(dst_folder)) dir.create(dst_folder, recursive = TRUE)
    
    old.dir = getwd()
    
    if (!is.null(uuids) && 
        length(uuids) > 0 && 
        length(list.dirs(tmpfolder,recursive = FALSE,full.names = FALSE)) != length(uuids)) {
        
        cat("Start preparing: ",name,"\n")
        
        scihub_base_url = "https://scihub.copernicus.eu/dhus/odata/v1/Products('{UUID}')/$value"
        
        # download data into the tmpfolder
        cat("Downloading all data into: ",tmpfolder,"\n")
        setwd(tmpfolder)
        
        # replace the variable in the scihub URL to create the downloadURLs
        urls = sapply(uuids,
                      function(uuid){
                          return(sub(pattern = "\\{UUID\\}",replacement = uuid,x = scihub_base_url))
                      }) %>% unname()
        
        # download the files based on the UUIDs
        for (url in urls) {
            downloadCopernicusData(url)
        }
    }
    
    zip_files = list.files(pattern = "\\.zip$")
    if (length(list.dirs(dst_folder,recursive = FALSE)) < length(zip_files) &&
        length(zip_files) > 0) {
        # sentinel data is stored in zip files
        cat("Unzipping packages... ")
        
        
        # unzip downloaded files into the destination folder
        sapply(zip_files,unzip, exdir=dst_folder)
        cat("[DONE]\n")
        
        # cat("Deleting downloaded files... ")
        # unlink(zip_files)
        # cat("[DONE]\n")
    }
    
    setwd(dst_folder)
    
    folders = list.dirs(".",recursive = FALSE)
    
    all = do.call(rbind,lapply(folders,readJP2))
    
    spatials = all %>% createSpatialAggregates()
    granules = spatials %>% createGranuleAggregates()
    
    # write lookup.csv ----
    
    writeLookup(lookuptable = spatials, granules = granules)
    
    # download md.json and adapt collection specific settings ----
    md_json = fromJSON("https://uni-muenster.sciebo.de/s/6gufQA5dGPeATMU/download")
    md_json$name = name
    md_json$title = title
    md_json$description = description
    md_json$`eo:epsg` = epsg
    
    write_json(md_json,auto_unbox = TRUE,pretty = TRUE,path="md.json")
    
    setwd(old.dir)
}

# Italy download and preparation ----
italy_uuids = c("9102386a-83e2-4bad-9e46-e2dec76bd6a4",
                "650c2e15-532c-482a-a652-34ecc83c713e",
                "bbee7dab-ddb8-4921-87e1-7bcacec50971")

prepareSentinel2Collection(name="val-s2-italy",
                           title="Validation data set Italy (Sentinel-2A MSI L2A)",
                           description="Sentinel-2A MSI L2A data set for the validation service. The data set covers single granules for zones T32TPT, T32TNT and T32TPS. In the time frame from 2018-06-16 to 2018-06-23",
                           epsg=32632,
                           uuids=italy_uuids,
                           tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/italy"),
                           dst_folder=paste0(Sys.getenv("DATA_DIR"),"/val-s2-italy"))

# Uganda download and preparation ----
uganda_uuids = c("d177067c-0768-4c5f-9c9d-a463a5821fb9", 
                 "eeac9b7b-814c-4a90-b486-d94180494d2f", 
                 "782f5fbc-29ae-46c2-bd39-c9971ddbb241")

prepareSentinel2Collection(name="val-s2-uganda",
                           title="Validation data set Uganda (Sentinel-2A MSI L2A)",
                           description="Sentinel-2A MSI L2A data set for the validation service",
                           epsg=32635,
                           uuids=uganda_uuids,
                           tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/uganda"),
                           dst_folder=paste0(Sys.getenv("DATA_DIR"),"/val-s2-uganda"))

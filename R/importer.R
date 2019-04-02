library(httr)
library(magrittr)
library(gdalUtils)
library(tibble)
library(dplyr)
library(lubridate)
library(stringr)
library(jsonlite)
library(raster)

data_italy = as.logical(Sys.getenv("DATA_ITALY"))
if (is.na(data_italy)) data_italy = FALSE

data_uganda = as.logical(Sys.getenv("DATA_UGANDA"))
if (is.na(data_uganda)) data_uganda = FALSE

data_switzerland = as.logical(Sys.getenv("DATA_SWITZERLAND"))
if (is.na(data_switzerland)) data_switzerland = FALSE

data_island = as.logical(Sys.getenv("DATA_ISLAND"))
if (is.na(data_island)) data_island = FALSE

data_venezuela = as.logical(Sys.getenv("DATA_VENEZUELA"))
if (is.na(data_venezuela)) data_venezuela = FALSE

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

readJP2 = function(folder, processing_level) {
    jp2s = list.files(folder,pattern="jp2$",recursive = TRUE)
    
    switch(
        processing_level,
        L1C = (tibble(files=jp2s) %>% 
                   mutate(filename=basename(files),
                          temp=strsplit(filename,split = "\\_|\\."),
                          timestamp=ymd_hms(unlist(strsplit(files,"/|\\_"))[5]),
                          granule=unlist(strsplit(files,"/|\\_"))[3],
                          processing_level=unlist(strsplit(files,"/|\\_"))[2]) %>%
                   rowwise() %>%
                   mutate(band = temp[3]) %>%
                   add_column(
                       resolution = (.) %>% (function(data){
                           sapply(paste0(folder,"/",data$files),raster) %>% lapply(res) %>% lapply(unique)
                       })
                   ) %>% 
                   mutate(resolution = unlist(resolution)) %>%
                   ungroup() %>%
                   group_by(band) %>%
                   arrange(resolution) %>%
                   summarise(file=first(files),
                             timestamp=first(timestamp),
                             resolution=first(resolution),
                             granule = first(granule),
                             processing_level=first(processing_level)) %>% 
                   mutate(file = paste0(folder,"/",file)))[c(1,2,3,4,5,6,7,8,13,9,10,11,12,14,15),] %>%
            mutate(band_index=1:15),
        L2A = (tibble(files=jp2s) %>% 
                   mutate(filename=basename(files),
                          temp=strsplit(filename,split = "\\_|\\."),
                          timestamp=ymd_hms(unlist(strsplit(files,"/|\\_"))[5]),
                          granule=unlist(strsplit(files,"/|\\_"))[3],
                          processing_level=unlist(strsplit(files,"/|\\_"))[2]) %>% 
                   rowwise() %>% 
                   mutate(type = temp[1],
                          band = ifelse(type != "MSK", temp[3], temp[2])) %>% 
                   add_column(
                       resolution = (.) %>% (function(data){
                           sapply(paste0(folder,"/",data$files),raster) %>% lapply(res) %>% lapply(unique)
                       })
                   ) %>%
                   mutate(resolution = unlist(resolution)) %>%
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
    ) %>% return()
}

reprojectSpatialAggregates = function(.data, t_srs) {
    .data %>% rowwise() %>% do(file = (.) %>% (function(data) {
        this_crs = crs(raster(.$file))
        if (compareCRS(this_crs, t_srs)) return(data$file)
        
        fdir = dirname(data$file)
        fname = sub(pattern = "[.][^.]*$",replacement = "",x = basename(data$file))
        vrt_file = paste0(fdir,"/",fname,".vrt")
        
        if (!file.exists(vrt_file)) {
            gdalwarp(srcfile = data$file,
                     dstfile = vrt_file,
                     t_srs=t_srs,
                     of="VRT",
                     dstnodata = 0,
                     overwrite = TRUE)
        }
        return(vrt_file)
    })) %>% unlist %>% unname
}

createSpatialAggregates = function(.data) {
    .data %>% filter(grepl(x=band,pattern="^B.*")) %>% group_by(band,timestamp) %>%
        do(vrt = (.) %>% (function(data){
            t = format(data$timestamp[1],format="%Y%m%dT%H%M%S")
            filename = paste0(getwd(),"/",t,"_",data$band[1],"_",data$resolution[1],"m.vrt")
            
            gdalbuildvrt(paste0(getwd(),"/",data$file),
                         filename,
                         srcnodata = 0,
                         vrtnodata = 0)
            
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
            
            data %>% 
                arrange(band_index) %>% 
                dplyr::select(vrt) %>% 
                unlist() %>% 
                gdalbuildvrt(filename,separate=TRUE,vrtnodata = 0)
            
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

prepareSentinel2Collection = function(name,title,description,epsg,uuids,tmpfolder,dst_folder,processing_level) {
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

        cat("Deleting downloaded files... ")
        unlink(zip_files)
        cat("[DONE]\n")
    }
    
    setwd(dst_folder)
    
    folders = list.dirs(recursive = FALSE,full.names = FALSE)
    
    all = do.call(rbind,lapply(folders,readJP2,processing_level=processing_level)) # TODO changes in readJP2 regarding level 1C data (e.g. band order)
    
    if (all  %>% mutate(zone=substr(granule,1,3)) %>% dplyr::select(zone) %>% distinct() %>% nrow() > 1) {
        cat("Reprojecting data not in EPSG:",epsg,"\n")
        files = all %>% reprojectSpatialAggregates(t_srs=crs(paste0("+init=epsg:",epsg)))
        all$file = files
        cat("\n[DONE]\n")
    }
    
    cat("\nBuilding spatial aggregates...\n")
    spatials = all %>% createSpatialAggregates()
    cat("\n[DONE]\n")
    
    cat("\nBuilding temporal aggregates...\n")
    granules = spatials %>% createGranuleAggregates()
    cat("\n[DONE]\n")
    
    # write lookup.csv ----
    
    writeLookup(lookuptable = spatials, granules = granules)
    
    # make a switch/case of this, take another STAC template for Level 1C data
    # download md.json and adapt collection specific settings ----
    
    md_json = switch(processing_level,
           L1C = fromJSON("https://uni-muenster.sciebo.de/s/RaIXcnRlFjDjxEi/download"),
           L2A = fromJSON("https://uni-muenster.sciebo.de/s/6gufQA5dGPeATMU/download")
           )
    
    if (!is.null(md_json)) {
        md_json$name = name
        md_json$title = title
        md_json$description = description
        md_json$`eo:epsg` = epsg
    }
    
    write_json(md_json,auto_unbox = TRUE,pretty = TRUE,path="md.json")
    
    setwd(old.dir)
}

# Italy download and preparation ----
if (data_italy) {
    italy_uuids = c("9102386a-83e2-4bad-9e46-e2dec76bd6a4",
                    "650c2e15-532c-482a-a652-34ecc83c713e",
                    "bbee7dab-ddb8-4921-87e1-7bcacec50971")
    
    prepareSentinel2Collection(name="val-s2-italy",
                               title="Validation data set Italy (Sentinel-2B MSI L2A)",
                               description="Sentinel-2B MSI L2A data set for the validation service. The data set covers single granules for zones T32TPT, T32TNT and T32TPS. In the time frame from 2018-06-16 to 2018-06-23",
                               epsg=32632,
                               uuids=italy_uuids,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/italy"),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/val-s2-italy"),
                               processing_level = "L2A")
    md_json_path = paste0(Sys.getenv("DATA_DIR"),"/val-s2-italy/md.json")
    md_json = fromJSON(md_json_path)
    #changes
    md_json$`eo:platform` = "sentinel-2b"
    
    write_json(md_json,auto_unbox = TRUE,pretty = TRUE,path=md_json_path)
    
}

# Uganda download and preparation ----
if (data_uganda) {
    uganda_uuids = c("d177067c-0768-4c5f-9c9d-a463a5821fb9", 
                     "eeac9b7b-814c-4a90-b486-d94180494d2f", 
                     "782f5fbc-29ae-46c2-bd39-c9971ddbb241")
    
    prepareSentinel2Collection(name="val-s2-uganda",
                               title="Validation data set Uganda (Sentinel-2B MSI L2A)",
                               description="Sentinel-2B MSI L2A data set for the validation service",
                               epsg=32635,
                               uuids=uganda_uuids,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/uganda"),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/val-s2-uganda"),
                               processing_level = "L2A")
    
    md_json_path = paste0(Sys.getenv("DATA_DIR"),"/val-s2-uganda/md.json")
    md_json = fromJSON(md_json_path)
    #changes
    md_json$`eo:platform` = "sentinel-2b"
    
    write_json(md_json,auto_unbox = TRUE,pretty = TRUE,path=md_json_path)
}

# Switzerland download and preparation ----
if (data_switzerland) {
    switzerland_uuids = c("acfc1852-ab3a-460a-93eb-bae846fd05da", 
                          "62096380-ea68-42f1-b02d-fa34b611b754", 
                          "88605485-103e-4b7f-b1a4-871654c7f230", 
                          "49066a33-6a09-4df5-828b-9f3aebda4311")
    
    prepareSentinel2Collection(name="val-s2-switzerland",
                               title="Validation data set Switzerland (Sentinel-2A MSI L2A)",
                               description="Sentinel-2A MSI L2A data set for the validation service for a region in Switzerland",
                               epsg=32632,
                               uuids=switzerland_uuids,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/switzerland"),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/val-s2-switzerland"),
                               processing_level = "L2A")
}

# Island download and preparation ----
if (data_island) {
    island_uuids = c("98a9078c-73ca-4756-9772-658fdc615e17", 
                     "7803fe25-4aa9-469b-aa61-362f7d595716", 
                     "119fa1d8-b16f-435c-9e67-0fd863999e4e", 
                     "dbdbb712-58ee-4870-ad96-66ec8afe257a")
    
    prepareSentinel2Collection(name="val-s2-island",
                               title="Validation data set Island (Sentinel-2A MSI L2A)",
                               description="Sentinel-2A MSI L2A data set for the validation service for a region in Island. The data was obtained in the protoype Level-2A-processing phase.",
                               epsg=32627,
                               uuids=island_uuids,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/island"),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/val-s2-island"),
                               processing_level = "L2A")
}

# Venezuela download and preparation ----
if (data_venezuela) {
    venezuela_uuids = c("ea6c82a8-4d7e-4591-83e9-8d0df2d95836", 
                        "29db6674-3f20-4c8f-a812-9b27fab32d6c", 
                        "1d40958a-4f13-41ec-8643-e9758a21a2ff", 
                        "bea90e72-a69f-458f-9e3b-1e3e5dbfd144")
    
    prepareSentinel2Collection(name="val-s2-venezuela",
                               title="Validation data set Venezuela (Sentinel-2A MSI L1C)",
                               description="Sentinel-2A MSI L1C data set for the validation service for a region in Venezuela.",
                               epsg=32619,
                               uuids=venezuela_uuids,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/venezuela"),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/val-s2-venezuela"),
                               processing_level = "L1C")
}

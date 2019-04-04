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

data_netherlands = as.logical(Sys.getenv("DATA_NETHERLANDS"))
if (is.na(data_netherlands)) data_netherlands = FALSE

delete_tmp_files = as.logical(Sys.getenv("DELETE_TMP_FILES"))
if (is.na(delete_tmp_files)) delete_tmp_files = FALSE

# CONFIG file definition ----
CONFIG = list(ITALY_ID = "val-s2-italy",
              ITALY_UUIDS = c("9102386a-83e2-4bad-9e46-e2dec76bd6a4",
                              "650c2e15-532c-482a-a652-34ecc83c713e",
                              "bbee7dab-ddb8-4921-87e1-7bcacec50971"),
              ITALY_TMP_FOLDER="italy",
              
              UGANDA_ID = "val-s2-uganda",
              UGANDA_UUIDS = c("d177067c-0768-4c5f-9c9d-a463a5821fb9", 
                               "eeac9b7b-814c-4a90-b486-d94180494d2f", 
                               "782f5fbc-29ae-46c2-bd39-c9971ddbb241"),
              UGANDA_TMP_FOLDER="uganda",
              
              SWITZERLAND_ID="val-s2-switzerland",
              SWITZERLAND_UUIDS=c("acfc1852-ab3a-460a-93eb-bae846fd05da", 
                                  "62096380-ea68-42f1-b02d-fa34b611b754", 
                                  "88605485-103e-4b7f-b1a4-871654c7f230", 
                                  "49066a33-6a09-4df5-828b-9f3aebda4311"),
              SWITZERLAND_TMP_FOLDER="switzerland",
              
              ISLAND_ID="val-s2-island",
              ISLAND_UUIDS=c("98a9078c-73ca-4756-9772-658fdc615e17", 
                             "7803fe25-4aa9-469b-aa61-362f7d595716", 
                             "119fa1d8-b16f-435c-9e67-0fd863999e4e", 
                             "dbdbb712-58ee-4870-ad96-66ec8afe257a"),
              ISLAND_TMP_FOLDER="island",
              
              VENEZUELA_ID="val-s2-venezuela",
              VENEZUELA_UUIDS=c("ea6c82a8-4d7e-4591-83e9-8d0df2d95836", 
                                "29db6674-3f20-4c8f-a812-9b27fab32d6c", 
                                "1d40958a-4f13-41ec-8643-e9758a21a2ff", 
                                "bea90e72-a69f-458f-9e3b-1e3e5dbfd144"),
              VENEZUELA_TMP_FOLDER="venezuela",
              
              NETHERLANDS_ID="val-s2-netherlands",
              NETHERLANDS_UUIDS=c("86b3f68a-6352-427b-8b0a-b9fcc15f83de", 
                                  "687c314d-e9e4-4f03-a523-5d1bb47d71f6", 
                                  "7f66c53e-09c8-4e53-9b94-92dab1daa3cc", 
                                  "edd49211-af4c-44fd-af8d-992eedacd166", 
                                  "c0dd1313-dbbc-4887-9a7b-3cee9e803ab1", 
                                  "11bb5236-f8ee-498e-b647-574455d00f09", 
                                  "c0ebdc4f-a663-490c-8560-29363bb0499d", 
                                  "c5722a25-e7c7-47e9-804c-cb7cc139aa88", 
                                  "7d4644df-687c-45b9-9c21-288d9b46d1ed", 
                                  "c0357c6d-3910-423a-883e-755e6ffba398", 
                                  "3e8bbcd8-af88-467b-9172-888057959567", 
                                  "c0ca6cbb-6748-4fbb-9ab3-0b156caf1983", 
                                  "d40f811a-e1a1-4967-a490-ab675e65f19e", 
                                  "4bb51557-3450-43bd-8601-fb80ccf2064c", 
                                  "099126c8-fada-44e8-9f72-2d13841645bc", 
                                  "4bc16fb9-4204-495f-bcd5-84a6d4bba819", 
                                  "2a5051f2-aa40-4e4d-b096-e39d5ce5ba3a", 
                                  "d37d8249-60b3-490c-974e-bd2b0f615393", 
                                  "4e777a0c-69fd-4a0b-833e-5b679536b6eb", 
                                  "f813f941-5e71-43ea-9074-fd26c99e580a", 
                                  "6e9f4501-ff9f-43b4-bf38-50623f4166ab", 
                                  "22306801-c9b5-4163-9c03-ab81e1f94478", 
                                  "0fd9f6e8-0a7f-4a0d-bf71-94806ade35a9", 
                                  "350b9656-05b0-4633-aeca-6723c9453a29", 
                                  "3e4d1a09-9d91-4220-af04-2f83e65455be", 
                                  "8633e2bd-aff9-480f-a93d-78c83c861395", 
                                  "84a8fa8e-bbb8-4cfd-b8d8-d315de866012", 
                                  "d91bf4ca-f416-4c62-a89a-34628132826d", 
                                  "7b65afda-3dfd-4f90-beb4-e2749e3c9087", 
                                  "89b94f10-b0ef-4b7b-87b8-944649b27ebc", 
                                  "ea13b78d-ac33-4035-98eb-3fa41355590f", 
                                  "1876e921-b852-4eae-9d8f-df29bcd2484d", 
                                  "74c25daa-514d-46c7-8514-163a57e86419", 
                                  "8253bbbf-e28f-416c-9dd6-9858c564f747", 
                                  "b3dfab9d-759a-4707-916f-48c9e0045896", 
                                  "1a95ed79-0d29-4384-bfe3-c95196bc0e1e", 
                                  "b8275cd6-ffc9-4109-8f5e-f256c575ab57", 
                                  "2e7d2a54-05d8-42d9-8868-ff2cee1d4bd9"),
              NETHERLANDS_TMP_FOLDER="netherlands")

downloadCopernicusData = function(uuids,tmpfolder) {
    if (is.null(uuids) || length(uuids) == 0) {
        cat("No UUIDs specified. Skip downloading.\n")
        return()
    }
        
    scihub_base_url = "https://scihub.copernicus.eu/dhus/odata/v1/Products('<UUID>')/$value"
    
    # download data into the tmpfolder
    cat("Downloading all data into: ",tmpfolder,"\n")
    
    # replace the variable in the scihub URL to create the downloadURLs
    urls = sapply(uuids,
                  function(uuid){
                      return(sub(pattern = "<UUID>",replacement = uuid,x = scihub_base_url))
                  }) %>% unname()
    
    # download the files based on the UUIDs
    for (url in urls) {
        cat("Downloading '",url,"'...\n")
        
        tmp_file = tempfile(tmpdir = tmpfolder)
        
        response = GET(url = url,
                       authenticate(Sys.getenv("COPERNICUS_USER"),
                                    Sys.getenv("COPERNICUS_PWD")),
                       write_disk(tmp_file,overwrite = TRUE),progress())
        
        fname <- str_match(headers(response)$`content-disposition`, "\"(.*)\"")[2]
        # rename
        file.rename(tmp_file, paste0(tmpfolder,"/",fname))
        cat("[DONE]\n")
    }
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
                     vrtnodata=0,
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
                         resolution="highest",
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
                gdalbuildvrt(filename,
                             separate=TRUE,
                             resolution="highest",
                             vrtnodata = 0)
            
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

fetchFilenames = function(uuids) {
    url_template="https://scihub.copernicus.eu/dhus/odata/v1/Products('<UUID>')/Attributes('Filename')"
    
    sapply(uuids,function(uuid) {
        url=sub(x = url_template,pattern = "<UUID>",uuid)
        response = GET(url = url,
                       authenticate(Sys.getenv("COPERNICUS_USER"),
                                    Sys.getenv("COPERNICUS_PWD")))
        content(response)$d$Value
    })
}

prepareSentinel2Collection = function(name,title,description,epsg,uuids,tmpfolder,dst_folder,processing_level) {
    
    if (!dir.exists(tmpfolder)) dir.create(tmpfolder, recursive = TRUE)
    if (!dir.exists(dst_folder)) dir.create(dst_folder, recursive = TRUE)
    
    old.dir = getwd()
    
    cat("Start preparing: ",name,"\n")
    
    filenames = fetchFilenames(uuids = uuids)
    unpacked_data = list.dirs(dst_folder,recursive = FALSE,full.names = FALSE)
    if (!all(filenames %in% unpacked_data)) {
        downloadedData = list.files(tmpfolder) %>% sub(pattern=".zip",replacement = ".SAFE")
        remaining_uuids = names(filenames[which(!filenames %in% downloadedData)])
        downloadCopernicusData(remaining_uuids,tmpfolder)
    } else {
        cat("Data already downloaded. Skip downloading.\n")
    }
    
    

    zip_files = list.files(path = tmpfolder,pattern = "\\.zip$",full.names = TRUE)
    if (length(list.dirs(dst_folder,recursive = FALSE)) < length(zip_files) &&
        length(zip_files) > 0) {
        
        unzipped_files = list.dirs(dst_folder,recursive = FALSE,full.names = FALSE) %>% sub(replacement=".zip",pattern = ".SAFE")
        
        remaining_zip_files = zip_files[which(!zip_files %in% unzipped_files)]
        if (length(remaining_zip_files) > 0){
            # sentinel data is stored in zip files
            cat("Unzipping packages... ")
            
            
            # unzip downloaded files into the destination folder
            sapply(remaining_zip_files,unzip, exdir=dst_folder)
            cat("[DONE]\n")
            
            if (delete_tmp_files) {
                cat("Deleting downloaded files... ")
                unlink(remaining_zip_files)
                cat("[DONE]\n")
            }
            
        }
        
    } else {
        cat("Data already extracted. Skip extraction.\n")
    }
    
    setwd(dst_folder)
    
    folders = list.dirs(recursive = FALSE,full.names = FALSE)
    
    if (length(folders) == 0) stop("No data found in the destination folder.")
    
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
    cat("Writing lookup table...")
    writeLookup(lookuptable = spatials, granules = granules)
    cat("[DONE]\n")
    
    # make a switch/case of this, take another STAC template for Level 1C data
    # download md.json and adapt collection specific settings ----
    
    cat("Preparing STAC Metadata file...")
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
    cat("[DONE]\n")
    setwd(old.dir)
}

# Italy download and preparation ----
if (data_italy) {
    
    prepareSentinel2Collection(name=CONFIG$ITALY_ID,
                               title="Validation data set Italy (Sentinel-2B MSI L2A)",
                               description="Sentinel-2B MSI L2A data set for the validation service. The data set covers single granules for zones T32TPT, T32TNT and T32TPS. In the time frame from 2018-06-16 to 2018-06-23",
                               epsg=32632,
                               uuids=CONFIG$ITALY_UUIDS,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/", CONFIG$ITALY_TMP_FOLDER),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/data/",CONFIG$ITALY_ID),
                               processing_level = "L2A")
    md_json_path = paste0(Sys.getenv("DATA_DIR"),"/data/val-s2-italy/md.json")
    md_json = fromJSON(md_json_path)
    #changes
    md_json$`eo:platform` = "sentinel-2b"
    
    write_json(md_json,auto_unbox = TRUE,pretty = TRUE,path=md_json_path)
    
}

# Uganda download and preparation ----
if (data_uganda) {
    
    prepareSentinel2Collection(name=CONFIG$UGANDA_ID,
                               title="Validation data set Uganda (Sentinel-2B MSI L2A)",
                               description="Sentinel-2B MSI L2A data set for the validation service",
                               epsg=32635,
                               uuids=CONFIG$UGANDA_UUIDS,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/", CONFIG$UGANDA_TMP_FOLDER),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/data/",CONFIG$UGANDA_ID),
                               processing_level = "L2A")
    
    md_json_path = paste0(Sys.getenv("DATA_DIR"),"/data/val-s2-uganda/md.json")
    md_json = fromJSON(md_json_path)
    #changes
    md_json$`eo:platform` = "sentinel-2b"
    
    write_json(md_json,auto_unbox = TRUE,pretty = TRUE,path=md_json_path)
}

# Switzerland download and preparation ----
if (data_switzerland) {
    
    prepareSentinel2Collection(name=CONFIG$SWITZERLAND_ID,
                               title="Validation data set Switzerland (Sentinel-2A MSI L2A)",
                               description="Sentinel-2A MSI L2A data set for the validation service for a region in Switzerland",
                               epsg=32632,
                               uuids=CONFIG$SWITZERLAND_UUIDS,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/",CONFIG$SWITZERLAND_TMP_FOLDER),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/data/",CONFIG$SWITZERLAND_ID),
                               processing_level = "L2A")
}

# Island download and preparation ----
if (data_island) {
    
    prepareSentinel2Collection(name=CONFIG$ISLAND_ID,
                               title="Validation data set Island (Sentinel-2A MSI L2A)",
                               description="Sentinel-2A MSI L2A data set for the validation service for a region in Island. The data was obtained in the protoype Level-2A-processing phase.",
                               epsg=32627,
                               uuids=CONFIG$ISLAND_UUIDS,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/",CONFIG$ISLAND_TMP_FOLDER),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/data/",CONFIG$ISLAND_ID),
                               processing_level = "L2A")
}

# Venezuela download and preparation ----
if (data_venezuela) {
    prepareSentinel2Collection(name=CONFIG$VENEZUELA_ID,
                               title="Validation data set Venezuela (Sentinel-2A MSI L1C)",
                               description="Sentinel-2A MSI L1C data set for the validation service for a region in Venezuela.",
                               epsg=32619,
                               uuids=CONFIG$VENEZUELA_UUIDS,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/", CONFIG$VENEZUELA_TMP_FOLDER),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/data/",CONFIG$VENEZUELA_ID),
                               processing_level = "L1C")
}

# netherlands timeseries download and preparation ----
if (data_netherlands) {
    prepareSentinel2Collection(name=CONFIG$NETHERLANDS_ID,
                               title="Validation data set Netherlands (Sentinel-2A MSI L2A)",
                               description="Sentinel-2 MSI L2A data set for the validation service for a spatial region in the Netherlands for several timesteps",
                               epsg=32631,
                               uuids=CONFIG$NETHERLANDS_UUIDS,
                               tmpfolder=paste0(Sys.getenv("DOWNLOAD_DIR"),"/", CONFIG$NETHERLANDS_TMP_FOLDER),
                               dst_folder=paste0(Sys.getenv("DATA_DIR"),"/data/",CONFIG$NETHERLANDS_ID),
                               processing_level = "L2A")
}

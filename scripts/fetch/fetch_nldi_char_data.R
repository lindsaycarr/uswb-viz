fetch.fetch_nldi_char_data <- function(viz = as.viz("fetch_nldi_char_data")) {
  
  deps <- readDepends(viz)
  required <- "fetch_HU_ids"
  checkRequired(deps, required)
  
  HU_ids <- deps[["fetch_HU_ids"]]
  
  chars <- c("TOT_IMPV11", "TOT_POPDENS10", "TOT_MIRAD_2012", 
             "TOT_NDAMS_2013", "TOT_STREAM_SLOPE", "TOT_FRESHWATER_WD")
  
  char_data <- setNames(rep(list(setNames(as.list(character(length = length(chars))), 
                                          chars)), 
                            length(HU_ids)), 
                        HU_ids)
  
  f_source <- "huc12pp"
  char_type = "TOT"
  
  for(ws in HU_ids) {
    for(char in chars) {
      char_data[[ws]][[char]] <- characteristics_nldi(f_source, ws, char_type, char)$characteristics$characteristic_value
    }
  }
  
  saveRDS(char_data, viz[["location"]])
  
}

fetchTimestamp.fetch_nldi_char_data <- alwaysCurrent

# http://localhost:8080/nldi-services/huc12pp/070900020503/TOT?characteristicId=TOT_BASIN_AREA
characteristics_nldi <- function(f_source = "huc12pp", f_id, char_type = "TOT", char_id = NULL) {
  nldi_base_url <- "https://cida.usgs.gov/nldi"
  
  url <- paste(nldi_base_url, f_source, f_id, char_type, 
               sep = "/")
  
  if(!is.null(char_id)) {
    url <- paste0(url, "?characteristicId=", char_id)
  }
  
  c <- ""
  
  try(c <- rawToChar(httr::GET(url)$content), silent = T)
  
  if(nchar(c)==0) {
    NULL
  } else {
    jsonlite::fromJSON(c)
  }
}

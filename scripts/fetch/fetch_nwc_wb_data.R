fetch.fetch_nwc_wb_data <- function(viz = as.viz("fetch-nwc_wb_data")) {
  
  deps <- readDepends(viz)
  required <- "fetch-HU_ids"
  checkRequired(deps, required)
  
  HU_ids <- deps[["fetch-HU_ids"]]
  
  wb_data <- list()
  
  for(ws in HU_ids) {
    
    wb_data[ws] <- list(NWCEd::getNWCData(ws, local = F))
    
  }
  
  saveRDS(wb_data, viz[["location"]])
  
}

fetchTimestamp.fetch_nwc_wb_data <- alwaysCurrent
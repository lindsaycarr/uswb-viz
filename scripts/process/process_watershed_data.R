process.process_watershed_map_data <- function(viz = as.viz("process-watershed_map_data")) {
  
  deps <- readDepends(viz)
  required <- c("fetch-HU_ids", 
                "fetch-huc12boundary", "fetch-huc12boundary_atts", 
                "fetch-huc12pp", "fetch-huc12pp_atts", 
                "fetch-nhdplusflowline")
  checkRequired(deps, required)
  
  saveRDS("", viz[['location']])
}
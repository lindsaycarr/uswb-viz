process.process_watershed_map_data <- function(viz = as.viz("process-watershed_map_data")) {

  deps <- readDepends(viz)
  required <- c("fetch-HU_ids", 
                "fetch-huc12boundary", "fetch-huc12boundary_atts", 
                "fetch-huc12pp", "fetch-huc12pp_atts", 
                "fetch-nhdplusflowline",
                "parameter-spatial")
  checkRequired(deps, required)
  
  # Load geojson and project
  watershed_map_data <- list(hu_boundary = deps$`fetch-huc12boundary`,
                             hu_outlet = deps$`fetch-huc12pp`,
                             nhd_flowline = deps$`fetch-nhdplusflowline`)
  
  watershed_map_data <- lapply(watershed_map_data, sf::st_transform, crs = deps$`parameter-spatial`$epsg_code)
  
  # simplify geometries
  watershed_map_data <- lapply(watershed_map_data, sf::st_simplify, dTolerance = deps$`parameter-spatial`$simplify_tolerance_m)
  
  saveRDS(watershed_map_data, viz[['location']])
}
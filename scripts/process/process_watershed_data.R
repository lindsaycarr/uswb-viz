process.process_watershed_map_data <- function(viz = as.viz("process-watershed_map_data")) {

  deps <- readDepends(viz)
  required <- c("fetch-huc12boundary", "fetch-huc12boundary_atts", 
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

process.process_watershed_annual_wb_data <- function(viz = as.viz("process-watershed-annual-wb-data")) {
  
  deps <- readDepends(viz)
  required <- c("fetch-nwc_wb_data")
  checkRequired(deps, required)
  
  wb_data <- deps$`fetch-nwc_wb_data`
  
  wb_data <- lapply(wb_data, process_wb_ts)
  
  saveRDS(wb_data, viz[["location"]])
}

process_wb_ts <- function(wb_data) {
  
  an_e <- NWCEd::annualize(wb_data$et, method = sum)
  an_e$data <- an_e$data*0.0393701
  an_e$group <- "ET"
  
  an_p <- NWCEd::annualize(wb_data$prcp, method = sum)
  an_p$data <- an_p$data*0.0393701
  an_p$group <- "PR"
  
  da_sqft <- dataRetrieval::readNWISsite(wb_data$streamflow$site_no[1])$drain_area_va*27878400
  
  an_q <- NWCEd::annualize(setNames(wb_data$streamflow[c("Date", "data_00060_00003")], 
                                    c("date", "data")),
                           method = mean)
  an_q$data <- an_q$data*(60 * 60 * 24 * 365.25) * 12/da_sqft
  an_q$group <- "Q"
  
  rbind(an_e, an_p, an_q)
}

process.process_watershed_por_wb_data <- function(viz = as.viz("process-watershed-por-wb-data")) {
  
  deps <- readDepends(viz)
  required <- c("process-watershed-annual-wb-data")
  checkRequired(deps, required)
  
  wb_data <- deps$`process-watershed-annual-wb-data`
  
  wb_data <- lapply(wb_data, process_wb_por)
  
  saveRDS(wb_data, viz[["location"]])
}

process_wb_por <- function(wb_data) {
  library(magrittr)
  
  component_labels = data.frame(group = c( "PR", "ET", "Q"), 
                                label = c("Precipitation",
                                          "Evapotranspiration",
                                          "Streamflow"),
                                inout = c("In", "Out", "Out"),
                                stringsAsFactors = F)

  dplyr::group_by(wb_data, group) %>% 
    dplyr::summarise(value = mean(data)) %>%
    dplyr::left_join(component_labels, by = "group") %>%
    dplyr::mutate(group = label) %>%
    dplyr::select(-label)
  
}

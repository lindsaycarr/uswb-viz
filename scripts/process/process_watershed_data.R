# Loads, projects, and simplifies geometries.
process.process_watershed_map_data <- function(viz = as.viz("process_watershed_map_data")) {

  deps <- readDepends(viz)
  required <- c("fetch_huc12boundary", 
                "fetch_huc12pp", 
                "fetch_nhdplusflowline",
                "parameter_spatial")
  checkRequired(deps, required)
  
  # Load geojson and project
  watershed_map_data <- list(hu_boundary = deps$`fetch_huc12boundary`,
                             hu_outlet = deps$`fetch_huc12pp`,
                             nhd_flowline = deps$`fetch_nhdplusflowline`)
  
  watershed_map_data <- lapply(watershed_map_data, sf::st_transform, crs = deps$`parameter_spatial`$crs)
  
  # simplify geometries
  watershed_map_data <- lapply(watershed_map_data, sf::st_simplify, dTolerance = deps$`parameter_spatial`$simplify_tolerance_m)
  
  saveRDS(watershed_map_data, viz[['location']])
}

# Adds svg markup to outlets
process.process_outlet_map_data <- function(viz = as.viz("process_outlet_map_data")) {
  
  deps <- readDepends(viz)
  required <- c("process_watershed_map_data")
  checkRequired(deps, required)
  
  library(dplyr)
  library(sf) # dplyr calls select out geometry with loading sf.
  boundaries <- deps[["process_watershed_map_data"]]$hu_boundary %>%
    select(huc12, areasqkm, name)
  sf::st_geometry(boundaries) <- NULL
  
  nwc_base <- 'https://cida.usgs.gov/nwc/#!waterbudget/achuc/'
  
  outlets <- deps[["process_watershed_map_data"]]$hu_outlet %>%
    dplyr::select(HUC_12) %>% 
    dplyr::left_join(boundaries, by = c("HUC_12" = "huc12")) %>%
    dplyr::mutate(hovertext = paste(name, " - ", (areasqkm * 0.386102), "sqmi"),
           r = '5',
           onmousemove = sprintf("hovertext('%s',evt);", hovertext),
           onmouseover=sprintf("setEmphasis('%s');", hovertext),
           onmouseout="clearEmphasis();",
           onclick = sprintf("clicklink('%s');", paste0(nwc_base, HUC_12)),
           class = 'outlet-dots') %>%
    dplyr::rename(id = HUC_12) %>%
    dplyr::select(-areasqkm, -hovertext, -name)
    
  saveRDS(outlets, viz[['location']])
}

process.process_watershed_annual_wb_data <- function(viz = as.viz("process_watershed_annual_wb_data")) {
  
  deps <- readDepends(viz)
  required <- c("fetch_nwc_wb_data")
  checkRequired(deps, required)
  
  wb_data <- deps$`fetch_nwc_wb_data`
  
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

process.process_watershed_por_wb_data <- function(viz = as.viz("process_watershed_por_wb_data")) {
  
  deps <- readDepends(viz)
  required <- c("process_watershed_annual_wb_data")
  checkRequired(deps, required)
  
  wb_data <- deps$`process_watershed_annual_wb_data`
  
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

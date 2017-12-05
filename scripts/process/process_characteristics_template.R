process.process_characteristics_template <- function(viz = as.viz("process_characteristics_template")) {
  
  deps <- readDepends(viz)
  required <- c("fetch_characteristics_template", 
                "fetch_nldi_char_data", 
                "process_wb_huc_nwis",
                "process_watershed_map_data")
  checkRequired(deps, required)
  
  template <- deps[["fetch_characteristics_template"]]
  
  nldi_chars <- deps[["fetch_nldi_char_data"]]
  
  nwis_sites <- deps[["process_wb_huc_nwis"]]
  
  boundaries <- dplyr::select(deps[["process_watershed_map_data"]]$`http://www.opengeospatial.org/standards/waterml2/hy_features/HY_CatchmentDivide`, huc12, name) 
  sf::st_geometry(boundaries) <- NULL
  boundaries <- data.frame(boundaries, stringsAsFactors = F)
  rownames(boundaries) <- boundaries$huc12
  
  for(hu in names(nldi_chars)) {
    nldi_chars[[hu]][["hu_id"]] <- hu
    nldi_chars[[hu]][["wb_id"]] <- paste0("wb-id-", hu)
    nldi_chars[[hu]][["nwis_id"]] <- nwis_sites[[hu]]
    nldi_chars[[hu]][["hu_name"]] <- boundaries[hu,"name"]
    nldi_chars[[hu]][["drainage_area"]] <- round((as.numeric(nldi_chars[[hu]][["TOT_BASIN_AREA"]]) * 0.386102))
    # ML/(yr*sqkm) -> 1000m^3/ML * (1/1000^2) m^2/km^2 * 39.3701 in/m
    nldi_chars[[hu]][["TOT_FRESHWATER_WD"]] <- round(1000*as.numeric(nldi_chars[[hu]][["TOT_FRESHWATER_WD"]]) * (39.3701/1000))/1000
  }
  
  nldi_chars <- list(hu_list = unname(nldi_chars))
  
  cat(whisker::whisker.render(template, nldi_chars), file = viz[['location']])
}

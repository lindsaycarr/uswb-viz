process.process_characteristics_template <- function(viz = as.viz("process_characteristics_template")) {
  
  deps <- readDepends(viz)
  required <- c("fetch_characteristics_template", "fetch_nldi_char_data", "process_wb_huc_nwis")
  checkRequired(deps, required)
  
  template <- deps[["fetch_characteristics_template"]]
  
  nldi_chars <- deps[["fetch_nldi_char_data"]]
  
  nwis_sites <- deps[["process_wb_huc_nwis"]]
  
  for(hu in names(nldi_chars)) {
    nldi_chars[[hu]][["wb_id"]] <- paste0("wb-id-", hu)
    nldi_chars[[hu]][["nwis_id"]] <- nwis_sites[[hu]]
  }
  
  nldi_chars <- list(hu_list = unname(nldi_chars))
  
  cat(whisker::whisker.render(template, nldi_chars), file = viz[['location']])
}

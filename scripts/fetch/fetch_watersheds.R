# Download watershed geometry
fetch.fetch_watersheds <- function(viz = as.viz("fetch-watersheds")) {
  
  deps <- readDepends(viz)
  required <- "fetch-HU_ids"
  checkRequired(deps, required)
  
  HU_ids <- deps["fetch-HU_ids"]
  
  saveRDS(HU_ids, viz[['location']])
}

fetchTimestamp.fetch_watersheds <- alwaysCurrent
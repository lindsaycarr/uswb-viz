fetch.fetch_map_data <- function(viz = as.viz("fetch_map_data")) {
  
  deps <- readDepends(viz)
  required <- "fetch_HU_ids"
  checkRequired(deps, required)
  
  HU_ids <- deps[["fetch_HU_ids"]]

  urls <- list(nhdplusflowline = "https://opengeospatial.github.io/ELFIE/usgs/nhdplusflowline/uswb/",
               huc12boundary = "https://opengeospatial.github.io/ELFIE/usgs/huc12boundary/uswb/",
               huc12pp = "https://opengeospatial.github.io/ELFIE/usgs/huc12pp/uswb/")
  
  md <- setNames(rep(list(list()), length(HU_ids)), HU_ids)
  map_data <- list()

  for(type in names(urls)) {
    
    url_base <- urls[[type]]
    
    for(ws in HU_ids) {
      
      url <- paste0(url_base, ws, ".json")
      
      content <- rawToChar(httr::GET(url)$content)
      jl <- jsonlite::fromJSON(content)
      
      if(!is.null(jl$geo) && !is.null(jl$geo$`@type`)) {
        if(jl$geo$`@type` == "schema:GeoCoordinates") {
          sfg <- sf::st_point(c(jl$geo$longitude, jl$geo$latitude))
        } else if(jl$geo$`@type` == "schema:GeoShape") {
          if(!is.null(jl$geo$polygon)) {
            if(dim(jl$geo$polygon$geometry$coordinates)[1]==1 && dim(jl$geo$polygon$geometry$coordinates)[2]==1) {
              sfg <- sf::st_polygon(list(matrix(jl$geo$polygon$geometry$coordinates[1,1,,], ncol = 2, byrow = F)))
            } else {
              stop("found a multipolygon or multiple features, not supported")
            }
          } else if(!is.null(jl$geo$li)) {
            sfg <- sf::st_multilinestring(lapply(jl$geo$line$geometry$coordinates[[1]], sf::st_linestring))
          }
        }
      }
      
      md[[ws]] <- sfg
      
    }
    
    map_data[[type]] <- sf::st_sf(geometry = sf::st_sfc(md), data.frame(huc12 = HU_ids, row.names = HU_ids, stringsAsFactors = F))
    
  }
  
  saveRDS(map_data, viz[["location"]])
  
}

fetchTimestamp.fetch_map_data <- alwaysCurrent

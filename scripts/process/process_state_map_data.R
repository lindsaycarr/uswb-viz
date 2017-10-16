
process.process_state_map_data <- function(viz = as.viz("process_state_map_data")){
  
  deps <- readDepends(viz)
  required <- c("parameter_spatial")
  crs <- deps[["parameter_spatial"]]$crs
  crs <- "+init=epsg:2163"
  
  shift_states <- 
    list(AK = list(scale = 0.37, shift = c(90,-460), rotate = -50,
                   sf_df = get_map_data(crs, database = "world", region = "USA:alaska")),
         HI = list(scale = 1, shift = c(520, -110), rotate = -35,
                   sf_df = get_map_data(crs, database = "world", region = "USA:hawaii")),
         PR = list(scale = 2.5, shift = c(-140, 90), rotate=20,
                   sf_df = get_map_data(crs, database = "world", region = "Puerto Rico")))
  
  states_out <- get_map_data(crs, database = 'state')
  
  for(ss in names(shift_states)){
    ss_data <- shift_states[[ss]]
    
    shifted <- shift_sf(sf_df = ss_data$sf_df, 
                        scale = ss_data$scale,
                        shift = ss_data$shift, 
                        ss_data$rotate, 
                        row_name = ss)
    
    states_out <- rbind(shifted, states_out)
  }
  
  library(dplyr)
  library(sf)
  states_out <- mutate(states_out, id = "state-polygons",
                       class = ifelse(ID %in% c("AK", "HI", "PR"), "exterior-state", "interior-state")) %>%
    select(-ID)
  
  saveRDS(states_out, file = viz[['location']])
}

get_map_data <- function(crs, ...){
  map <- sf::st_as_sf(maps::map(..., fill=TRUE, plot = FALSE))
  map <- sf::st_transform(map, crs)
  return(map)
}

shift_sf <- function(sf_df, scale = NULL, shift = NULL, 
                     rotate = 0, ref=sf_df, row_name = NULL){

  if (is.null(scale) & is.null(shift) & rotate == 0){
    return(sf_df)
  }
  
  orig_cent <- sf::st_coordinates(sf::st_centroid(sf_df))

  bbox <- sf::st_bbox(ref)
  
  scale <- max(c(bbox[3]-bbox[1], bbox[4]-bbox[2])) * scale
  
  # Have to switch to sp for the "elide" function.
  sp_geom <- sf::as_Spatial(sf_df$geometry)
  sp_geom_ref <- sf::as_Spatial(ref$geometry)
  
  sp_geom_obj <- maptools::elide(sp_geom, rotate=rotate, center=orig_cent, bb = sp::bbox(sp_geom_ref))
  sp_geom_ref <- maptools::elide(sp_geom_ref, rotate=rotate, center=orig_cent, bb = sp::bbox(sp_geom_ref))
  sp_geom_obj <- maptools::elide(sp_geom_obj, scale=scale, center=orig_cent, bb = sp::bbox(sp_geom_ref))
  sp_geom_ref <- maptools::elide(sp_geom_ref, scale=scale, center=orig_cent, bb = sp::bbox(sp_geom_ref))

  ref <- sf::st_as_sf(sp_geom_obj)
  
  new_cent <- sf::st_coordinates(sf::st_centroid(ref))
  
  sp_geom_obj <- maptools::elide(sp_geom_obj, shift=shift*10000+c(orig_cent-new_cent))
  
  obj <- sf::st_as_sf(sp_geom_obj)
  obj <- sf::st_sf(data.frame(obj, ID = row_name, stringsAsFactors = F))
  row.names(obj) <- row_name
  sf::st_crs(obj) <- sf::st_crs(sf_df)
  
  return(obj)
}


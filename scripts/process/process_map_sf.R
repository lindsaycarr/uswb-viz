
process.process_map <- function(viz = as.viz("process-map")){
  
  deps <- readDepends(viz)
  required <- c("parameter-spatial")
  crs <- deps[["parameter-spatial"]]$crs
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
    sf_df <- shift_states[[ss]]$sf_df
    scale <- shift_states[[ss]]$scale
    shift <- shift_states[[ss]]$shift
    rotate <- shift_states[[ss]]$rotate
    ref <- sf_df
    shifted <- shift_sf(sf_df = sf_df, scale = scale,
                        shift = shift, rotate = rotate, row_name = ss)
    states_out <- rbind(shifted, states_out)
  }
  
  saveRDS(states.out, file = viz[['location']])
}

get_map_data <- function(crs, ...){
  map <- sf::st_as_sf(maps::map(..., fill=TRUE, plot = FALSE))
  map <- sf::st_transform(map, crs)
  return(map)
}

sf_df <- shift_states$AK$sf_df
scale <- shift_states$AK$scale
shift <- shift_states$AK$shift
rotate <- shift_states$AK$rotate
ref <- sf_df
row_name = "AK"

shift_sf <- function(sf_df, scale = NULL, shift = NULL, 
                     rotate = 0, ref=sf_df, row_name = NULL){
  # May need to add row name handling.
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

  ref <- st_as_sf(sp_geom_obj)
  
  new_cent <- sf::st_coordinates(sf::st_centroid(ref))
  
  sp_geom_obj <- maptools::elide(sp_geom_obj, shift=shift*10000+c(orig_cent-new_cent))
  
  obj <- sf::st_as_sf(sp_geom_obj)
  obj <- st_sf(data.frame(obj, ID = row_name, stringsAsFactors = F))
  row.names(obj) <- row_name
  sf::st_crs(obj) <- sf::st_crs(sf_df)
  
  return(obj)
}


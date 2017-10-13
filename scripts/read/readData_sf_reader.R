readData.sf_reader <- function(viz) {
  sf::read_sf(viz[['location']], stringsAsFactors = F)
}
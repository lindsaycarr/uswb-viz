visualize.visualize_map_thumbnail <- function(viz){
  library(dplyr)
  
  data <- readDepends(viz)
  states <- data[['process_state_map_data']]
  # Just commenting out sites and bars to keep ideas in code for later.
  # sites <- data[['site-map']]
  # bars <- data[['bar-data']]
  height <- viz[['fig-height']]
  width <- viz[['fig-width']]
  
  png(filename = viz[['location']], width = width, height = height, units = 'px')
  createThumbnailPlot(states, width)
  dev.off()
}

visualize.visualize_map <- function(viz = as.viz("visualize_map")){
  
  deps <- readDepends(viz)
  required <- c("visualize_svg_base_map", "fetch_usgs_watermark")
  checkRequired(deps, required)
  # Also expects alttext, title, id, and location from viz.
  
  svg <- deps[["visualize_svg_base_map"]]
  watermark <- deps[['fetch_usgs_watermark']]
  
  xml2::xml_attr(svg, "id") <- viz[['id']]
  vb <- strsplit(xml2::xml_attr(svg, 'viewBox'),'[ ]')[[1]] # can be used for portrait vs landscape, see Maria
  
  # get the big dog that has all the stuff that is geo:
  map_elements <- xml2::xml_find_first(svg, "//*[local-name()='g'][@id='map-elements']") 
  
  # add sibling element below map_elements (before in xml, below in svg layer)
  non_geo_bot <- xml2::xml_add_sibling(map_elements, 'g', 'id' = 'non-geo-bottom', .where='before')
  
  # add alttext and title before the children of the root element
  xml2::xml_add_sibling(xml2::xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml2::xml_add_sibling(xml2::xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  
  # Add sibling of map_elements (after in xml, above in svg layer)
  non_geo_top <- xml2::xml_add_sibling(map_elements, 'g', 'id' = 'non-geo-top', .where='after')
  
  # Add tooltip group
  g_tool <- xml2::xml_add_sibling(non_geo_top, 'g', id='tooltip-group')
  
  # Add defs
  d <- xml2::xml_add_child(svg, 'defs', .where='before')
  
  # Add background with pause class
  xml2::xml_add_child(non_geo_bot, 'rect', width="100%", height="100%", class='viz-background-color viz-pause', id='viz-background')
  
  # map-elements-{mode}-top is where all of the mouseovers, tips, and click events go
  map_elements_top <- xml2::xml_add_child(svg, 'g', id = "map-elements-mouser")
  
  # Could use "as_mouse_topper" here. See Maria.
  
  xml2::xml_add_child(g_tool, 'rect', id="tooltip-box", height="24", class="tooltip-box")
  xml2::xml_add_child(g_tool, 'path', id="tooltip-point", d="M-6,-11 l6,10 l6,-11", class="tooltip-box")
  xml2::xml_add_child(g_tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="tooltip-text-label svg-text", " ")
  
  g_watermark <- xml2::xml_add_child(non_geo_top, watermark)
  xml2::xml_attr(g_watermark, "transform") <- sprintf('translate(%s,%s)scale(0.2)', 
                                                as.character(as.numeric(vb[3])-115), 
                                                as.character(as.numeric(vb[1])+10))
  
  # Hack job here because there is a problem in svg_base_map
  poly_lines <- xml2::xml_find_all(svg, "//*[local-name()='polyline']")
  xml2::xml_attr(poly_lines, "id") <- 'flowline'
  xml2::xml_attr(poly_lines, "class") <- 'nhdplus-flowline'
  
  xml2::write_xml(svg, viz[['location']])
  
}

#' remove mouser events from style geometries, and add them to a new invisible mouser group overlay
as_mouse_topper <- function(svg, style.group.id, mouser.parent.id){
  
  style.kids <- xml_children(
    xml_find_first(
      svg, sprintf("//*[local-name()='g'][@id='%s']", style.group.id)
    )
  )
  
  parent.element <- xml_find_first(
    svg, sprintf("//*[local-name()='g'][@id='%s']", mouser.parent.id)
  )
  g.mouser <- xml_add_child(parent.element, 'g', id = paste0(style.group.id, '-mousers'))
  
  
  transfers <- c('onmousemove', 'onmouseout', 'onclick')
  for (style.kid in style.kids){
    mouse.kid <- xml_add_child(g.mouser, 'use', 'xlink:href'=sprintf("#%s", xml_attr(style.kid, 'id')), class = 'mouser')
    .jnk <- lapply(X = transfers, FUN = function(x) {
      xml_attr(mouse.kid, x) <- xml_attr(style.kid, x)
      xml_attr(style.kid, x) <- NULL
    }
    )
  }
  # does it work to let the reference element keep its class? no
  
}

#' Create the actual map + bars separately from saving image
#' 
#' @param states spatial polygon of state outlines
# @param sites spatial polygon of site locations
# @param bars filepath to the xml file with bar data
#' 
#' @result a plot
createThumbnailPlot <- function(states, width){
  library(xml2)
  
  par(mar=c(0,0,0,0), oma=c(0,0,0,0), bg='black')
  library(sf)
  plot(states, col='grey2', border = 'grey2')
  # Could add watersheds?
  # sp::plot(sites, add=TRUE, pch = 16, cex=width/2500, col='#CCFFFF33') # 20% opacity
  
  # bars.xml <- xml2::read_xml(bars) %>% xml_child()
  # rects <- xml_children(bars.xml)
  # xleft <- xml_attr(rects, 'x') %>% as.numeric()
  # ys <- xml_attr(rects, 'y') %>% as.numeric()
  # ytop <- max(ys) - ys
  # 
  # par(new=TRUE, mar=c(0,0,0,0), oma=c(0,0,0,0))
  # plot(xleft, ytop, type = 'l', axes=FALSE , 
  #      ylab="", xlab="", col='#2c5258CC', lwd = width/60)
  
}

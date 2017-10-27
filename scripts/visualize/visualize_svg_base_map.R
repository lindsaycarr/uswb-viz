visualize.visualize_svg_base_map <- function(viz = as.viz('visualize_svg_base_map')){
  
  # -- added px based precip --
  depends <- readDepends(viz)
  checkRequired(depends, c("fetch_view_limits", "parameter_spatial"))
  
  # makes the assumption that all inputs other than view-limits are 
  # geometries to be injected into the svg base map.
  vl <- which(names(depends) == 'fetch_view_limits')
  view_limits <- depends[[vl]]
  geoms <- depends[-vl]
  g_ids <- names(depends)[-vl]
  
  ps <- which(names(depends) == "parameter_spatial")
  p_spatial <- geoms[[ps]]
  geoms <- geoms[-ps]
  g_ids <- names(geoms)[-ps]
  
  checkRequired(p_spatial, c("bbox", "width", "height", "pointsize"))
  
  view_limits <- append(view_limits, p_spatial[c("height", "width", "pointsize")])
  
  # 1) set up shell svg, w/ proper size and aspect
  svg <- init_svg(width = p_spatial$width, height = p_spatial$height)
  geom_base_group <- xml2::xml_add_child(svg, 'g', 'id' = 'map-elements')
  
  # 2) add basic groups etc, including <defs><g id="template-geoms"/></defs> and <g id="styled-geoms"/>
  # 3) read in depends geoms
  # 4) loop through depends, trim, then add geoms to <use/> elements (in id="template-geoms"), with id="u-{id}"
  xlim <- view_limits$xlim
  ylim <- view_limits$ylim
  for (g in geoms){
    # # clip the spatial object so that it only contains features and data that are within the plotting range:
    # g.clip <- clip_sp(g, xlim, ylim) Don't need - will bring back later.
    g_node <- xml2::xml_add_child(geom_base_group, do.call(get_svg_geoms, append(list(sp = g), view_limits)))
    xml2::xml_attr(g_node, 'id') <- g_ids[1L]
    # Drop the geometry
    st_geometry(g) <- NULL
    # Add the rest as attrs
    add_attrs(xml2::xml_children(g_node), data = g)
    g_ids <- tail(g_ids, -1L)
  }
  
  xml2::write_xml(x = svg, viz[['location']])
  # 6) create geoms to mirror ids in <use/> elements, add attributes
}

add_attrs <- function(nodes, data){
  for (d.i in seq_len(length(nodes))){
    for (value in names(data)){
      xml2::xml_attr(nodes[d.i], value) <- data[d.i, ][[value]]
    }
  }
}

init_svg <- function(..., width = 10, height = 8){
  # fragile, this is coded into svglite:
  ppi <- 72
  library(xml2)
  vb <- sprintf("%s %s %s %s", 0, 0, width*ppi, height*ppi)
  xml2::xml_new_root('svg', viewBox = vb, preserveAspectRatio="xMidYMid meet", 
                     xmlns="http://www.w3.org/2000/svg", `xmlns:xlink`="http://www.w3.org/1999/xlink", version="1.1" )
}

#' extract the svg elements from an sp object
#' 
#' @param sp a spatial object
#' @param ... additional arguments passed to the plotting methods of `sp` (e.g., `expandBB`)
#' @param width the width (in inches) of the svg view
#' @param height the height (in inches) of the svg view
#' @param pointsize number of pixels per inch
#' @param xlim x limits (in sp units) of the plot
#' @param ylim y limits (in sp units) of the plot
#' 
#' @return an `xml_document`
get_svg_geoms <- function(sp, ..., width = 10, height = 8, pointsize = 12, xlim, ylim){
  
  if("sf" %in% class(sp)) { # this should work till we can convert this to sf.
    library(sf)
    sp <- as(sp, "Spatial")
  }
  
  stopifnot(packageVersion('svglite') == '1.2.0.9003')
  
  if (missing(xlim)){
    xlim <- get_sp_lims(sp, ..., return = 'xlim')
  }
  if (missing(ylim)){
    ylim <- get_sp_lims(sp, ..., return = 'ylim')
  }
  
  rendered <- svglite::xmlSVG(width = width, height = height, pointsize = pointsize, standalone = F, {
    set_sp_plot()
    for (j in seq_len(length(sp))){
      if (inherits(sp, 'SpatialPoints')){
        sp::plot(sp[j, ], ..., xlim = xlim, ylim = ylim, add = ifelse(j == 1, F, T), pch = 20)
      } else {
        sp::plot(sp[j, ], ..., xlim = xlim, ylim = ylim, add = ifelse(j == 1, F, T))
      }
      
    }
    
  })
  svg.g <- xml2::xml_child(rendered)
  if (xml2::xml_length(svg.g) == length(sp) + 1){
    xml_remove(xml_child(svg.g)) # remove the <rect thing it puts in there>
  } else if (xml2::xml_length(svg.g) != length(sp)){
    message('something might be wrong. Length of svg elements is different than number of features',
            'but ignore this warning for lines.')
    xml_remove(xml_child(svg.g))
  }
  
  # here either strip the important attributes out and re-add them with a xml_set_attrs call, or lapply the nodeset and add attrs one by one:
  
  return(svg.g) # removing the <rect/> element...
}

#' set up the basic plot par for a map
set_sp_plot <- function(){
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
}

visualize.visualize_watershed_por_wb_data <- function(viz = as.viz("visualize_watershed_por_wb_data")) {
  
  deps <- readDepends(viz)
  required <- c("process_watershed_por_wb_data", "fetch_wb_bar_template", "visualize_map", "process_watershed_map_data")
  checkRequired(deps, required)
  
  all_wb_data <- deps[["process_watershed_por_wb_data"]]
  
  library(sf)
  title_data <- deps[["process_watershed_map_data"]]$hu_boundary %>%
    dplyr::select(huc12, name)
  sf::st_geometry(title_data) <- NULL 

  titles <- setNames(as.character(title_data$name), title_data$huc12)
  
  template <- deps[["fetch_wb_bar_template"]]
  
  hus <- setNames(names(all_wb_data), names(all_wb_data))
  
  wb_svg_size <- c(0, 0, 288, 288)
  svgs <- lapply(X = hus,
                 FUN = build_watershed_por_wb_svg_list,
                 all_wb_data,
                 view_box = wb_svg_size,
                 titles = titles)
  
  svg <- deps[["visualize_map"]]
  
  # add alttext and title before the children of the root element
  xml2::xml_add_sibling(xml2::xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml2::xml_add_sibling(xml2::xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  
  vb <- as.numeric(strsplit(xml2::xml_attr(svg, 'viewBox'),'[ ]')[[1]]) # can be used for portrait vs landscape, see Maria
  # Expand viewbox to include svgs to be added.
  vb[4] <- vb[4] + wb_svg_size[4]
  xml2::xml_attr(svg, 'viewBox') <- paste(vb, collapse = " ")
  
  non_geo_top <- xml2::xml_find_first(svg, "//*[local-name()='g'][@id='non-geo-top']")
  
  for(hu in names(svgs)) {
    wb_svg <- xml2::xml_add_child(non_geo_top, xml2::read_xml(whisker::whisker.render(template, svgs[[hu]])))
  
    xml2::xml_attr(wb_svg, "id") <- paste0("wb-bar-", hu)
    xml2::xml_attr(wb_svg, "transform") <- paste0("translate(0,",
                                               (vb[4] - wb_svg_size[4]), 
                                               ")scale(1)")
    xml2::xml_attr(wb_svg, "class") <- "nill"
  }
  
  xml2::write_xml(svg, viz[['location']])
}

build_watershed_por_wb_svg_list <- function(wb, all_wb_data, view_box = c(0, 0, 288, 288), titles) {
  min_x <- view_box[1]
  min_y <- view_box[2]
  max_x <- view_box[3]
  max_y <- view_box[4]  
  
  wb_data <- tidyr::spread(all_wb_data[[wb]][c("group", "value")], key = "group", value = "value")

  # these are just for code clarity

  pr <- wb_data$Precipitation 
  et <- wb_data$Evapotranspiration
  q <- wb_data$Streamflow
  
  u <- pr - et - q
  
  # ET is too big in some places. Adjust like this?
  if(et > (pr - q)) {
    max_bar <- et + q
    u = 0
  } else {
    max_bar <- pr
  }
  
  # Constants
  # Fractions are relative to 288
  margin <- 15 # 0.052
  x_tick_allow <- 25 # 0.087
  x_tick_text_adj <- 3 # 0.01
  y_tick_allow <- 25 # 0.087
  y_tick_text_x <- 30
  
  title_x = y_tick_text_x
  title_y = margin / 2
  
  rect_w <- 60 # 0.2
  
  # Ticks
  top_tick <- floor(pr / 10) * 10
  bottom_tick <- top_tick / 2
  
  #fictitious "max_bar"
  max_bar_y <- margin
  max_bar_h <- (max_y - min_y) - (2 * margin + x_tick_allow)
  
  # Relative calculations
  p_rect_x <- margin + x_tick_allow 
  p_rect_y <- max_bar_y + ((max_bar - pr) / max_bar) * max_bar_h
  p_rect_h <- max_bar_h * pr / max_bar
  
  ueq_rect_x <- p_rect_x + rect_w
  
  u_rect_y <- max_bar_y
  u_rect_h <- max_bar_h * u / max_bar 
  
  e_rect_y <- u_rect_y + u_rect_h 
  e_rect_h <- max_bar_h * et / max_bar
  
  q_rect_y <- e_rect_y + e_rect_h
  q_rect_h <- max_bar_h * q / max_bar
  
  top_tick_text_y <- p_rect_y + max_bar_h - max_bar_h * top_tick / max_bar
  bottom_tick_text_y <- p_rect_y + max_bar_h - max_bar_h * bottom_tick / max_bar
  top_tick_y <- top_tick_text_y - 1
  bottom_tick_y <- bottom_tick_text_y - 1
  tick_x1 <- y_tick_text_x + 2
  tick_x2 <- tick_x1 + 5
  
  in_x <- p_rect_x + rect_w / 2
  in_y <- out_y <- p_rect_y + max_bar_h + y_tick_allow / 2
  out_x <- ueq_rect_x + rect_w / 2
  
  y_units_x <- margin
  y_units_y <- max_x / 2.5
  
  template_list <-list(title_x = title_x, title_y = title_y, title = titles[[wb]],
       rect_w = rect_w,
       p_rect_h = p_rect_h, p_rect_x = p_rect_x, p_rect_y = p_rect_y,
       ueq_rect_x = ueq_rect_x,
       u_rect_y = u_rect_y, u_rect_h = u_rect_h,
       q_rect_y = q_rect_y, q_rect_h = q_rect_h,
       e_rect_y = e_rect_y, e_rect_h = e_rect_h,
       top_tick_text_y = top_tick_text_y, bottom_tick_text_y = bottom_tick_text_y,
       y_tick_text_x = y_tick_text_x, bottom_tick_y = bottom_tick_y,
       top_tick_y = top_tick_y, bottom_tick = bottom_tick, top_tick = top_tick,
       tick_x1 = tick_x1, tick_x2 = tick_x2,
       in_x = in_x, in_y = in_y, out_x = out_x, out_y = out_y,
       y_units_x = y_units_x, y_units_y = y_units_y)
  
  template_list
}

visualize.visualize_watershed_por_wb_data <- function(viz = as.viz("visualize_watershed_por_wb_data")) {
  
  deps <- readDepends(viz)
  required <- c("process_watershed_por_wb_data", "fetch_wb_bar_template", "visualize_map")
  checkRequired(deps, required)
  
  all_wb_data <- deps[["process_watershed_por_wb_data"]]
  
  template <- deps[["fetch_wb_bar_template"]]
  
  hus <- setNames(names(all_wb_data), names(all_wb_data))
  
  wb_svg_size <- c(0, 0, 288, 288)
  svgs <- lapply(X = hus,
                 FUN = build_watershed_por_wb_svg_list,
                 all_wb_data,
                 view_box = wb_svg_size)
  
  svg <- deps[["visualize_map"]]
  
  vb <- as.numeric(strsplit(xml2::xml_attr(svg, 'viewBox'),'[ ]')[[1]]) # can be used for portrait vs landscape, see Maria
  # Expand viewbox to include svgs to be added.
  vb[4] <- vb[4] + wb_svg_size[4]
  xml2::xml_attr(svg, 'viewBox') <- paste(vb, collapse = " ")
  
  non_geo_top <- xml2::xml_find_first(svg, "//*[local-name()='g'][@id='non-geo-top']")
  
  hu <- names(svgs)[1]
  # will loop this once figured out
  wb_svg <- xml2::xml_add_child(non_geo_top, xml2::read_xml(whisker::whisker.render(template, svgs[[hu]])))
  
  xml2::xml_attr(wb_svg, "id") <- paste0("wb-bar-", hu)
  xml2::xml_attr(wb_svg, "transform") <- paste0("translate(0,",
                                               vb[4] - wb_svg_size[4], 
                                               ")scale(1)")
  xml2::write_xml(svg, viz[['location']])
  
}

build_watershed_por_wb_svg_list <- function(wb, all_wb_data, view_box = c(0, 0, 288, 288)) {
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
    et = pr - q
    u = 0
  }
  
  # Constants
  # Fractions are relative to 288
  margin <- 15 # 0.052
  x_tick_allow <- 25 # 0.087
  x_tick_text_adj <- 3 # 0.01
  y_tick_allow <- 25 # 0.087
  y_tick_text_x <- 20 # 0.07
  
  rect_w <- 60 # 0.2
  
  in_x <- 66 # 0.23
  out_x <- 124 # 0.43
  inout_y <- max_y - 20 # 0.07
  
  # Ticks
  top_tick <- floor(pr / 10) * 10
  bottom_tick <- top_tick / 2
  
  # Relative calculations
  p_rect_x <- margin + x_tick_allow 
  p_rect_y <- margin
  p_rect_h <- (max_y - min_y) - (2 * margin + x_tick_allow)
  
  ueq_rect_x <- p_rect_x + rect_w
  
  u_rect_y <- p_rect_y
  u_rect_h <- p_rect_h * u / pr 
  
  e_rect_y <- u_rect_y + u_rect_h 
  e_rect_h <- p_rect_h * et / pr
  
  q_rect_y <- e_rect_y + e_rect_h
  q_rect_h <- p_rect_h * q / pr
  
  top_tick_y <- p_rect_y + p_rect_h - p_rect_h * top_tick / pr
  bottom_tick_y <- p_rect_y + p_rect_h - p_rect_h * bottom_tick / pr
  
  template_list <-list(rect_w = rect_w,
       p_rect_h = p_rect_h, p_rect_x = p_rect_x, p_rect_y = p_rect_y,
       ueq_rect_x = ueq_rect_x,
       u_rect_y = u_rect_y, u_rect_h = u_rect_h,
       q_rect_y = q_rect_y, q_rect_h = q_rect_h,
       e_rect_y = e_rect_y, e_rect_h = e_rect_h,
       y_tick_text_x = y_tick_text_x, bottom_tick_y = bottom_tick_y,
       top_tick_y = top_tick_y, bottom_tick = bottom_tick, top_tick = top_tick)
  
  template_list
}

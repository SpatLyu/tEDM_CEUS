.process_xmap_result = \(g,type = c("xmap","pxmap")){
  type = match.arg(type)
  tempdf = g[[type]]
  tempdf$x = g$varname[1]
  tempdf$y = g$varname[2]
  tempdf = tempdf |> 
    dplyr::select(1, x, y,
                  x_xmap_y_mean,x_xmap_y_sig,
                  y_xmap_x_mean,y_xmap_x_sig,
                  dplyr::everything()) |> 
    dplyr::slice_tail(n = 1)
  
  g1 = tempdf |>
    dplyr::select(x,y,y_xmap_x_mean,y_xmap_x_sig)|>
    purrr::set_names(c("cause","effect","cs","sig"))
  g2 = tempdf |>
    dplyr::select(y,x,x_xmap_y_mean,x_xmap_y_sig) |>
    purrr::set_names(c("cause","effect","cs","sig"))
  
  return(rbind(g1,g2))
}

plot_cs_matrix = \(.tbf,legend_title = expression(rho)){
  .tbf = .tbf |>
    dplyr::mutate(sig_marker = dplyr::case_when(
      sig > 0.05 ~ sprintf("paste(%.4f^'#')", cs),
      TRUE ~ sprintf('%.4f', cs)
    ))
  
  fig = ggplot2::ggplot(data = .tbf,
                        ggplot2::aes(x = effect, y = cause)) +
    ggplot2::geom_tile(color = "black", ggplot2::aes(fill = cs)) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         color = "black", linewidth = 0.25) +
    ggplot2::geom_text(ggplot2::aes(label = sig_marker), parse = TRUE,
                       color = "black", family = "serif") +
    ggplot2::labs(x = "Effect", y = "Cause", fill = legend_title) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_fill_gradient(low = "#9bbbb8", high = "#256c68") +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, family = "serif"),
      axis.text.y = ggplot2::element_text(color = "black", family = "serif"),
      axis.title.y = ggplot2::element_text(angle = 90, family = "serif"),
      axis.title.x = ggplot2::element_text(color = "black", family = "serif",
                                           margin = ggplot2::margin(t = 5.5, unit = "pt")),
      legend.text = ggplot2::element_text(family = "serif"),
      legend.title = ggplot2::element_text(family = "serif"),
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      #legend.direction = "horizontal",
      #legend.position = "bottom",
      legend.margin = ggplot2::margin(t = 1, r = 0, b = 0, l = 0, unit = "pt"),
      legend.key.width = ggplot2::unit(20, "pt"),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA)
    )
  return(fig)
}
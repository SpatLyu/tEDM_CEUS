set.seed(123)

chaos = tibble::tibble(
  t = 1:200,
  x = sin(2 * pi * t / 20) + rnorm(200, sd = 0.1),
  y = cos(2 * pi * t / 20 + 1) + rnorm(200, sd = 0.1)
)

fig_x = ggplot2::ggplot(data = chaos) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = x),color = "#c7e0f6") +
  ggplot2::scale_y_continuous(limits = c(-2.5,2.5)) +
  ggplot2::theme_void()

fig_y = ggplot2::ggplot(data = chaos) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = y),color = "#f8cccb") +
  ggplot2::scale_y_continuous(limits = c(-2.5,2.5)) +
  ggplot2::theme_void()

fig_x + ggview::canvas(2.5,1.05,bg = "transparent",dpi = 300)
fig_y + ggview::canvas(2.5,1.05,bg = "transparent",dpi = 300)

ggview::save_ggplot(fig_x + ggview::canvas(2.5,1.05,bg = "transparent",dpi = 300),
                    './Schematic for temporal empirical dynamic modeling/x.jpg')
ggview::save_ggplot(fig_y + ggview::canvas(2.5,1.05,bg = "transparent",dpi = 300),
                    './Schematic for temporal empirical dynamic modeling/y.jpg')
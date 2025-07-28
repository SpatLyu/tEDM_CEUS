# --- Time series x and y (origin state space) ---
set.seed(123)

chaos = tibble::tibble(
  t = 1:200,
  x = sin(2 * pi * t / 20) + rnorm(200, sd = 0.1),
  y = cos(2 * pi * t / 20 + 1) + rnorm(200, sd = 0.1)
)

fig_x = ggplot2::ggplot(data = chaos) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = x),color = "#c7e0f6",linewidth = 1.05) +
  ggplot2::scale_y_continuous(limits = c(-2.5,2.5)) +
  ggplot2::annotate("segment",
                     x = 0, xend = 210,
                     y = -1.5, yend = -1.5,
                     arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                            ends = "last", type = "closed"),
                     color = "grey40", linewidth = 0.45) +
  ggplot2::annotate("segment",
                    x = 0, xend = 0,
                    y = -1.5, yend = 1.85,
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                           ends = "last", type = "closed"),
                    color = "grey40", linewidth = 0.45) +
  ggplot2::theme_void()
fig_x + ggview::canvas(2.5,1.05,bg = "transparent",dpi = 300)

fig_y = ggplot2::ggplot(data = chaos) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = y),color = "#f8cccb",linewidth = 1.05) +
  ggplot2::scale_y_continuous(limits = c(-2.5,2.5)) +
  ggplot2::annotate("segment",
                    x = 0, xend = 210,
                    y = -1.5, yend = -1.5,
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                           ends = "last", type = "closed"),
                    color = "grey40", linewidth = 0.45) +
  ggplot2::annotate("segment",
                    x = 0, xend = 0,
                    y = -1.5, yend = 1.85,
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                           ends = "last", type = "closed"),
                    color = "grey40", linewidth = 0.45) +
  ggplot2::theme_void()

fig_y + ggview::canvas(2.5,1.05,bg = "transparent",dpi = 300)

ggview::save_ggplot(fig_x + ggview::canvas(2.5,1.05,bg = "transparent",dpi = 300),
                    './Schematic for temporal empirical dynamic modeling/x.png')
ggview::save_ggplot(fig_y + ggview::canvas(2.5,1.05,bg = "transparent",dpi = 300),
                    './Schematic for temporal empirical dynamic modeling/y.png')

# --- Reconstructed attractors ---
simulate_attractor = function(f, x0, y0, z0, steps = 5000, dt = 0.01) {
  x = numeric(steps)
  y = numeric(steps)
  z = numeric(steps)
  x[1] = x0; y[1] = y0; z[1] = z0
  for (i in 1:(steps - 1)) {
    d = f(x[i], y[i], z[i])
    x[i + 1] = x[i] + dt * d$dx
    y[i + 1] = y[i] + dt * d$dy
    z[i + 1] = z[i] + dt * d$dz
  }
  cbind(x, y, z)
}

fLorenz = function(x, y, z, sigma = 10, rho = 28, beta = 8 / 3) {
  dx = sigma * (y - x)
  dy = x * (rho - z) - y
  dz = x * y - beta * z
  list(dx = dx, dy = dy, dz = dz)
}

lorenz = simulate_attractor(fLorenz, 0, 0.1, 0, steps = 5000)

# Plot M view
png("./Schematic for temporal empirical dynamic modeling/M.png", 
    width = 1600, height = 1600, res = 300, bg = "transparent")
plot3D::lines3D(lorenz[,1], lorenz[,2], lorenz[,3], colvar = NULL, col = "#e6a922",
                theta = 10, phi = 0, pch = 19, lwd = 1.25, bty = "n", axes = FALSE)
dev.off()

# Plot MX view
png("./Schematic for temporal empirical dynamic modeling/Mx.png", 
    width = 1600, height = 1600, res = 300, bg = "transparent")
plot3D::lines3D(lorenz[,1], lorenz[,2], lorenz[,3], colvar = NULL, col = "#c7e0f6",
                theta = 60, phi = 85, pch = 19, lwd = 1.25, bty = "n", axes = FALSE)
dev.off()

# Plot MY view
png("./Schematic for temporal empirical dynamic modeling/My.png",
    width = 1600, height = 1600, res = 300, bg = "transparent")
plot3D::lines3D(lorenz[,1], lorenz[,2], lorenz[,3], colvar = NULL, col = "#f8cccb",
                theta = 60, phi = -95, pch = 19, lwd = 1.25, bty = "n", axes = FALSE)
dev.off()

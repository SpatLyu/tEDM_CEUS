# --- Lorenz attractors ---
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

# --- Time series x and y (origin state space) ---
chaos = tibble::as_tibble(lorenz[1751:3500,]) |> 
  dplyr::mutate(dplyr::across(dplyr::everything(),
                              \(.x) sdsfun::normalize_vector(.x,-1,1))) |> 
  dplyr::mutate(t = 1:1750)

ggplot2::ggplot(data = chaos) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = x),
                     color = "#c7e0f6", linewidth = 0.55)

ggplot2::ggplot(data = chaos) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = y),
                     color = "#c7e0f6", linewidth = 0.55)

fig_x = ggplot2::ggplot(data = chaos) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = x),color = "#c7e0f6",linewidth = 0.55) +
  ggplot2::scale_y_continuous(limits = c(-1.2,1.2)) +
  ggplot2::annotate("segment",
                     x = 0, xend = 1800,
                     y = -1.05, yend = -1.05,
                     arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                            ends = "last", type = "closed"),
                     color = "grey40", linewidth = 0.15) +
  ggplot2::annotate("segment",
                    x = 0, xend = 0,
                    y = -1.05, yend = 1.05,
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                           ends = "last", type = "closed"),
                    color = "grey40", linewidth = 0.15) +
  ggplot2::theme_void()
fig_x + ggview::canvas(2.5,1.05,dpi = 300)

fig_y = ggplot2::ggplot(data = chaos) +
  ggplot2::geom_line(ggplot2::aes(x = t, y = y),color = "#f8cccb",linewidth = 0.55) +
  ggplot2::scale_y_continuous(limits = c(-1.2,1.2)) +
  ggplot2::annotate("segment",
                     x = 0, xend = 1800,
                     y = -1.05, yend = -1.05,
                     arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                            ends = "last", type = "closed"),
                     color = "grey40", linewidth = 0.15) +
  ggplot2::annotate("segment",
                    x = 0, xend = 0,
                    y = -1.05, yend = 1.05,
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                           ends = "last", type = "closed"),
                    color = "grey40", linewidth = 0.15) +
  ggplot2::theme_void()
fig_y + ggview::canvas(2.5,1.05,dpi = 300)

ggview::save_ggplot(fig_x + ggview::canvas(2.5,1.05,dpi = 300),
                    './Schematic for temporal empirical dynamic modeling/x.png')
ggview::save_ggplot(fig_y + ggview::canvas(2.5,1.05,dpi = 300),
                    './Schematic for temporal empirical dynamic modeling/y.png')

# --- Plot M view ---
png("./Schematic for temporal empirical dynamic modeling/M.png", 
    width = 1600, height = 1600, res = 300, bg = "white")
plot3D::lines3D(lorenz[,1], lorenz[,2], lorenz[,3], colvar = NULL, col = "#e6a922",
                theta = 10, phi = 0, pch = 19, lwd = 1.25, bty = "n", axes = FALSE)
dev.off()

# --- Reconstruct shadow manifolds ---
GenStateSpace = \(ts, E = 3, tau = 1) {
  # Input validation
  if (!is.numeric(ts)) {
    stop("Time series must be numeric", call. = FALSE)
  }
  if (E < 2) {
    stop("E must be an integer greater than 1", call. = FALSE)
  }
  if (tau < 1) {
    stop("Time delay must be positive", call. = FALSE)
  }

  # Create embedding matrix
  M = matrix(NA_real_, length(ts) - (E - 1) * tau , E)
  for (i in 1:nrow(M)) {
    M_vec = ts[seq(from = i, to = i + (E - 1) * tau, by = tau)]
    if (!anyNA(M_vec)) {
      M[i,] = M_vec
    }
  }
  return(M)
}

Mx = GenStateSpace(lorenz[,"x"],E = 20,tau = 3)
My = GenStateSpace(lorenz[,"y"],E = 20,tau = 3)

# --- Plot Mx view ---
png("./Schematic for temporal empirical dynamic modeling/Mx.png", width = 1600, height = 1600, res = 300, bg = "white")
plot3D::scatter3D(Mx[,3], Mx[,6], Mx[,9],
                  colvar = NULL, pch = 19, col = "#c7e0f6",
                  theta = 30, phi = 0, cex = 0.25, bty = "n")
dev.off()

# --- Plot MY view ---
png("./Schematic for temporal empirical dynamic modeling/My.png", width = 1600, height = 1600, res = 300, bg = "white")
plot3D::scatter3D(Mz[,3], Mz[,6], Mz[,9],
                  colvar = NULL, pch = 19, col = "#f8cccb",
                  theta = 10, phi = 0, cex = 0.25, bty = "n")
dev.off()

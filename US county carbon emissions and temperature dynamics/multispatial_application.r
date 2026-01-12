library(tEDM)

carbon = readr::read_csv(system.file("case/carbon.csv.gz",package = "tEDM"))
head(carbon)

#-----------------------------------------------------------------------------#
#------               Convergent Cross Mapping analysis                 ------#
#-----------------------------------------------------------------------------#

carbon_list = dplyr::group_split(carbon, by = fips)
length(carbon_list)

# purrr::map(carbon_list,
#            \(.x) tEDM::fnn(.x, "carbon", E = 2:10,
#                            eps = stats::sd(.x$carbon)))

tEDM::fnn(carbon_list[[100]],"carbon",E = 2:10,
          eps = stats::sd(carbon_list[[100]]$carbon))

res = carbon_list |>
  purrr::map_dfr(\(.x) {
    g = tEDM::ccm(.x,"tem","carbon",E = 3,k = 4,dist.metric = "L2",progressbar = FALSE)
    return(g$xmap)
  })
head(res)

res_ccm = res |>
  dplyr::select(libsizes,
                carbon_tem = x_xmap_y_mean,
                tem_carbon = y_xmap_x_mean) |>
  tidyr::pivot_longer(c(carbon_tem, tem_carbon),
                      names_to = "variable", 
                      values_to = "value")
res_ccm$variable = factor(res_ccm$variable,
                          levels = c("carbon_tem", "tem_carbon"),
                          labels = c("carbon → tem", "tem → carbon"))
head(res_ccm)
readr::write_rds(res_ccm,"./US county carbon emissions and temperature dynamics/res_ccm.rds")

res_ccm = readr::read_rds("./US county carbon emissions and temperature dynamics/res_cmc.rds")
fig_ccm = ggplot2::ggplot(res_ccm,
                          ggplot2::aes(x = variable, y = value, fill = variable)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", color = "red", linewidth = 0.8) +
  ggplot2::theme_bw() +
  ggplot2::scale_x_discrete(name = "") +
  ggplot2::scale_y_continuous(name = "Cross Mapping Skill",
                              expand = c(0,0),
                              limits = c(-1,1),
                              breaks = seq(-1,1,by = 0.2)) +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(size = 12),
                 axis.text.y = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(size = 12.5))
fig_ccm + ggview::canvas(4.5,4.5,dpi = 300)
ggview::save_ggplot(fig_ccm + ggview::canvas(4.5,4.5,dpi = 300),
                    "./US county carbon emissions and temperature dynamics/carbon_us_ccm.pdf",
                    device = cairo_pdf)

#-----------------------------------------------------------------------------#
#------                   Multispatial CCM analysis                     ------#
#-----------------------------------------------------------------------------#

res_gc = carbon_list |>
  purrr::map_dfr(\(.x) {
    f1 = lmtest::grangertest(tem ~ carbon, order = 3, data = .x)
    f2 = lmtest::grangertest(carbon ~ tem, order = 3, data = .x)
    return(tibble::tibble(
      carbon_tem = f1$`Pr(>F)`[2],
      tem_carbon = f2$`Pr(>F)`[2]
    ))
  }) |>
  tidyr::pivot_longer(c(carbon_tem, tem_carbon),
                      names_to = "variable",
                      values_to = "value")
res_gc$variable = factor(res_gc$variable,
                         levels = c("carbon_tem", "tem_carbon"),
                         labels = c("carbon → tem", "tem → carbon"))
head(res_gc)
readr::write_rds(res_gc,"./US county carbon emissions and temperature dynamics/res_gc.rds")

res_gc = readr::read_rds("./US county carbon emissions and temperature dynamics/res_gc.rds")
fig_gc = ggplot2::ggplot(res_gc,
                         ggplot2::aes(x = variable, y = value, fill = variable)) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.7) + 
  ggplot2::geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +  
  ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 0.8) +
  ggplot2::theme_bw() +
  ggplot2::scale_x_discrete(name = "") +
  ggplot2::scale_y_continuous(
    name = "P Value for Granger Causality",
    expand = c(0, 0),
    limits = c(0, 1),
    breaks = c(0, 0.05, 0.1, 0.5, 1) 
  ) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 12),
                 axis.text.y = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(size = 12.5)
  )
fig_gc + ggview::canvas(4.5, 4.5, dpi = 300)
ggview::save_ggplot(fig_gc + ggview::canvas(4.5, 4.5, dpi = 300),
                    "./US county carbon emissions and temperature dynamics/carbon_us_gc.pdf",
                    device = cairo_pdf)

library(tEDM)

carbon = readr::read_csv(system.file("case/carbon.csv.gz",package = "tEDM"))
head(carbon)

carbon_list = dplyr::group_split(carbon, by = fips)
length(carbon_list)

tEDM::fnn(carbon_list[[100]],"carbon",E = 2:10,
          eps = stats::sd(carbon_list[[100]]$carbon))

res = carbon_list |>
  purrr::map_dfr(\(.x) {
    g = tEDM::cmc(.x,"tem","carbon",E = 3,k = 20,progressbar = FALSE)
    return(g$xmap)
  })
head(res)

res_carbon = res |>
  dplyr::select(neighbors,
                carbon_tem = x_xmap_y_mean,
                tem_carbon = y_xmap_x_mean) |>
  tidyr::pivot_longer(c(carbon_tem, tem_carbon),
                      names_to = "variable", values_to = "value")
head(res_carbon)

fig_county_us = ggplot2::ggplot(res_carbon,
                            ggplot2::aes(x = variable, y = value, fill = variable)) +
  ggplot2::geom_boxplot() +
  ggplot2::theme_bw() +
  ggplot2::scale_x_discrete(name = "",
                            labels = c("carbon_tem" = "carbon → tem",
                                       "tem_carbon" = "tem → carbon")) +
  ggplot2::scale_y_continuous(name = "Causal Strength") +
  ggplot2::theme(legend.position = "none")

fig_county_us + ggview::canvas(4.5,4.5,dpi = 300)
ggview::save_ggplot(fig_county_us + ggview::canvas(4.5,4.5,dpi = 300),
                    "./US county carbon emissions and temperature dynamics/carbon_us.pdf",
                    device = cairo_pdf)

library(tEDM)

carbon = readr::read_csv(system.file("case/carbon.csv.gz",package = "tEDM"))
head(carbon)

carbon = carbon |> 
  dplyr::mutate(fips = as.character(fips)) |> 
  dplyr::mutate(fips = stringr::str_pad(fips, width = 5, side = "left", pad = "0"))
carbon_list = dplyr::group_split(carbon, by = fips)
names(carbon_list) = purrr::map_chr(carbon_list, \(.l) unique(.l$fips))

# us_counties = tigris::counties(resolution = "20m", year = 2017) |> 
#   dplyr::select(fips = GEOID, name = NAME)
# us_counties = tibble::tibble(fips = names(carbon_list)) |> 
#   dplyr::left_join(us_counties,by = "fips") |> 
#   sf::st_as_sf()
# plot(sf::st_geometry(us_counties))

# nb = sdsfun::spdep_nb(us_counties,k = 8)
# spdep::write.nb.gal(nb,'./US county carbon emissions and temperature dynamics/us_county.gal')

#-----------------------------------------------------------------------------#
#------               Convergent Cross Mapping analysis                 ------#
#-----------------------------------------------------------------------------#

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

res_ccm = readr::read_rds("./US county carbon emissions and temperature dynamics/res_ccm.rds")
fig_ccm = ggplot2::ggplot(res_ccm,
                          ggplot2::aes(x = variable, y = abs(value), fill = variable)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", color = "red", linewidth = 0.8) +
  ggplot2::theme_bw() +
  ggplot2::scale_x_discrete(name = "") +
  ggplot2::scale_y_continuous(name = "Cross Mapping Skill (CCM)",
                              expand = c(0,0),
                              limits = c(0,1),
                              breaks = seq(0,1,by = 0.2)) +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(size = 12),
                 axis.text.y = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(size = 12.5))
fig_ccm + ggview::canvas(4.5,4.5,dpi = 300)

#-----------------------------------------------------------------------------#
#------                   Multispatial CCM analysis                     ------#
#-----------------------------------------------------------------------------#

nb = spdep::read.gal('./US county carbon emissions and temperature dynamics/us_county.gal')

construct_multiobs = \(.i){
  obs = carbon_list[c(nb[[.i]],.i)]
    suppressMessages({
      obs_plot = list("tem" = purrr::map_dfc(obs,\(.x) dplyr::select(.x,tem)), 
                      "carbon" = purrr::map_dfc(obs,\(.x) dplyr::select(.x,carbon)))
    })
  return(obs_plot)
}

tEDM::simplex(construct_multiobs(1),"tem","carbon",E = 3:10,k = 4:15)

res_multispatialccm = seq_along(carbon_list) |> 
  purrr::map_dfr(\(.i) {
    obs = construct_multiobs(.i)
    g = tEDM::multispatialccm(obs,"tem","carbon",libsizes = length(obs),E = 4,k = 5,
                              boot = 1,dist.metric = "L1",progressbar = FALSE)
    return(g$xmap)
  }) |>
  dplyr::select(libsizes,
                carbon_tem = x_xmap_y_mean,
                tem_carbon = y_xmap_x_mean) |>
  tidyr::pivot_longer(c(carbon_tem, tem_carbon),
                      names_to = "variable", 
                      values_to = "value")

res_multispatialccm$variable = factor(res_multispatialccm$variable,
                                      levels = c("carbon_tem", "tem_carbon"),
                                      labels = c("carbon → tem", "tem → carbon"))
head(res_multispatialccm)
readr::write_rds(res_multispatialccm,"./US county carbon emissions and temperature dynamics/res_multispatialccm.rds")

res_multispatialccm = readr::read_rds("./US county carbon emissions and temperature dynamics/res_multispatialccm.rds")
fig_multispatialccm = ggplot2::ggplot(res_multispatialccm,
                                      ggplot2::aes(x = variable, y = abs(value), fill = variable)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", color = "red", linewidth = 0.8) +
  ggplot2::theme_bw() +
  ggplot2::scale_x_discrete(name = "") +
  ggplot2::scale_y_continuous(name = "Cross Mapping Skill (Multispatial CCM)",
                              expand = c(0,0),
                              limits = c(0,1),
                              breaks = seq(0,1,by = 0.2)) +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(size = 12),
                 axis.text.y = ggplot2::element_text(size = 12),
                 axis.title.y = ggplot2::element_text(size = 12.5))
fig_multispatialccm + ggview::canvas(4.5,4.5,dpi = 300)

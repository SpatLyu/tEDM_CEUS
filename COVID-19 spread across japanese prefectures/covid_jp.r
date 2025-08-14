library(tEDM)

covid = readr::read_csv(system.file("case/covid.csv",package = "tEDM"))
head(covid)

covid = covid |>
  dplyr::mutate(dplyr::across(dplyr::everything(),
                              \(.x) c(NA,diff(.x))))

tEDM::fnn(covid,"Tokyo",E = 2:30,eps = stats::sd(covid$Tokyo)/10)

tEDM::simplex(covid,"Tokyo","Tokyo",E = 4:50,k = 5:60)

res = names(covid)[-match("Tokyo",names(covid))] |>
  purrr::map_dfr(\(.l) {
    g = tEDM::ccm(covid,"Tokyo",.l,E = 4,k = 7,progressbar = FALSE)
    res = dplyr::mutate(g$xmap,x = "Tokyo",y = .l)
    return(res)
  })
head(res)

df1 = res |>
  dplyr::select(x,y,y_xmap_x_mean,y_xmap_x_sig)|>
  purrr::set_names(c("cause","effect","cs","sig"))
df2 = res |>
  dplyr::select(y,x,x_xmap_y_mean,x_xmap_y_sig) |>
  purrr::set_names(c("cause","effect","cs","sig"))
res_covid = dplyr::bind_rows(df1,df2)|>
  dplyr::filter(cause == "Tokyo") |>
  dplyr::arrange(dplyr::desc(cs))
head(res_covid,10)

res_covid = res_covid |>
  dplyr::mutate(cs = round(res_covid$cs,2)) |>
  dplyr::filter(cs >= 0.90)
res_covid

if (!requireNamespace("rnaturalearth")) {
  install.packages("rnaturalearth")
}
jp = rnaturalearth::ne_states(country = "Japan")

if (!requireNamespace("tidygeocoder")) {
  install.packages("tidygeocoder")
}
jpp = tibble::tibble(name = c("Tokyo",res_covid$effect)) |>
  dplyr::mutate(type = factor(c("source",rep("target",6)),
                              levels = c("source","target"))) |> 
  tidygeocoder::geocode(state = name, method = "arcgis",
                        long = "lon", lat = "lat")

fig_covid_jp = ggplot2::ggplot() +
  ggplot2::geom_sf(data = jp, fill = "#ffe7b7", color = "grey", linewidth = 0.45) +
  ggplot2::geom_curve(data = jpp[-1,],
                      ggplot2::aes(x = jpp[1,"lon",drop = TRUE],
                                   y = jpp[1,"lat",drop = TRUE],
                                   xend = lon, yend = lat),
                      curvature = 0.2,
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                      color = "#6eab47", linewidth = 1) +
  ggplot2::geom_point(data = jpp,
                      ggplot2::aes(x = lon, y = lat, color = type),
                      size = 1.25, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = jpp, 
                           ggplot2::aes(label = name, x = lon, y = lat, color = type),
                           show.legend = FALSE) +
  ggplot2::scale_color_manual(values = c(source = "#2c74b7", 
                                         target = "#cf574b")) +
  ggplot2::coord_sf(xlim = range(jpp$lon) + c(-0.45,0.45),
                    ylim = range(jpp$lat) + c(-0.75,0.75)) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#9cd1fd", color = NA))

fig_covid_jp + ggview::canvas(5.55,3.15,dpi = 300)
ggview::save_ggplot(fig_covid_jp + ggview::canvas(5.55,3.15,dpi = 300),
                    './COVID-19 spread across japanese prefectures/covid_jp.pdf',
                    device = cairo_pdf)

library(tEDM)

cvd = readr::read_csv(system.file("case/cvd.csv",package = "tEDM"))
head(cvd)

cvd_long = cvd |>
  tibble::rowid_to_column("id") |>
  tidyr::pivot_longer(cols = -id,
                      names_to = "variable", values_to = "value")

fig_cvd_ts = ggplot2::ggplot(cvd_long, ggplot2::aes(x = id, y = value, color = variable)) +
  ggplot2::geom_line(linewidth = 0.5) +
  ggplot2::labs(x = "Days (from 1995-3 to 1997-11)", y = "Concentrations or \nNO. of CVD admissions", color = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.direction = "horizontal",
                 legend.position = "inside",
                 legend.justification = c("center","top"),
                 legend.background = ggplot2::element_rect(fill = "transparent", color = NA))
fig_cvd_ts + ggview::canvas(6.5,3.75)

tEDM::fnn(cvd,"cvd",E = 2:50,eps = stats::sd(cvd$cvd))

tEDM::simplex(cvd,"cvd","cvd",E = 11:25,k = 12:26)
tEDM::simplex(cvd,"rsp","rsp",E = 11:25,k = 12:26)
tEDM::simplex(cvd,"no2","no2",E = 11:25,k = 12:26)
tEDM::simplex(cvd,"so2","so2",E = 11:25,k = 12:26)
tEDM::simplex(cvd,"o3","o3",E = 11:25,k = 12:26)

s1 = tEDM::simplex(cvd,"cvd","cvd",E = 11,k = 12:26)
s2 = tEDM::simplex(cvd,"rsp","rsp",E = 11,k = 12:26)
s3 = tEDM::simplex(cvd,"no2","no2",E = 11,k = 12:26)
s4 = tEDM::simplex(cvd,"so2","so2",E = 11,k = 12:26)
s5 = tEDM::simplex(cvd,"o3","o3",E = 11,k = 12:26)

list(s1,s2,s3,s4,s5)

simplex_df = purrr::map2_dfr(list(s1,s2,s3,s4,s5),
                             c("cvd","rsp","no2","so2","o3"),
                             \(.list,.name) dplyr::mutate(.list$xmap,variable = .name))
ggplot2::ggplot(data = simplex_df) +
  ggplot2::geom_line(ggplot2::aes(x = k, y = rho, color = variable))

vars = c("cvd", "rsp", "no2", "so2", "o3")
res = list()
var_pairs = combn(vars, 2, simplify = FALSE)

for (pair in var_pairs) {
  var1 = pair[1]
  var2 = pair[2]
  conds = setdiff(vars, pair)
  key = paste0(var1, "_", var2)
  res[[key]] = tEDM::pcm(data = cvd,
                         cause = var2,
                         effect = var1,
                         conds = conds,
                         libsizes = seq(12, 1012, 100),
                         E = 11, k = 12)
}
readr::write_rds(res,'./Air pollution and cardiovascular health in hong kong/air_hk.rds')

source('./Air pollution and cardiovascular health in hong kong/utils.r')
res = readr::read_rds('./Air pollution and cardiovascular health in hong kong/air_hk.rds')
ccm_df = purrr::map_dfr(res,\(.list) .process_xmap_result(.list,type = "xmap"))
pcm_df = purrr::map_dfr(res,\(.list) .process_xmap_result(.list,type = "pxmap"))
fig_cs = plot_cs_matrix(ccm_df)
fig_pcs = plot_cs_matrix(pcm_df)
fig_causal_link = figpatch::fig('./Air pollution and cardiovascular health in hong kong/causal_link.pdf')

library(patchwork)

fig_cvds_hk = fig_cvd_ts / (fig_pcs | fig_causal_link) +
  plot_annotation(tag_levels = 'a')  &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold"))

fig_cvds_hk + ggview::canvas(7.65,6.5)
ggview::save_ggplot(fig_cvds_hk + ggview::canvas(7.65,6.5),
                    './Air pollution and cardiovascular health in hong kong/cvds_hk.pdf',
                    device = cairo_pdf)

res[["cvd_o3"]]
res[["cvd_no2"]]
res[["cvd_so2"]]
res[["cvd_rsp"]]
res[["no2_so2"]]
res[["rsp_o3"]]

plot(res[["cvd_no2"]],ylimits = c(0,0.4))

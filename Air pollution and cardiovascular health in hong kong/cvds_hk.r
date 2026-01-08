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

#-----------------------------------------------------------------------------#
#------                Partial Cross Mapping analysis                   ------#
#-----------------------------------------------------------------------------#

tEDM::fnn(cvd,"cvd",E = 2:50,eps = stats::sd(cvd$cvd))

tEDM::simplex(cvd,"cvd","cvd",E = 7:10,k = 8:12)
tEDM::simplex(cvd,"rsp","rsp",E = 7:10,k = 8:12)
tEDM::simplex(cvd,"no2","no2",E = 7:10,k = 8:12)
tEDM::simplex(cvd,"so2","so2",E = 7:10,k = 8:12)
tEDM::simplex(cvd,"o3","o3",E = 7:10,k = 8:12)

s1 = tEDM::simplex(cvd,"cvd","cvd",E = 7,k = 8:12)
s2 = tEDM::simplex(cvd,"rsp","rsp",E = 7,k = 8:12)
s3 = tEDM::simplex(cvd,"no2","no2",E = 7,k = 8:12)
s4 = tEDM::simplex(cvd,"so2","so2",E = 7,k = 8:12)
s5 = tEDM::simplex(cvd,"o3","o3",E = 7,k = 8:12)

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
                         dist.metric = "L2",
                         libsizes = seq(20, 1020, 100),
                         E = 7, k = 8)
}
readr::write_rds(res,'./Air pollution and cardiovascular health in hong kong/cvds_hk_pcm.rds')

source('./Air pollution and cardiovascular health in hong kong/utils.r')
res = readr::read_rds('./Air pollution and cardiovascular health in hong kong/cvds_hk_pcm.rds')
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
                    './Air pollution and cardiovascular health in hong kong/cvds_hk_pcm.pdf',
                    device = cairo_pdf)

# Convergence plot of cross mapping

theme_crossmapping = ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 30, size = 11),
                axis.text.y = ggplot2::element_text(size = 11),
                axis.title.x = ggplot2::element_text(size = 11),
                axis.title.y = ggplot2::element_text(size = 11),
                legend.text = ggplot2::element_text(size = 11),
                legend.position = "inside",
                legend.justification = c(-0.01,1))

fig_cvd_rsp = plot_ccm_output(res[["cvd_rsp"]]) + theme_crossmapping
fig_cvd_rsp_p = plot_pcm_output(res[["cvd_rsp"]]) + theme_crossmapping

fig_cvd_no2 = plot_ccm_output(res[["cvd_no2"]]) + theme_crossmapping
fig_cvd_no2_p = plot_pcm_output(res[["cvd_no2"]]) + theme_crossmapping

fig_cvd_so2 = plot_ccm_output(res[["cvd_so2"]]) + theme_crossmapping
fig_cvd_so2_p = plot_pcm_output(res[["cvd_so2"]]) + theme_crossmapping

fig_cvd_o3 = plot_ccm_output(res[["cvd_o3"]]) + theme_crossmapping
fig_cvd_o3_p = plot_pcm_output(res[["cvd_o3"]]) + theme_crossmapping

fig_rsp_no2 = plot_ccm_output(res[["rsp_no2"]],
                              ylimits = c(0.1,1), 
                              ybreaks = seq(0.1,1,0.1)) + theme_crossmapping
fig_rsp_no2_p = plot_pcm_output(res[["rsp_no2"]],
                                ylimits = c(0.1,1), 
                                ybreaks = seq(0.1,1,0.1)) + theme_crossmapping

fig_rsp_so2 = plot_ccm_output(res[["rsp_so2"]]) + theme_crossmapping
fig_rsp_so2_p = plot_pcm_output(res[["rsp_so2"]]) + theme_crossmapping

fig_rsp_o3 = plot_ccm_output(res[["rsp_o3"]]) + theme_crossmapping
fig_rsp_o3_p = plot_pcm_output(res[["rsp_o3"]]) + theme_crossmapping

fig_no2_so2 = plot_ccm_output(res[["no2_so2"]]) + theme_crossmapping
fig_no2_so2_p = plot_pcm_output(res[["no2_so2"]]) + theme_crossmapping

fig_no2_o3 = plot_ccm_output(res[["no2_o3"]]) + theme_crossmapping
fig_no2_o3_p = plot_pcm_output(res[["no2_o3"]]) + theme_crossmapping

fig_so2_o3 = plot_ccm_output(res[["so2_o3"]]) + theme_crossmapping
fig_so2_o3_p = plot_pcm_output(res[["so2_o3"]]) + theme_crossmapping

fig_appendix = cowplot::plot_grid(fig_cvd_rsp, fig_cvd_rsp_p, fig_cvd_no2, fig_cvd_no2_p,  
                                  fig_cvd_so2, fig_cvd_so2_p, fig_cvd_o3, fig_cvd_o3_p,  
                                  fig_rsp_no2, fig_rsp_no2_p, fig_rsp_so2, fig_rsp_so2_p, 
                                  fig_rsp_o3, fig_rsp_o3_p, fig_no2_so2, fig_no2_so2_p,  
                                  fig_no2_o3, fig_no2_o3_p, fig_so2_o3, fig_so2_o3_p,
                                  ncol = 4, label_fontfamily = 'serif',
                                  labels = letters[1:20],
                                  label_x = -0.005, label_y = 1)
fig_appendix + ggview::canvas(16.55,18.55)
ggview::save_ggplot(fig_appendix + ggview::canvas(16.55,18.55),
                    './Air pollution and cardiovascular health in hong kong/cvds_hk_convergence.pdf',
                    device = cairo_pdf)

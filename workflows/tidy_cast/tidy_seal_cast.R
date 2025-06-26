# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -c, --config  the name of the configuration file [default:
#                                                       /mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_cast/t02.00000.01.yaml]

suppressPackageStartupMessages({
  library(agua)
  library(charlier)
  library(argparser)
  library(dplyr)
  library(stars)
  library(brickman)
  library(twinkle)
  library(maxnet)
  library(maxnetic)
  library(ggplot2)
  library(rsample)
  library(parsnip)
  library(tidysdm)
  library(tidymodels)
  library(workflows)
  library(workflowsets)
  library(effectplots)
})

args = argparser::arg_parser("tidymodels/tidysdm casting for seal habitat suitability",
                             name = "tidy_seal_cast.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_cast/t12.000300.08.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
for (f in list.files(cfg$source_path, pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}

vpars = charlier::parse_version(cfg$version)
vpath = file.path(cfg$root_path, cfg$output_path, "versions", vpars[["major"]], vpars[["minor"]], cfg$version)
min_vpath = file.path(cfg$root_path, cfg$output_path, "versions", vpars[["major"]], vpars[["minor"]])
if (!dir.exists(vpath)) 
  dir.create(vpath, showWarnings = FALSE, recursive = TRUE)

charlier::start_logger(filename = file.path(vpath, "log"))
charlier::info("writing config")
charlier::write_config(cfg, filename = file.path(vpath, basename(args$config)))

bb = cofbb::get_bb("nefsc_carcharodon", form = "sf")
coast = rnaturalearth::ne_coastline(scale = "large", returnclass = "sf") |>
  sf::st_geometry()

if (cfg$thinned) {
  obs_bg = file.path(cfg$root_path, cfg$thinned_data_path, "thinned_obs_bg.gpkg")
} else {
  obs_bg = file.path(cfg$root_path, cfg$gather_data_path, "brickman_covar_obs_bg.gpkg")
}

obs = read_brickman_points(file = obs_bg) |>
  sf::st_as_sf() |>
  dplyr::filter(id == 1, basisOfRecord %in% cfg$obs_filter$basisOfRecord) |>
  dplyr::select(all_of(cfg$vars)) |>
  dplyr::filter(month %in% as.numeric(cfg$month)) |>
  dplyr::mutate(class = "presence")


var_list = list()

if ("brick_tbtm" %in% cfg$vars) {
  tbtm = stars::read_stars(file.path(cfg$data_path, cfg$brick_path, sprintf("%s_%s_Tbtm_mon.tif", cfg$scenario, cfg$year))) |>
    dplyr::rename(tbtm = sprintf("%s_%s_Tbtm_mon.tif", cfg$scenario, cfg$year))
  var_list[["tbtm"]] = tbtm
}
if ("brick_mld" %in% cfg$vars) {
  mld = stars::read_stars(file.path(cfg$data_path, cfg$brick_path, sprintf("%s_%s_MLD_mon.tif", cfg$scenario, cfg$year))) |>
    dplyr::rename(mld = sprintf("%s_%s_MLD_mon.tif", cfg$scenario, cfg$year))
  var_list[["mld"]] = mld
}
if ("brick_sss" %in% cfg$vars) {
  sss = stars::read_stars(file.path(cfg$data_path, cfg$brick_path, sprintf("%s_%s_SSS_mon.tif", cfg$scenario, cfg$year))) |>
    dplyr::rename(sss = sprintf("%s_%s_SSS_mon.tif", cfg$scenario, cfg$year))
  var_list[["sss"]] = sss
}
if ("brick_sbtm" %in% cfg$vars) {
  sbtm = stars::read_stars(file.path(cfg$data_path, cfg$brick_path, sprintf("%s_%s_Sbtm_mon.tif", cfg$scenario, cfg$year))) |>
    dplyr::rename(sbtm = sprintf("%s_%s_Sbtm_mon.tif", cfg$scenario, cfg$year))
  var_list[["sbtm"]] = sbtm
}
if ("brick_sst" %in% cfg$vars) {
  sst = stars::read_stars(file.path(cfg$data_path, cfg$brick_path, sprintf("%s_%s_SST_mon.tif", cfg$scenario, cfg$year))) |>
    dplyr::rename(sst = sprintf("%s_%s_SST_mon.tif", cfg$scenario, cfg$year))
  var_list[["sst"]] = sst
}
if ("brick_u" %in% cfg$vars) {
  u = stars::read_stars(file.path(cfg$data_path, cfg$brick_path, sprintf("%s_%s_U_mon.tif", cfg$scenario, cfg$year))) |>
    dplyr::rename(u = sprintf("%s_%s_U_mon.tif", cfg$scenario, cfg$year))
  var_list[["u"]] = u
}
if ("brick_v" %in% cfg$vars) {
  v = stars::read_stars(file.path(cfg$data_path, cfg$brick_path, sprintf("%s_%s_V_mon.tif", cfg$scenario, cfg$year))) |>
    dplyr::rename(v = sprintf("%s_%s_V_mon.tif", cfg$scenario, cfg$year)) 
  var_list[["v"]] = v
}
if ("vel_mag" %in% cfg$vars) {
  vel_mag = read_vel_mag(scenario = cfg$scenario, year = cfg$year, band_as_time = FALSE)
  var_list[["vel_mag"]] = vel_mag
}
if ("brick_xbtm" %in% cfg$vars) {
  xbtm = stars::read_stars(file.path(cfg$data_path, cfg$brick_path, sprintf("%s_%s_Xbtm_mon.tif", cfg$scenario, cfg$year))) |>
    dplyr::rename(xbtm = sprintf("%s_%s_Xbtm_mon.tif", cfg$scenario, cfg$year))
  var_list[["xbtm"]] = xbtm
}

dynamic_preds = twinkle::bind_attrs(var_list) |>
  set_names(cfg$dynamic_names)

mon_no = seq(from = 1, to = 12)
if ("dfs" %in% cfg$vars) {
  dfs = stars::read_stars(file.path(cfg$data_path, cfg$dfs_path, "etopo_warped_distance_to_shore_meters.tif")) |>
    dplyr::rename(dfs = "etopo_warped_distance_to_shore_meters.tif") 
  dfs2 = sapply(mon_no, function(mon) {dfs}, simplify = FALSE)
  dfs = do.call(c, append(dfs2, list(along = list(band = mon_no)))) |>
    stars::st_set_dimensions("band", offset = NA_real_, delta = NA_real_) |>
    stars::st_warp(dest = dynamic_preds)
}
if ("log_depth" %in% cfg$vars) {
  depth = stars::read_stars(file.path(cfg$data_path, cfg$depth_path, "PRESENT_PRESENT_Bathy_depth_mon.tif")) |>
    dplyr::rename(log_depth = "PRESENT_PRESENT_Bathy_depth_mon.tif") |>
    dplyr::mutate(log_depth = log(log_depth))
  dd = sapply(mon_no, function(mon) {depth}, simplify = FALSE)
  depth = do.call(c, append(dd, list(along = list(band = mon_no)))) |>
    stars::st_set_dimensions("band", offset = NA_real_, delta = NA_real_)
}

if (exists("depth") && exists("dfs")) {
  preds = c(dynamic_preds, depth, dfs) |>
    dplyr::slice(band, as.integer(cfg$month))
} else if (exists("depth") && !exists("dfs")) {
  preds = c(dynamic_preds, depth) |>
    dplyr::slice(band, as.integer(cfg$month))
} else if (!exists("depth") && exists("dfs")) {
  preds = c(dynamic_preds, dfs) |>
    dplyr::slice(band, as.integer(cfg$month))
} else {
  preds = dynamic_preds |>
    dplyr::slice(band, as.integer(cfg$month))
}

preds = avg_covs(preds) |>
  mutate(month = as.numeric(cfg$month))

if (!is.null(cfg$mask_name)) {
  mask = stars::read_stars(file.path(cfg$root_path, cfg$data_path, cfg$mask_name)) |>
    rlang::set_names("mask") |>
    st_warp(dest = preds)
  
  ix = (mask[["mask"]] <= 0)
  mask[["mask"]][ix] = NA_real_
  
  preds[is.na(mask)] = NA_real_ 
}

if (!is.null(cfg$contour_name)) {
  mask_contour = sf::read_sf(file.path(cfg$data_path, cfg$contour_name))
} else {NULL}

#rf pred----
final_rf_workflow = readr::read_rds(file.path(cfg$root_path, cfg$wf_path, cfg$wf_version, sprintf("%s_final_rf_wf.Rds", cfg$wf_version)))

rf_pred = predict_stars(final_rf_workflow, preds, type = "prob") |>
  dplyr::select(.pred_presence) |>
  write_stars(file.path(vpath, "rf_prediction.tif"))

rf_pred_plot = ggplot() +
  geom_stars(data = rf_pred) +
  scale_fill_binned(type = "viridis", 
                    name = cfg$graphics$ggtitle, 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
  theme_void() 
if(cfg$graphics$add_pres_pts == TRUE) {
  rf_pred_plot = rf_pred_plot +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("HumanObservation" = 16),
                       labels = c("Human Observation"))
}
if (cfg$graphics$plot_contour) {
  rf_pred_plot = rf_pred_plot +
    geom_sf(data = mask_contour, color = "white")
}

if (stringr::str_sub(vpars["minor"], start = -1) == 0) {
  legend_grob = ggpubr::get_legend(rf_pred_plot)
  legend_only = cowplot::ggdraw(legend_grob)
  ggsave(filename = file.path(cfg$output_path, "graphics/legend_only.png"), 
         legend_only, width = 3, height = 5, dpi = 300, create.dir = TRUE)
}
if(cfg$graphics$add_pres_pts == TRUE) {
  rf_pred_plot = rf_pred_plot +
    theme(legend.position = "none")
}
png(filename = file.path(vpath, sprintf("%s_rf_prediction.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(rf_pred_plot)
ok = dev.off()

# bt pred----
final_bt_workflow = readr::read_rds(file.path(cfg$root_path, cfg$wf_path, cfg$wf_version, sprintf("%s_final_bt_wf.Rds", cfg$wf_version)))

bt_pred = predict_stars(final_bt_workflow, preds, type = "prob") |>
  dplyr::select(.pred_presence) |>
  write_stars(file.path(vpath, "bt_prediction.tif"))

bt_pred_plot = ggplot() +
  geom_stars(data = bt_pred) +
  scale_fill_binned(type = "viridis", 
                    name = cfg$graphics$ggtitle, 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
  theme_void() 
if(cfg$graphics$add_pres_pts == TRUE) {
  bt_pred_plot = bt_pred_plot +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("HumanObservation" = 16),
                       labels = c("Human Observation"))
}
if (cfg$graphics$plot_contour) {
  bt_pred_plot = bt_pred_plot +
    geom_sf(data = mask_contour, color = "white")
}
if(cfg$graphics$add_pres_pts == TRUE) {
  bt_pred_plot = bt_pred_plot +
    theme(legend.position = "none")
}
png(filename = file.path(vpath, sprintf("%s_bt_prediction.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(bt_pred_plot)
ok = dev.off()

#maxent pred----
final_maxent_workflow = readr::read_rds(file.path(cfg$root_path, cfg$wf_path, cfg$wf_version, sprintf("%s_final_maxent_wf.Rds", cfg$wf_version)))

maxent_pred = predict_stars(final_maxent_workflow, preds, type = "prob") |>
  dplyr::select(.pred_presence) |>
  write_stars(file.path(vpath, "maxent_prediction.tif"))
maxent_pred_plot = ggplot() +
  geom_stars(data = maxent_pred) +
  scale_fill_binned(type = "viridis", 
                    name = cfg$graphics$ggtitle, 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
  theme_void() 
if(cfg$graphics$add_pres_pts == TRUE) {
  maxent_pred_plot = maxent_pred_plot +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("HumanObservation" = 16),
                       labels = c("Human Observation"))
}
if (cfg$graphics$plot_contour) {
  maxent_pred_plot = maxent_pred_plot +
    geom_sf(data = mask_contour, color = "white")
}
if(cfg$graphics$add_pres_pts == TRUE) {
  maxent_pred_plot  = maxent_pred_plot +
    theme(legend.position = "none")
}
png(filename = file.path(vpath, sprintf("%s_maxent_prediction.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(maxent_pred_plot)
ok = dev.off()

#gam----
final_gam_workflow = readr::read_rds(file.path(cfg$root_path, cfg$wf_path, cfg$wf_version, sprintf("%s_final_gam_wf.Rds", cfg$wf_version)))

gam_pred = predict_stars(final_gam_workflow, preds, type = "prob") |>
  dplyr::select(.pred_presence) |>
  write_stars(file.path(vpath, "gam_prediction.tif"))
gam_pred_plot = ggplot() +
  geom_stars(data = gam_pred) +
  scale_fill_binned(type = "viridis", 
                    name = cfg$graphics$ggtitle, 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
  theme_void() 
if(cfg$graphics$add_pres_pts == TRUE) {
  gam_pred_plot = gam_pred_plot +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("HumanObservation" = 16),
                       labels = c("Human Observation"))
}
if (cfg$graphics$plot_contour) {
  gam_pred_plot = gam_pred_plot +
    geom_sf(data = mask_contour, color = "white")
}
if(cfg$graphics$add_pres_pts == TRUE) {
  gam_pred_plot = gam_pred_plot +
    theme(legend.position = "none")
}
png(filename = file.path(vpath, sprintf("%s_gam_prediction.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(gam_pred_plot)
ok = dev.off()

#glm----
final_glm_workflow = readr::read_rds(file.path(cfg$root_path, cfg$wf_path, cfg$wf_version, sprintf("%s_final_glm_wf.Rds", cfg$wf_version)))

glm_pred = predict_stars(final_glm_workflow, preds, type = "prob") |>
  dplyr::select(.pred_presence) |>
  write_stars(file.path(vpath, "glm_prediction.tif"))
glm_pred_plot = ggplot() +
  geom_stars(data = glm_pred) +
  scale_fill_binned(type = "viridis", 
                    name = cfg$graphics$ggtitle, 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
  theme_void() 
if(cfg$graphics$add_pres_pts == TRUE) {
  glm_pred_plot = glm_pred_plot +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("HumanObservation" = 16),
                       labels = c("Human Observation"))
}
if (cfg$graphics$plot_contour) {
  glm_pred_plot = glm_pred_plot +
    geom_sf(data = mask_contour, color = "white")
}
if(cfg$graphics$add_pres_pts == TRUE) {
  glm_pred_plot = glm_pred_plot +
    theme(legend.position = "none")
}
png(filename = file.path(vpath, sprintf("%s_glm_prediction.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(glm_pred_plot)
ok = dev.off()

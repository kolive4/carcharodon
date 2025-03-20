# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -c, --config  the name of the configuration file [default:
#                                                       /mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidysdm_workflow/t01.00000.01.yaml]

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

args = argparser::arg_parser("tidymodels/tidysdm modeling and forecasting for white shark habitat suitability",
                             name = "tidy_seals.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_workflow/t03.00030.w.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
for (f in list.files(file.path(cfg$root_path, cfg$source_path), pattern = "^.*\\.R$", full.names = TRUE)){
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

obs = read_brickman_points(file = file.path(cfg$root_path, cfg$gather_data_path, "brickman_covar_obs_bg.gpkg")) |>
  sf::st_as_sf() |>
  dplyr::filter(id == 1, basisOfRecord %in% cfg$obs_filter$basisOfRecord) |>
  dplyr::select(all_of(cfg$vars)) |>
  dplyr::filter(month %in% as.numeric(cfg$month)) |>
  dplyr::mutate(class = "presence")

bg = read_brickman_points(file = file.path(cfg$root_path, cfg$gather_data_path, "brickman_covar_obs_bg.gpkg")) |>
  sf::st_as_sf() |>
  dplyr::filter(id == 0) |>
  dplyr::select(all_of(cfg$vars)) |>
  dplyr::filter(month %in% as.numeric(cfg$month)) |>
  dplyr::mutate(class = "background")

data = dplyr::bind_rows(obs, bg) |>
  dplyr::select(-dplyr::all_of(c("eventDate", "Year", "month", "basisOfRecord"))) |>
  na.omit() |>
  dplyr::mutate(class = factor(class, levels = c("presence", "background")))

split = rsample::initial_split(data,
                          prop = 4/5,
                          strata = class)

split_data = ggplot() +
  geom_coastline(coast = coast, bb = bb, color = "darkgrey") +
  geom_sf(data = training(split), aes(shape = class), color = "orange", alpha = 0.5) +
  geom_sf(data = testing(split), aes(shape = class), color = "navy", alpha = 0.5)
png(filename = file.path(vpath, sprintf("%s_initial_split.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(split_data)
dev.off()

ws_training = training(split)
ws_train_cv = spatialsample::spatial_block_cv(data = ws_training, 
                                                  v = cfg$n_folds, 
                                                  n = 5)
cv_plot = autoplot(ws_train_cv) +
  geom_coastline(coast, bb, color = "darkgrey")
png(filename = file.path(vpath, sprintf("%s_cv_plot.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(cv_plot)
dev.off()

wflow = workflows::workflow()
rec = recipes::recipe(head(data), class ~ .)

model_maxent = tidysdm::maxent(
  mode = "classification",
  engine = "maxnet",
  feature_classes = tune(),
  regularization_multiplier = tune()
)

model_rf = parsnip::rand_forest(
  mode = "classification",
  engine = "ranger",
  mtry = tune(),
  trees = NULL,
  min_n = NULL
) 

model_bt = parsnip::boost_tree(
  mode = "classification",
  engine = "xgboost",
  mtry = tune(),
  trees = tune(),
  min_n = NULL,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = NULL,
  stop_iter = tune()
)

ws_models <-
  workflow_set(
    preproc = list(
      simple = rec
    ),
    models = list(
      bt = model_bt, 
      rf = model_rf,
      maxent = model_maxent
    ),
    cross = TRUE
  )

ws_models <-
  ws_models |>
  # The first argument is a function name from the {{tune}} package
  # such as `tune_grid()`, `fit_resamples()`, etc.
  workflow_map("tune_grid",
               resamples = ws_train_cv, 
               grid = cfg$n_grid,
               control = control_grid(save_pred = TRUE, save_workflow = TRUE),
               metrics = tidysdm::sdm_metric_set(accuracy), verbose = TRUE
  )

model_comp = autoplot(ws_models) +
  scale_color_viridis_d()
png(filename = file.path(vpath, sprintf("%s_model_comp.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(model_comp)
dev.off()

#rf-----
rf_model_ranks = metric_table(ws_models, "simple_rf")
rf_ws_workflow_final = ws_models |>
  extract_workflow("simple_rf") |>
  finalize_workflow(best_hyperparams(rf_model_ranks))

rf_ws_fit_final = rf_ws_workflow_final |>
  tune::last_fit(split, metrics = tidysdm::sdm_metric_set(accuracy))

rf_fit_final_metrics = rf_ws_fit_final |>
  tune::collect_metrics(summarize = FALSE) |>
  write.csv(file.path(vpath, paste0(cfg$version, "_rf_final_metrics.csv")))

final_rf_workflow = extract_workflow(rf_ws_fit_final)

final_rf_model = extract_fit_engine(rf_ws_fit_final)

p_rf = predict(final_rf_workflow, rsample::testing(split), type = "prob") |>
  dplyr::mutate(.pred_class = ifelse(.pred_presence >= 0.5, "presence", "background") |>
                  factor(levels = c("presence", "background")),
                class = testing(split)$class |>
                  factor(levels = c("presence", "background")))

cm_rf = yardstick::conf_mat(p_rf, truth = class, estimate = .pred_class)
autoplot(cm_rf, type = "heatmap")

roc_rf = yardstick::roc_curve(p_rf, .pred_presence, truth = class)
auc_rf = yardstick::roc_auc(p_rf, .pred_presence, truth = class)

rf_roc_plot = plot_roc(p_rf, truth = class, pred = .pred_presence, title = "Random Forest ROC")
png(filename = file.path(vpath, sprintf("%s_rf_pauc.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(rf_roc_plot)
ok = dev.off()

rf_pd = partial_dependence(object = extract_fit_engine(rf_ws_fit_final), 
                           v = extract_var_names(rf_ws_fit_final), 
                           data = training(split) |>
                             dplyr::select(-class) |>
                             sf::st_drop_geometry(),
                           which_pred = "presence",
                           prob = TRUE)
rf_pd_plot = plot(rf_pd, share_y = "all")
png(filename = file.path(vpath, sprintf("%s_rf_pd.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(rf_pd_plot)
ok = dev.off()

rf_vi = variable_importance(x = final_rf_workflow, y = training(split), type = "prob") |>
  write.csv(file.path(vpath, paste0(cfg$version, "_rf_vi.csv")))

#bt-----
bt_model_ranks = metric_table(ws_models, "simple_bt")
bt_ws_workflow_final = ws_models |>
  extract_workflow("simple_bt") |>
  finalize_workflow(best_hyperparams(bt_model_ranks))

bt_ws_fit_final = bt_ws_workflow_final |>
  tune::last_fit(split, metrics = tidysdm::sdm_metric_set(accuracy))

bt_fit_final_metrics = bt_ws_fit_final |>
  tune::collect_metrics(summarize = FALSE) |>
  write.csv(file.path(vpath, paste0(cfg$version, "_bt_final_metrics.csv")))

final_bt_workflow = extract_workflow(bt_ws_fit_final)

final_bt_model = extract_fit_engine(bt_ws_fit_final)

p_bt = predict(final_bt_workflow, rsample::testing(split), type = "prob") |>
  dplyr::mutate(.pred_class = ifelse(.pred_presence >= 0.5, "presence", "background") |>
                  factor(levels = c("presence", "background")),
                class = testing(split)$class |>
                  factor(levels = c("presence", "background")))

cm_bt = yardstick::conf_mat(p_bt, truth = class, estimate = .pred_class)
autoplot(cm_bt, type = "heatmap")

roc_bt = yardstick::roc_curve(p_bt, .pred_presence, truth = class)
auc_bt = yardstick::roc_auc(p_bt, .pred_presence, truth = class)

bt_roc_plot = plot_roc(p_bt, truth = class, pred = .pred_presence, title = "Boost Tree ROC")
png(filename = file.path(vpath, sprintf("%s_bt_pauc.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(bt_roc_plot)
ok = dev.off()

bt_pd = partial_dependence(object = extract_fit_engine(bt_ws_fit_final), 
                           v = extract_var_names(bt_ws_fit_final), 
                           data = training(split) |>
                             dplyr::select(-class) |>
                             sf::st_drop_geometry() |>
                             as.matrix(),
                           which_pred = "presence",
                           prob = TRUE)
bt_pd_plot = plot(bt_pd, share_y = "all")
png(filename = file.path(vpath, sprintf("%s_bt_pd.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(bt_pd_plot)
ok = dev.off()

bt_vi = variable_importance(x = final_bt_workflow, y = training(split), type = "prob") |>
  write.csv(file.path(vpath, paste0(cfg$version, "_bt_vi.csv")))


#maxent-----
maxent_model_ranks = metric_table(ws_models, "simple_maxent")
maxent_ws_workflow_final = ws_models |>
  extract_workflow("simple_maxent") |>
  finalize_workflow(best_hyperparams(maxent_model_ranks))

maxent_ws_fit_final = maxent_ws_workflow_final |>
  tune::last_fit(split, metrics = tidysdm::sdm_metric_set(accuracy))

maxent_fit_final_metrics = maxent_ws_fit_final |>
  tune::collect_metrics(summarize = FALSE) |>
  write.csv(file.path(vpath, paste0(cfg$version, "_maxent_final_metrics.csv")))

final_maxent_workflow = extract_workflow(maxent_ws_fit_final)

final_maxent_model = extract_fit_engine(maxent_ws_fit_final)

p_maxent = predict(final_maxent_workflow, rsample::testing(split), type = "prob") |>
  dplyr::mutate(.pred_class = ifelse(.pred_presence >= 0.5, "presence", "background") |>
                  factor(levels = c("presence", "background")),
                class = testing(split)$class |>
                  factor(levels = c("presence", "background")))

cm_maxent = yardstick::conf_mat(p_maxent, truth = class, estimate = .pred_class)
autoplot(cm_maxent, type = "heatmap")

roc_maxent = yardstick::roc_curve(p_maxent, .pred_presence, truth = class)
auc_maxent = yardstick::roc_auc(p_maxent, .pred_presence, truth = class)

maxent_roc_plot = plot_roc(p_maxent, truth = class, pred = .pred_presence, title = "Maxent ROC")
png(filename = file.path(vpath, sprintf("%s_maxent_pauc.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(maxent_roc_plot)
ok = dev.off()

maxent_pd = partial_dependence(object = extract_fit_engine(maxent_ws_fit_final), 
                               v = extract_var_names(maxent_ws_fit_final), 
                               data = training(split) |>
                                 dplyr::select(-class) |>
                                 sf::st_drop_geometry(),
                               which_pred = "presence",
                               prob = TRUE)
maxent_pd_plot = plot(maxent_pd, share_y = "all")
png(filename = file.path(vpath, sprintf("%s_maxent_pd.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(maxent_pd_plot)
ok = dev.off()

maxent_vi = variable_importance(x = final_maxent_workflow, y = training(split), type = "prob") |>
  write.csv(file.path(vpath, paste0(cfg$version, "_maxent_vi.csv")))

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
  dfs = stars::read_stars(file.path(cfg$data_path, cfg$dfs_path, "gebco_distance_to_shore_meters.tif")) |>
    dplyr::rename(dfs = "gebco_distance_to_shore_meters.tif") 
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

preds = avg_covs(preds)

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
            fill = "white",
            alpha = 0.5,
            show.legend = "point")
}
if (cfg$graphics$plot_contour) {
  rf_pred_plot = rf_pred_plot +
    geom_sf(data = mask_contour, color = "white")
}
rf_pred_plot
png(filename = file.path(vpath, sprintf("%s_rf_prediction.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(rf_pred_plot)
ok = dev.off()

# bt pred----
bt_pred = predict_stars(final_bt_workflow, preds, type = "prob") |>
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
            fill = "white",
            alpha = 0.5,
            show.legend = "point")
}
if (cfg$graphics$plot_contour) {
  bt_pred_plot = bt_pred_plot +
    geom_sf(data = mask_contour, color = "white")
}
bt_pred_plot
png(filename = file.path(vpath, sprintf("%s_bt_prediction.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(bt_pred_plot)
dev.off()

#maxent pred----
maxent_pred = predict_stars(final_maxent_workflow, preds, type = "prob") |>
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
            fill = "white",
            alpha = 0.5,
            show.legend = "point")
}
if (cfg$graphics$plot_contour) {
  maxent_pred_plot = maxent_pred_plot +
    geom_sf(data = mask_contour, color = "white")
}
maxent_pred_plot
png(filename = file.path(vpath, sprintf("%s_maxent_prediction.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(maxent_pred_plot)
dev.off()


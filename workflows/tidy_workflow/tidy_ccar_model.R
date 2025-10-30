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
  library(sf)
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
  library(butcher)
})

args = argparser::arg_parser("tidymodels/tidysdm modeling and forecasting for white shark habitat suitability",
                             name = "tidy_ccar_model.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_workflow/t11.10096.20.yaml",
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
mask = stars::read_stars(file.path(cfg$data_path, cfg$mask_name)) |>
  rlang::set_names("mask")

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

bg = read_brickman_points(file = obs_bg) |>
  sf::st_as_sf() |>
  dplyr::filter(id == 0) |>
  dplyr::select(all_of(cfg$vars)) |>
  dplyr::filter(month %in% as.numeric(cfg$month)) |>
  dplyr::mutate(class = "background")

data = dplyr::bind_rows(obs, bg) |>
  dplyr::select(dplyr::all_of(c("geom", "class", cfg$static_names, cfg$dynamic_names))) |>
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
ok = dev.off()

ws_training = training(split)
ws_train_cv = spatialsample::spatial_block_cv(data = ws_training, 
                                                  v = cfg$n_folds, 
                                                  n = 5)
cv_plot = autoplot(ws_train_cv) +
  geom_coastline(coast, bb, color = "darkgrey")
png(filename = file.path(vpath, sprintf("%s_cv_plot.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(cv_plot)
ok = dev.off()

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
  trees = tune(),
  min_n = tune()
) 

model_bt = parsnip::boost_tree(
  mode = "classification",
  engine = "xgboost",
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  stop_iter = tune()
)

model_glm = parsnip::logistic_reg(
  engine = "glm",
  mode = "classification"
)

model_gam = tidysdm::sdm_spec_gam(
  select_features = TRUE,
  adjust_deg_free = 4
)

ws_models <-
  workflow_set(
    preproc = list(
      simple = rec
      # pa = pa_rec
      # ap = ap_rec
    ),
    models = list(
      bt = model_bt, 
      rf = model_rf,
      maxent = model_maxent,
      glm = model_glm,
      gam = model_gam
    ),
    cross = TRUE
  ) |>
  # anti_join(tibble(wflow_id = c("pa_gam", "pa_glm", "ap_bt", "ap_maxent", "ap_rf")),
  # by = "wflow_id"
  # )
  workflowsets::update_workflow_model("simple_gam",
                                      spec = sdm_spec_gam(),
                                      formula = tidysdm::gam_formula(rec)
)

m = rlang::syms(cfg$metrics)
ws_metrics = eval(rlang::expr(yardstick::metric_set(!!!m)))

ws_models <-
  ws_models |>
  # The first argument is a function name from the {{tune}} package
  # such as `tune_grid()`, `fit_resamples()`, etc.
  workflow_map("tune_grid",
               resamples = ws_train_cv, 
               grid = cfg$n_grid,
               control = control_grid(save_pred = TRUE, save_workflow = TRUE),
               metrics = ws_metrics, verbose = TRUE
  )

model_comp = autoplot(ws_models) +
  scale_color_viridis_d()
png(filename = file.path(vpath, sprintf("%s_model_comp.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(model_comp)
ok = dev.off()

#rf-----
rf_model_ranks = metric_table(ws_models, "simple_rf")
rf_ws_workflow_final = ws_models |>
  extract_workflow("simple_rf") |>
  finalize_workflow(best_hyperparams(rf_model_ranks))

rf_ws_fit_final = rf_ws_workflow_final |>
  tune::last_fit(split, metrics = ws_metrics) 

rf_fit_final_metrics = rf_ws_fit_final |>
  tune::collect_metrics(summarize = FALSE) |>
  write.csv(file.path(vpath, paste0(cfg$version, "_rf_final_metrics.csv")))

final_rf_workflow = extract_workflow(rf_ws_fit_final) |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_final_rf_wf.Rds")))

final_rf_model = extract_fit_engine(rf_ws_fit_final)

p_rf = predict(final_rf_workflow, rsample::testing(split), type = "prob") |>
  dplyr::mutate(.pred_class = ifelse(.pred_presence >= 0.5, "presence", "background") |>
                  factor(levels = c("presence", "background")),
                class = testing(split)$class |>
                  factor(levels = c("presence", "background"))) |>
  readr::write_csv(file = file.path(vpath, paste0(cfg$version, "_rf_pred.csv")))

perdev_ex_rf = perdev_ex(probs = p_rf$.pred_presence, truth = p_rf$class) |>
  as_tibble() |>
  set_names("% dev. explained") |>
  readr::write_csv(file.path(vpath, paste0(cfg$version, "_rf_perdevex.csv")))

cm_rf = yardstick::conf_mat(p_rf, truth = class, estimate = .pred_class) |>
  write_RDS(file = file.path(vpath, paste0(cfg$version, "_rf_conf_mat.rds")))
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
                           prob = TRUE) |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_rf_pd.rds")))
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
  tune::last_fit(split, metrics = ws_metrics)

bt_fit_final_metrics = bt_ws_fit_final |>
  tune::collect_metrics(summarize = FALSE) |>
  write.csv(file.path(vpath, paste0(cfg$version, "_bt_final_metrics.csv")))

final_bt_workflow = extract_workflow(bt_ws_fit_final) |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_final_bt_wf.Rds")))

final_bt_model = extract_fit_engine(bt_ws_fit_final)

p_bt = predict(final_bt_workflow, rsample::testing(split), type = "prob") |>
  dplyr::mutate(.pred_class = ifelse(.pred_presence >= 0.5, "presence", "background") |>
                  factor(levels = c("presence", "background")),
                class = testing(split)$class |>
                  factor(levels = c("presence", "background"))) |>
  readr::write_csv(file = file.path(vpath, paste0(cfg$version, "_bt_pred.csv")))

perdev_ex_bt = perdev_ex(probs = p_bt$.pred_presence, truth = p_bt$class) |>
  as_tibble() |>
  set_names("% dev. explained") |>
  readr::write_csv(file.path(vpath, paste0(cfg$version, "_bt_perdevex.csv")))

cm_bt = yardstick::conf_mat(p_bt, truth = class, estimate = .pred_class) |>
  write_RDS(file = file.path(vpath, paste0(cfg$version, "_bt_conf_mat.rds")))
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
                           prob = TRUE) |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_bt_pd.rds")))
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
  tune::last_fit(split, metrics = ws_metrics)

maxent_fit_final_metrics = maxent_ws_fit_final |>
  tune::collect_metrics(summarize = FALSE) |>
  write.csv(file.path(vpath, paste0(cfg$version, "_maxent_final_metrics.csv")))

final_maxent_workflow = extract_workflow(maxent_ws_fit_final) |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_final_maxent_wf.Rds")))

final_maxent_model = extract_fit_engine(maxent_ws_fit_final)

p_maxent = predict(final_maxent_workflow, rsample::testing(split), type = "prob") |>
  dplyr::mutate(.pred_class = ifelse(.pred_presence >= 0.5, "presence", "background") |>
                  factor(levels = c("presence", "background")),
                class = testing(split)$class |>
                  factor(levels = c("presence", "background"))) |>
  readr::write_csv(file = file.path(vpath, paste0(cfg$version, "_maxent_pred.csv")))

perdev_ex_maxent = perdev_ex(probs = p_maxent$.pred_presence, truth = p_maxent$class) |>
  as_tibble() |>
  set_names("% dev. explained") |>
  readr::write_csv(file.path(vpath, paste0(cfg$version, "_maxent_perdevex.csv")))

cm_maxent = yardstick::conf_mat(p_maxent, truth = class, estimate = .pred_class) |>
  write_RDS(file = file.path(vpath, paste0(cfg$version, "_maxnet_conf_mat.rds")))
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
                               prob = TRUE,
                               type = "cloglog") |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_maxent_pd.rds")))
maxent_pd_plot = plot(maxent_pd, share_y = "all")
png(filename = file.path(vpath, sprintf("%s_maxent_pd.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(maxent_pd_plot)
ok = dev.off()

maxent_vi = variable_importance(x = final_maxent_workflow, y = training(split), type = "prob") |>
  write.csv(file.path(vpath, paste0(cfg$version, "_maxent_vi.csv")))

#gam-----
gam_model_ranks = metric_table(ws_models, "simple_gam")
gam_ws_workflow_final = ws_models |>
  extract_workflow("simple_gam") |>
  finalize_workflow(best_hyperparams(gam_model_ranks))

gam_ws_fit_final = gam_ws_workflow_final |>
  tune::last_fit(split, metrics = ws_metrics)

gam_fit_final_metrics = gam_ws_fit_final |>
  tune::collect_metrics(summarize = FALSE) |>
  write.csv(file.path(vpath, paste0(cfg$version, "_gam_final_metrics.csv")))

final_gam_workflow = extract_workflow(gam_ws_fit_final) |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_final_gam_wf.Rds")))

final_gam_model = extract_fit_engine(gam_ws_fit_final)

p_gam = predict(final_gam_workflow, rsample::testing(split), type = "prob") |>
  dplyr::mutate(.pred_class = ifelse(.pred_presence >= 0.5, "presence", "background") |>
                  factor(levels = c("presence", "background")),
                class = testing(split)$class |>
                  factor(levels = c("presence", "background"))) |>
  readr::write_csv(file = file.path(vpath, paste0(cfg$version, "_gam_pred.csv")))

perdev_ex_gam = perdev_ex(probs = p_gam$.pred_presence, truth = p_gam$class) |>
  as_tibble() |>
  set_names("% dev. explained") |>
  readr::write_csv(file.path(vpath, paste0(cfg$version, "_gam_perdevex.csv")))

cm_gam = yardstick::conf_mat(p_gam, truth = class, estimate = .pred_class) |>
  write_RDS(file = file.path(vpath, paste0(cfg$version, "_maxnet_conf_mat.rds")))
autoplot(cm_gam, type = "heatmap")

roc_gam = yardstick::roc_curve(p_gam, .pred_presence, truth = class)
auc_gam = yardstick::roc_auc(p_gam, .pred_presence, truth = class)

gam_roc_plot = plot_roc(p_gam, truth = class, pred = .pred_presence, title = "gam ROC")
png(filename = file.path(vpath, sprintf("%s_gam_pauc.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(gam_roc_plot)
ok = dev.off()

gam_pd = partial_dependence(object = extract_fit_engine(gam_ws_fit_final), 
                            v = extract_var_names(gam_ws_fit_final), 
                            data = training(split) |>
                              dplyr::select(-class) |>
                              sf::st_drop_geometry(),
                            which_pred = "presence",
                            prob = TRUE,
                            type = "response") |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_gam_pd.rds")))
gam_pd_plot = plot(gam_pd, share_y = "all")
png(filename = file.path(vpath, sprintf("%s_gam_pd.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(gam_pd_plot)
ok = dev.off()

gam_vi = variable_importance(x = final_gam_workflow, y = training(split), type = "prob") |>
  write.csv(file.path(vpath, paste0(cfg$version, "_gam_vi.csv")))

#glm-----
glm_model_ranks = metric_table(ws_models, "simple_glm")
glm_ws_workflow_final = ws_models |>
  extract_workflow("simple_glm") |>
  finalize_workflow(best_hyperparams(glm_model_ranks))

glm_ws_fit_final = glm_ws_workflow_final |>
  tune::last_fit(split, metrics = ws_metrics)

glm_fit_final_metrics = glm_ws_fit_final |>
  tune::collect_metrics(summarize = FALSE) |>
  write.csv(file.path(vpath, paste0(cfg$version, "_glm_final_metrics.csv")))

final_glm_workflow = extract_workflow(glm_ws_fit_final) |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_final_glm_wf.Rds")))

final_glm_model = extract_fit_engine(glm_ws_fit_final)

p_glm = predict(final_glm_workflow, rsample::testing(split), type = "prob") |>
  dplyr::mutate(.pred_class = ifelse(.pred_presence >= 0.5, "presence", "background") |>
                  factor(levels = c("presence", "background")),
                class = testing(split)$class |>
                  factor(levels = c("presence", "background"))) |>
  readr::write_csv(file = file.path(vpath, paste0(cfg$version, "_glm_pred.csv")))

perdev_ex_glm = perdev_ex(probs = p_glm$.pred_presence, truth = p_glm$class) |>
  as_tibble() |>
  set_names("% dev. explained") |>
  readr::write_csv(file.path(vpath, paste0(cfg$version, "_glm_perdevex.csv")))

cm_glm = yardstick::conf_mat(p_glm, truth = class, estimate = .pred_class) |>
  write_RDS(file = file.path(vpath, paste0(cfg$version, "_maxnet_conf_mat.rds")))
autoplot(cm_glm, type = "heatmap")

roc_glm = yardstick::roc_curve(p_glm, .pred_presence, truth = class)
auc_glm = yardstick::roc_auc(p_glm, .pred_presence, truth = class)

glm_roc_plot = plot_roc(p_glm, truth = class, pred = .pred_presence, title = "glm ROC")
png(filename = file.path(vpath, sprintf("%s_glm_pauc.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(glm_roc_plot)
ok = dev.off()

glm_pd = partial_dependence(object = extract_fit_engine(glm_ws_fit_final), 
                            v = extract_var_names(glm_ws_fit_final), 
                            data = training(split) |>
                              dplyr::select(-class) |>
                              sf::st_drop_geometry(),
                            which_pred = "presence",
                            prob = TRUE,
                            type = "response") |>
  readr::write_rds(file.path(vpath, paste0(cfg$version, "_glm_pd.rds")))
glm_pd_plot = plot(glm_pd, share_y = "all")
png(filename = file.path(vpath, sprintf("%s_glm_pd.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(glm_pd_plot)
ok = dev.off()

glm_vi = variable_importance(x = final_glm_workflow, y = training(split), type = "prob") |>
  write.csv(file.path(vpath, paste0(cfg$version, "_glm_vi.csv")))


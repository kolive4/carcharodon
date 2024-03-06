# Combine brickman depth stars object and brickman sst object to make a predictive layer

# depth for brickman
brick_depth3 = load_brickman(scenario = 'PRESENT', vars = 'Bathy_depth', path = here::here("data/brickman/bathy"))
nowcast_depth = brick_depth3

# sst for brickman present
present_brick = load_brickman(scenario = 'PRESENT', vars = c("SST", "Tbtm"), interval = "mon", path = here::here("data/brickman/gom_carcharodon"))
# may
nowcast_may_sst = present_brick[1,,,5, drop = TRUE]
nowcast_jun_sst = present_brick[1,,,6, drop = TRUE]
nowcast_jul_sst = present_brick[1,,,7, drop = TRUE]
nowcast_aug_sst = present_brick[1,,,8, drop = TRUE]
nowcast_sep_sst = present_brick[1,,,9, drop = TRUE]
nowcast_oct_sst = present_brick[1,,,10, drop = TRUE]
nowcast_nov_sst = present_brick[1,,,11, drop = TRUE]


# combine covars and rename variables to match model variable names
nowcast_combined_covars_may = c(nowcast_depth, nowcast_may_sst, along = NA_integer_) |>
  rlang::set_names(c("depth", "sst"))
nowcast_combined_covars_jun = c(nowcast_depth, nowcast_jun_sst, along = NA_integer_) |>
  rlang::set_names(c("depth", "sst"))
nowcast_combined_covars_jul = c(nowcast_depth, nowcast_jul_sst, along = NA_integer_) |>
  rlang::set_names(c("depth", "sst"))
nowcast_combined_covars_aug = c(nowcast_depth, nowcast_aug_sst, along = NA_integer_) |>
  rlang::set_names(c("depth", "sst"))
nowcast_combined_covars_sep = c(nowcast_depth, nowcast_sep_sst, along = NA_integer_) |>
  rlang::set_names(c("depth", "sst"))
nowcast_combined_covars_oct = c(nowcast_depth, nowcast_oct_sst, along = NA_integer_) |>
  rlang::set_names(c("depth", "sst"))
nowcast_combined_covars_nov = c(nowcast_depth, nowcast_nov_sst, along = NA_integer_) |>
  rlang::set_names(c("depth", "sst"))

# nowcast model
may_pred = predict(may.ws.model, nowcast_combined_covars_may, type = "cloglog")
plot(may_pred, main = "May Nowcast")
jun_pred = predict(jun.ws.model, nowcast_combined_covars_jun, type = "cloglog")
plot(jun_pred, main = "June Nowcast")
jul_pred = predict(jul.ws.model, nowcast_combined_covars_jul, type = "cloglog")
plot(jul_pred, main = "July Nowcast")
aug_pred = predict(aug.ws.model, nowcast_combined_covars_aug, type = "cloglog")
plot(aug_pred, main = "August Nowcast")
sep_pred = predict(sep.ws.model, nowcast_combined_covars_sep, type = "cloglog")
plot(sep_pred, main = "September Nowcast")
oct_pred = predict(oct.ws.model, nowcast_combined_covars_oct, type = "cloglog")
plot(oct_pred, main = "October Nowcast")
nov_pred = predict(nov.ws.model, nowcast_combined_covars_nov, type = "cloglog")
plot(nov_pred, main = "November Nowcast")

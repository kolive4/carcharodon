library(ggplot2)
library(viridis)
library(gridExtra)

curated = read.csv(file.path(cfg$data_path, "historical/curated_literature1.csv")) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

august_curated = curated |>
  dplyr::filter(Month == "August") |>
  dplyr::mutate(month = match(Month, month.name)) |>
  dplyr::select(-c(Month, eventDate)) |>
  dplyr::mutate(extractDate = as.Date(sprintf("2020-%0.2i-01", month))) |>
  sf::st_crop(shark_box)

curated_shark_depth = st_extract(brickman_bathymetry, at = august_curated) |>
  st_as_sf() |>
  as_tibble()
curated_shark_covars = brickman_extract(monthly_present_brick, august_curated, time_column = "extractDate") 

august_curated = dplyr::mutate(august_curated, depth = curated_shark_depth$Bathy_depth) |>
  dplyr::bind_cols(curated_shark_covars) |>
  write_sf(file.path(cfg$data_path, "covars", "brickman_covars_curated_shark_occs.gpkg"))

aug_obs_bg = obs_bg_brick |>
  filter(month == 8)

aug_bg = filter(aug_obs_bg, id == 0) |>
  rename(brick_sst = PRESENT_PRESENT_SST_mon.tif) |>
  rename(brick_tbtm = PRESENT_PRESENT_Tbtm_mon.tif) |>
  select(c("brick_sst", "brick_tbtm", "depth")) |>
  st_drop_geometry() |>
  na.omit()

aug_obis_obs = filter(aug_obs_bg, id == 1) |>
  rename(brick_sst = PRESENT_PRESENT_SST_mon.tif) |>
  rename(brick_tbtm = PRESENT_PRESENT_Tbtm_mon.tif) |>
  select(c("brick_sst", "brick_tbtm", "depth")) |>
  na.omit()

aug_combo_obs = filter(aug_obs_bg, id == 1) |>
  bind_rows(august_curated) |>
  rename(brick_sst = PRESENT_PRESENT_SST_mon.tif) |>
  rename(brick_tbtm = PRESENT_PRESENT_Tbtm_mon.tif) |>
  select(c("brick_sst", "brick_tbtm", "depth")) |>
  na.omit()

aug_cur = august_curated |>
  rename(brick_sst = PRESENT_PRESENT_SST_mon.tif) |>
  rename(brick_tbtm = PRESENT_PRESENT_Tbtm_mon.tif) |>
  select(c("brick_sst", "brick_tbtm", "depth")) |>
  na.omit()
  
aug_obis_obs_drop = st_drop_geometry(aug_obis_obs)
aug_combo_obs_drop = st_drop_geometry(aug_combo_obs)
aug_cur_obs_drop = st_drop_geometry(aug_cur)

aug.ws.obis.flag <- c(rep(1, nrow(aug_obis_obs_drop)), rep(0, nrow(aug_bg)))
aug.ws.obis.model <- maxnet::maxnet(aug.ws.obis.flag,
                                     dplyr::bind_rows(aug_obis_obs_drop, aug_bg))

aug.ws.combo.flag <- c(rep(1, nrow(aug_combo_obs)), rep(0, nrow(aug_bg)))
aug.ws.combo.model <- maxnet::maxnet(aug.ws.combo.flag,
                               dplyr::bind_rows(aug_combo_obs_drop, aug_bg))

aug.ws.cur.flag <- c(rep(1, nrow(aug_cur_obs_drop)), rep(0, nrow(aug_bg)))
aug.ws.cur.model <- maxnet::maxnet(aug.ws.cur.flag,
                               dplyr::bind_rows(aug_cur_obs_drop, aug_bg))

plot(aug.ws.obis.model, type = "cloglog")
plot(aug.ws.cur.model, type = "cloglog")
plot(aug.ws.combo.model, type = "cloglog")


# Combine brickman depth stars object and brickman sst object to make a predictive layer

# depth for brickman
depth = brickman_bathymetry
plot(brickman_bathymetry)

plot(monthly_present_brick[2])
# august present sst
nowcast_aug_sst = monthly_present_brick[1,,,8, drop = TRUE]
plot(nowcast_aug_sst)
# august present tbtm
nowcast_aug_tbtm = monthly_present_brick[2,,,8, drop = TRUE]
plot(nowcast_aug_tbtm)
# combine covars and rename variables to match model variable names
nowcast_combined_covars_aug = c(depth, nowcast_aug_sst, nowcast_aug_tbtm, along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm"))
plot(nowcast_combined_covars_aug[3])
# nowcast model
aug_pred = predict(aug.ws.model, nowcast_combined_covars_aug, type = "cloglog")
aug_pauc = maxnetic::pAUC(aug_pred, aug_obs)
plot(aug_pauc)

now = ggplot() +
        geom_stars(data = aug_pred) +
        scale_fill_binned(type = "viridis", name = "Habitat Suitability", limits = c(0, 1), n.breaks = 6) +
        ggtitle("August Nowcast") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5))
now
ggsave(filename = "aug_nowcast.png", plot = now, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

pmonthly_rcp85_2075_brick = load_brickman(scenario = "RCP85", year = 2075, vars = c("SST", "Tbtm"), interval = "mon", path = file.path(cfg$data_path, "brickman/gom_carcharodon"))

rcp85_2075_aug_sst = monthly_rcp85_2075_brick[1,,,8, drop = TRUE]
plot(rcp85_2075_aug_sst)
rcp85_2075_aug_tbtm = monthly_rcp85_2075_brick[2,,,8, drop = TRUE]
plot(rcp85_2075_aug_tbtm)
rcp85_2075_combined_covars_aug = c(depth, rcp85_2075_aug_sst, rcp85_2075_aug_tbtm, along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm"))
aug_2075_rcp85_pred = predict(aug.ws.model, rcp85_2075_combined_covars_aug, type = "cloglog")
aug = ggplot() +
        geom_stars(data = aug_2075_rcp85_pred) +
        scale_fill_binned(type = "viridis", name = "Habitat Suitability", limits = c(0, 1), n.breaks = 6) +
        ggtitle("August RCP85 2075 Prediction") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5))
aug
ggsave(filename = "aug_rcp85_2075_pred.png", plot = aug, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

rcp85_2075_jun_sst = monthly_rcp85_2075_brick[1,,,6, drop = TRUE]
rcp85_2075_jun_tbtm = monthly_rcp85_2075_brick[2,,,6, drop = TRUE]
rcp85_2075_combined_covars_jun = c(depth, rcp85_2075_jun_sst, rcp85_2075_jun_tbtm, along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm"))
jun_2075_rcp85_pred = predict(aug.ws.model, rcp85_2075_combined_covars_jun, type = "cloglog")
jun = ggplot() +
        geom_stars(data = jun_2075_rcp85_pred) +
        scale_fill_viridis(name = "Habitat Suitability", limits = c(0, 1)) +
        ggtitle("June RCP85 2075 Prediction (august model)") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5))


rcp85_2075_jul_sst = monthly_rcp85_2075_brick[1,,,7, drop = TRUE]
rcp85_2075_jul_tbtm = monthly_rcp85_2075_brick[2,,,7, drop = TRUE]
rcp85_2075_combined_covars_jul = c(depth, rcp85_2075_jul_sst, rcp85_2075_jul_tbtm, along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm"))
jul_2075_rcp85_pred = predict(aug.ws.model, rcp85_2075_combined_covars_jul, type = "cloglog")
jul = ggplot() +
        geom_stars(data = jul_2075_rcp85_pred) +
        scale_fill_viridis(name = "Habitat Suitability", limits = c(0, 1)) +
        ggtitle("July RCP85 2075 Prediction (august model)") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5))


rcp85_2075_sep_sst = monthly_rcp85_2075_brick[1,,,9, drop = TRUE]
rcp85_2075_sep_tbtm = monthly_rcp85_2075_brick[2,,,9, drop = TRUE]
rcp85_2075_combined_covars_sep = c(depth, rcp85_2075_sep_sst, rcp85_2075_sep_tbtm, along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm"))
sep_2075_rcp85_pred = predict(aug.ws.model, rcp85_2075_combined_covars_sep, type = "cloglog")
sep = ggplot() +
        geom_stars(data = sep_2075_rcp85_pred) +
        scale_fill_viridis(name = "Habitat Suitability", limits = c(0, 1)) +
        ggtitle("Sept RCP85 2075 Prediction (Aug model)") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5))


rcp85_2075_oct_sst = monthly_rcp85_2075_brick[1,,,10, drop = TRUE]
rcp85_2075_oct_tbtm = monthly_rcp85_2075_brick[2,,,10, drop = TRUE]
rcp85_2075_combined_covars_oct = c(depth, rcp85_2075_oct_sst, rcp85_2075_oct_tbtm, along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm"))
oct_2075_rcp85_pred = predict(aug.ws.model, rcp85_2075_combined_covars_oct, type = "cloglog")
oct = ggplot() +
        geom_stars(data = oct_2075_rcp85_pred) +
        scale_fill_viridis(name = "Habitat Suitability", limits = c(0, 1)) +
        ggtitle("October RCP85 2075 Prediction (Aug model)") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5))


grid.arrange(now, jun, jul, aug, sep, oct, nrow = 2)

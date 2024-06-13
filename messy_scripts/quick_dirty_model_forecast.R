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


#New covariate layers ---- 
brickman_bathymetry = load_brickman(scenario = 'PRESENT', 
                                    vars = "Bathy_depth", 
                                    band_as_time = TRUE, 
                                    path = file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/bathy"))
log_bathy = log10(brickman_bathymetry)
log_bathymetry_binned_plot = ggplot() +
  geom_stars(data = log_bathy) +
  scale_fill_steps(name = "log(Depth)", n.breaks = 7, low = "#deebf7", high = "#08306b") +
  geom_sf(data = aug_shark, aes(shape = basisOfRecord), fill = "white", show.legend = "point") +
  scale_shape_manual(name = "Method", values = c(21, 24, 22)) +
  theme_void() 
log_bathymetry_binned_plot
ggsave(filename = "log_depth_binned.png", plot = log_bathymetry_binned_plot, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)


present_present = load_brickman(scenario = "PRESENT",
                                year = NA,
                                vars = c("SST",
                                         "Tbtm",
                                         "MLD",
                                         "SSS",
                                         "Sbtm",
                                         "U",
                                         "V",
                                         "Xbtm"), 
                                interval = "mon", 
                                band_as_time = TRUE, 
                                path = file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon"))

obs_bg_brick = read_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/covars/brickman_covar_obs_bg.gpkg"))
aug_shark = obs_bg_brick |>
  dplyr::filter(month == 8) |>
  dplyr::filter(id == 1)

present_sst_points = ggplot() +
  geom_stars(data = present_present[1,,,8]) +
  scale_fill_steps(name = "Sea Surface Temperature (\u00B0C)", limits = c(15, 28), n.breaks = 7, low = "#FEEDDE", high = "#8C2D04") +
  geom_sf(data = aug_shark, aes(shape = basisOfRecord), fill = "white", show.legend = "point") +
  scale_shape_manual(name = "Method", values = c(21, 24, 22)) +
  theme_void() 
present_sst_points
ggsave(filename = "present_sst_points.png", plot = present_sst_points, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

present_tbtm_points = ggplot() +
  geom_stars(data = present_present[2,,,8]) +
  scale_fill_steps(name = "Temperature at Bottom (\u00B0C)", limits = c(0, 25), n.breaks = 7, low = "#DBFAF9", high = "#02877A") +
  geom_sf(data = aug_shark, aes(shape = basisOfRecord), fill = "white", show.legend = "point") +
  scale_shape_manual(name = "Method", values = c(21, 24, 22)) +
  theme_void() 
present_tbtm_points
ggsave(filename = "present_tbtm_points.png", plot = present_tbtm_points, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

present_mld_points = ggplot() +
  geom_stars(data = present_present[3,,,8]) +
  scale_fill_steps(name = "Mixed Layer Depth (m)", limits = c(0, 20), n.breaks = 7, low = "#cbc2b9", high = "#5e3719") +
  geom_sf(data = aug_shark, aes(shape = basisOfRecord), fill = "white", show.legend = "point") +
  scale_shape_manual(name = "Method", values = c(21, 24, 22)) +
  theme_void() 
present_mld_points
ggsave(filename = "present_mld_points.png", plot = present_mld_points, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

present_sss_points = ggplot() +
  geom_stars(data = present_present[4,,,8]) +
  scale_fill_steps(name = "Sea Surface Salinity (ppm)", limits = c(24, 36), n.breaks = 7, low = "#ffd2b6", high = "#c24e00") +
  geom_sf(data = aug_shark, aes(shape = basisOfRecord), fill = "white", show.legend = "point") +
  scale_shape_manual(name = "Method", values = c(21, 24, 22)) +
  theme_void() 
present_sss_points
ggsave(filename = "present_sss_points.png", plot = present_sss_points, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

present_sbtm_points = ggplot() +
  geom_stars(data = present_present[5,,,8]) +
  scale_fill_steps(name = "Salinity at Bottom (ppm)", limits = c(24, 36), n.breaks = 7, low = "#fff5b5", high = "#a49c00") +
  geom_sf(data = aug_shark, aes(shape = basisOfRecord), fill = "white", show.legend = "point") +
  scale_shape_manual(name = "Method", values = c(21, 24, 22)) +
  theme_void() 
present_sbtm_points
ggsave(filename = "present_sbtm_points.png", plot = present_sbtm_points, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

aug_pres_pres_sst = present_present[1,,,8, drop = TRUE]
aug_pres_pres_tbtm = present_present[2,,,8, drop = TRUE]
aug_pres_pres_mld = present_present[3,,,8, drop = TRUE]
aug_pres_pres_sss = present_present[4,,,8, drop = TRUE]
aug_pres_pres_sbtm = present_present[5,,,8, drop = TRUE]
aug_pres_pres_combined_covars = c(brickman_bathymetry, aug_pres_pres_sst, aug_pres_pres_tbtm, aug_pres_pres_mld, aug_pres_pres_sss, aug_pres_pres_sbtm, along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm", "brick_mld", "brick_sss", "brick_sbtm"))

aug.ws.model = read_maxnet("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/modeling_workflow/versions/v01/v01.008/model.rds")
plot(aug.ws.model)
aug_nowcast = predict(aug.ws.model, aug_pres_pres_combined_covars, type = "cloglog")
now = ggplot() +
  geom_stars(data = aug_nowcast) +
  scale_fill_binned(type = "viridis", name = "Habitat Suitability", limits = c(0, 1), n.breaks = 6) +
  ggtitle("August Nowcast") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
now
ggsave(filename = "aug_nowcast.png", plot = now, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

present_2055 = load_brickman(scenario = "PRESENT",
                                year = 2055,
                                vars = c("SST",
                                         "Tbtm",
                                         "MLD",
                                         "SSS",
                                         "Sbtm",
                                         "U",
                                         "V",
                                         "Xbtm"), 
                                interval = "mon", 
                                band_as_time = TRUE, 
                                path = file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon"))

present_2075 = load_brickman(scenario = "PRESENT",
                                year = 2075,
                                vars = c("SST",
                                         "Tbtm",
                                         "MLD",
                                         "SSS",
                                         "Sbtm",
                                         "U",
                                         "V",
                                         "Xbtm"), 
                                interval = "mon", 
                                band_as_time = TRUE, 
                                path = file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon"))

rcp45_2055 = load_brickman(scenario = "RCP45",
                                year = 2055,
                                vars = c("SST",
                                         "Tbtm",
                                         "MLD",
                                         "SSS",
                                         "Sbtm",
                                         "U",
                                         "V",
                                         "Xbtm"), 
                                interval = "mon", 
                                band_as_time = TRUE, 
                                path = file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon"))

rcp45_2075 = load_brickman(scenario = "RCP45",
                                year = 2075,
                                vars = c("SST",
                                         "Tbtm",
                                         "MLD",
                                         "SSS",
                                         "Sbtm",
                                         "U",
                                         "V",
                                         "Xbtm"), 
                                interval = "mon", 
                                band_as_time = TRUE, 
                                path = file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon"))

rcp85_2055 = load_brickman(scenario = "RCP85",
                           year = 2055,
                           vars = c("SST",
                                    "Tbtm",
                                    "MLD",
                                    "SSS",
                                    "Sbtm",
                                    "U",
                                    "V",
                                    "Xbtm"), 
                           interval = "mon", 
                           band_as_time = TRUE, 
                           path = file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon"))

rcp85_2055_sst = ggplot() +
  geom_stars(data = rcp85_2055[1,,,8]) +
  scale_fill_steps(name = "Sea Surface Temperature (\u00B0C)", n.breaks = 7, low = "#FEEDDE", high = "#8C2D04") +
  theme_void() 
rcp85_2055_sst
ggsave(filename = "rcp85_2055_sst.png", plot = rcp85_2055_sst, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

rcp85_2055_tbtm = ggplot() +
  geom_stars(data = rcp85_2055[2,,,8]) +
  scale_fill_steps(name = "Temperature at Bottom (\u00B0C)", n.breaks = 7, low = "#DBFAF9", high = "#02877A") +
  theme_void() 
rcp85_2055_tbtm
ggsave(filename = "rcp85_2055_tbtm.png", plot = rcp85_2055_tbtm, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

rcp85_2055_mld = ggplot() +
  geom_stars(data = rcp85_2055[3,,,8]) +
  scale_fill_steps(name = "Mixed Layer Depth (m)", n.breaks = 7, low = "#cbc2b9", high = "#5e3719") +
  theme_void() 
rcp85_2055_mld
ggsave(filename = "rcp85_2055_mld.png", plot = rcp85_2055_mld, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

rcp85_2055_sss = ggplot() +
  geom_stars(data = rcp85_2055[4,,,8]) +
  scale_fill_steps(name = "Sea Surface Salinity (ppm)", n.breaks = 7, low = "#ffd2b6", high = "#c24e00") +
  theme_void() 
rcp85_2055_sss
ggsave(filename = "rcp85_2055_sss.png", plot = rcp85_2055_sss, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

rcp85_2055_sbtm = ggplot() +
  geom_stars(data = rcp85_2055[5,,,8]) +
  scale_fill_steps(name = "Salinity at Bottom (ppm)", n.breaks = 7, low = "#fff5b5", high = "#a49c00") +
  theme_void() 
rcp85_2055_sbtm
ggsave(filename = "rcp85_2055_sbtm.png", plot = rcp85_2055_sbtm, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

aug_rcp85_2055_sst  = rcp85_2055[1,,,8, drop = TRUE]
aug_rcp85_2055_tbtm = rcp85_2055[2,,,8, drop = TRUE]
aug_rcp85_2055_mld  = rcp85_2055[3,,,8, drop = TRUE]
aug_rcp85_2055_sss  = rcp85_2055[4,,,8, drop = TRUE]
aug_rcp85_2055_sbtm = rcp85_2055[5,,,8, drop = TRUE]
aug_rcp85_2055_combined_covars = c(brickman_bathymetry, 
                                   aug_rcp85_2055_sst, 
                                   aug_rcp85_2055_tbtm, 
                                   aug_rcp85_2055_mld, 
                                   aug_rcp85_2055_sss, 
                                   aug_rcp85_2055_sbtm, 
                                   along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm", "brick_mld", "brick_sss", "brick_sbtm"))

aug.ws.model = read_maxnet("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/modeling_workflow/versions/v01/v01.008/model.rds")
plot(aug.ws.model)
aug_rcp85_2055 = predict(aug.ws.model, aug_rcp85_2055_combined_covars, type = "cloglog")
aug_rcp85_2055_pred = ggplot() +
  geom_stars(data = aug_rcp85_2055) +
  scale_fill_binned(type = "viridis", name = "Habitat Suitability", limits = c(0, 1), n.breaks = 6) +
  ggtitle("August RCP 8.5 2055 Forecast") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
aug_rcp85_2055_pred
ggsave(filename = "aug_rcp85_2055_pred.png", plot = aug_rcp85_2055_pred, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)


rcp85_2075 = load_brickman(scenario = "RCP85",
                           year = 2075,
                           vars = c("SST",
                                    "Tbtm",
                                    "MLD",
                                    "SSS",
                                    "Sbtm",
                                    "U",
                                    "V",
                                    "Xbtm"), 
                           interval = "mon", 
                           band_as_time = TRUE, 
                           path = file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon"))

rcp85_2075_sst = ggplot() +
  geom_stars(data = rcp85_2075[1,,,8]) +
  scale_fill_steps(name = "Sea Surface Temperature (\u00B0C)", limits = c(12, 30), n.breaks = 7, low = "#FEEDDE", high = "#8C2D04") +
  theme_void() 
rcp85_2075_sst
ggsave(filename = "rcp85_2075_sst.png", plot = rcp85_2075_sst, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

rcp85_2075_tbtm = ggplot() +
  geom_stars(data = rcp85_2075[2,,,8]) +
  scale_fill_steps(name = "Temperature at Bottom (\u00B0C)", limits = c(12, 28), n.breaks = 7, low = "#DBFAF9", high = "#02877A") +
  theme_void() 
rcp85_2075_tbtm
ggsave(filename = "rcp85_2075_tbtm.png", plot = rcp85_2075_tbtm, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

rcp85_2075_mld = ggplot() +
  geom_stars(data = rcp85_2075[3,,,8]) +
  scale_fill_steps(name = "Mixed Layer Depth (m)", limits = c(8, 38), n.breaks = 7, low = "#cbc2b9", high = "#5e3719") +
  theme_void() 
rcp85_2075_mld
ggsave(filename = "rcp85_2075_mld.png", plot = rcp85_2075_mld, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

rcp85_2075_sss = ggplot() +
  geom_stars(data = rcp85_2075[4,,,8]) +
  scale_fill_steps(name = "Sea Surface Salinity (ppm)", limits = c(10, 30), n.breaks = 7, low = "#ffd2b6", high = "#c24e00") +
  theme_void() 
rcp85_2075_sss
ggsave(filename = "rcp85_2075_sss.png", plot = rcp85_2075_sss, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

rcp85_2075_sbtm = ggplot() +
  geom_stars(data = rcp85_2075[5,,,8]) +
  scale_fill_steps(name = "Salinity at Bottom (ppm)", limits = c(12, 30), n.breaks = 7, low = "#fff5b5", high = "#a49c00") +
  theme_void() 
rcp85_2075_sbtm
ggsave(filename = "rcp85_2075_sbtm.png", plot = rcp85_2075_sbtm, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

aug_rcp85_2075_sst  = rcp85_2075[1,,,8, drop = TRUE]
aug_rcp85_2075_tbtm = rcp85_2075[2,,,8, drop = TRUE]
aug_rcp85_2075_mld  = rcp85_2075[3,,,8, drop = TRUE]
aug_rcp85_2075_sss  = rcp85_2075[4,,,8, drop = TRUE]
aug_rcp85_2075_sbtm = rcp85_2075[5,,,8, drop = TRUE]
aug_rcp85_2075_combined_covars = c(brickman_bathymetry, 
                                   aug_rcp85_2075_sst, 
                                   aug_rcp85_2075_tbtm, 
                                   aug_rcp85_2075_mld, 
                                   aug_rcp85_2075_sss, 
                                   aug_rcp85_2075_sbtm, 
                                   along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm", "brick_mld", "brick_sss", "brick_sbtm"))

aug.ws.model = read_maxnet("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/modeling_workflow/versions/v01/v01.008/model.rds")
plot(aug.ws.model)
aug_rcp85_2075 = predict(aug.ws.model, aug_rcp85_2075_combined_covars, type = "cloglog")
aug_rcp85_2075_pred = ggplot() +
  geom_stars(data = aug_rcp85_2075) +
  scale_fill_binned(type = "viridis", name = "Habitat Suitability", limits = c(0, 1), n.breaks = 6) +
  ggtitle("August RCP 8.5 2075 Forecast") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
aug_rcp85_2075_pred
ggsave(filename = "aug_rcp85_2075_pred.png", plot = aug_rcp85_2075_pred, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

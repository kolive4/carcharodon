suppressPackageStartupMessages({
  library(stars)
  library(dplyr)
  library(viridis)
  library(colorRamps)
  library(colorspace)
  library(devtools)
  library(brickman)
  library(RColorBrewer)
  library(ggplot2)
  library(gridExtra)
})

curated = read.csv(file.path(cfg$data_path, "historical/curated_literature1.csv")) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

august_curated = curated |>
  dplyr::filter(Month == "August") |>
  dplyr::mutate(month = match(Month, month.name)) |>
  dplyr::select(-c(Month, eventDate)) |>
  dplyr::mutate(extractDate = as.Date(sprintf("2020-%0.2i-01", month))) |>
  sf::st_crop(shark_box)

aug_cur = aug_cur |>
  dplyr::mutate(method = "Historical")
aug_obis_obs = aug_obis_obs |>
  dplyr::mutate(method = "OBIS")
aug_combo = bind_rows(aug_obis_obs, aug_cur)

ggplot() +
  geom_sf(data = aug_combo, shape = aug_combo$method)

shark_recent = read_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis/shark_recent_occs.gpkg"))
aug_shark = shark_recent |>
  filter(month == 8)
plot(aug_shark)

present_sst = read_stars(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon/PRESENT_PRESENT_SST_mon.tif"))
plot(present_sst[,,,8], col = blue2red(10), key.pos = 4, reset = FALSE)
plot(aug_shark, pch = "x", cex = 1.5, col = "black", add = TRUE)
#OR
plot(present_sst[,,,8], col = rev(heat_hcl(10)), key.pos = 4, reset = FALSE)
plot(aug_shark, pch = "x", cex = 1.5, col = "black", add = TRUE)

present_sst_points = ggplot() +
                        geom_stars(data = present_sst[,,,8]) +
                        scale_fill_steps(name = "Temperature (\u00B0C)", limits = c(15, 28), n.breaks = 7, low = "#FEEDDE", high = "#8C2D04") +
                        # geom_sf(data = aug_shark, shape = 23, fill = "white", color = "black", size = 2.5) +
                        # geom_sf(data = august_curated, shape = 23, fill = "white", color = "black", size = 2.5) +
  geom_sf(data = aug_combo, aes(shape = method), show.legend = "point") +
  scale_shape_manual(name = "Method", values = c(21, 24)) +
                        theme_void() 
present_sst_points
ggsave(filename = "present_sst_points.png", plot = present_sst_points, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)


present_tbtm = read_stars(file.path(cfg$data_path, "brickman/gom_carcharodon/PRESENT_PRESENT_Tbtm_mon.tif"))
plot(present_tbtm[,,,8], col = blue2red(10), key.pos = 4, reset = FALSE)
plot(aug_shark, pch = "x", cex = 1.5, col = "black", add = TRUE)
#OR
plot(present_tbtm[,,,8], col = rev(heat_hcl(10)), key.pos = 4, reset = FALSE)
plot(aug_shark, pch = "x", cex = 1.5, col = "black", add = TRUE)

present_tbtm_points = ggplot() +
                        geom_stars(data = present_tbtm[,,,8]) +
  scale_fill_steps(name = "Temperature (\u00B0C)", limits = c(0, 28), n.breaks = 7, low = "#e5f5e0", high = "#00441b") +
  geom_sf(data = aug_shark, shape = 24, fill = "white", color = "black", size = 2.5) +
  geom_sf(data = august_curated, shape = 21, fill = "white", color = "black", size = 2.5) +
                        theme_void()
present_tbtm_points
ggsave(filename = "present_tbtm_points.png", plot = present_tbtm_points, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)


bathy = read_stars(file.path(cfg$data_path, "brickman/bathy/PRESENT_PRESENT_Bathy_depth_mon.tif"))
log_bathy = log10(bathy)
plot(bathy, key.pos = 4, reset = FALSE)
plot(aug_shark, pch = "x", cex = 1.5, col = "orange", add = TRUE)
#OR
plot(bathy, col = rev(blues9), key.pos = 4, reset = FALSE)
plot(aug_shark, pch = "x", cex = 1.5, col = "orange", add = TRUE)

bathymetry_plot = ggplot() +
  geom_stars(data = bathy) +
  scale_fill_binned(type = "gradient", name = "Depth", low = "white", high = "black", n.breaks = 5) +
  geom_sf(data = aug_shark, shape = 23, fill = "white", color = "black", size = 2.5) +
  ggtitle("Depth") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
bathymetry_plot

log_bathymetry_plot = ggplot() +
  geom_stars(data = log_bathy) +
  scale_fill_gradient(name = "log(Depth)", low = "white", high = "black") +
  geom_sf(data = aug_shark, shape = 23, fill = "white", color = "blue", size = 1.5) +
  ggtitle("Log Depth") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
log_bathymetry_plot

log_bathymetry_binned_plot = ggplot() +
  geom_stars(data = log_bathy) +
  scale_fill_steps(name = "log(Depth)", n.breaks = 7, low = "#deebf7", high = "#08306b") +
  geom_sf(data = aug_shark, shape = 21, fill = "white", color = "black", size = 2.5) +
  geom_sf(data = august_curated, shape = 24, fill = "white", color = "black", size = 2.5) +
  # geom_sf(data = aug_combo, shape = id, fill = "white", color = "black", size = 2.5)+
  theme_void() 
log_bathymetry_binned_plot
ggsave(filename = "log_depth_binned.png", plot = log_bathymetry_binned_plot, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)



RCP85_2075_sst = load_brickman(scenario = "RCP85", 
                               year = 2075, 
                               vars = "SST", 
                               interval = "mon",
                               path = file.path(cfg$data_path, "brickman/gom_carcharodon"))
plot(RCP85_2075_sst[,,,8], col = rev(heat_hcl(10)), key.pos = 4, reset = FALSE)

RCP85_2075_tbtm = load_brickman(scenario = "RCP85", 
                               year = 2075, 
                               vars = "Tbtm", 
                               interval = "mon",
                               path = file.path(cfg$data_path, "brickman/gom_carcharodon"))


rcp85_2075_sst_plot = ggplot() +
                        geom_stars(data = RCP85_2075_sst[,,,8]) +
  scale_fill_steps(name = "Temperature (\u00B0C)", limits = c(15, 28), n.breaks = 7, low = "#FEEDDE", high = "#8C2D04") +
                        theme_void()
rcp85_2075_sst_plot
ggsave(filename = "rcp85_2075_sst.png", plot = rcp85_2075_sst_plot, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)


rcp85_2075_tbtm_plot = ggplot() +
                        geom_stars(data = RCP85_2075_tbtm[,,,8]) +
  scale_fill_steps(name = "Temperature (\u00B0C)", limits = c(0, 28), n.breaks = 7, low = "#e5f5e0", high = "#00441b") +
                        theme_void() 
rcp85_2075_tbtm_plot
ggsave(filename = "rcp85_2075_tbtm.png", plot = rcp85_2075_tbtm_plot, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)


plot(RCP85_2075_tbtm[,,,8], col = scale_color_continuous_sequential(palette = "Heat", begin = 3, end = 27), key.pos = 4, reset = FALSE)


# Coastline mapping
# need to read in brickman bathymetry for this to work
brick_total_sa = buffer_depth(brickman_bathymetry, cfg$static_covariates, depth = 5000)
brick_total_sa_contour = st_contour(brick_total_sa, breaks = c(0, Inf))
write_sf(brick_total_sa_contour, file.path(cfg$data_path, "brickman/study_area_coastline.gpkg"))
coastline = read_sf(file.path(cfg$data_path, "brickman/study_area_coastline.gpkg"))
cc_coastline = ggplot() +
  geom_sf(data = coastline, 
          aes(), colour = "black", fill = "white", lwd = 1.5) 
cc_coastline
ggsave(filename = "carcharodon_coastline.png", plot = cc_coastline, path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/figures", width = 11, height = 8.5, units = "in", dpi = 300)

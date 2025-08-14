suppressPackageStartupMessages({
  library(charlier)
  library(argparser)
  library(dplyr)
  library(stars)
  library(brickman)
  library(twinkle)
  library(maxnet)
  library(maxnetic)
  library(ggplot2)
  library(patchwork)
  library(png)
  library(viridis)
})

args = argparser::arg_parser("a tool to cast monthly predictions into one figure",
                             name = "compiled_report_sharks.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports/c21.100760.01_12.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
for (f in list.files(cfg$source_path, pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}
vpars = charlier::parse_version(cfg$version)
vpath = file.path(cfg$root_path, cfg$report_path, "versions", vpars[["major"]], vpars[["minor"]])
if (!dir.exists(vpath)) 
  dir.create(vpath, showWarnings = FALSE, recursive = TRUE)

charlier::start_logger(filename = file.path(vpath, "log"))
charlier::info("writing config")
charlier::write_config(cfg, filename = file.path(vpath, basename(args$config)))

nefsc_cc_bb = cofbb::get_bb("nefsc_carcharodon", "sf")

coast = rnaturalearth::ne_coastline(scale = 'large', returnclass = 'sf') |>
  sf::st_geometry() |>
  sf::st_crop(nefsc_cc_bb)

plot_coast = function() {
  plot(coast, col = 'black', add = TRUE)
}

pal = terra::map.pal("magma", 10)
breaks = seq(from = 0, to = 1, length.out = length(pal) + 1)

if (stringr::str_sub(vpars["major"], start = 2, end = 2) %in% c(1,2)) {
  obs_bg = file.path(cfg$root_path, cfg$thinned_data_path, "thinned_obs_bg.gpkg")
} else {
  obs_bg = file.path(cfg$root_path, cfg$gather_data_path, "brickman_covar_obs_bg.gpkg")
}

obs = read_brickman_points(file = obs_bg) |>
  sf::st_as_sf() |>
  dplyr::filter(id == 1, basisOfRecord %in% cfg$basisOfRecord) |>
  dplyr::mutate(class = "presence") |>
  dplyr::filter(month %in% as.numeric(seq(from = 1, to = 12))) 

obs$month = factor(month.abb[obs$month], levels = month.abb)

rf_cast_files = list.files(path = file.path(cfg$root_path, cfg$tidy_path), 
                           pattern = "rf_prediction.tif",
                           recursive = TRUE,
                           full.names = TRUE) 

z = basename(dirname(rf_cast_files))
len = nchar(z) 
imonth = substring(z, len -1) |>
  as.numeric()

rf_cast_plots = stars::read_stars(rf_cast_files, along = list(month = month.abb[imonth])) |>
  dplyr::rename("Habitat Suitability Index" = "rf_prediction.tif")

rf = ggplot() +
  geom_stars(data = rf_cast_plots) +
  scale_fill_binned(type = "viridis", 
                    name = "Habitat Suitability\nIndex", 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb"))
if(stringr::str_sub(vpars["minor"], start = -1) == 0) {
  rf = rf +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            size = 1,
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("OBIS" = 16,
                                  "curated" = 17,
                                  "iNaturalist" = 0,
                                  "PSAT" = 3,
                                  "SPOT" = 12
                       ),
                       labels = c("OBIS", "Curated", "iNaturalist", "PSAT", "SPOT")) 
}
rf = rf +
  facet_wrap(~month) +
  theme_void()

png(file.path(vpath, paste0(cfg$version, "_rf_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(rf,
     col = pal,
     breaks = breaks,
     hook = plot_coast)
ok = dev.off()

bt_cast_files = list.files(path = file.path(cfg$root_path, cfg$tidy_path), 
                           pattern = "bt_prediction.tif",
                           recursive = TRUE,
                           full.names = TRUE) 

bt_cast_plots = stars::read_stars(bt_cast_files, along = list(month = month.abb[imonth])) |>
  dplyr::rename("Habitat Suitability Index" = "bt_prediction.tif")

bt = ggplot() +
  geom_stars(data = bt_cast_plots) +
  scale_fill_binned(type = "viridis", 
                    name = "Habitat Suitability\nIndex", 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb"))
if(stringr::str_sub(vpars["minor"], start = -1) == 0) {
  bt = bt +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            size = 1,
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("OBIS" = 16,
                                  "curated" = 17,
                                  "iNaturalist" = 0,
                                  "PSAT" = 3,
                                  "SPOT" = 12
                       ),
                       labels = c("OBIS", "Curated", "iNaturalist", "PSAT", "SPOT")) 
}
bt = bt +
  facet_wrap(~month) +
  theme_void()

png(file.path(vpath, paste0(cfg$version, "_bt_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(bt,
     col = pal,
     breaks = breaks,
     hook = plot_coast)
ok = dev.off()

maxent_cast_files = list.files(path = file.path(cfg$root_path, cfg$tidy_path), 
                               pattern = "maxent_prediction.tif",
                               recursive = TRUE,
                               full.names = TRUE) 

maxent_cast_plots = stars::read_stars(maxent_cast_files, along = list(month = month.abb[imonth])) |>
  dplyr::rename("Habitat Suitability Index" = "maxent_prediction.tif")

maxent = ggplot() +
  geom_stars(data = maxent_cast_plots) +
  scale_fill_binned(type = "viridis", 
                    name = "Habitat Suitability\nIndex", 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb"))
if(stringr::str_sub(vpars["minor"], start = -1) == 0) {
  maxent = maxent +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            size = 1,
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("OBIS" = 16,
                                  "curated" = 17,
                                  "iNaturalist" = 0,
                                  "PSAT" = 3,
                                  "SPOT" = 12
                       ),
                       labels = c("OBIS", "Curated", "iNaturalist", "PSAT", "SPOT"))
}
maxent = maxent +
  facet_wrap(~month) +
  theme_void()

png(file.path(vpath, paste0(cfg$version, "_maxent_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(maxent,
     col = pal,
     breaks = breaks,
     hook = plot_coast)
ok = dev.off()

gam_cast_files = list.files(path = file.path(cfg$root_path, cfg$tidy_path), 
                            pattern = "gam_prediction.tif",
                            recursive = TRUE,
                            full.names = TRUE) 

gam_cast_plots = stars::read_stars(gam_cast_files, along = list(month = month.abb[imonth])) |>
  dplyr::rename("Habitat Suitability Index" = "gam_prediction.tif")

gam = ggplot() +
  geom_stars(data = gam_cast_plots) +
  scale_fill_binned(type = "viridis", 
                    name = "Habitat Suitability\nIndex", 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb"))
if(stringr::str_sub(vpars["minor"], start = -1) == 0) {
  gam = gam +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            size = 1,
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("OBIS" = 16,
                                  "curated" = 17,
                                  "iNaturalist" = 0,
                                  "PSAT" = 3,
                                  "SPOT" = 12
                       ),
                       labels = c("OBIS", "Curated", "iNaturalist", "PSAT", "SPOT"))
}
gam = gam +
  facet_wrap(~month) +
  theme_void()

png(file.path(vpath, paste0(cfg$version, "_gam_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(gam,
     col = pal,
     breaks = breaks,
     hook = plot_coast)
ok = dev.off()

glm_cast_files = list.files(path = file.path(cfg$root_path, cfg$tidy_path), 
                            pattern = "glm_prediction.tif",
                            recursive = TRUE,
                            full.names = TRUE) 

glm_cast_plots = stars::read_stars(glm_cast_files, along = list(month = month.abb[imonth])) |>
  dplyr::rename("Habitat Suitability Index" = "glm_prediction.tif")

glm = ggplot() +
  geom_stars(data = glm_cast_plots) +
  scale_fill_binned(type = "viridis", 
                    name = "Habitat Suitability\nIndex", 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb"))
if(stringr::str_sub(vpars["minor"], start = -1) == 0) {
  glm = glm +
    geom_sf(data = obs, 
            aes(shape = basisOfRecord), 
            color = "red",
            size = 1,
            show.legend = "point") +
    scale_shape_manual(name = "Observation Type",
                       values = c("OBIS" = 16,
                                  "curated" = 17,
                                  "iNaturalist" = 0,
                                  "PSAT" = 3,
                                  "SPOT" = 12
                       ),
                       labels = c("OBIS", "Curated", "iNaturalist", "PSAT", "SPOT"))
}
glm = glm +
  facet_wrap(~month) +
  theme_void()

png(file.path(vpath, paste0(cfg$version, "_glm_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(glm,
     col = pal,
     breaks = breaks,
     hook = plot_coast)
ok = dev.off()

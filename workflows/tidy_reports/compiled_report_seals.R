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
                             name = "compiled_report_seals.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports/c12.00020.01_12.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)

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

png(file.path(vpath, paste0(cfg$version, "rf_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(rf_cast_plots,
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

png(file.path(vpath, paste0(cfg$version, "bt_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(bt_cast_plots,
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

png(file.path(vpath, paste0(cfg$version, "maxent_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(maxent_cast_plots,
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

png(file.path(vpath, paste0(cfg$version, "gam_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(gam_cast_plots,
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

png(file.path(vpath, paste0(cfg$version, "glm_compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(glm_cast_plots,
     col = pal,
     breaks = breaks,
     hook = plot_coast)
ok = dev.off()

rf_metrics_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                        pattern = sprintf("%s_rf_final_metrics.csv", cfg$tidy_w_vers),
                        recursive = TRUE,
                        full.names = TRUE) 
if (TRUE %in% file.exists(rf_metrics_files)) {
    rf_metrics = lapply(rf_metrics_files, readr::read_csv) |>
      dplyr::bind_rows(.id = "month") |>
      dplyr::select(c("month", ".metric", ".estimate")) |>
      dplyr::mutate(month = as.numeric(month))
  
  rf_metric_plot = ggplot() +
    geom_line(data = rf_metrics, aes(x = month, y = .estimate, color = .metric)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(1, 12), n.breaks = 12) +
    theme_classic() +
    ggtitle(cfg$graphics$rf_metrics_title) +
    labs(x = cfg$graphics$x)
  rf_metric_plot
  ggsave(filename = sprintf("%s_rf_metrics.png", cfg$version),
         plot = rf_metric_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}

bt_metrics_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                              pattern = sprintf("%s_bt_final_metrics.csv", cfg$tidy_w_vers),
                              recursive = TRUE,
                              full.names = TRUE) 
if (TRUE %in% file.exists(bt_metrics_files)) {
  bt_metrics = lapply(bt_metrics_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", ".metric", ".estimate")) |>
    dplyr::mutate(month = as.numeric(month))
  
  bt_metric_plot = ggplot() +
    geom_line(data = bt_metrics, aes(x = month, y = .estimate, color = .metric)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(1, 12), n.breaks = 12) +
    theme_classic() +
    ggtitle(cfg$graphics$bt_metrics_title) +
    labs(x = cfg$graphics$x)
  bt_metric_plot
  ggsave(filename = sprintf("%s_bt_metrics.png", cfg$version),
         plot = bt_metric_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}

maxent_metrics_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                              pattern = sprintf("%s_maxent_final_metrics.csv", cfg$tidy_w_vers),
                              recursive = TRUE,
                              full.names = TRUE) 
if (TRUE %in% file.exists(maxent_metrics_files)) {
  maxent_metrics = lapply(maxent_metrics_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", ".metric", ".estimate")) |>
    dplyr::mutate(month = as.numeric(month))
  
  maxent_metric_plot = ggplot() +
    geom_line(data = maxent_metrics, aes(x = month, y = .estimate, color = .metric)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(1, 12), n.breaks = 12) +
    theme_classic() +
    ggtitle(cfg$graphics$maxent_metrics_title) +
    labs(x = cfg$graphics$x)
  maxent_metric_plot
  ggsave(filename = sprintf("%s_maxent_metrics.png", cfg$version),
         plot = maxent_metric_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}

gam_metrics_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                               pattern = sprintf("%s_gam_final_metrics.csv", cfg$tidy_w_vers),
                               recursive = TRUE,
                               full.names = TRUE) 
if (TRUE %in% file.exists(gam_metrics_files)) {
  gam_metrics = lapply(gam_metrics_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", ".metric", ".estimate")) |>
    dplyr::mutate(month = as.numeric(month))
  
  gam_metric_plot = ggplot() +
    geom_line(data = gam_metrics, aes(x = month, y = .estimate, color = .metric)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(1, 12), n.breaks = 12) +
    theme_classic() +
    ggtitle(cfg$graphics$gam_metrics_title) +
    labs(x = cfg$graphics$x)
  gam_metric_plot
  ggsave(filename = sprintf("%s_gam_metrics.png", cfg$version),
         plot = gam_metric_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}

glm_metrics_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                               pattern = sprintf("%s_glm_final_metrics.csv", cfg$tidy_w_vers),
                               recursive = TRUE,
                               full.names = TRUE) 
if (TRUE %in% file.exists(glm_metrics_files)) {
  glm_metrics = lapply(glm_metrics_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", ".metric", ".estimate")) |>
    dplyr::mutate(month = as.numeric(month))
  
  glm_metric_plot = ggplot() +
    geom_line(data = glm_metrics, aes(x = month, y = .estimate, color = .metric)) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(1, 12), n.breaks = 12) +
    theme_classic() +
    ggtitle(cfg$graphics$glm_metrics_title) +
    labs(x = cfg$graphics$x)
  glm_metric_plot
  ggsave(filename = sprintf("%s_glm_metrics.png", cfg$version),
         plot = glm_metric_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}

rf_vi_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                        pattern = sprintf("%s_rf_vi.csv", cfg$tidy_w_vers),
                        recursive = TRUE,
                        full.names = TRUE) 
if (TRUE %in% file.exists(rf_vi_files)) {
    rf_vi = lapply(rf_vi_files, readr::read_csv) |>
      dplyr::bind_rows(.id = "month") |>
      dplyr::select(c("month", "var", "importance")) |>
      dplyr::mutate(month = as.numeric(month))
  
  rf_vi_plot = ggplot() +
    geom_bar(data = rf_vi, aes(x = month, y = importance, fill = var),
             position = "fill", stat = "identity") +
    scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
    theme_classic() +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(cfg$graphics$vi_title) +
    labs(x = cfg$graphics$x, 
         y = cfg$graphics$vi_y)
  rf_vi_plot
  ggsave(filename = sprintf("%s_rf_varimp.png", cfg$version),
         plot = rf_vi_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}


bt_vi_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                         pattern = sprintf("%s_bt_vi.csv", cfg$tidy_w_vers),
                         recursive = TRUE,
                         full.names = TRUE) 
if (TRUE %in% file.exists(bt_vi_files)) {
  bt_vi = lapply(bt_vi_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", "var", "importance")) |>
    dplyr::mutate(month = as.numeric(month))
  
  bt_vi_plot = ggplot() +
    geom_bar(data = bt_vi, aes(x = month, y = importance, fill = var),
             position = "fill", stat = "identity") +
    scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
    theme_classic() +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(cfg$graphics$vi_title) +
    labs(x = cfg$graphics$x, 
         y = cfg$graphics$vi_y)
  bt_vi_plot
  ggsave(filename = sprintf("%s_bt_varimp.png", cfg$version),
         plot = bt_vi_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}


maxent_vi_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                         pattern = sprintf("%s_maxent_vi.csv", cfg$tidy_w_vers),
                         recursive = TRUE,
                         full.names = TRUE) 
if (TRUE %in% file.exists(maxent_vi_files)) {
  maxent_vi = lapply(maxent_vi_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", "var", "importance")) |>
    dplyr::mutate(month = as.numeric(month))
  
  maxent_vi_plot = ggplot() +
    geom_bar(data = maxent_vi, aes(x = month, y = importance, fill = var),
             position = "fill", stat = "identity") +
    scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
    theme_classic() +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(cfg$graphics$vi_title) +
    labs(x = cfg$graphics$x, 
         y = cfg$graphics$vi_y)
  maxent_vi_plot
  ggsave(filename = sprintf("%s_maxent_varimp.png", cfg$version),
         plot = maxent_vi_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}

gam_vi_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                          pattern = sprintf("%s_gam_vi.csv", cfg$tidy_w_vers),
                          recursive = TRUE,
                          full.names = TRUE) 
if (TRUE %in% file.exists(gam_vi_files)) {
  gam_vi = lapply(gam_vi_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", "var", "importance")) |>
    dplyr::mutate(month = as.numeric(month))
  
  gam_vi_plot = ggplot() +
    geom_bar(data = gam_vi, aes(x = month, y = importance, fill = var),
             position = "fill", stat = "identity") +
    scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
    theme_classic() +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(cfg$graphics$vi_title) +
    labs(x = cfg$graphics$x, 
         y = cfg$graphics$vi_y)
  gam_vi_plot
  ggsave(filename = sprintf("%s_gam_varimp.png", cfg$version),
         plot = gam_vi_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}

glm_vi_files = list.files(path = file.path(cfg$root_path, cfg$tidy_w_path, cfg$tidy_w_vers), 
                          pattern = sprintf("%s_glm_vi.csv", cfg$tidy_w_vers),
                          recursive = TRUE,
                          full.names = TRUE) 
if (TRUE %in% file.exists(glm_vi_files)) {
  glm_vi = lapply(glm_vi_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", "var", "importance")) |>
    dplyr::mutate(month = as.numeric(month))
  
  glm_vi_plot = ggplot() +
    geom_bar(data = glm_vi, aes(x = month, y = importance, fill = var),
             position = "fill", stat = "identity") +
    scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
    theme_classic() +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(cfg$graphics$vi_title) +
    labs(x = cfg$graphics$x, 
         y = cfg$graphics$vi_y)
  glm_vi_plot
  ggsave(filename = sprintf("%s_glm_varimp.png", cfg$version),
         plot = glm_vi_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}


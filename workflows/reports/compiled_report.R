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
                             name = "compiled_report.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/reports/c01.0900.01_12.yaml",
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

cast_files = list.files(path = file.path(cfg$root_path, cfg$f_path), 
                   pattern = "prediction.tif",
                   recursive = TRUE,
                   full.names = TRUE) 

coast = rnaturalearth::ne_coastline(scale = 'large', returnclass = 'sf') |>
  sf::st_geometry() |>
  sf::st_crop(nefsc_cc_bb)

plot_coast = function() {
  plot(coast, col = 'black', add = TRUE)
}

pal = terra::map.pal("magma", 10)
breaks = seq(from = 0, to = 1, length.out = length(pal) + 1)

cast_plots = stars::read_stars(cast_files, along = list(month = month.abb)) |>
  dplyr::rename("Habitat Suitability Index" = "prediction.tif")

png(file.path(vpath, paste0(cfg$version, "compiled_casts.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(cast_plots,
     col = pal,
     breaks = breaks,
     hook = plot_coast)
ok = dev.off()

pauc_files = list.files(path = file.path(cfg$root_path, cfg$f_path), 
                        pattern = "pauc.csv",
                        recursive = TRUE,
                        full.names = TRUE) 
if (TRUE %in% file.exists(pauc_files)) {
  paucs = lapply(pauc_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", "value")) |>
    dplyr::rename(pauc = "value") |>
    dplyr::mutate(month = as.numeric(month))
  
  pauc_plot = ggplot() +
    # geom_area(data = paucs, aes(x = month, y = pauc), color = "navy", fill = "skyblue4") +
    geom_line(data = paucs, aes(x = month, y = pauc), color = "navy") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(1, 12), n.breaks = 12) +
    theme_classic() +
    ggtitle(cfg$graphics$pauc_title) +
    labs(x = cfg$graphics$x, 
         y = cfg$grapics$pauc_y)
  pauc_plot
  ggsave(filename = sprintf("%s_paucs.png", cfg$version),
         plot = pauc_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}

vi_files = list.files(path = file.path(cfg$root_path, cfg$m_path), 
                        pattern = "variable_importance.csv",
                        recursive = TRUE,
                        full.names = TRUE) 
if (TRUE %in% file.exists(vi_files)) {
  vi = lapply(vi_files, readr::read_csv) |>
    dplyr::bind_rows(.id = "month") |>
    dplyr::select(c("month", "var", "importance")) |>
    dplyr::mutate(month = as.numeric(month))
  
  vi_plot = ggplot() +
    geom_bar(data = vi, aes(x = month, y = importance, fill = var),
             position = "fill", stat = "identity") +
    scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
    theme_classic() +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(cfg$graphics$vi_title) +
    labs(x = cfg$graphics$x, 
         y = cfg$graphics$vi_y)
  vi_plot
  ggsave(filename = sprintf("%s_varimp.png", cfg$version),
         plot = vi_plot, 
         path = vpath, 
         width = 11, height = 8.5, units = "in", dpi = 300)
}


# cov_path = list.files(path = file.path(cfg$root_path, cfg$f_path), 
#                       pattern = "*.*.*")
# list_cov_files = function(x){
#   files = list.files(path = file.path(cfg$root_path, cfg$f_path, x, "figures"),
#                      pattern = "*.png",
#                      full.names = TRUE)
#   return(files)
# }
# cov_fig_path = lapply(cov_path, 
#                       list_cov_files)
# 
# covs = lapply(cov_fig_path, png::readPNG)
# png(file.path(vpath, paste0(cfg$version, "compiled_covariates.png")), 
#     bg = "white", width = 11, height = 8.5, units = "in", res = 300)
# plot(covs)
# ok = dev.off()

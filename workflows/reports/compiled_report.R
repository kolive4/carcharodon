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
})

args = argparser::arg_parser("a tool to cast monthly predictions into one figure",
                             name = "compiled_report.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/reports/c01.0001.01_12.yaml",
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

cast_plots = stars::read_stars(cast_files, along = list(month = month.abb)) |>
  dplyr::rename("Habitat Suitability Index" = "prediction.tif")

png(file.path(vpath, paste0(cfg$version, "compiled_casts.png")))
plot(cast_plots,
     col = pal,
     hook = plot_coast)
ok = dev.off()

pauc_files = list.files(path = file.path(cfg$root_path, cfg$f_path), 
                        pattern = "pauc.csv",
                        recursive = TRUE,
                        full.names = TRUE) 

paucs = lapply(pauc_files, readr::read_csv) |>
  dplyr::bind_rows(.id = "month") |>
  dplyr::select(c("month", "value")) |>
  dplyr::rename(pauc = "value") |>
  dplyr::mutate(month = as.numeric(month))

pauc_plot = ggplot() +
  geom_area(data = paucs, aes(x = month, y = pauc), color = "navy", fill = "skyblue4") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(1, 12), n.breaks = 11) +
  theme_classic() +
  ggtitle(cfg$graphics$title) +
  labs(x = cfg$graphics$x, 
       y = "pAUC")
pauc_plot
ggsave(filename = sprintf("%s_paucs.png", cfg$version),
       plot = pauc_plot, 
       path = vpath, 
       width = 11, height = 8.5, units = "in", dpi = 300)

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
                             name = "compiled_casts.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/reports/c01.100.01_12.yaml",
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

cast_plots = read_stars(cast_files, along = list(month = month.abb)) |>
  rename("Habitat Suitability Index" = "prediction.tif")

png(file.path(vpath, paste0(cfg$version, "compiled_casts.png")))
plot(cast_plots,
     col = pal,
     hook = plot_coast)
ok = dev.off()

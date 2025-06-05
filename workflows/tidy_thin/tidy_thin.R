suppressPackageStartupMessages({
  library(charlier)
  library(argparser)
  library(dplyr)
  library(stars)
  library(brickman)
  library(twinkle)
  library(purrr)
  library(ggplot2)
})

args = argparser::arg_parser("thinning observation and background data",
                             name = "tidy_thin.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_thin/v03.00000.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
for (f in list.files(cfg$source_path, pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}

vpars = charlier::parse_version(cfg$version)
vpath = file.path(cfg$root_path, cfg$output_path, "versions", vpars[["major"]], cfg$version)
if (!dir.exists(vpath)) 
  dir.create(vpath, showWarnings = FALSE, recursive = TRUE)

charlier::start_logger(filename = file.path(vpath, "log"))
charlier::info("writing config")
charlier::write_config(cfg, filename = file.path(vpath, basename(args$config)))

vfigure_path = file.path(vpath, "figures")
if (!dir.exists(vfigure_path)) 
  dir.create(vfigure_path, showWarnings = FALSE, recursive = TRUE)

mask = mask = stars::read_stars(file.path(cfg$data_path, cfg$mask_name)) |>
  rlang::set_names("mask")

obs_bg = read_sf(file.path(cfg$gather_data_path, "brickman_covar_obs_bg.gpkg")) 

thin_obs = obs_bg |>
  dplyr::filter(id == 1) |>
  thin_by_BoR(BoR = cfg$bor_to_thin, mask = mask, dist = 10) 

thin_bg = obs_bg |>
  dplyr::filter(id == 0) |>
  thin_background(obs = thin_obs,
                  dist = 10)

thin_obs_bg = dplyr::bind_rows(thin_obs, thin_bg) |>
  write_sf(file.path(vpath, "thinned_obs_bg.gpkg"))

thin_obs_inst = match_institution(raw = file.path(cfg$root_path, cfg$data_path, cfg$obis_path, cfg$raw_file),
                                  thinned = thin_obs_bg)






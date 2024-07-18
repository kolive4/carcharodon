# usage: maxent_modeling.R [--] [--help] [--config CONFIG]
# 
# maxent modeling for white shark obs
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -c, --config  the name of the configuration file [default:
#                                                       /mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/modeling_workflow/v0.000.yaml]


suppressPackageStartupMessages({
  library(charlier)
  library(argparser)
  library(dplyr)
  library(stars)
  library(brickman)
  library(twinkle)
  library(maxnet)
  library(maxnetic)
})

args = argparser::arg_parser("maxent modeling for white shark obs",
                             name = "maxent_modeling.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/modeling_workflow/v01.0008.yaml",
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

nefsc_cc_bb = cofbb::get_bb("nefsc_carcharodon", "sf")

obs_brick = read_brickman_points(file.path(cfg$root_path, cfg$gather_data_vpath, "brickman_covar_obs_bg.gpkg"), bb = nefsc_cc_bb) |>
  dplyr::filter(month %in% cfg$month) |>
  dplyr::filter(id == 1, basisOfRecord %in% cfg$obs_filter$basisOfRecord) |>
  select(c("brick_sst", "brick_tbtm", "log_depth", 
           #"fish_biomass", "brick_depth",
           "brick_mld", "brick_sss", "brick_sbtm"
           #, "brick_u", "brick_v", "brick_xbtm"
           )) |>
  na.omit() |>
  write_sf(file.path(vpath, "obs_brick.gpkg"))

obs_drop = obs_brick |>
  st_drop_geometry()
  
bg_brick = read_brickman_points(file.path(cfg$root_path, cfg$gather_data_vpath, "brickman_covar_obs_bg.gpkg"), bb = nefsc_cc_bb) |>
  dplyr::filter(month %in% cfg$month) |>
  dplyr::filter(id == 0) |>
  select(c("brick_sst", "brick_tbtm", "log_depth",
           #"fish_biomass", "brick_depth",
           "brick_mld", "brick_sss", "brick_sbtm"
           #, "brick_u", "brick_v", "brick_xbtm"
           )) |>
  na.omit() |>
  write_sf(file.path(vpath, "bg_brick.gpkg"))

bg_drop = bg_brick |>
  st_drop_geometry()
plot(bg_brick["brick_sss"], pch = ".", reset = FALSE)
plot(coast, add = TRUE)

combo_drop = dplyr::bind_rows(obs_drop, bg_drop, .id = "presence")

ws.flag <- c(rep(1, nrow(obs_drop)), rep(0, nrow(bg_drop)))
ws.model <- maxnet::maxnet(ws.flag,
                                    dplyr::select(combo_drop, -presence),
                           addsamplestobackground = FALSE
                           ) |>
  write_maxnet(file.path(vpath, "model.rds"))

var_imp = maxnetic::variable_importance(ws.model, dplyr::bind_rows(obs_drop, bg_drop), type = "cloglog", arrange = "decreasing")

pdf(file.path(vpath, "variable_likelihood.pdf"))
plot(ws.model, type = "cloglog")
dev.off()

# observations: id = 1
# bg: id = 0
# separate by month variable








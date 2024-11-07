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
  library(readr)
})

args = argparser::arg_parser("maxent modeling for white shark obs",
                             name = "maxent_modeling.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/modeling_workflow/v02.000.12.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
for (f in list.files(cfg$source_path, pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}

vpars = charlier::parse_version(cfg$version)
vpath = file.path(cfg$root_path, cfg$output_path, "versions", vpars[["major"]], vpars[["minor"]], cfg$version)
if (!dir.exists(vpath)) 
  dir.create(vpath, showWarnings = FALSE, recursive = TRUE)

charlier::start_logger(filename = file.path(vpath, "log"))
charlier::info("writing config")
charlier::write_config(cfg, filename = file.path(vpath, basename(args$config)))

nefsc_cc_bb = cofbb::get_bb("nefsc_carcharodon", "sf")

obs_brick = read_brickman_points(file.path(cfg$root_path, cfg$gather_data_vpath, "brickman_covar_obs_bg.gpkg"), bb = nefsc_cc_bb) |>
  dplyr::filter(month %in% cfg$month) |>
  dplyr::filter(id == 1, basisOfRecord %in% cfg$obs_filter$basisOfRecord) |>
  select(c(cfg$mon_covariates, cfg$static_covariates)) |>
  dplyr::rename_with(~ stringr::str_remove(., "^brick_")) |>
  na.omit() |>
  sf::write_sf(file.path(vpath, "obs_brick.gpkg"))

obs_drop = obs_brick |>
  st_drop_geometry()
  
bg_brick = read_brickman_points(file.path(cfg$root_path, cfg$gather_data_vpath, "brickman_covar_obs_bg.gpkg"), bb = nefsc_cc_bb) |>
  dplyr::filter(month %in% cfg$month) |>
  dplyr::filter(id == 0) |>
  select(c(cfg$mon_covariates, cfg$static_covariates)) |>
  dplyr::rename_with(~ stringr::str_remove(., "^brick_")) |>
  na.omit() |>
  write_sf(file.path(vpath, "bg_brick.gpkg"))

bg_drop = bg_brick |>
  st_drop_geometry()

combo_drop = dplyr::bind_rows(obs_drop, bg_drop, .id = "presence")

count_tbl = tibble(n_obs = nrow(obs_drop), n_bg = nrow(bg_drop)) |>
  write_csv(file.path(vpath, "obs_bg_counts.csv"))

ws.flag <- c(rep(1, nrow(obs_drop)), rep(0, nrow(bg_drop)))
ws.model <- maxnet::maxnet(ws.flag,
                                    dplyr::select(combo_drop, -presence),
                           addsamplestobackground = TRUE
                           ) |>
  write_maxnet(file.path(vpath, "model.rds"))
p = predict(ws.model, dplyr::select(combo_drop, -presence)) |>
  as.vector()

ws.model.collect = plot(ws.model, type = "cloglog", plot = FALSE)
# x = model_rename(ws.model.collect)
# p = gather_plots(x)

var_imp = maxnetic::variable_importance(ws.model, dplyr::bind_rows(obs_drop, bg_drop), type = "cloglog", arrange = "decreasing") |>
  write.csv(file.path(vpath, "variable_importance.csv"))

png(file.path(vpath, "variable_likelihood.png"))
plot(ws.model, type = "cloglog")
ok = dev.off()
pdf(file.path(vpath, "variable_likelihood.pdf"))
plot(ws.model, type = "cloglog")
ok = dev.off()

# observations: id = 1
# bg: id = 0
# separate by month variable








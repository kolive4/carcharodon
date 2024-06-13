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

args = argparser::arg_parser("forecasting for white shark habitat suitability",
                             name = "forecasting.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/forecast_workflow/v00.000.yaml",
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

points = read_brickman_points(bb = nefsc_cc_bb)

mon_shark_obs = points |>
  dplyr::filter(month %in% as.numeric(cfg$month)) |>
  filter(id == 1)


# combine covariate layers together to make a predictive layer, do this for all years and scenarios

brickman_bathymetry = load_brickman(scenario = cfg$bathy_scenario, 
                                    vars = cfg$bathy_var, 
                                    band_as_time = TRUE, 
                                    path = file.path(cfg$root_path, cfg$data_path, "brickman/bathy"))
log_bathy = log10(brickman_bathymetry)


brick_covars = load_brickman(scenario = cfg$scenario,
                                year = cfg$year,
                                vars = cfg$covars, 
                                interval = "mon", 
                                band_as_time = TRUE, 
                                path = file.path(cfg$root_path, cfg$data_path, "brickman/gom_carcharodon")) # fix path

plot_covars(cfg, bathy = log_bathy, covars = brick_covars, obs = mon_shark_obs)

sst = brick_covars[1,,,as.numeric(cfg$month), drop = TRUE]
tbtm = brick_covars[2,,,as.numeric(cfg$month), drop = TRUE]
mld =  brick_covars[3,,,as.numeric(cfg$month), drop = TRUE]
sss =  brick_covars[4,,,as.numeric(cfg$month), drop = TRUE]
sbtm = brick_covars[5,,,as.numeric(cfg$month), drop = TRUE]
combined_covars = c(log_bathy, 
                    sst, 
                    tbtm, 
                    mld, 
                    sss, 
                    sbtm, along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm", "brick_mld", "brick_sss", "brick_sbtm"))

ws.model = read_maxnet(file.path(cfg$root_path, "workflows/modeling_workflow/versions", cfg$model_version_maj, cfg$model_version_maj_minor, "model.rds"))
plot(ws.model)
prediction = predict(ws.model, combined_covars, type = "cloglog")
pred = ggplot() +
  geom_stars(data = prediction) +
  scale_fill_binned(type = "viridis", 
                    name = "Habitat Suitability", 
                    limits = c(0, 1), 
                    n.breaks = 6) +
  geom_sf(data = mon_shark_obs, 
          aes(shape = basisOfRecord), 
          fill = "white", 
          show.legend = "point") +
  ggtitle(cfg$graphics$ggtitle) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
pred
ggsave(filename = sprintf("%s_prediction.png", cfg$version),
       plot = pred, 
       path = file.path(vpath, "figures"), 
       width = 11, height = 8.5, units = "in", dpi = 300)


# predict given the compiled covariate layers
aug_pres_pres_sst = present_present[1,,,8, drop = TRUE]
aug_pres_pres_tbtm = present_present[2,,,8, drop = TRUE]
aug_pres_pres_mld = present_present[3,,,8, drop = TRUE]
aug_pres_pres_sss = present_present[4,,,8, drop = TRUE]
aug_pres_pres_sbtm = present_present[5,,,8, drop = TRUE]
aug_pres_pres_combined_covars = c(brickman_bathymetry, aug_pres_pres_sst, aug_pres_pres_tbtm, aug_pres_pres_mld, aug_pres_pres_sss, aug_pres_pres_sbtm, along = NA_integer_) |>
  rlang::set_names(c("depth", "brick_sst", "brick_tbtm", "brick_mld", "brick_sss", "brick_sbtm"))

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
  library(ggplot2)
})

args = argparser::arg_parser("forecasting for white shark habitat suitability",
                             name = "forecasting.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/forecast_workflow/v01.09012.01.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
for (f in list.files(cfg$source_path, pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}

vpars = charlier::parse_version(cfg$version)
vpath = file.path(cfg$root_path, cfg$output_path, "versions", vpars[["major"]], vpars[["minor"]], cfg$version)
min_vpath = file.path(cfg$root_path, cfg$output_path, "versions", vpars[["major"]], vpars[["minor"]])
if (!dir.exists(vpath)) 
  dir.create(vpath, showWarnings = FALSE, recursive = TRUE)

charlier::start_logger(filename = file.path(vpath, "log"))
charlier::info("writing config")
charlier::write_config(cfg, filename = file.path(vpath, basename(args$config)))

nefsc_cc_bb = cofbb::get_bb("nefsc_carcharodon", "sf")
points = read_brickman_points(bb = nefsc_cc_bb)

mon_shark_obs = points |>
  dplyr::filter(month %in% as.numeric(cfg$month)) |>
  filter(id == 1, basisOfRecord %in% cfg$obs_filter$basisOfRecord)

cvr = sapply(cfg$covars, function(covar) {
  sprintf("%s_%s_%s_mon.tif", cfg$scenario, cfg$year, covar)
})
lookup = purrr::set_names(cvr, tolower(cfg$covars))

brick_covars = load_brickman(scenario = cfg$scenario,
                                year = cfg$year,
                                vars = cfg$covars, 
                                interval = "mon", 
                                band_as_time = TRUE, 
                                path = file.path(cfg$root_path, cfg$data_path, "brickman/gom_carcharodon")) |> # fix path
  rename(any_of(lookup))

combo_covar = dplyr::slice(brick_covars, time, as.numeric(cfg$month))

mask = stars::read_stars(file.path(cfg$root_path, cfg$data_path, cfg$mask_name)) |>
  rlang::set_names("mask") |>
  st_warp(dest = combo_covar)

ix = (mask[["mask"]] <= 0)
mask[["mask"]][ix] = NA_real_

# combine covariate layers together to make a predictive layer, do this for all years and scenarios
if ("Bathy_depth" %in% cfg$static_vars) {
  brickman_bathymetry = load_brickman(scenario = cfg$bathy_scenario, 
                                      vars = "Bathy_depth", 
                                      band_as_time = TRUE, 
                                      path = file.path(cfg$root_path, cfg$data_path, "brickman/bathy")) |>
    dplyr::rename(depth = "Bathy_depth")
  combo_covar = c(combo_covar, brickman_bathymetry) 
} 
if ("log_depth" %in% cfg$static_vars) { # is this right?
  if (!exists("brickman_bathymetry")) {
    brickman_bathymetry = load_brickman(scenario = cfg$bathy_scenario, 
                                        vars = "Bathy_depth", 
                                        band_as_time = TRUE, 
                                        path = file.path(cfg$root_path, cfg$data_path, "brickman/bathy")) |>
      dplyr::rename(depth = "Bathy_depth")
  }
  log_bathy = log10(brickman_bathymetry) |>
    dplyr::rename(log_depth = depth)
  combo_covar = c(combo_covar, log_bathy)
    
}

if("fish_biomass" %in% cfg$static_vars) {
  fish_layer = NULL
  if (cfg$which_fish == "COMBO") {
    if (as.numeric(cfg$month) %in% cfg$fall_fish_mon) {
      fish_layer = read_stars(file.path(cfg$data_path, cfg$fish_path, cfg$fall_fish_file)) |>
        sf::st_crop(nefsc_cc_bb) |>
        stars::st_warp(dest = brickman_bathymetry) |>
        dplyr::rename(fish_biomass = cfg$fall_fish_file)
    }
    if (as.numeric(cfg$month) %in% cfg$spring_fish_mon) {
      fish_layer = read_stars(file.path(cfg$data_path, cfg$fish_path, cfg$spring_fish_file)) |>
        sf::st_crop(nefsc_cc_bb) |>
        stars::st_warp(dest = combo_covar) |>
        dplyr::rename(fish_biomass = cfg$spring_fish_file)
    }  
  }
  if (cfg$which_fish == "FALL") {
    fish_layer = read_stars(file.path(cfg$data_path, cfg$fish_path, cfg$fall_fish_file)) |>
      sf::st_crop(nefsc_cc_bb) |>
      stars::st_warp(dest = combo_covar) |>
      dplyr::rename(fish_biomass = cfg$fall_fish_file)
  }
  if (cfg$which_fish == "SPRING") {
    fish_layer = read_stars(file.path(cfg$data_path, cfg$fish_path, cfg$spring_fish_file)) |>
      sf::st_crop(nefsc_cc_bb) |>
      stars::st_warp(dest = combo_covar) |>
      dplyr::rename(fish_biomass = cfg$spring_fish_file)
  }
  if (!is.null(fish_layer)) {
    combo_covar = c(combo_covar, fish_layer)
  }
}

if("dfs" %in% cfg$static_vars) {
  dfs_layer = read_stars(file.path(cfg$data_path, cfg$dfs_path, cfg$dfs_file)) |>
    dplyr::rename(dfs = cfg$dfs_file) |>
    stars::st_warp(dest = combo_covar)
  combo_covar = c(combo_covar, dfs_layer)
}

if("hseal" %in% cfg$static_vars) {
  if("seal_scenario" %in% names(cfg)) {
    hseal_layer = load_seal(scenario = cfg$seal_scenario, year = cfg$seal_year, species = "harbor") |>
      dplyr::slice(band, as.numeric(cfg$month)) |>
      dplyr::rename(hseal = "prediction.tif") |>
      stars::st_warp(dest = combo_covar)
    combo_covar = c(combo_covar, hseal_layer) 
  } else if (!"seal_scenario" %in% names(cfg)){
    hseal_layer = load_seal(scenario = cfg$scenario, year = cfg$year, species = "harbor") |>
      dplyr::slice(band, as.numeric(cfg$month)) |>
      dplyr::rename(hseal = "prediction.tif") |>
      stars::st_warp(dest = combo_covar)
    combo_covar = c(combo_covar, hseal_layer) 
  }
} 

if("gseal" %in% cfg$static_vars) {
  if("seal_scenario" %in% names(cfg)) {
    gseal_layer = load_seal(scenario = cfg$seal_scenario, year = cfg$seal_year, species = "gray") |>
      dplyr::slice(band, as.numeric(cfg$month)) |>
      dplyr::rename(gseal = "prediction.tif") |>
      stars::st_warp(dest = combo_covar)
    combo_covar = c(combo_covar, gseal_layer) 
  } else if (!"seal_scenario" %in% names(cfg)){
    gseal_layer = load_seal(scenario = cfg$scenario, year = cfg$year, species = "gray") |>
      dplyr::slice(band, as.numeric(cfg$month)) |>
      dplyr::rename(gseal = "prediction.tif") |>
      stars::st_warp(dest = combo_covar)
    combo_covar = c(combo_covar, gseal_layer) 
  }
}

combo_covar[is.na(mask)] = NA_real_

if (!is.null(cfg$contour_name)) {
  mask_contour = sf::read_sf(file.path(cfg$data_path, cfg$contour_name))
} else {NULL}

plot_covars(cfg, 
            bathy = if("Bathy_depth" %in% cfg$static_vars) {combo_covar["depth"]}
                    else{NULL},
            log_bathy = if ("log_depth" %in% cfg$static_vars) {combo_covar["log_depth"]} 
                    else {NULL} , 
            fish = if("fish_biomass" %in% cfg$static_vars){combo_covar["fish_biomass"]} 
                   else {NULL}, 
            dfs = if("dfs" %in% cfg$static_vars) {combo_covar["dfs"]} 
                  else {NULL},
            gseal = if("gseal" %in% cfg$static_vars) {combo_covar["gseal"]}
                  else {NULL},
            hseal = if("hseal" %in% cfg$static_vars) {combo_covar["hseal"]}
            else {NULL},
            covars = combo_covar, 
            obs = mon_shark_obs,
            contour = mask_contour,
            plot_contour = cfg$graphics$plot_contour,
            plot_points = cfg$graphics$plot_points
            )

ws.model = read_maxnet(file.path(cfg$root_path, cfg$modeling_vpath, "model.rds"))
plot(ws.model, type = "cloglog")
prediction = predict(ws.model, combo_covar, type = "cloglog") |>
  write_stars(file.path(vpath, "prediction.tif"))
pred = ggplot() +
  geom_stars(data = prediction) +
  scale_fill_binned(type = "viridis", 
                    name = "Habitat Suitability", 
                    limits = c(0, 1), 
                    n.breaks = 11) +
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
  ggtitle(cfg$graphics$ggtitle) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
  if (cfg$graphics$plot_points) {
    pred = pred +
      geom_sf(data = mon_shark_obs, 
            aes(shape = basisOfRecord), 
            fill = "white", 
            show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol)
  }
  if (cfg$graphics$plot_contour) {
    pred = pred +
      geom_sf(data = mask_contour, color = "white")
  }
pred
png(filename = file.path(vpath, sprintf("%s_prediction.png", cfg$version)), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
print(pred)
dev.off()
# ggsave(filename = sprintf("%s_prediction.png", cfg$version),
#        plot = pred, 
#        path = vpath, 
#        width = 11, height = 8.5, units = "in", dpi = 300)

if (cfg$scenario == "PRESENT") {
  obs_brick = read_sf(file.path(cfg$modeling_vpath, "obs_brick.gpkg"))
  
  pauc = pAUC(prediction, obs_brick, thr = seq(from = 1, to = 0, by = -1/10000))
  
  pauc_plot = plot(pauc)
  png(filename = file.path(vpath, sprintf("%s_pauc.png", cfg$version)), 
      bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(pauc_plot)
  dev.off()
  # ggsave(filename = sprintf("%s_pauc.png", cfg$version),
  #        plot = pauc_plot, 
  #        path = vpath)
  # 
  pauc_area = pauc$area |>
    as_tibble() |>
    write.csv(file.path(vpath, paste0(cfg$version, "_pauc.csv")))
}


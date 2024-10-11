# usage: gather_data.R [--] [--help] config
# 
# assembling observation and background data
# 
# positional arguments:
#   config      the name of the configuration file
# 
# flags:
#   -h, --help  show this help message and exit


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

args = argparser::arg_parser("assembling observation and background data",
                             name = "gather_data.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/get_data_workflow/v01.000.yaml",
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

# read in species data & filter species by dates & subset species by bounding box
shark_box = cofbb::get_bb(cfg$bbox_name, form = "sf")

curated = read.csv(file.path(cfg$data_path, "historical/curated_literature2.csv")) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  dplyr::filter(nchar(eventDate) >= 10) |>
  dplyr::mutate(eventDate = as.Date(eventDate, format = c("%m/%d/%Y"))) |>
  dplyr::mutate(basisOfRecord = "curated") |>
  dplyr::rename(citation = X) |>
  dplyr::rename(note2 = X.1) |>
  dplyr::rename(month.chr = Month) |>
  dplyr::mutate(month = match(.data$month.chr, month.name))

psat = read.csv(file.path(cfg$data_path, "satellite/Skomal_PSAT_data.csv")) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  dplyr::rename(shark_id = `Shark.ID`) |>
  dplyr::mutate(date_time = as.POSIXct(Date, format = "%m/%d/%y %H:%M", tz = "UTC-05")) |>
  dplyr::mutate(month = format(date_time, "%m")) |>
  dplyr::mutate(hhmm = format(date_time, "%H:%M")) |>
  dplyr::mutate(Year = format(date_time, "%Y")) |>
  dplyr::arrange(shark_id, date_time) |>
  dplyr::select(-Date) |>
  dplyr::mutate(basisOfRecord = "PSAT")
psat$date = as.Date(psat$date_time, format = "%m/%d/%y")
psat$month = as.numeric(format(psat$date, "%m"))
psat$Year = format(psat$date, "%Y")

spot = read.csv(file.path(cfg$data_path, "satellite/Skomal_SPOT_data.csv")) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  dplyr::rename(shark_id = `Shark.ID`) |>
  dplyr::mutate(date_time = as.POSIXct(Date, format = "%m/%d/%y %H:%M", tz = "UTC-05")) |>
  dplyr::mutate(month = format(date_time, "%m")) |>
  dplyr::mutate(hhmm = format(date_time, "%H:%M")) |>
  dplyr::mutate(Year = format(date_time, "%Y")) |>
  dplyr::arrange(shark_id, date_time) |>
  dplyr::select(-Date) |>
  dplyr::mutate(basisOfRecord = "SPOT")
spot$date = as.Date(spot$date_time, format = "%m/%d/%y")
spot$month = as.numeric(format(spot$date, "%m"))
spot$Year = format(spot$date, "%Y")

satellite = bind_rows(psat, spot)

if(cfg$fetch_obis){
  fetch_obis(scientificname =  cfg$species)
  wshark = read_obis(species = cfg$species, 
                     dwc = TRUE, 
                     path = file.path(cfg$data_path, "obis"))
}else{
  wshark = read_obis(species = cfg$species, 
                      dwc = TRUE, 
                      path = file.path(cfg$data_path, "obis"))
}
wshark <- wshark |>
  distinct() |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric()) |>
  dplyr::mutate(Year = format(eventDate, "%Y")) |>
  dplyr::bind_rows(curated, satellite) |>
  dplyr::mutate(extractDate = as.Date(sprintf("2020-%0.2i-01", month))) |>
  dplyr::rename(c(obis_sst = sst, obis_depth = depth)) |>
  sf::st_crop(shark_box) |>
  sf::write_sf(file.path(cfg$data_path, "obis", "shark_occs.gpkg"))

shark_mon_hist = ggplot2::ggplot() +
  ggplot2::geom_bar(data = wshark,
           ggplot2::aes(x = as.factor(month), fill = basisOfRecord)) +
  ggplot2::labs(x = "Month", y = "Count") +
  ggplot2::theme_classic()
shark_mon_hist
ggplot2::ggsave(filename = "occ_monthly_hist.png", plot = shark_mon_hist, path = file.path(cfg$figure_path), width = 11, height = 8.5, units = "in", dpi = 300)

shark_yr_hist = ggplot2::ggplot() +
  ggplot2::geom_bar(data = wshark,
                    ggplot2::aes(x = as.factor(Year), fill = basisOfRecord)) +
  ggplot2::labs(x = "Year", y = "Count") +
  ggplot2::theme_classic()
shark_yr_hist

occs = ggplot() +
  geom_coastline(bb = shark_box) +
  geom_sf(data = wshark, aes(shape = basisOfRecord), fill = "white", color = "black", size = 2.5) + 
  theme_void() 
occs
ggsave(filename = "occurrences.png", plot = occs, path = file.path(cfg$figure_path), width = 11, height = 8.5, units = "in", dpi = 300)

# load covariates from brickman
if(cfg$brickman_subset){
  monthly_brick_cov = subset_brickman(scenario = cfg$scenario, 
                                          year = cfg$year,
                                          vars = cfg$mon_covariates, 
                                          interval = "mon", 
                                          bb = shark_box, 
                                          band_as_time = TRUE, 
                                          path = file.path(cfg$data_path, "brickman/gom_carcharodon"))

  brickman_bathymetry = subset_brickman(scenario = 'PRESENT', 
                                        vars = cfg$static_covariates, 
                                        bb = shark_box, 
                                        band_as_time = FALSE, # does this line need to be set to false for depth? it's static no?
                                        path = file.path(cfg$data_path, "brickman/bathy"))
}else{
  monthly_brick_cov = load_brickman(scenario = cfg$scenario,
                                        year = cfg$year,
                                        vars = cfg$mon_covariates, 
                                        interval = "mon", 
                                        band_as_time = TRUE, 
                                        path = file.path(cfg$data_path, "brickman/gom_carcharodon"))
  
  brickman_bathymetry = load_brickman(scenario = 'PRESENT', 
                                      vars = cfg$static_covariates, 
                                      band_as_time = TRUE, 
                                      path = file.path(cfg$data_path, "brickman/bathy"))
}

log_brickman_bathymetry = log10(brickman_bathymetry)

fish_layer = read_stars(file.path(cfg$data_path, cfg$fish_path, cfg$fish_file)) |>
  sf::st_crop(shark_box) |>
  st_warp(dest = brickman_bathymetry)
  # need to add a way to manipulate this layer to the brickman layer dimensions

#distance from shore layer
dfs_layer = read_stars(file.path(cfg$data_path, cfg$dfs_path, cfg$dfs_file)) |>
  rename(dfs = cfg$dfs_file) |>
  st_warp(dest = brickman_bathymetry)

# extract covariate data at obs points
shark_depth = st_extract(brickman_bathymetry, at = wshark) |>
  st_as_sf() |>
  as_tibble()
log_shark_depth = st_extract(log_brickman_bathymetry, at = wshark) |>
  st_as_sf() |>
  as_tibble()
shark_fish = st_extract(fish_layer, at = wshark) |>
  st_as_sf() |>
  as_tibble() |>
  rename(fish_biomass = cfg$fish_file)
shark_dfs = st_extract(dfs_layer, at = wshark) |>
  sf::st_as_sf() |>
  dplyr::as_tibble() 
shark_covars = brickman_extract(monthly_brick_cov, wshark, time_column = "extractDate") # changed to eventDate because it is consistent amongst all obs

wshark = dplyr::mutate(wshark, fish_biomass = shark_fish$fish_biomass) |>
  dplyr::mutate(wshark, dfs = shark_dfs$dfs) |>
  dplyr::mutate(wshark, brick_depth = shark_depth$Bathy_depth) |>
  dplyr::mutate(wshark, log_depth = log_shark_depth$Bathy_depth) |>
  dplyr::bind_cols(shark_covars) |>
  write_sf(file.path(cfg$data_path, "covars", "brickman_covars_shark_occs.gpkg"))

# random bg selection
####should we make this a new variable so we can keep the original bathymetry layer???

# IS THIS NEEDED ANYMORE?
# brick_masked = buffer_depth(brickman_bathymetry, cfg$static_covariates, depth = cfg$depth_threshold)
# 
# if(cfg$buffer_exist){
#   brick_buf = read_sf(file.path(cfg$data_path, cfg$polygon))
# } else {
#   brick_buf = st_contour(brick_masked, breaks = c(0, cfg$depth_threshold))
#   brick_buf = brick_buf[1]
#   write_sf(brick_buf, file.path(cfg$data_path, cfg$polygon))
# }

# using mask as area to plot on?
mask = stars::read_stars(file.path(cfg$data_path, cfg$mask_name)) |>
  rlang::set_names("mask")

# mask_buf = stars::st_contour(mask)
# mask_buf = mask_buf[1]
# write_sf(mask_buf, file.path(cfg$data_path, cfg$buffer_name))

bg_brick = twinkle::random_points(mask, 
                                  n = cfg$bg_number, 
                                  m = cfg$bg_m, 
                                  na.rm = TRUE, 
                                  # polygon = mask_buf, 
                                  form = "sf") |>
  filter(.data$mask == 1) # chooses points within the buffer

# extract covariate data at bg points
min_date = as.Date(cfg$start_date)
max_date = as.Date(cfg$end_date)
date_seq = seq(from = min_date, to = max_date, by = "day")
dates = sample(date_seq, nrow(bg_brick), replace = TRUE)

bg_brick = dplyr::mutate(bg_brick, eventDate = dates) |>
  dplyr::select(eventDate) |>
  dplyr::mutate(month = as.numeric(format(eventDate, "%m"))) |>
  dplyr::mutate(time = as.Date(format(eventDate, "2020/%m/01"))) |>
  write_sf(file.path(vpath, "standard_bg_brick.gpkg"))

brick_bg_depth = stars::st_extract(brickman_bathymetry, at = bg_brick)
brick_bg_log_depth = stars::st_extract(log_brickman_bathymetry, at = bg_brick)
brick_bg_fish = stars::st_extract(fish_layer, at = bg_brick)
brick_bg_dfs = stars::st_extract(dfs_layer, at = bg_brick)
brick_bg_covars = brickman_extract(monthly_brick_cov, bg_brick, time_column = "time", interpolate_time = FALSE)

bg_brick = dplyr::mutate(bg_brick, brick_depth = brick_bg_depth$Bathy_depth) |>
  dplyr::mutate(bg_brick, fish_biomass = brick_bg_fish$fish_biomass) |>
  dplyr::mutate(bg_brick, dfs = brick_bg_dfs$dfs) |>
  dplyr::mutate(bg_brick, log_depth = brick_bg_log_depth$Bathy_depth) |>
  dplyr::bind_cols(brick_bg_covars) |>
  write_sf(file.path(vpath, "brickman_covar_bg.gpkg"))

bg = ggplot() +
  geom_coastline(bb = shark_box) +
  geom_sf(data = bg_brick, aes(), color = "black", size = 0.1) +
  theme_void() 
bg
ggsave(filename = paste0(cfg$version, "bg.png"), plot = bg, path = file.path(vpath, "figures"), width = 11, height = 8.5, units = "in", dpi = 300, create.dir = TRUE)

# bind together in a single geopackage w/ flag for bg and obs data
obs_bg_brick = bind_rows(list(wshark, bg_brick), .id = "id") |>
  dplyr::mutate(id = as.numeric(id == 1)) |>
  write_sf(file.path(vpath, "brickman_covar_obs_bg.gpkg"))

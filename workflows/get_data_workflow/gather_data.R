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
                          default = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/get_data_workflow/v01.004.yaml",
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
# read in species data & filter species by dates & subset species by bounding box
shark_box = cofbb::get_bb(cfg$bbox_name, form = "sf")

mask = stars::read_stars(file.path(cfg$data_path, cfg$mask_name)) |>
  rlang::set_names("mask")

if (!is.null(cfg$contour_name)) {
  mask_contour = sf::read_sf(file.path(cfg$data_path, cfg$contour_name))
}

curated = read.csv(file.path(cfg$data_path, "historical/curated_literature2.csv")) |>
  dplyr::filter(nchar(eventDate) != 0) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  dplyr::mutate(eventDate = as.Date(eventDate, format = c("%Y-%m-%d"))) |>
  dplyr::mutate(basisOfRecord = "curated") |>
  dplyr::rename(citation = X) |>
  dplyr::rename(note2 = X.1) |>
  dplyr::rename(month = Month) |>
  sf::st_crop(shark_box)
curated$Year = as.numeric(curated$Year)

curated_plot = ggplot() +
  geom_coastline(bb = shark_box, color = "red") +
  geom_sf(data = curated, aes(shape = basisOfRecord), fill = "white", color = "black", size = 2.5) + 
  theme_void() 
if (!is.null(cfg$contour_name)) {
  curated_plot = curated_plot +
    geom_sf(data = mask_contour, color = "red")
  
}
curated_plot
png(filename = file.path(vpath, "figures", paste0(cfg$version, "_curated_occs.png")), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
curated_plot
ok = dev.off()

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
psat$eventDate = as.Date(psat$date_time, format = "%m/%d/%y")
psat$month = as.numeric(format(psat$eventDate, "%m"))
psat$Year = as.numeric(format(psat$eventDate, "%Y"))

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
spot$eventDate = as.Date(spot$date_time, format = "%m/%d/%y")
spot$month = as.numeric(format(spot$eventDate, "%m"))
spot$Year = as.numeric(format(spot$eventDate, "%Y"))

satellite = bind_rows(psat, spot)
satellite_plot = ggplot() +
  geom_coastline(bb = shark_box, color = "red") +
  geom_sf(data = satellite, aes(shape = basisOfRecord, color = shark_id), size = 2.5) + 
  theme_void() 
if (!is.null(cfg$contour_name)) {
  satellite_plot = satellite_plot +
    geom_sf(data = mask_contour, color = "red")
  
}
png(filename = file.path(vpath, "figures", paste0(cfg$version, "_satellite_occs.png")), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
satellite_plot
ok = dev.off()

inat_removed_cols = c("uuid", "observed_on_string", "url", "image_url", "sound_url", "tag_list", "captive_cultivated", "oauth_application_id", "private_place_guess", "private_latitude", "private_longitude", "geoprivacy", "taxon_geoprivacy", "coordinates_obscured", "species_guess")
inat = read.csv(file.path(cfg$data_path, cfg$inat_file)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  dplyr::filter(quality_grade == "research") |>
  dplyr::select(!all_of(inat_removed_cols)) |>
  dplyr::mutate(basisOfRecord = "iNaturalist")
inat$eventDate = as.Date(inat$observed_on, format = "%Y-%m-%d")
inat$month = as.numeric(format(inat$eventDate, "%m"))
inat$Year = as.numeric(format(inat$eventDate, "%Y"))

inat_plot = ggplot() +
  geom_coastline(bb = shark_box, color = "red") +
  geom_sf(data = inat, aes(shape = basisOfRecord), fill = "white", color = "black", size = 2.5) + 
  theme_void() 
if (!is.null(cfg$contour_name)) {
  inat_plot = inat_plot +
    geom_sf(data = mask_contour, color = "red")
  
}
png(filename = file.path(vpath, "figures", paste0(cfg$version, "_inat_occs.png")), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
inat_plot
ok = dev.off()

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
  dplyr::filter(basisOfRecord == "HumanObservation") |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric()) |>
  dplyr::mutate(Year = as.numeric(format(eventDate, "%Y"))) |>
  dplyr::bind_rows(curated, inat, satellite) |>
  dplyr::mutate(extractDate = as.Date(sprintf("2020-%0.2i-01", month))) |>
  dplyr::rename(c(obis_sst = sst, obis_depth = depth)) |>
  dplyr::filter(!is.na(month)) |>
  sf::st_crop(shark_box) |>
  dplyr::mutate(basisOfRecord = dplyr::if_else(basisOfRecord == "HumanObservation", "OBIS", basisOfRecord))

obis = wshark |>
  dplyr::filter(basisOfRecord == "OBIS")

obis_plot = ggplot() +
  geom_coastline(bb = shark_box, color = "red") +
  geom_sf(data = obis, aes(shape = basisOfRecord), fill = "white", color = "black", size = 2.5) + 
  theme_void() 
if (!is.null(cfg$contour_name)) {
  obis_plot = obis_plot +
    geom_sf(data = mask_contour, color = "red")
  
}
png(filename = file.path(vpath, "figures", paste0(cfg$version, "_obis_occs.png")), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
obis_plot
ok = dev.off()

wshark.mask = st_extract(mask, wshark)

wshark = wshark |>
  dplyr::filter(wshark.mask$mask == 1) |>
  sf::write_sf(file.path(cfg$data_path, "obis", "shark_occs.gpkg"))

shark_mon_hist = ggplot2::ggplot() +
  ggplot2::geom_bar(data = wshark,
           ggplot2::aes(x = as.factor(month), fill = basisOfRecord)) +
  ggplot2::labs(x = "Month", y = "Count") +
  ggplot2::theme_classic()
shark_mon_hist
ggplot2::ggsave(filename = "occ_monthly_hist.png", 
                plot = shark_mon_hist, 
                path = file.path(vpath, "figures"), 
                width = 11, height = 8.5, units = "in", dpi = 300, create.dir = TRUE)

shark_yr_hist = ggplot2::ggplot() +
  ggplot2::geom_bar(data = wshark,
                    ggplot2::aes(x = as.factor(Year), fill = basisOfRecord)) +
  ggplot2::labs(x = "Year", y = "Count") +
  ggplot2::theme_classic()
shark_yr_hist

occs = ggplot() +
  geom_coastline(bb = shark_box) +
  geom_sf(data = wshark, aes(shape = basisOfRecord), fill = "white", color = "black", size = 2.5) + 
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb"), color = "red") +
  theme_void() 
if (!is.null(cfg$contour_name)) {
  occs = occs +
    geom_sf(data = mask_contour, color = "red")
  
}
occs
png(filename = file.path(vpath, "figures", paste0(cfg$version, "_occs.png")), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
occs
ok = dev.off()

non_sat_BOR = c("OBIS", "curated", "iNaturalist")
non_sat = ggplot() +
  geom_coastline(bb = shark_box) +
  geom_sf(data = dplyr::filter(wshark, basisOfRecord %in% non_sat_BOR), 
          aes(shape = basisOfRecord), fill = "white", color = "black", size = 2.5) + 
  geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb"), color = "red") +
  theme_void() 
if (!is.null(cfg$contour_name)) {
  non_sat = non_sat +
    geom_sf(data = mask_contour, color = "red")
  
}
non_sat
png(filename = file.path(vpath, "figures", paste0(cfg$version, "_non_sat_occs.png")), 
    bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
non_sat
ok = dev.off()
  

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
uv_s = read_vel_mag(scenario = cfg$scenario, year = cfg$year, band_as_time = TRUE)

fall_fish_layer = read_stars(file.path(cfg$data_path, cfg$fish_path, cfg$fall_fish_file)) |>
  sf::st_crop(shark_box) |>
  st_warp(dest = brickman_bathymetry)
spring_fish_layer = read_stars(file.path(cfg$data_path, cfg$fish_path, cfg$spring_fish_file)) |>
  sf::st_crop(shark_box) |>
  st_warp(dest = brickman_bathymetry)

#seal layers
if (cfg$seal_tidy == TRUE) {
  if(cfg$hseal == "harbor") {
    hseal_layer = load_tidy_seal(species = cfg$hseal, model_type = cfg$seal_model_type, band_as_time = TRUE) |>
      dplyr::rename(hseal = sprintf("%s_prediction.tif", cfg$seal_model_type)) |>
      stars::st_warp(dest = brickman_bathymetry) 
  } 
  
  if(cfg$gseal == "gray") {
    gseal_layer = load_tidy_seal(species = cfg$gseal, model_type = cfg$seal_model_type, band_as_time = TRUE) |>
      dplyr::rename(gseal = sprintf("%s_prediction.tif", cfg$seal_model_type)) |>
      stars::st_warp(dest = brickman_bathymetry)  
  }
} else if(cfg$seal_tidy == FALSE) {
  if(cfg$hseal == "harbor") {
    hseal_layer = load_seal(scenario = cfg$scenario, year = cfg$year, species = cfg$hseal, path = file.path(cfg$root_path, cfg$hseal_path), band_as_time = TRUE) |>
      dplyr::rename(hseal = "prediction.tif") |>
      stars::st_warp(dest = brickman_bathymetry) 
  } 
  
  if(cfg$gseal == "gray") {
    gseal_layer = load_seal(scenario = cfg$scenario, year = cfg$year, species = cfg$gseal, path = file.path(cfg$root_path, cfg$seal_path), band_as_time = TRUE) |>
      dplyr::rename(gseal = "prediction.tif") |>
      stars::st_warp(dest = brickman_bathymetry) 
  }
}

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

if (cfg$which_fish == "SPRING") {
  shark_fish = st_extract(spring_fish_layer, at = wshark) |>
    st_as_sf() |>
    as_tibble() |>
    rename(fish_biomass = cfg$spring_fish_file)
}
if (cfg$which_fish == "FALL") {
  shark_fish = st_extract(fall_fish_layer, at = wshark) |>
    st_as_sf() |>
    as_tibble() |>
    rename(fish_biomass = cfg$fall_fish_file)
}
if (cfg$which_fish == "COMBO") {
  spring_shark = wshark |>
    dplyr::filter(month %in% cfg$spring_fish_mon)
  spring_shark_fish = st_extract(spring_fish_layer, at = spring_shark) |>
    st_as_sf() |>
    as_tibble() |>
    rename(fish_biomass = cfg$spring_fish_file)
  fall_shark = wshark |>
    dplyr::filter(month %in% cfg$fall_fish_mon)
  fall_shark_fish = st_extract(fall_fish_layer, at = fall_shark) |>
    st_as_sf() |>
    as_tibble() |>
    rename(fish_biomass = cfg$fall_fish_file)
  shark_fish = dplyr::bind_rows(spring_shark_fish, fall_shark_fish)
}

shark_dfs = st_extract(dfs_layer, at = wshark) |>
  sf::st_as_sf() |>
  dplyr::as_tibble() 
shark_covars = brickman_extract(monthly_brick_cov, wshark, time_column = "extractDate") # changed to eventDate because it is consistent among all obs
shark_hseal = brickman_extract(hseal_layer, wshark, time_column = "extractDate")
shark_gseal = brickman_extract(gseal_layer, wshark, time_column = "extractDate")
shark_vel_mag = brickman_extract(uv_s, wshark, time_column = "extractDate")

wshark = wshark |>
  dplyr::mutate(fish_biomass = shark_fish$fish_biomass) |>
  dplyr::mutate(dfs = shark_dfs$dfs) |>
  dplyr::mutate(brick_depth = shark_depth$Bathy_depth) |>
  dplyr::mutate(log_depth = log_shark_depth$Bathy_depth) |>
  dplyr::mutate(vel_mag = shark_vel_mag$vel_mag) |>
  dplyr::bind_cols(shark_covars, 
                   shark_gseal, 
                   shark_hseal
                   ) |>
  write_sf(file.path(cfg$data_path, "covars", "brickman_covars_shark_occs.gpkg"))
  
# option to the above would be to thin all of the observation data regardless of observation type
# wshark |>
# tidysdm::thin_by_cell(mask) |>
# tidysdm::thin_by_dist(dist_min = km2m(20))

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
bg_brick = twinkle::random_points(mask, 
                                  n = cfg$bg_number, 
                                  m = cfg$bg_m, 
                                  na.rm = TRUE, 
                                  form = "sf") |>
  filter(.data$mask == 1)

# extract covariate data at bg points
min_date = as.Date(cfg$start_date)
max_date = as.Date(cfg$end_date)
date_seq = seq(from = min_date, to = max_date, by = "day")
dates = sample(date_seq, nrow(bg_brick), replace = TRUE)

bg_brick = dplyr::mutate(bg_brick, eventDate = dates) |>
  dplyr::select(eventDate) |>
  dplyr::mutate(month = as.numeric(format(eventDate, "%m"))) |>
  dplyr::mutate(time = as.Date(format(eventDate, "2020/%m/01"))) |>
  write_sf(file.path(vpath, "bg_brick.gpkg"))

if (cfg$which_fish == "SPRING") {
  brick_bg_fish = stars::st_extract(spring_fish_layer, at = bg_brick) |>
    rename(fish_biomass = cfg$spring_fish_file)
}
if (cfg$which_fish == "FALL") {
  brick_bg_fish = stars::st_extract(fall_fish_layer, at = bg_brick) |>
    rename(fish_biomass = cfg$fall_fish_file)
}
if (cfg$which_fish == "COMBO") {
  fall_bg = bg_brick |>
    dplyr::filter(month %in% cfg$fall_fish_mon)
  fall_bg_fish = stars::st_extract(fall_fish_layer, at = fall_bg) |>
    rename(fish_biomass = cfg$fall_fish_file)
  spring_bg = bg_brick |>
    dplyr::filter(month %in% cfg$spring_fish_mon)
  spring_bg_fish = stars::st_extract(spring_fish_layer, at = spring_bg) |>
    rename(fish_biomass = cfg$spring_fish_file)
  brick_bg_fish = dplyr::bind_rows(fall_bg_fish, spring_bg_fish)
}

brick_bg_depth = stars::st_extract(brickman_bathymetry, at = bg_brick)
brick_bg_log_depth = stars::st_extract(log_brickman_bathymetry, at = bg_brick)
brick_bg_dfs = stars::st_extract(dfs_layer, at = bg_brick)
brick_bg_covars = brickman_extract(monthly_brick_cov, bg_brick, time_column = "time", interpolate_time = FALSE)
brick_bg_hseal = brickman_extract(hseal_layer, bg_brick, time_column = "time")
brick_bg_gseal = brickman_extract(gseal_layer, bg_brick, time_column = "time")
brick_bg_vel_mag = brickman_extract(uv_s, bg_brick, time_column = "time")

bg_brick = dplyr::mutate(bg_brick, brick_depth = brick_bg_depth$Bathy_depth) |>
  dplyr::mutate(bg_brick, fish_biomass = brick_bg_fish$fish_biomass) |>
  dplyr::mutate(bg_brick, dfs = brick_bg_dfs$dfs) |>
  dplyr::mutate(bg_brick, log_depth = brick_bg_log_depth$Bathy_depth) |>
  dplyr::mutate(bg_brick, vel_mag = brick_bg_vel_mag$vel_mag) |>
  dplyr::bind_cols(brick_bg_covars 
                   #brick_bg_gseal, 
                   #brick_bg_hseal
                   ) |>
  write_sf(file.path(vpath, "brickman_covar_bg.gpkg"))

bg = ggplot() +
  geom_coastline(bb = shark_box) +
  geom_sf(data = bg_brick, aes(), color = "black", size = 0.1) +
  theme_void() 
bg
ggsave(filename = paste0(cfg$version, "bg.png"), 
       plot = bg, 
       path = file.path(vpath, "figures"), 
       width = 11, height = 8.5, units = "in", dpi = 300, create.dir = TRUE)

# bind together in a single geopackage w/ flag for bg and obs data
obs_bg_brick = bind_rows(list(wshark, bg_brick), .id = "id") |>
  dplyr::mutate(id = as.numeric(id == 1)) 

cell = stars::st_cells(mask, obs_bg_brick)

obs_bg_brick = obs_bg_brick |>
  dplyr::mutate(cell = as.integer(cell)) |>
  write_sf(file.path(vpath, "brickman_covar_obs_bg.gpkg"))


all_points = ggplot() +
  geom_coastline(bb = shark_box) +
  geom_sf(data = obs_bg_brick, aes(), color = "black", size = 0.1) +
  theme_void() 
all_points


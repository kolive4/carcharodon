# Polygon for background sampling created in s04_brick2_bathy_bound, combine with mask from study area, generate and plot bg points
brick_buf

# Sample on mask to throw out random points that show NA, so we don't sample on land (ask for 20,000 so we can throw out 10,000 and be okay) (twinkle package random points function)
mask = stars::read_stars(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/mapping", "study_area.binary.tif")) |>
  rlang::set_names("mask")

plot(mask, reset = FALSE)

# Take random background points within the polygon space. We ask for 10,000 background points in total. Sample on mask to throw out random points that show NA, so we don't sample on land (ask for 20,000 so we can throw out 10,000 and be okay) (twinkle package random points function). Ended up with 9,888 points because there are only 9,888 pixels in the mask.
bg_brick = twinkle::random_points(mask, n = 10000, m = 2, na.rm = TRUE, polygon = brick_buf, form = "sf")

plot(st_geometry(bg_brick), pch = ".", add = TRUE)


# Define date column that's randomly sampled enclosed in date range for shark samples (month before earliest shark obs -> present)
min_date = as.Date("2004-01-01")# min(total_occs$eventDate) - 30
max_date = as.Date("2024-01-08")
date_seq = seq(from = min_date, to = max_date, by = "day")
dates = sample(date_seq, nrow(bg_bathy), replace = TRUE)

bg_brick = dplyr::mutate(bg_brick, date = dates) |>
  dplyr::select(date) |>
  write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/covars", "standard_bg_brick.gpkg"))
plot(bg_brick, pch = ".")


cofbb::get_table()
gomcc_bb = cofbb::get_bb("gom_carcharodon", "sf")

# depth layer based on brickman bathymetry data
brick_depth2

# attribute depth and temperature for background df and shark occ df
depth = stars::st_extract(brick_depth2, at = bg_brick) 
brick_sst = stars::st_extract(present_brick[1,,,], at = bg_brick) |>
  st_as_sf() |>
  st_drop_geometry() |>
  set_names(paste("sst", month.abb, sep = "_"))

bg_brick = dplyr::mutate(bg_brick, depth = depth$Bathy_depth) |> 
  dplyr::bind_cols(brick_sst) |>
  #dplyr::mutate(brick_sst = brick_sst$PRESENT_PRESENT_SST_mon.tif[1]) |> 
  write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/covars", "brickman_covar_bg.gpkg"))
glimpse(bg_brick)
plot(bg_brick["depth"], pch = ".")


shark_depth = st_extract(brick_depth2, at = shark_occs)
shark_occs = dplyr::mutate(shark_occs, depth = shark_depth$Bathy_depth) |>
  write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/covars", "brickman_depth_shark_occs.gpkg"))
glimpse(shark_occs)
shark_occs.sf <- sf::st_transform(shark_occs, crs = eckertIV)
plot(shark_occs["depth"], pch = ".")


# Generate climatology map again, computing the convex hull to define one polygon for background sampling area.
total_buf = sf::read_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", 
                                  "shark_occurrence_buf_climatology.gpkg")) |>
  st_union()|>
  st_convex_hull() |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/mapping", 
                         "total_buf.gpkg"))

plot(total_buf, axes = TRUE)


# Sample on mask to throw out random points that show NA, so we don't sample on land (ask for 20,000 so we can throw out 10,000 and be okay) (twinkle package random points function)
mask = stars::read_stars(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/mapping", "study_area.binary.tif")) |>
  rlang::set_names("mask")

plot(mask, reset = FALSE)


# Take random background points within the masked space. We ask for 10,000 background points in total. Sample on mask to throw out random points that show NA, so we don't sample on land (ask for 20,000 so we can throw out 10,000 and be okay) (twinkle package random points function). Ended up with 9,888 points because there are only 9,888 pixels in the mask.
bg = twinkle::random_points(mask, n = 10000, m = 2, na.rm = TRUE, polygon = total_buf, form = "sf")

plot(mask, reset = FALSE)
plot(st_geometry(bg), pch = ".", add = TRUE)
glimpse(bg)


# Define date column that's randomly sampled enclosed in date range for shark samples (month before earliest shark obs -> present)
total_occs = read_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", "shark_recent_occs.gpkg"))
min_date = as.Date("2004-01-01")# min(total_occs$eventDate) - 30
max_date = as.Date("2023-04-24")
date_seq = seq(from = min_date, to = max_date, by = "day")
dates = sample(date_seq, nrow(bg), replace = TRUE)

bg = dplyr::mutate(bg, date = dates) |>
  dplyr::select(date) |>
  write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/covars", "standard_bg.gpkg"))
plot(bg, pch = ".")


bb = as_bb(total_buf)

# set_root_path(path = "/mnt/s1/projects/ecocast/coredata/bathy", filename = "~/.topodata")
(etopo_files = list_etopo())

etopo <- topotools::read_etopo("ETOPO_2022_v1_60s_N90W180_surface.nc", bb = bb)
etopo

plot(etopo, main = "ETOPO1", axes = TRUE)

# (gebco_files = topotools::list_gebco())
# 
# gebco <- topotools::read_gebco(gebco_files, bb = bb)
# gebco
# 
# plot(gebco, main = "GEBCO", axes = TRUE)

masked = mask_topo(etopo[total_buf])
plot(masked)

#plot(etopo[total_buf])
# plot(gebco[total_buf])

# attribute depth for background df and shark occ df
depth = stars::st_extract(masked, at = bg) # issues here maybe masked isnt a stars or bg isnt read as an sf, sfc, or matrix
bg = dplyr::mutate(bg, depth = depth$z) |>
  write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/covars", "depth_bg.gpkg"))
glimpse(bg)
plot(bg["depth"], pch = ".")

shark_depth = st_extract(masked, at = shark_occs)
shark_occs = dplyr::mutate(shark_occs, depth = shark_depth$z) |>
  write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/covars", "depth_shark.gpkg"))
glimpse(shark_occs)
plot(shark_occs["depth"], pch = ".")

gseal_depth = st_extract(masked, at = grey_seal_occs)
grey_seal_occs = dplyr::mutate(grey_seal_occs, depth = gseal_depth$z) |>
  write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/covars", "depth_gseal.gpkg"))
glimpse(grey_seal_occs)
plot(grey_seal_occs["depth"], pch = ".")

hseal_depth = st_extract(masked, at = harbor_seal_occs)
harbor_seal_occs = dplyr::mutate(harbor_seal_occs, depth = hseal_depth$z) |>
  write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/covars", "depth_hseal.gpkg"))
glimpse(harbor_seal_occs)
plot(harbor_seal_occs["depth"], pch = ".")

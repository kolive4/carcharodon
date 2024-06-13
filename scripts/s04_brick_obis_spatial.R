spp <- list_obis()
global_coast <- rnaturalearth::ne_coastline(returnclass = "sf")
shark_coast <- sf::st_crop(global_coast, shark_box)

plot(shark_coast)

# can use the seasonality function here to get occurrences for each species based on seasons
shark_occs = shark_recent |>
  sf::st_crop(shark_box)

eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
shark_occs.sf <- sf::st_transform(shark_occs, crs = eckertIV)

brick_shark_occs.buf <- sf::st_buffer(shark_coast, dist = 100000) |>
  sf::st_union() |> 
  #sf::st_convex_hull() |>
  sf::st_sf(crs = eckertIV) |>
  sf::st_transform(crs = sf::st_crs(present_brick)) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", 
                         "shark_coast_buf_brickman.gpkg"))

plot(brick_shark_occs.buf)

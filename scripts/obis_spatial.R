spp <- list_obis()
global_coast <- rnaturalearth::ne_coastline(returnclass = "sf")
nwa_coast <- sf::st_crop(global_coast, bb)

# can use the seasonality function here to get occurrences for each species based on seasons
shark_occs = shark_recent |>
  sf::st_crop(bb)
grey_seal_occs = grey_seal_recent |>
  sf::st_crop(bb)
harbor_seal_occs = harbor_seal_recent |>
  sf::st_crop(bb)


eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
shark_occs.sf <- sf::st_transform(shark_occs, crs = eckertIV)
gseal_occs.sf <- sf::st_transform(grey_seal_occs, crs = eckertIV)
hseal_occs.sf <- sf::st_transform(harbor_seal_occs, crs = eckertIV)


shark_occs.buf <- sf::st_buffer(shark_occs.sf, dist = 200000) |>
  sf::st_union() |> 
  sf::st_sf(crs = eckertIV) |>
  sf::st_transform(crs = sf::st_crs(envs)) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", 
                         "shark_occurrence_buf_climatology.gpkg"))

gseal_occs.buf <- sf::st_buffer(gseal_occs.sf, dist = 200000) |>
  sf::st_union() |> 
  sf::st_sf(crs = eckertIV) |>
  sf::st_transform(crs = sf::st_crs(envs)) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", 
                         "grey_seal_occurrence_buf_climatology.gpkg"))

hseal_occs.buf <- sf::st_buffer(hseal_occs.sf, dist = 200000) |>
  sf::st_union() |> 
  sf::st_sf(crs = eckertIV) |>
  sf::st_transform(crs = sf::st_crs(envs)) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", 
                         "harbor_seal_occurrence_buf_climatology.gpkg"))
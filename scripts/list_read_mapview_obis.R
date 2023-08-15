(spp <- list_obis())

bb <- cofbb::get_bb("nwa2", "sf")

x <- read_obis(spp[1], dwc = TRUE) |>
  distinct() |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())

shark_recent <- dplyr::filter(x, eventDate > as.Date("2003-01-01")) |>
  sf::st_crop(bb) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", "shark_recent_occs.gpkg"))
hist(shark_recent$month)

glimpse(shark_recent)


grey_seal <- read_obis(spp[2], dwc = TRUE) |>
  distinct() |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())

grey_seal_recent <- dplyr::filter(grey_seal, eventDate > as.Date("2003-01-01")) |>
  sf::st_crop(bb) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", "grey_recent_occs.gpkg"))


summary(grey_seal_recent)
hist(grey_seal$sst)

glimpse(grey_seal)


harbor_seal <- read_obis(spp[3], dwc = TRUE) |>
  distinct() |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())

harbor_seal_recent <- dplyr::filter(harbor_seal, eventDate > as.Date("2003-01-01")) |>
  sf::st_crop(bb) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", "harbor_recent_occs.gpkg"))

glimpse(harbor_seal_recent)

hist(harbor_seal_recentl$month)

plot_obis(shark_recent, what = 'mapview')
plot_obis(grey_seal_recent, what = 'mapview')
plot_obis(harbor_seal_recent, what = 'mapview')

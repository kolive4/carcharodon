(spp <- list_obis())

bb <- cofbb::get_bb("nwa2", "sf")

wshark <- read_obis(spp[2], dwc = TRUE) |>
  distinct() |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())

shark_recent <- dplyr::filter(wshark, eventDate > as.Date("2003-01-01")) |>
  sf::st_crop(bb) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", "shark_recent_occs.gpkg"))
hist(shark_recent$month)

glimpse(shark_recent)


grey_seal <- read_obis(spp[4], dwc = TRUE) |>
  distinct() |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())

grey_seal_recent <- dplyr::filter(grey_seal, eventDate > as.Date("2003-01-01")) |>
  sf::st_crop(bb) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", "grey_recent_occs.gpkg"))


summary(grey_seal_recent)
hist(grey_seal_recent$month)
plot_obis(grey_seal_recent, what = "mapview")
glimpse(grey_seal_recent, what = "mapview")

jan_feb_gseal = grey_seal_recent |>
  filter(month %in% c(1, 2))
plot(st_geometry(jan_feb_gseal))


harbor_seal <- read_obis(spp[6], dwc = TRUE) |>
  distinct() |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())

harbor_seal_recent <- dplyr::filter(harbor_seal, eventDate > as.Date("2003-01-01")) |>
  sf::st_crop(bb) |>
  sf::write_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/obis", "harbor_recent_occs.gpkg"))

glimpse(harbor_seal_recent)

hist(harbor_seal_recent$month)

plot_obis(shark_recent, what = 'mapview')
plot_obis(grey_seal_recent, what = 'mapview')
plot_obis(jan_feb_gseal, what = "mapview")
plot_obis(harbor_seal_recent, what = 'mapview')

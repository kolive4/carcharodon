library(lubridate)
library(rnaturalearth)
devtools::install_github("ropensci/rnaturalearthhires") 

coast = rnaturalearth::ne_coastline(scale = "large", returnclass = "sf")

psat = read_csv(file.path(cfg$data_path, "satellite/Skomal_PSAT_data.csv"))
psat = psat |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  dplyr::rename(shark_id = `Shark ID`) |>
  dplyr::mutate(date_time = as.POSIXct(Date, format = "%m/%d/%y %H:%M", tz = "UTC-05")) |>
  dplyr::mutate(month = format(date_time, "%m")) |>
  dplyr::mutate(hhmm = format(date_time, "%H:%M")) |>
  dplyr::mutate(year = format(date_time, "%Y")) |>
  dplyr::arrange(shark_id, date_time)

psat$date = as.Date(psat$date_time, format = "%m/%d/%y")
psat$month = format(psat$date, "%m")
psat$year = format(psat$date, "%Y")

psat_aug = psat |>
  filter(month == "08")
psat_aug_fig = ggplot() +
  geom_sf(data = psat_aug, aes(color = shark_id)) +
  geom_coastline() +
  lims(x = bb[c("xmin", "xmax")], y = bb[c("ymin", "ymax")])
psat_aug_fig

psat_fig = ggplot() +
  geom_sf(data = psat, aes(color = shark_id)) +
  geom_coastline() +
  lims(x = bb[c("xmin", "xmax")], y = bb[c("ymin", "ymax")])
psat_fig

bb = st_bbox(shark_box)

coast = ggplot() +
  geom_coastline() 
coast

ma0901 = psat |>
  dplyr::filter(shark_id == "MA0901") 
ma0901_fig = ggplot() +
  geom_sf(data = ma0901, aes(color = month)) + 
  geom_coastline() +
  lims(x = bb[c("xmin", "xmax")], y = bb[c("ymin", "ymax")])
ma0901_fig

sc1803 = psat |>
  dplyr::filter(shark_id == "SC1803") 
sc1803_fig = ggplot() +
  geom_sf(data = sc1803, aes(color = month)) + 
  geom_coastline() +
  lims(x = bb[c("xmin", "xmax")], y = bb[c("ymin", "ymax")])
sc1803_fig

plot(st_geometry(coast), axes = TRUE, extent = ma0901, reset = FALSE)
plot(st_geometry(ma0901), type = "b", add = TRUE, col = ma0901$month)


psat_month = ggplot() +
  geom_bar(data = psat, 
           aes(month))
psat_month

psat_year = ggplot() +
  geom_bar(data = psat,
           aes(year))
psat_year

spot = read_csv(file.path(cfg$data_path, "satellite/Skomal_SPOT_data.csv"))
spot = spot |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  dplyr::rename(shark_id = `Shark ID`) |>
  dplyr::mutate(date_time = as.POSIXct(Date, format = "%m/%d/%y %H:%M", tz = "UTC-05")) |>
  dplyr::mutate(month = format(date_time, "%m")) |>
  dplyr::mutate(hhmm = format(date_time, "%H:%M")) |>
  dplyr::mutate(year = format(date_time, "%Y")) |>
  dplyr::arrange(shark_id, date_time)

spot$date = as.Date(spot$date_time, format = "%m/%d/%y")
spot$month = format(spot$date, "%m")
spot$year = format(spot$date, "%Y")

x = st_covers(spot, coast)
len = lengths(x)
table(len)

spot_fig = ggplot() +
  geom_coastline() +
  geom_sf(data = spot) +
  lims(x = bb[c("xmin", "xmax")], y = bb[c("ymin", "ymax")])
spot_fig


spot_month = ggplot() +
  geom_bar(data = spot, 
           aes(month))
spot_month

psat_year = ggplot() +
  geom_bar(data = psat,
           aes(year))
psat_year

sc1911 = spot |>
  dplyr::filter(shark_id == "SC1911")

plot(st_geometry(sc1911), type = "l")

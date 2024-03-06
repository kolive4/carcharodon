cofbb::get_table()
bb_name = "nwa2"
bb = cofbb::get_bb(bb_name, "sf")
gom_bb = cofbb::get_bb("gom", "sf")
njgb_bb = cofbb::get_bb("njgb", "sf")
earliest_date = as.Date("2003-01-01")

njgb_sharks = sf::read_sf(get_path("covars", "depth_shark.gpkg")) |>
  sf::st_crop(bb) |>
  dplyr::filter(eventDate > earliest_date) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())
plot_obis(njgb_sharks, what = "mapview")

nwa2_sharks = sf::read_sf(get_path("covars", "depth_shark.gpkg")) |>
  sf::st_crop(bb) |>
  dplyr::filter(eventDate > earliest_date) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())
plot_obis(nwa2_sharks, what = "mapview")
#----
summer_sharks = seasonality(njgb_sharks, season = "summer") |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
sum.ws.flag <- c(rep(1, nrow(summer_sharks)), rep(0, nrow(summer_bg)))
sum.ws.model <- maxnet::maxnet(sum.ws.flag,
                           dplyr::bind_rows(summer_sharks, summer_bg))
plot(sum.ws.model, type = "cloglog")

#----

fall_sharks = seasonality(njgb_sharks, season = "autumn") |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
fall.ws.flag <- c(rep(1, nrow(fall_sharks)), rep(0, nrow(fall_bg)))
fall.ws.model <- maxnet::maxnet(fall.ws.flag,
                               dplyr::bind_rows(fall_sharks, fall_bg))
plot(fall.ws.model, type = "cloglog")

#----

spring_sharks = seasonality(njgb_sharks, season = "spring") |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
spr.ws.flag <- c(rep(1, nrow(spring_sharks)), rep(0, nrow(spring_bg)))
spr.ws.model <- maxnet::maxnet(spr.ws.flag,
                               dplyr::bind_rows(spring_sharks, spring_bg))
plot(spr.ws.model, type = "cloglog") # only 5 spring occurrences so in the danger zone

winter_sharks = seasonality(njgb_sharks, season = "winter") # no sharks

plot_obis(winter_sharks, what = "mapview")

#----

njgb_gseal = read_sf(get_path("covars", "depth_gseal.gpkg")) |>
  st_crop(bb) |>
  filter(eventDate > earliest_date) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())
gs.flag <- c(rep(1, nrow(njgb_gseal)), rep(0, nrow(njgb_bg)))
gs.model <- maxnet::maxnet(gs.flag,
                               dplyr::bind_rows(njgb_gseal, njgb_bg))
plot(gs.model, type = "cloglog")
plot_obis(njgb_gseal, what = "mapview")

#----
summer_gseal = seasonality(njgb_gseal, season = "summer") |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
sum.gs.flag <- c(rep(1, nrow(summer_gseal)), rep(0, nrow(summer_bg)))
sum.gs.model <- maxnet::maxnet(sum.gs.flag,
                               dplyr::bind_rows(summer_gseal, summer_bg))
plot(sum.gs.model, type = "cloglog")

fall_gseal = seasonality(njgb_gseal, season = "autumn") |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
spring_gseal = seasonality(njgb_gseal, season = "spring")|>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
winter_gseal = seasonality(njgb_gseal, season = "winter") |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()

plot_obis(fall_gseal, what = "mapview")

#----

njgb_hseal = read_sf(get_path("covars", "depth_hseal.gpkg")) |>
  st_crop(bb) |>
  filter(eventDate > earliest_date) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())
plot_obis(njgb_hseal, what = "mapview")

summer_hseal = seasonality(njgb_hseal, season = "summer")|>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
fall_hseal = seasonality(njgb_hseal, season = "autumn") |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
spring_hseal = seasonality(njgb_hseal, season = "spring")|>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
winter_hseal = seasonality(njgb_hseal, season = "winter")|>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()
plot_obis(winter_hseal, what = "mapview")

# seasonal bg ----
njgb_bg = read_sf(get_path("covars", "covar_bg.gpkg")) |>
  st_crop(bb) |>
  filter(date > earliest_date) |>
  dplyr::mutate(month = format(date, "%m") |>
                 as.numeric()) |>
  dplyr::mutate(season = date_to_season(date))

summer_bg = dplyr::filter(njgb_bg, season == "summer") |>
  sf::st_drop_geometry() |>
  dplyr::rename(sst = obpg_sst) |>
  dplyr::mutate(month = as.factor(sprintf("%0.2i", month))) |>
  #dplyr::mutate(woy = factor(date_to_week(eventDate), levels = sprintf("%0.2i", 1:53))) |>
  dplyr::select(c(-date, -season)) |>
  na.omit()
fall_bg = dplyr::filter(njgb_bg, season == "autumn") |>
  sf::st_drop_geometry() |>
  dplyr::rename(sst = obpg_sst) |>
  dplyr::mutate(month = as.factor(sprintf("%0.2i", month))) |>
  #dplyr::mutate(woy = factor(date_to_week(eventDate), levels = sprintf("%0.2i", 1:53))) |>
  dplyr::select(c(-date, -season)) |>
  na.omit()
spring_bg = dplyr::filter(njgb_bg, season == "spring") |>
  sf::st_drop_geometry() |>
  dplyr::rename(sst = obpg_sst) |>
  dplyr::mutate(month = as.factor(sprintf("%0.2i", month))) |>
  #dplyr::mutate(woy = factor(date_to_week(eventDate), levels = sprintf("%0.2i", 1:53))) |>
  dplyr::select(-date, -season) |>
  na.omit()
winter_bg = dplyr::filter(njgb_bg, season == "winter") |>
  sf::st_drop_geometry() |>
  dplyr::rename(sst = obpg_sst) |>
  dplyr::mutate(month = as.factor(sprintf("%0.2i", month))) |>
  #dplyr::mutate(woy = factor(date_to_week(eventDate), levels = sprintf("%0.2i", 1:53))) |>
  dplyr::select(-date) |>
  na.omit()


#----

ws_obs.drop = nwa2_sharks |>
  # dplyr::mutate(woy = factor(date_to_week(eventDate), levels = sprintf("%0.2i", 1:53))) |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()

gs_obs = gseal_occs.sf |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()

hs_obs = hseal_occs.sf |>
  dplyr::select(c("sst", "depth", "month")) |>
  dplyr::mutate(month = factor(sprintf("%0.2i", month), levels = sprintf("%0.2i", 1:12))) |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  na.omit()

bg.drop = bg |>
  sf::st_crop(bb) |>
  sf::st_drop_geometry() |>
  dplyr::rename(eventDate = date)|>
  dplyr::rename(sst = obpg_sst) |>
  dplyr::mutate(month = factor(format(eventDate, "%m"), levels = sprintf("%0.2i", 1:12))) |>
  #dplyr::mutate(woy = factor(date_to_week(eventDate), levels = sprintf("%0.2i", 1:53))) |>
  dplyr::select(-eventDate) |>
  na.omit()

#----
ws.flag <- c(rep(1, nrow(ws_obs.drop)), rep(0, nrow(bg.drop)))
ws.model <- maxnet::maxnet(ws.flag,
                           dplyr::bind_rows(ws_obs.drop, bg.drop))
plot(ws.model, type = "cloglog")

gs.flag <- c(rep(1, nrow(gs_obs)), rep(0, nrow(bg.drop)))
gs.model <- maxnet::maxnet(gs.flag,
                           dplyr::bind_rows(gs_obs, bg.drop))
plot(gs.model, type = "cloglog")

hs.flag <- c(rep(1, nrow(hs_obs)), rep(0, nrow(bg.drop)))
hs.model <- maxnet::maxnet(hs.flag,
                           dplyr::bind_rows(hs_obs, bg.drop))
plot(hs.model, type = "cloglog")


cofbb::get_table()
gomcc_bb = cofbb::get_bb("gom_carcharodon", "sf")
earliest_date = as.Date("2003-01-01")

gom_sharks = sf::read_sf(get_path("covars", "brickman_depth_shark_occs.gpkg")) |>
  sf::st_crop(gomcc_bb) |>
  dplyr::filter(eventDate > earliest_date) |>
  dplyr::mutate(month = format(eventDate, "%m") |>
                  as.numeric())
plot_obis(gom_sharks, what = "mapview")

apr_shark <- gom_sharks |>
  dplyr::filter(month == 4)
may_shark <- gom_sharks |>
  dplyr::filter(month == 5)
jun_shark <- gom_sharks |>
  dplyr::filter(month == 6)
jul_shark <- gom_sharks |>
  dplyr::filter(month == 7)
aug_shark <- gom_sharks |>
  dplyr::filter(month == 8)
sep_shark <- gom_sharks |>
  dplyr::filter(month == 9)
oct_shark <- gom_sharks |>
  dplyr::filter(month == 10)
nov_shark <- gom_sharks |>
  dplyr::filter(month == 11)

# monthly bg development ----
gomcc_bg = read_sf(get_path("covars", "brickman_covar_bg.gpkg")) |>
  st_crop(gomcc_bb) |>
  filter(date > earliest_date) |>
  dplyr::mutate(month = format(date, "%m") |>
                  as.numeric()) |>
  dplyr::mutate(season = date_to_season(date))
gomcc_bg


may_bg <- gomcc_bg |>
  dplyr::select(c("depth", "sst_May")) |>
  dplyr::rename(sst = sst_May) |>
  st_drop_geometry() |>
  na.omit()

jun_bg <- gomcc_bg |>
  dplyr::select(c("depth", "sst_Jun")) |>
  dplyr::rename(sst = sst_Jun) |>
  st_drop_geometry() |>
  na.omit()

jul_bg <- gomcc_bg |>
  dplyr::select(c("depth", "sst_Jul")) |>
  dplyr::rename(sst = sst_Jul) |>
  st_drop_geometry() |>
  na.omit()

aug_bg <- gomcc_bg |>
  dplyr::select(c("depth", "sst_Aug")) |>
  dplyr::rename(sst = sst_Aug) |>
  st_drop_geometry() |>
  na.omit()

sep_bg <- gomcc_bg |>
  dplyr::select(c("depth", "sst_Sep")) |>
  dplyr::rename(sst = sst_Sep) |>
  st_drop_geometry() |>
  na.omit()

oct_bg <- gomcc_bg |>
  dplyr::select(c("depth", "sst_Oct")) |>
  dplyr::rename(sst = sst_Oct) |>
  st_drop_geometry() |>
  na.omit()

nov_bg <- gomcc_bg |>
  dplyr::select(c("depth", "sst_Nov")) |>
  dplyr::rename(sst = sst_Nov) |>
  st_drop_geometry() |>
  na.omit()


# monthly white shark models ---- 
# doesn't work not enough obs

may_sharks_pred = may_shark |>
  dplyr::select(c("sst", "depth")) |>
  sf::st_crop(gomcc_bb) |>
  sf::st_drop_geometry() |>
  na.omit()
may.ws.flag <- c(rep(1, nrow(may_sharks_pred)), rep(0, nrow(may_bg)))
may.ws.model <- maxnet::maxnet(may.ws.flag,
                               dplyr::bind_rows(may_sharks_pred, may_bg))
# warning data sparse
plot(may.ws.model, type = "cloglog")

jun_sharks_pred = jun_shark |>
  dplyr::select(c("sst", "depth")) |>
  sf::st_crop(gomcc_bb) |>
  sf::st_drop_geometry() |>
  na.omit()
jun.ws.flag <- c(rep(1, nrow(jun_sharks_pred)), rep(0, nrow(jun_bg)))
jun.ws.model <- maxnet::maxnet(jun.ws.flag,
                               dplyr::bind_rows(jun_sharks_pred, jun_bg))
# warning data sparse
plot(jun.ws.model, type = "cloglog")

jul_sharks_pred = jul_shark |>
  dplyr::select(c("sst", "depth")) |>
  sf::st_crop(gomcc_bb) |>
  sf::st_drop_geometry() |>
  na.omit()
jul.ws.flag <- c(rep(1, nrow(jul_sharks_pred)), rep(0, nrow(jul_bg)))
jul.ws.model <- maxnet::maxnet(jul.ws.flag,
                               dplyr::bind_rows(jul_sharks_pred, jul_bg))
# warning data sparse
plot(jul.ws.model, type = "cloglog")

aug_sharks_pred = aug_shark |>
  dplyr::select(c("sst", "depth")) |>
  sf::st_crop(gomcc_bb) |>
  sf::st_drop_geometry() |>
  na.omit()
aug.ws.flag <- c(rep(1, nrow(aug_sharks_pred)), rep(0, nrow(aug_bg)))
aug.ws.model <- maxnet::maxnet(aug.ws.flag,
                               dplyr::bind_rows(aug_sharks_pred, aug_bg))
plot(aug.ws.model, type = "cloglog")

sep_sharks_pred = sep_shark |>
  dplyr::select(c("sst", "depth")) |>
  sf::st_crop(gomcc_bb) |>
  sf::st_drop_geometry() |>
  na.omit()
sep.ws.flag <- c(rep(1, nrow(sep_sharks_pred)), rep(0, nrow(sep_bg)))
sep.ws.model <- maxnet::maxnet(sep.ws.flag,
                               dplyr::bind_rows(sep_sharks_pred, sep_bg))
plot(sep.ws.model, type = "cloglog")

oct_sharks_pred = oct_shark |>
  dplyr::select(c("sst", "depth")) |>
  sf::st_crop(gomcc_bb) |>
  sf::st_drop_geometry() |>
  na.omit()
oct.ws.flag <- c(rep(1, nrow(oct_sharks_pred)), rep(0, nrow(oct_bg)))
oct.ws.model <- maxnet::maxnet(oct.ws.flag,
                               dplyr::bind_rows(oct_sharks_pred, oct_bg))
# warning data sparse
plot(oct.ws.model, type = "cloglog")

nov_sharks_pred = nov_shark |>
  dplyr::select(c("sst", "depth")) |>
  sf::st_crop(gomcc_bb) |>
  sf::st_drop_geometry() |>
  na.omit()
nov.ws.flag <- c(rep(1, nrow(nov_sharks_pred)), rep(0, nrow(nov_bg)))
nov.ws.model <- maxnet::maxnet(nov.ws.flag,
                               dplyr::bind_rows(nov_sharks_pred, nov_bg))
# warning data sparse
plot(nov.ws.model, type = "cloglog")

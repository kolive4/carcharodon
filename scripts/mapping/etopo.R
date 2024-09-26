data_path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data"
depth_threshold = 750

gebco = topotools::read_gebco(bb = c(-74.9, -65, 38.8, 46))

etopow = topotools::read_etopo(bb = c(-74.9, -65, 38.8, 46)) |>
  write_stars(file.path(data_path, "mapping/etopo/etopo.tif")) |>
  stars::st_warp(gebco) |>
  write_stars(file.path(data_path, "mapping/etopo/etopo_warped.tif"))

etopow_750_mask = etopow |>
  dplyr::mutate(z = dplyr::between(z, -depth_threshold, 0)) |>
  write_stars(file.path(data_path, "mapping/etopo/etopo_warped_750_mask.tif"))

etopow_mask = etopow |>
  dplyr::mutate(z = z <= 0) |>
  write_stars(file.path(data_path, "mapping/etopo/etopo_warped_mask.tif"))


etopow_dist = stars::read_stars(here::here("data", "mapping", "etopo", "etopo_warped_mask.tif")) |>
  rlang::set_names("mask") |>
  distance_to_shore() |>
  stars::write_stars(here::here("data", "mapping", "etopo", "etopo_warped_distance_to_shore_meters.tif"))

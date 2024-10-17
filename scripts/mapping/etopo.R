data_path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data"
depth_threshold = 750
distance_threshold = 46300

gebco = topotools::read_gebco(bb = c(-74.9, -65, 38.8, 46))

etopow = topotools::read_etopo(bb = c(-74.9, -65, 38.8, 46)) |>
  write_stars(file.path(data_path, "mapping/etopo/etopo.tif")) |>
  stars::st_warp(gebco) |>
  write_stars(file.path(data_path, "mapping/etopo/etopo_warped.tif"))

etopow_750_mask = etopow |>
  dplyr::mutate(z = dplyr::between(z, -depth_threshold, 0)) |>
  write_stars(file.path(data_path, "mapping/etopo/etopo_warped_750_mask.tif"))

etopow_750_contour = etopow |>
  st_contour(contour_lines = TRUE, breaks = -depth_threshold) |>
  write_sf(file.path(data_path, "mapping/etopo/etopo_warped_750_contour.gpkg"))

etopow_mask = etopow |>
  dplyr::mutate(z = z <= 0) |>
  write_stars(file.path(data_path, "mapping/etopo/etopo_warped_mask.tif"))

etopow_dist = stars::read_stars(here::here("data", "mapping", "etopo", "etopo_warped_mask.tif")) |>
  rlang::set_names("mask") |>
  distance_to_shore() |>
  stars::write_stars(here::here("data", "mapping", "etopo", "etopo_warped_distance_to_shore_meters.tif"))

etopow_25nm_shoreline_mask = etopow_dist |>
  dplyr::mutate(distance = dplyr::between(distance, 0.01, distance_threshold)) |>
  write_stars(file.path(data_path, "mapping/25nm_shoreline_mask.tif"))

etopow_25nm_contour = etopow_dist |>
  st_contour(contour_lines = TRUE, breaks = distance_threshold) |>
  write_sf(file.path(data_path, "mapping/25nm_shoreline_contour.gpkg"))

data_path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data"
depth_threshold = 750

gebco = topotools::read_gebco(bb = c(-74.9, -65, 38.8, 46)) |>
  write_stars(file.path(data_path, "mapping/gebco/gebco.tif"))

gebco_750_mask = topotools::read_gebco(bb = c(-74.9, -65, 38.8, 46)) |>
  dplyr::mutate(z = dplyr::between(z, -depth_threshold, 0)) |>
  write_stars(file.path(data_path, "mapping/gebco/gebco_750_mask.tif"))
# gebco_750_buf = sf::st_as_sf(gebco_750_mask) |>
#   write_sf(file.path(data_path, "mapping/gebco_750_buf.gpkg"))

gebco_mask = topotools::read_gebco(bb = c(-74.9, -65, 38.8, 46)) |>
  dplyr::mutate(z = z <= 0) |>
  write_stars(file.path(data_path, "mapping/gebco/gebco_mask.tif"))
# gebco_domain_buf = sf::st_as_sf(gebco_mask, merge = TRUE, as_points = FALSE) |>
#   write_sf(file.path(data_path, "mapping/gebco_domain.gpkg"))

coast_mask = rnaturalearth::ne_coastline(scale = "Large", returnclass = "sf") |>
  st_crop(shark_box) |>
  st_buffer(46300) |> # 46300 m = 25 nmi
  st_union() |>
  st_cast ("POLYGON") 
write_sf(coast_mask[1], file.path(data_path, "mapping/25nm_shoreline_buffer.gpkg"))
coast_mask = read_sf(file.path(data_path, "mapping/25nm_shoreline_buffer.gpkg"))
cmask = stars::st_rasterize(coast_mask, gebco_750_mask*0) |>
  write_stars(file.path(data_path, "mapping/25nm_shoreline_buffer.tif"))

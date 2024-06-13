# not sure if needed
mask = stars::read_stars(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/obis_thermal_niche/inst/ext_data", "world.binary.tif")) |>
  st_crop(st_bbox(total_buf))

mask[total_buf] |>
  write_stars(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/obis_thermal_niche/inst/ext_data", "study_area.binary.tif")) 

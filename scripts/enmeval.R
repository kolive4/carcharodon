# Crop environmental rasters to match the study extent
envs.bg <- sf::st_crop(envs, occs.buf)
# Next, mask the rasters to the shape of the buffers
envs.bg <- envs.bg[occs.buf]
plot(envs.bg[1], main = names(envs)[1], reset = FALSE)
plot(occs, col = 'orange', add = TRUE)
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)


envs.spring.bg = sf::st_crop(envs[2], occs.spring.buf)
envs.spring.bg = envs.spring.bg[occs.spring.buf]
plot(envs.spring.bg[1], main = names(envs)[2], reset = FALSE)
plot(occs.spring, col = "orange", add = TRUE)
plot(occs.spring.buf, border = "blue", lwd = 3, add = TRUE)

envs.summer.bg = sf::st_crop(envs[3], occs.summer.buf)
envs.summer.bg = envs.summer.bg[occs.summer.buf]
plot(envs.summer.bg[1], main = names(envs)[3], reset = FALSE)
plot(occs.summer, col = "orange", add = TRUE)
plot(occs.summer.buf, border = "blue", lwd = 3, add = TRUE)

envs.autumn.bg = sf::st_crop(envs[4], occs.autumn.buf)
envs.autumn.bg = envs.autumn.bg[occs.autumn.buf]
plot(envs.autumn.bg[1], main = names(envs)[4], reset = FALSE)
plot(occs.autumn, col = "orange", add = TRUE)
plot(occs.autumn.buf, border = "blue", lwd = 3, add = TRUE)

envs.winter.bg = sf::st_crop(envs[5], occs.winter.buf)
envs.winter.bg = envs.winter.bg[occs.winter.buf]
plot(envs.winter.bg[1], main = names(envs)[5], reset = FALSE)
plot(occs.winter, col = "orange", add = TRUE)
plot(occs.winter.buf, border = "blue", lwd = 3, add = TRUE)



# Randomly sample 10,000 background points from one background extent raster 
# (only one per cell without replacement). Note: Since the raster has <10,000 pixels, 
# you'll get a warning and all pixels will be used for background. We will be sampling 
# from the biome variable because it is missing some grid cells, and we are trying to 
# avoid getting background points with NA. If one raster in the stack has NAs where the
# other rasters have data, ENMeval internally converts these cells to NA.
N <- 10000
bg <- sf::st_sample(occs.buf, N) |>
  sf::st_as_sf() |>
  sf::st_set_geometry("geometry") |>
  sf::write_sf(file.path("~/Desktop/GradSchoolCode/packages/obis_thermal_niche/inst/ext_data", 
                         paste0("buffered_bg_points_", N, ".gpkg")))

spring.bg = sf::st_sample(occs.spring.buf, N) |>
  sf::st_as_sf() |>
  sf::st_set_geometry("geometry")

summer.bg = sf::st_sample(occs.summer.buf, N) |>
  sf::st_as_sf() |>
  sf::st_set_geometry("geometry")

autumn.bg = sf::st_sample(occs.autumn.buf, N) |>
  sf::st_as_sf() |>
  sf::st_set_geometry("geometry")

# Notice how we have pretty good coverage (every cell).
plot(envs.bg[1], reset = FALSE)
plot(bg, add = TRUE, pch = ".", col = 'orange')

plot(envs.spring.bg[1], reset = FALSE)
plot(spring.bg, add = TRUE, pch = ".", col = "orange")

plot(envs.summer.bg[1], reset = FALSE)
plot(summer.bg, add = TRUE, pch = ".", col = "orange")

plot(envs.autumn.bg[1], reset = FALSE)
plot(autumn.bg, add = TRUE, pch = ".", col = "orange")



occsr = as_dataframe(occs) |>
  dplyr::select(c("longitude", "latitude"))

occsr.spring = as_dataframe(occs.spring) |>
  dplyr::select(c("longitude", "latitude"))


bgr = as_dataframe(bg)

bgr.spring = as_dataframe(spring.bg)


envsr = as(envs, "Raster")

envsr.spring = as(envs[2], "Raster")

occsr.spring = dplyr::mutate(occsr.spring, sst = raster::extract(envsr.spring, occsr.spring))
bgr.spring = dplyr::mutate(bgr.spring, sst = raster::extract(envsr.spring, bgr.spring))
saveRDS(bgr.spring, file = "bgr.spring.RDS")
saveRDS(occsr.spring, file = "occsr.spring.RDS")

e.mx.l <- ENMevaluate(occs = occsr, envs = envsr, bg = bgr, 
                      algorithm = 'maxnet', partitions = 'block', 
                      tune.args = list(fc = "L", rm = 1:2))

e.mx.l.spring <- ENMevaluate(occs = occsr.spring, 
                             # envs = envsr.spring,
                             bg = bgr.spring,
                             algorithm = 'maxnet', partitions = 'block',
                             tune.args = list(fc = "L", rm = 1:2))

raster::plot(e.mx.l@predictions, col=colorRampPalette(c("white", "blue"))(255))

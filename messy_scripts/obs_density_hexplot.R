suppressPackageStartupMessages({
  library(rlang)
  library(maxnetic)
  library(stars)
  library(sf)
  library(dplyr)
  library(patchwork)
  library(ggplot2)
  library(rnaturalearth)
})

pred = read_stars("prediction.tif")
obs = read_sf("obs_brick.gpkg")
mobs = st_cast(obs, "MULTIPOINT")
coast = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")

hex = st_make_grid(mobs, n = c(30, 30), square = FALSE) |>
  st_as_sf()
plot(hex)

# hex2 = st_join(hex, obs) |>
#   summarize(count = n())

ix = st_intersects(hex, mobs)
lengths(ix)

hex = dplyr::mutate(hex, count = lengths(ix))
plot(hex["count"], reset = FALSE)
plot(coast, col = "orange", add = TRUE)
plot(st_geometry(obs), col = "pink", add = TRUE, pch = ".")

nc = st_read(system.file("shape/nc.shp", package="sf"))
g = st_make_grid(nc)
plot(g)
plot(st_geometry(nc), add = TRUE)
# g[nc] selects cells that intersect with nc:
plot(g[nc], col = '#ff000088', add = TRUE)

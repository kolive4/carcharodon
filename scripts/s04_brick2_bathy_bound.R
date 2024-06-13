source("../setup.R")

brick_depth = subset_brickman(scenario = 'PRESENT', vars = 'Bathy_depth', bb = shark_box, path = here::here("data/brickman/bathy"))
plot(brick_depth)

m = brick_depth[["Bathy_depth"]] |>
  as.matrix()
ix = m > 750
m[ix] = NA
brick_depth[["Bathy_depth"]] = m

plot(brick_depth["Bathy_depth"], reset = FALSE, breaks = "equal")
plot(shark_occs, add = TRUE, col = 'orange')

brick_buf = st_contour(brick_depth, breaks = c(0, 750))
brick_buf = brick_buf[1]
plot(brick_buf)
write_sf(brick_buf, here::here("data/brickman/bathy/PRESENT_PRESENT_Bathy_depth_750.gpkg"))

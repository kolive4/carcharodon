bathymetry = topotools::read_etopo("ETOPO_2022_v1_60s_N90W180_surface.nc", bb = bb) |>
  st_warp(covars) |>
  topotools::mask_topo(value = 0, where = "above")

lvl = sprintf("%0.2i", 1:12)
month = bathymetry |>
  mutate(z = factor("01", levels = lvl)) |>
  rename(month = z)

st_bbox(masked)
st_bbox(covars)

plot(bathymetry)

combined_covars = c(covars_winter, bathymetry, make_month_layer(bathymetry, 1), along = NA_integer_) |>
  dplyr::rename(depth = z)
  

jan_pred <- predict(ws.model, combined_covars, type = "cloglog")
jan_pred
plot(jan_pred, col = sf.colors(n = nbreaks), reset = FALSE)


combined_covars = c(covars, bathymetry, make_month_layer(bathymetry, 8), along = NA_integer_) |>
  dplyr::rename(depth = z)

combined_covars = combined_covars[total_buf]

aug_pred <- predict(ws.model, combined_covars, type = "cloglog")
aug_pred
nbreaks = 11
plot(aug_pred, col = sf.colors(n = nbreaks), reset = FALSE)
plot(total_buf, add = TRUE)

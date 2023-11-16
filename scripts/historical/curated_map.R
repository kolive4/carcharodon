library(mapview)

curated_occs <- read.csv("data/historical/curated_literature.csv")

mapview::mapView(curated_occs, xcol = "Longitude", ycol = "Latitude", grid = FALSE, crs = 4326)
curated_points = st_as_sf(curated_occs, coords = c("Longitude", "Latitude"), crs = 4326)

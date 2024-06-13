suppressPackageStartupMessages({
  library(stars)
  library(sf)
  library(leaflet)
})

z = read_sf("/home/koliveira/output.csv")
y = read_stars("/mnt/s1/projects/ecocast/coredata/mur/nwa/2023/0101/20230101090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1-DAY-sst-0.01d.tif")

leaflet(data = z) |>
  addTiles() |>
  addRasterImage(as(y, "Raster")) |>
  addCircleMarkers(data = z, stroke = FALSE, fillOpacity = 0.5, radius = 3)

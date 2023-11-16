# This script is aimed at extracting occurrences from OBIS, and taking a look at their institute and dataset IDs to compare with other occurrences found in the literature

bb_text = sf::st_as_text(bb$geom)

inst_obis = robis::occurrence(scientificname = "Carcharodon carcharias",
                              startdate = as.Date("1872-01-01"),
                              enddate = as.Date("2017-01-01"),
                              geometry = bb_text) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

unique(inst_obis$institutionCode)

plot_obis(inst_obis, what = "mapview")

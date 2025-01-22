x = unique(wshark$occurrenceID)
z = readr::read_csv(file.path(cfg$data_path, "obis/carcharodon_carcharias-raw.csv")) |>
  distinct() |>
  dplyr::filter(basisOfRecord == "HumanObservation") |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  #dplyr::bind_rows(curated, inat, satellite) |>
  sf::st_crop(shark_box) |>
  dplyr::mutate(basisOfRecord = dplyr::if_else(basisOfRecord == "HumanObservation", "OBIS", basisOfRecord)) |>
  filter(occurrenceID %in% x)

nodes = unique(z$node_id)


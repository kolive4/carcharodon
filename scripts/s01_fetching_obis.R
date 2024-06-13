white_shark = fetch_obis(scientificname = "Carcharodon carcharias")  |>
  distinct() |>
  dplyr::glimpse()

grey_seal = fetch_obis(scientificname = "Halichoerus grypus") |>
  distinct() |>
  dplyr::glimpse()

harbor_seal = fetch_obis(scientificname = "Phoca vitulina") |>
  distinct() |>
  dplyr::glimpse()

library(tidysdm)
library(tidymodels)
library(dplyr)

data("bradypus", package = "maxnet")
bradypus_tb <- tibble::as_tibble(bradypus) %>%
  dplyr::mutate(presence = relevel(
    factor(
      dplyr::case_match(presence, 1 ~ "presence", 0 ~ "absence")
    ),
    ref = "presence"
  )) %>%
  select(-ecoreg)
bradypus_tb[12,] = NA

wflow = workflows::workflow()
rec = recipes::recipe(head(bradypus_tb), presence ~ .) |>
  recipes::step_naomit()

model = maxent(
  mode = "classification",
  engine = "maxnet",
  feature_classes = NULL,
  regularization_multiplier = NULL
)

wflow = wflow |>
  workflows::add_recipe(rec) |>
  workflows::add_model(model)

fitted_wflow = fit(wflow, data = bradypus_tb)

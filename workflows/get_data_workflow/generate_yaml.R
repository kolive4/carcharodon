cfg = list(version = "v0.000",
           species = "Carcharodon carcharias",
           start_date = "2000-01-01",
           end_date = "2024-01-01",
           bbox_name = "gom_carcharodon",
           months = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"),
           mon_covariates = c("SST", "Tbtm"),
           static_covariates = "Bathy_depth",
           data_path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data",
           bg_number = 10000,
           bg_m = 2,
           polygon = "brickman/bathy/PRESENT_PRESENT_Bathy_depth_750.gpkg",
           bg_seed = NULL) |>
  charlier::write_config(filename = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/get_data_workflow/v0.000.yaml")

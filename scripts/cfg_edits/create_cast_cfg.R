files = list.files(path = "workflows/tidy_workflow", pattern = "^t11\\.[0-1][1,2]036.\\d+\\.yaml$", full.names = TRUE)

create_cast_cfg = function(file,
                           template_file = "workflows/tidy_cast/t11.000500.01.yaml") {
  if (FALSE) {
    file = files[2]
  }
  scenarios = 0:4
  months = sprintf("%02d", 1:12)
  
  scenario_map <- list(
    "0" = list(name = "PRESENT", year = "PRESENT"),
    "1" = list(name = "RCP45", year = 2055),
    "2" = list(name = "RCP45", year = 2075),
    "3" = list(name = "RCP85", year = 2055),
    "4" = list(name = "RCP85", year = 2075)
  )
  
  filename = basename(file)
  wf_version <- sub("\\.yaml$", "", filename)
  vpars = charlier::parse_version(wf_version)
  wf_cfg = charlier::read_config(file)
  
  if (vpars["release"] == 20) {
    x = unname(vpars["major"])
    substr(x, start = 2, stop = 2) <- "2"
    cast_cfg_major = x
  } else {
    cast_cfg_major = vpars["major"]
  }
  
  base = paste0(cast_cfg_major, ".", vpars["minor"])
  output_dir = "workflows/tidy_cast"
  
  for (scenario in scenarios) {
    for (month in months) {
      if (FALSE) {
        scenario = 1
        month = "01"
      }
      cast_name <- sprintf("%s%d.%s", base, scenario, month)
      cast_vpars = charlier::parse_version(cast_name)
      file_path <- file.path(output_dir, paste0(cast_name, ".yaml"))
      
      cfg = charlier::read_config(template_file)
      map_entry = scenario_map[[as.character(scenario)]]
      cfg$version = cast_name
      cfg$gather_data_path = wf_cfg$gather_data_path
      cfg$thinned_data_path = wf_cfg$thinned_data_path
      cfg$scenario = map_entry$name
      cfg$year = factor(map_entry$year)
      cfg$month = factor(month)
      cfg$wf_version = wf_version
      cfg$wf_path = sprintf("workflows/tidy_workflow/versions/%s/%s", unname(vpars["major"]), unname(vpars["minor"]))
      if (scenario != 0) {
        cfg$graphics$add_pres_pts = FALSE
      } else {
        cfg$graphics$add_pres_pts = TRUE
      }
      if (substr(cast_vpars["minor"], 1, 1) == 1) {
        cfg$contour_name = "mapping/etopo/etopo_warped_750_contour.gpkg"
        cfg$mask_name = "mapping/etopo/etopo_warped_750_mask.tif"
        cfg$graphics$plot_contour = TRUE
      }
      cfg$vars = wf_cfg$vars
      cfg$dynamic_names = wf_cfg$dynamic_names
      cfg$static_names = wf_cfg$static_names
      cfg$obs_filter = wf_cfg$obs_filter
      if (substr(cast_vpars["major"], 2, 2) == 1) {
        if (substr(cast_vpars["minor"], 4, 4) %in% c(5, 6)) {
          cfg$seal_bg_ratio = "one_to_two"
          cfg$hseal_model_type = "bt"
          cfg$gseal_model_type = "bt"
        }
      }
      
      charlier::write_config(cfg, file_path)
    }
  }
}
lapply(files, create_cast_cfg, template_file = "workflows/tidy_cast/t11.000300.01.yaml")




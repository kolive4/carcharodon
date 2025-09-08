suppressPackageStartupMessages({
  library(charlier)
})

files = list.files(path = "workflows/tidy_cast", 
                   pattern = "^t[1-2]1\\.[0-1][1,2]096[0-4].01.yaml$",
                   full.names = TRUE)

create_report_cfg = function(cast_file,
                           template_file = "workflows/tidy_reports/c12.000300.01_12.yaml") {
  if (FALSE) {
    cast_file = files[361]
    template_file = "workflows/tidy_reports/c12.000300.01_12.yaml"
  }
  
  cast_filename = basename(cast_file)
  cast_version = sub("\\.yaml$", "", cast_filename)
  cast_vpars = parse_version(cast_version)
  cast_major = unname(cast_vpars["major"])
  cast_cfg = charlier::read_config(cast_file)
  wf_file = paste0("workflows/tidy_workflow", "/", cast_cfg$wf_version, ".yaml")
  wf_cfg = charlier::read_config(wf_file)
  wf_vpars = parse_version(wf_cfg$version)
  
  report_yaml_filename = paste0("c", substr(cast_major, 2, 3), ".", cast_vpars["minor"], ".01_12.yaml")
  report_version = sub("\\.yaml$", "", report_yaml_filename)
  report_vpars = parse_version(report_version)
  output_dir = "workflows/tidy_reports"
  output_file_path = file.path(output_dir, report_yaml_filename)
  
  scenario_map <- list(
    "0" = "Present Conditions",
    "1" = "RCP 4.5 2055 Conditions",
    "2" = "RCP 4.5 2075 Conditions",
    "3" = "RCP 8.5 2055 Conditions",
    "4" = "RCP 8.5 2075 Conditions"
  )
  
  scenario_code = substr(unname(report_vpars["minor"]), 
         nchar(unname(report_vpars["minor"])), 
         nchar(unname(report_vpars["minor"])))
  
  cfg = charlier::read_config(template_file)
  cfg$version = report_version
  cfg$tidy_w_vers = wf_cfg$version
  cfg$tidy_w_path = cast_cfg$wf_path
  cfg$tidy_path = paste0("workflows/tidy_cast/versions/", cast_vpars["major"], "/", cast_vpars["minor"])
  cfg$gather_data_path = cast_cfg$gather_data_path
  cfg$thinned_data_path = cast_cfg$thinned_data_path
  cfg$scenario = scenario_map[[as.character(scenario_code)]]
  cfg$basisOfRecord = cast_cfg$obs_filter$basisOfRecord
  
  charlier::write_config(cfg, output_file_path)
}

lapply(files, create_report_cfg, template_file = "workflows/tidy_reports/c11.000300.01_12.yaml")




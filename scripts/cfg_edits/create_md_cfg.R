suppressPackageStartupMessages({
  library(charlier)
})

files = list.files(path = "workflows/tidy_reports", pattern = "^c[1-2]1\\.[0-1]0096[0-4].01_12.yaml$", full.names = TRUE)

create_md_cfg = function(report_file,
                         template_file = "workflows/tidy_md/m00.000000.yaml") {
  if (FALSE) {
    report_file = files[1]
    template_file = "workflows/tidy_md/m00.000000.yaml"
  }
  
  report_filename = basename(report_file)
  report_version = sub("\\.yaml$", "", report_filename)
  report_vpars = parse_version(report_version)
  report_major = unname(report_vpars["major"])
  report_minor = unname(report_vpars["minor"])
  report_cfg = charlier::read_config(report_file)
  
  md_yaml_filename = paste0("m", substr(report_major, 2, 3), ".", substr(report_minor, 1, nchar(report_minor)-1), ".yaml")
  md_version = sub("\\.yaml$", "", md_yaml_filename)
  md_vpars = parse_version(md_version)
  md_major = unname(md_vpars["major"])
  md_minor = unname(md_vpars["minor"])
  
  thin_version = basename(report_cfg$thinned_data_path)
  thin_vpars = parse_version(thin_version)
  thin_minor = unname(thin_vpars["minor"])
  
  thin_map_sharks = list(
    "0" = "No thinning",
    "1" = "Thinned satellite data (PSAT and SPOT)",
    "2" = "Thinned all data"
  )
  thin_code = substr(thin_minor, 4, 4)
  thin_map_seals = list(
    "0" = "Thinned observations"
  )

  ratio_map = list(
  	"0" = "All pseudo-absence/background points",
  	"1" = "1:2 observation:pseudo-absence ratio"
  )
  ratio_code = substr(thin_minor, 5, 5)
  
  species_map = list(
  	"1" = "White shark (Carcharodon carcharias)",
  	"2" = "Gray seal (Halichoerus grypus)",
  	"3" = "Harbor seal (Phoca vitulina)"
  )
  species_code = substr(md_major, nchar(md_major), nchar(md_major))
  
  extent_map = list(
  	"0" = "Full extent",
  	"1" = "Cropped to 750 m isobath"
  )
  extent_code = substr(md_minor, 1, 1)
  
  covariates_map = list(
    "00" = "all covariates",
    "01" = "shark specific (tbtm, sss, sbtm, mld, log_depth, gseal, hseal)",
    "02" = "seal specific (sst, dfs, velocity magnitude, xbtm)",
    "03" = "all covariates (u and v become vel_mag)",
    "04" = "seal specific v2 (sss, dfs, month, mld)",
    "05" = "all covariates (u and v become vel_mag) and seals",
    "06" = "non-seal (sst, tbtm, log depth, sbtm, seals, vel_mag, xbtm)",
    "07" = "shark specific v2 (sst, tbtm, sbtm, log depth, and seals)"
  )
  covariates_code = substr(md_minor, 3, 4)
  
  metrics_map = list(
    "0" = "evaluated using all metrics",
    "1" = "evaluated using area under the receiver operator curve (roc_auc)",
    "2" = "evaluated using true skill statistic (tss)",
    "3" = "evaluated using continuous boyce index",
    "4" = "evaluated using Brier score",
    "5" = "evaluated using accuracy",
    "6" = "evaluated using true skill staistic (tss) and area under the receiver operator curve (roc_auc)"
  )
  metrics_code = substr(md_minor, 5, 5)
  
  cfg = charlier::read_config(template_file)
  cfg$version = md_version
  if (species_code != 1) {
    thin_message = thin_map_seals[[as.character(thin_code)]]
  } else {
    thin_message = thin_map_sharks[[as.character(thin_code)]]
  }
  cfg$message = list(species = species_map[[as.character(species_code)]], 
  					   thin_message = thin_message,
  					   obs_bg_ratio = ratio_map[[as.character(ratio_code)]],
					   spat_extent = extent_map[[as.character(extent_code)]], 
					   covs = covariates_map[[as.character(covariates_code)]], 
					   metrics = metrics_map[[as.character(metrics_code)]])
  cfg$t_wf_version = report_cfg$tidy_w_vers
  cfg$t_rep_now_version = paste0(report_major, ".", substr(report_minor, 1, nchar(report_minor)-1), "0.01_12")
  cfg$t_rep_fore_version = paste0(report_major, ".", substr(report_minor, 1, nchar(report_minor)-1), "4.01_12")
  
  output_dir = "workflows/tidy_md"
  output_file_path = file.path(output_dir, md_yaml_filename)
  
  charlier::write_config(cfg, output_file_path)
}

lapply(files, create_md_cfg, template_file = "workflows/tidy_md/m00.000000.yaml")




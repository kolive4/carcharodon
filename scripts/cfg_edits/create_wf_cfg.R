#' Function to create workflow scripts
#' 
#' @param wf_version version(s) to create config for
#' @param template_file file on which to base this config off of
#' @return workflow config yaml

copy_amend_wf_cfg = function(wf_version,
                             template_file = "workflows/tidy_workflow/t11.00000.00.yaml"
                             ) {
  if (FALSE) {
    wf_version = "t11.10066.20"
    template_file = "workflows/tidy_workflow/t11.00000.00.yaml"
  }

  cov_map <- list(
    "03" = list(dynamic_names = c("brick_sst", "brick_tbtm", "brick_sss", "brick_sbtm", 
                                  "brick_mld", "vel_mag", "brick_xbtm"),
                static_names = c("log_depth", "dfs", "month")),
    "05" = list(dynamic_names = c("brick_sst", "brick_tbtm", "brick_sss", "brick_sbtm", 
                                  "brick_mld", "vel_mag", "brick_xbtm", "gseal", "hseal"),
                static_names = c("log_depth", "dfs", "month")),
    "06" = list(dynamic_names = c("brick_sst", "brick_tbtm", "brick_sbtm", 
                                  "vel_mag", "brick_xbtm", "gseal", "hseal"),
                static_names = "log_depth"),
    "07" = list(dynamic_names = c("brick_sst", "brick_tbtm", "brick_sbtm", "gseal", "hseal"),
                static_names = "log_depth")
  )
  
  tidy_thin_map = list(
    "00" = list(thinned_data_path = "workflows/tidy_thin/versions/t01/t01.00010"),
    "20" = list(thinned_data_path = "workflows/tidy_thin/versions/t01/t01.00011")
  )
  
  metrics_map = list(
    "0" = list(metrics = c("roc_auc", "accuracy", "boyce_cont", "brier_class", "tss_max")),
    "1" = list(metrics = "roc_auc"),
    "2" = list(metrics = "tss_max"),
    "3" = list(metrics = "boyce_cont"),
    "4" = list(metrics = "brier_class"),
    "5" = list(metrics = "accuracy"),
    "6" = list(metrics = c("roc_auc", "tss_max"))
  )
  
  extent_map = list(
    "0" = list(contour_name = NULL,
               mask_name = "mapping/etopo/etopo_warped_mask.tif",
               graphics = list(ggtitle = "Nowcast Habitat Suitability",
                               add_pres_pts = "yes",
                               plot_contour = "no")),
    "1" = list(contour_name = "mapping/etopo/etopo_warped_750_contour.gpkg",
               mask_name = "mapping/etopo/etopo_warped_750_mask.tif",
               graphics = list(ggtitle = "Nowcast Habitat Suitability",
                               add_pres_pts = "yes",
                               plot_contour = "yes"))
  )
  
  output_dir = "workflows/tidy_workflow"
  file_path = paste0(output_dir, "/", wf_version, ".yaml")
  vpars = parse_version(wf_version)
  template_cfg = charlier::read_config(template_file)
  cov_code = substr(vpars["minor"], 3, 4)
  tidy_thin_code = substr(vpars["release"], 1, 2)
  metric_code = substr(vpars["minor"], 5, 5)
  extent_code = substr(vpars["minor"], 1, 1)
  
  cfg = template_cfg
  cfg$version = wf_version
  cfg$dynamic_names = cov_map[[as.character(cov_code)]]$dynamic_names
  cfg$static_names = cov_map[[as.character(cov_code)]]$static_names
  cfg$thinned_data_path = tidy_thin_map[[as.character(tidy_thin_code)]]$thinned_data_path
  cfg$metrics = metrics_map[[as.character(metric_code)]]$metrics
  cfg$contour_name = extent_map[[as.character(extent_code)]]$contour_name
  cfg$mask_name = extent_map[[as.character(extent_code)]]$mask_name
  cfg$graphics = extent_map[[as.character(extent_code)]]$graphics
      
  charlier::write_config(cfg, file_path)
}

# Generate valid version combinations
prefix = "t11"
first_digit = 0:1
hundreds = "00"
three_five = c("3","5")
last_digit = 0:6
suffix <- c("00", "20")

# Create all combinations
grid = expand.grid(
  a = first_digit,
  b = three_five,
  c = last_digit,
  d = suffix,
  stringsAsFactors = FALSE
)

# Format middle section with leading zeros
middle = paste0(grid$a, hundreds, grid$b, grid$c)

# Final version strings
versions = paste0(prefix, ".", middle, ".", grid$d)

# Check output
head(versions)

lapply(versions, copy_amend_wf_cfg, template_file = "workflows/tidy_workflow/t11.00000.00.yaml")




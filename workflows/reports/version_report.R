suppressPackageStartupMessages({
  library(charlier)
  library(argparser)
  library(dplyr)
  library(stars)
  library(brickman)
  library(twinkle)
  library(maxnet)
  library(maxnetic)
  library(ggplot2)
})

args = argparser::arg_parser("reports for white shark habitat suitability",
                             name = "version_report.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/reports/r01.0010.01.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)

vpars = charlier::parse_version(cfg$version)
vpath = file.path(cfg$root_path, cfg$report_path, "versions", vpars[["major"]], vpars[["minor"]], cfg$version)
min_vpath = file.path(cfg$root_path, cfg$report_path, "versions", vpars[["major"]], vpars[["minor"]])
if (!dir.exists(vpath)) 
  dir.create(vpath, showWarnings = FALSE, recursive = TRUE)

charlier::start_logger(filename = file.path(vpath, "log"))
charlier::info("writing config")
charlier::write_config(cfg, filename = file.path(vpath, basename(args$config)))

mpars = charlier::parse_version(cfg$m_version)
m_path = file.path(cfg$root_path, cfg$m_workflow_path, "versions", mpars[["major"]], mpars[["minor"]], cfg$m_version)

if(file.exists(file.path(m_path, "model.rds"))){
  rmarkdown::render(file.path(cfg$root_path, cfg$report_path, "version_report.Rmd"), 
                    output_dir = vpath, 
                    output_file = paste0(cfg$version, ".md"))
  ret = 0
} else {ret = 28}

quit(save = "no", status = ret)




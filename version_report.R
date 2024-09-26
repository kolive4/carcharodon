version = "r00.0000"
output_path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/reports"

rmarkdown::render("version_report.Rmd", output_dir = output_path, output_file = paste0(version, ".md"))

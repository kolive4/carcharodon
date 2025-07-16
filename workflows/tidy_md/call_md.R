suppressPackageStartupMessages({
    library(charlier)
    library(rmarkdown)
})

cfg_file = commandArgs(trailingOnly = TRUE)[1]
cfg_file = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md/m21.10076.yaml"
cfg = read_config(cfg_file)
vpars = parse_version(cfg$version)
md_path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md"

output_filename = paste0(cfg$version, "_tidy_compiled")

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/quarto/bin/tools/x86_64")

rmarkdown::render(input = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md/tidy_md.Rmd", 
                  params = list(cfg_file = cfg_file),
                  output_format = "html_document",
                  output_file = paste0(output_filename, ".html"),
                  output_dir = file.path(md_path, "versions", vpars["major"], vpars["minor"]),
                  envir = new.env()) 

rmarkdown::render(input = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md/tidy_md.Rmd", 
                  params = list(cfg_file = cfg_file),
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_file = paste0(output_filename, ".md"),
                  output_dir = file.path(md_path, "versions", vpars["major"], vpars["minor"]),
                  envir = new.env()) 

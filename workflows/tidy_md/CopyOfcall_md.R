suppressPackageStartupMessages({
    library(charlier)
    library(rmarkdown)
})

cfg_file = commandArgs(trailingOnly = TRUE)[1]
cfg = read_config(cfg_file)
vpars = parse_version(cfg$version)
md_path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md"

output_filename = paste0(cfg$version, "_tidy_seal_compiled.html")

rmarkdown::render("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md/tidy_seal_md.Rmd", 
                  params = list(cfg_file = cfg_file),
                  output_file = output_filename,
                  output_dir = file.path(md_path, "versions", vpars["major"], vpars["minor"])) 


suppressPackageStartupMessages({
  library(charlier)
  library(argparser)
  library(dplyr)
  library(stars)
  library(brickman)
  library(twinkle)
  library(purrr)
  library(ggplot2)
  library(jsonlite)
  library(readr)
  library(stringr)
})

args = argparser::arg_parser("thinning observation and background data",
                             name = "tidy_thin.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_thin/v02.00000.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
for (f in list.files(cfg$source_path, pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}

vpars = charlier::parse_version(cfg$version)
vpath = file.path(cfg$root_path, cfg$output_path, "versions", vpars[["major"]], cfg$version)
if (!dir.exists(vpath)) 
  dir.create(vpath, showWarnings = FALSE, recursive = TRUE)

charlier::start_logger(filename = file.path(vpath, "log"))
charlier::info("writing config")
charlier::write_config(cfg, filename = file.path(vpath, basename(args$config)))

vfigure_path = file.path(vpath, "figures")
if (!dir.exists(vfigure_path)) 
  dir.create(vfigure_path, showWarnings = FALSE, recursive = TRUE)

mask = stars::read_stars(file.path(cfg$data_path, cfg$mask_name)) |>
  rlang::set_names("mask")

obs_bg = read_sf(file.path(cfg$gather_data_path, "brickman_covar_obs_bg.gpkg")) 

thin_obs = obs_bg |>
  dplyr::filter(id == 1) |>
  thin_by_BoR(BoR = cfg$bor_to_thin, mask = mask, dist = 10) 

thin_bg = obs_bg |>
  dplyr::filter(id == 0) |>
  thin_background(obs = thin_obs,
                  dist = 10) 
if(!is.null(cfg$bg_x)) {
  thin_bg = thin_bg |>
    dplyr::slice_sample(n = cfg$bg_x*nrow(thin_obs))
}

thin_obs_bg = dplyr::bind_rows(thin_obs, thin_bg) |>
  write_sf(file.path(vpath, "thinned_obs_bg.gpkg"))

thin_obs_inst = match_institution(raw = file.path(cfg$root_path, cfg$data_path, cfg$obis_path, cfg$raw_file),
                                  thinned = thin_obs_bg)

exp_thin_cite = thin_obs_inst |>
  dplyr::mutate(cleanedCitation = purrr::map_chr(bibliographicCitation, clean_and_format_citations)) |>
  dplyr::select(datasetName, n, cleanedCitation) |>
  dplyr::rename(c(Count = n,
                  `Dataset Name` = datasetName,
                  `Citation` = cleanedCitation)) |>
  write_csv(file.path(vpath, "obs_citations.csv"))
  
obs_remark = c("Visual; shore", "Visual; plane", "Visual; boat")

thin_obs_inst = thin_obs_inst |>
  dplyr::mutate(figdatasetName = stringr::str_wrap(datasetName, width = 35)) |>
  dplyr::filter(occurrenceRemarks %in% obs_remark) |>
  dplyr::mutate(cleanedCitation = purrr::map_chr(bibliographicCitation, clean_and_format_citations)) |>
  dplyr::rename(Count = n)

obs_source = ggplot(data = thin_obs_inst) +
  geom_col(aes(x = occurrenceRemarks, y = Count, fill = figdatasetName)) +
  scale_fill_viridis_d(name = "Dataset") +
  labs(x = "Observation Method",
       y = "Count") +
  theme_classic() +
  theme(legend.key.spacing.y = unit(10, "pt"))

png(file.path(vpath, paste0(cfg$version, "obs_source.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
plot(obs_source)
ok = dev.off()
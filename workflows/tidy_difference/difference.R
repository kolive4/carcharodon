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
  library(patchwork)
  library(png)
  library(viridis)
})

args = argparser::arg_parser("a tool to cast monthly predictions into one figure",
                             name = "compiled_report.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_difference/d01.00000.01_12.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
for (f in list.files(cfg$source_path, pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}
vpars = charlier::parse_version(cfg$version)
vpath = file.path(cfg$root_path, cfg$diff_path, "versions", vpars[["major"]], vpars[["minor"]])
if (!dir.exists(vpath)) 
  dir.create(vpath, showWarnings = FALSE, recursive = TRUE)

charlier::start_logger(filename = file.path(vpath, "log"))
charlier::info("writing config")
charlier::write_config(cfg, filename = file.path(vpath, basename(args$config)))

nefsc_cc_bb = cofbb::get_bb("nefsc_carcharodon", "sf")

coast = rnaturalearth::ne_coastline(scale = 'large', returnclass = 'sf') |>
  sf::st_geometry() |>
  sf::st_crop(nefsc_cc_bb)

a_files = list.files(path = file.path(cfg$root_path, cfg$a_path, cfg$a), 
                     pattern = sprintf("%s_prediction.tif", cfg$model_type),
                     recursive = TRUE,
                     full.names = TRUE) 
b_files = list.files(path = file.path(cfg$root_path, cfg$b_path, cfg$b), 
                     pattern = sprintf("%s_prediction.tif", cfg$model_type),
                     recursive = TRUE,
                     full.names = TRUE) 

a_z = basename(dirname(a_files))
a_len = nchar(a_z) 
a_imonth = substring(a_z, a_len -1) |>
  as.numeric()

b_z = basename(dirname(b_files))
b_len = nchar(b_z) 
b_imonth = substring(b_z, b_len -1) |>
  as.numeric()

if (length(b_imonth) < length(a_imonth)) {
  imonth = b_imonth
} else {imonth = a_imonth}

a = stars::read_stars(a_files, along = list(month = month.abb[a_imonth]))
b = stars::read_stars(b_files, along = list(month = month.abb[b_imonth]))

az = st_get_dimension_values(a, "month")
bz = st_get_dimension_values(b, "month")
z = intersect(az, bz)

a = a |>
  dplyr::slice("month", which(az %in% z)) |>
  reset_stars_start("month")

b = b |>
  dplyr::slice("month", which(bz %in% z)) |>
  reset_stars_start("month")

dif = b - a

if (!is.null(cfg$contour_name)) {
  mask_contour = sf::read_sf(file.path(cfg$data_path, cfg$contour_name))
} else {NULL}

dif_plot = ggplot() +
  geom_stars(data = dif, aes(fill = rf_prediction.tif)) +
  facet_wrap(~ month) +
  scale_fill_fermenter(name = expression(paste(Delta, " Habitat Suitability")), 
                       palette = "BrBG",
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, 0.25),
                       type = "div", 
                       direction = -1,
                       na.value = "grey75") +
  geom_sf(data = coast) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_classic()
if (!is.null(cfg$contour_name)) {
  dif_plot = dif_plot +
    geom_sf(data = mask_contour)
}

png(file.path(vpath, paste0(cfg$version, "_difference.png")), 
    bg = "white", width = 11, height = 8.5, units = "in", res = 300)
print(dif_plot)
ok = dev.off()

mon_colors =  c("01" = "#5978a3",
                "02" = "#83b9e2",
                "03" = "#003c00",
                "04" = "#30662d",
                "05" = "#5b9359",
                "06" = "#be9200",
                "07" = "#dbb142",
                "08" = "#ffda7a",
                "09" = "#9a0000",
                "10" = "#a83f38",
                "11" = "#ad7272",
                "12" = "#292f56")

a_pd_files = list.files(path = file.path(cfg$root_path, cfg$tidy_wf_path, cfg$a),
                         pattern = sprintf("%s_pd.rds", cfg$model_type),
                         recursive = TRUE,
                         full.names = TRUE)
a_pd_names = basename(a_pd_files) |>
  substring(11, 12)
if (TRUE %in% file.exists(a_pd_files)) {
  a_pd = lapply(a_pd_files, readr::read_rds) |>
    rlang::set_names(a_pd_names)
  a_pd = pd_cov(a_pd)
}
b_pd_files = list.files(path = file.path(cfg$root_path, cfg$tidy_wf_path, cfg$b),
                        pattern = sprintf("%s_pd.rds", cfg$model_type),
                        recursive = TRUE,
                        full.names = TRUE)
b_pd_names = basename(b_pd_files) |>
  substring(11, 12)
if (TRUE %in% file.exists(b_pd_files)) {
  b_pd = lapply(b_pd_files, readr::read_rds) |>
    rlang::set_names(b_pd_names)
  b_pd = pd_cov(b_pd)
}

z = difference_pd(a_pd, b_pd, normalize = TRUE)

covar_labels = c("brick_mld" = "Mixed Layer Depth (m)",
                    "brick_sbtm" = "Bottom Salinity (ppm)",
                    "brick_sss" = "Sea Surface Salinity (ppm)",
                    "brick_tbtm" = "Bottom Temperature (°C)",
                    "gseal" = "Gray Seal Habitat Suitability",
                    "hseal" = "Harbor Seal Habitat Suitability",
                    "log_depth" = "log(Depth (m))")

dif_pd_plot = ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(data = z, aes(x = x,
                              y = y,
                              color = month)) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = mon_colors) +
  theme_classic() +
  labs(x = NULL,
       y = "Partial Dependence",
       color = "Month")  +
  facet_wrap(~ covar, scales = "free_x", labeller = labeller(covar = covar_labels))
dif_pd_plot
ggsave(filename = sprintf("%s_rf_pd_dif.png", cfg$version),
       plot = dif_pd_plot,
       path = vpath,
       width = 11, height = 8.5, units = "in", dpi = 300)


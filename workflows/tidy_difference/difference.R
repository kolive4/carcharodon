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
  library(stringr)
  library(rlang)
})

args = argparser::arg_parser("a tool to cast monthly predictions into one figure",
                             name = "compiled_report.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_difference/d01.19000.01_12.yaml",
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
                     pattern = "_prediction.tif",
                     #pattern = sprintf("%s_prediction.tif", cfg$model_type),
                     recursive = TRUE,
                     full.names = TRUE) 
b_files = list.files(path = file.path(cfg$root_path, cfg$b_path, cfg$b), 
                     pattern = "_prediction.tif",
                     #pattern = sprintf("%s_prediction.tif", cfg$model_type),
                     recursive = TRUE,
                     full.names = TRUE) 

parse_filenames = function(files) {
  #tibble with columns for filename, month, model type, model number, version number
  model_lut = c("bt", "gam", "glm", "maxent", "rf")
  lapply(files, function(file){
    model_name = basename(file) |>
      strsplit("_", fixed = TRUE) |>
      getElement(1) |>
      getElement(1)
    model_num = which(model_lut == model_name)
    month = dirname(file) |>
      basename() |>
      strsplit(".", fixed = TRUE) |>
      getElement(1) |>
      getElement(3) |>
      as.numeric() 
    version = dirname(file) |>
      basename() |>
      strsplit(".", fixed = TRUE) |>
      getElement(1)
    version = paste(version[1:2], collapse = ".") 
    dplyr::tibble(model_name = model_name, model_num = model_num, month = month, version = version, filename = file)
  }) |>
    dplyr::bind_rows()
}

a_tib = parse_filenames(a_files) |>
  dplyr::mutate(group = "a", .before = 1)
b_tib = parse_filenames(b_files) |>
  dplyr::mutate(group = "b", .before = 1)
tib = bind_rows(a_tib, b_tib)

r = dplyr::group_by(tib, model_name) |>
  dplyr::group_map(function(tbl, key){
    a = tbl |>
      dplyr::filter(group == "a") |>
      dplyr::pull(filename) |>
      stars::read_stars(along = list(month = month.abb)) |>
      rlang::set_names("a")
    
    b = tbl |>
      dplyr::filter(group == "b") |>
      dplyr::pull(filename) |>
      stars::read_stars(along = list(month = month.abb)) |>
      rlang::set_names("b")
    d = b - a
    names(d) = "dif"
    z = tibble(model_name = tbl$model_name[1], a_version = tbl$version[1], b_version = tbl$version[length(tbl$version)], a = list(a), b = list(b), dif = list(d)) 
    return(z)
  }, .keep = TRUE) |>
  dplyr::bind_rows() |>
  readr::write_rds(file = file.path(vpath, paste0(cfg$version, "_abdif.rds")))


dif_figs = dplyr::rowwise(r) |>
  dplyr::group_map(function(row, key){
    dif_plot = ggplot() +
      geom_stars(data = row$dif[[1]]) +
      facet_grid(cols = vars(month)) +
      scale_fill_steps2(name = expression(paste(Delta, " Habitat Suitability")), 
                        low = "#01665E",
                        midpoint = 0,
                        mid = "white",
                        high = "#8C510A",
                        limits = c(-1, 1),
                        labels = scales::label_number(accuracy = 0.01),
                        breaks = seq(-1, 1, length.out = 10),
                        na.value = "grey75") +
      geom_sf(data = coast) +
      labs(x = "",
           y = "") +
      theme_void() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_blank())
    
    a_plot = ggplot() +
      geom_stars(data = row$a[[1]]) +
      facet_grid(cols = vars(month)) +
      scale_fill_fermenter(name = expression("Habitat Suitability"), 
                           palette = "BuGn",
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.15),
                           type = "seq", 
                           direction = 1,
                           na.value = "grey75") +
      geom_sf(data = coast) +
      labs(x = "",
           y = row$a_version[[1]]) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            panel.grid = element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_blank())
    
    b_plot = ggplot() +
      geom_stars(data = row$b[[1]]) +
      facet_grid(cols = vars(month), switch = "y") +
      scale_fill_fermenter(name = expression("Habitat Suitability"), 
                           palette = "YlOrBr",
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.15),
                           type = "seq", 
                           direction = 1,
                           na.value = "grey75") +
      geom_sf(data = coast) +
      labs(x = "",
           y = row$b_version[[1]]) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            strip.background = element_rect(colour = "black", fill = "white"))
    
    sxs_plot = b_plot / a_plot / dif_plot 
    #+ plot_layout(guides = "collect") & theme(legend.position = "bottom")
    # return(sxs_plot)
    ggsave(filename = paste0(row$model_name, "_abdif.png"), plot = sxs_plot, 
           path = file.path(vpath, "figures"), create.dir = TRUE,
           width = 18, height = 5, units = "in", dpi = 300, bg = "white")
    
  }, .keep = TRUE)










# a_z = basename(dirname(a_files))
# a_len = nchar(a_z) 
# a_imonth = substring(a_z, a_len -1) |>
#   as.numeric()
# a_model = str_extract(basename(a_files), "^[^_]+")
# 
# b_z = basename(dirname(b_files))
# b_len = nchar(b_z) 
# b_imonth = substring(b_z, b_len -1) |>
#   as.numeric()
# b_model = str_extract(basename(b_files), "^[^_]+")
# 
# if (length(b_imonth) < length(a_imonth)) {
#   imonth = b_imonth
# } else {imonth = a_imonth}
# 
# a = lapply(a_files, stars::read_stars)
# 
# a = stars::read_stars(a_files, along = list(model = unique(a_model),
#                                             month = month.abb[unique(a_imonth)]))
# b = stars::read_stars(b_files, along = list(model = unique(b_model),
#                                             month = month.abb[unique(b_imonth)]))
# 
# az_month = st_get_dimension_values(a, "month")
# bz_month = st_get_dimension_values(b, "month")
# z_month = intersect(az_month, bz_month)
# 
# az_model = st_get_dimension_values(a, "model")
# bz_model = st_get_dimension_values(b, "model")
# z_model = intersect(az_model, bz_model)
# 
# a = a |>
#   dplyr::slice("month", which(az_month %in% z_month)) |>
#   dplyr::slice("model", which(az_model %in% z_model)) |>
#   reset_stars_start("month")
# 
# b = b |>
#   dplyr::slice("month", which(bz_month %in% z_month)) |>
#   dplyr::slice("model", which(bz_model %in% z_model)) |>
#   reset_stars_start("month")
# 
# dif = b - a
# 
# 
# if(FALSE){
#   n = 27
#   model_lut = str_extract(basename(a_files), "^[^_]+") |>
#     unique() 
#   modelname = basename(a_files[n]) |>
#     strsplit("_", fixed = TRUE) |>
#     getElement(1) |>
#     getElement(1)
#   model_num = which(model_lut == modelname)
#   month = dirname(a_files[n]) |>
#     basename() |>
#     strsplit(".", fixed = TRUE) |>
#     getElement(1) |>
#     getElement(3) |>
#     as.numeric()
#   
#   a1 = stars::read_stars(a_files[n])
#   b1 = stars::read_stars(b_files[n])
#   
#   dif1 = b1 - a1
#   
#   d1 = dif |>
#     slice("model", model_num) |>
#     slice("month", month)
#   
#   dd = d1 - dif1
#   }
# 
# 
# if (!is.null(cfg$contour_name)) {
#   mask_contour = sf::read_sf(file.path(cfg$data_path, cfg$contour_name))
# } else {NULL}
# 
# models = st_get_dimension_values(dif, "model")
# 
# for (m in models) {
#   if(FALSE){
#     m = "rf"
#   }
#   a_plot = ggplot() +
#     geom_stars(data = a |>
#                  dplyr::slice("model", m) #, 
#                #aes(fill = paste0(m, "_prediction.tif"))
#     ) +
#     facet_wrap(~ month) +
#     scale_fill_viridis_c(limits = c(0,1),
#                          breaks = seq(0,1,0.1)) +
#     # scale_fill_fermenter(name = expression(paste(Delta, " Habitat Suitability")), 
#     #                      palette = "BrBG",
#     #                      limits = c(-1, 1),
#     #                      breaks = seq(-1, 1, 0.25),
#     #                      type = "div", 
#     #                      direction = -1,
#     #                      na.value = "grey75") +
#     geom_sf(data = coast) +
#     labs(x = "Longitude",
#          y = "Latitude") +
#     theme_classic()
#   
#   b_plot = ggplot() +
#     geom_stars(data = b |>
#                  dplyr::slice("model", m) #, 
#                #aes(fill = paste0(m, "_prediction.tif"))
#     ) +
#     facet_wrap(~ month) +
#     scale_fill_viridis_c(limits = c(0,1),
#                          breaks = seq(0,1,0.1)) +
#     # scale_fill_fermenter(name = expression(paste(Delta, " Habitat Suitability")), 
#     #                      palette = "BrBG",
#     #                      limits = c(-1, 1),
#     #                      breaks = seq(-1, 1, 0.25),
#     #                      type = "div", 
#     #                      direction = -1,
#     #                      na.value = "grey75") +
#     geom_sf(data = coast) +
#     labs(x = "Longitude",
#          y = "Latitude") +
#     theme_classic()
#   
#   dif_plot = ggplot() +
#     geom_stars(data = dif |>
#                  dplyr::slice("model", m) #, 
#                #aes(fill = paste0(m, "_prediction.tif"))
#                ) +
#     facet_grid(rows = month) +
#     scale_fill_fermenter(name = expression(paste(Delta, " Habitat Suitability")), 
#                          palette = "BrBG",
#                          limits = c(-1, 1),
#                          breaks = seq(-1, 1, 0.25),
#                          type = "div", 
#                          direction = -1,
#                          na.value = "grey75") +
#     geom_sf(data = coast) +
#     labs(x = "Longitude",
#          y = "Latitude") +
#     theme_classic()
#   if (!is.null(cfg$contour_name)) {
#     dif_plot = dif_plot +
#       geom_sf(data = mask_contour)
#   }
#   png(file.path(vpath, paste0(cfg$version, "_", m, "_difference.png")), 
#       bg = "white", width = 11, height = 8.5, units = "in", res = 300)
#   print(dif_plot)
#   ok = dev.off()
# }
# 
# 
# mon_colors =  c("01" = "#5978a3",
#                 "02" = "#83b9e2",
#                 "03" = "#003c00",
#                 "04" = "#30662d",
#                 "05" = "#5b9359",
#                 "06" = "#be9200",
#                 "07" = "#dbb142",
#                 "08" = "#ffda7a",
#                 "09" = "#9a0000",
#                 "10" = "#a83f38",
#                 "11" = "#ad7272",
#                 "12" = "#292f56")
# 
# a_pd_files = list.files(path = file.path(cfg$root_path, cfg$tidy_wf_path, cfg$a),
#                          pattern = sprintf("%s_pd.rds", cfg$model_type),
#                          recursive = TRUE,
#                          full.names = TRUE)
# a_pd_names = basename(a_pd_files) |>
#   substring(11, 12)
# if (TRUE %in% file.exists(a_pd_files)) {
#   a_pd = lapply(a_pd_files, readr::read_rds) |>
#     rlang::set_names(a_pd_names)
#   a_pd = pd_cov(a_pd)
# }
# b_pd_files = list.files(path = file.path(cfg$root_path, cfg$tidy_wf_path, cfg$b),
#                         pattern = sprintf("%s_pd.rds", cfg$model_type),
#                         recursive = TRUE,
#                         full.names = TRUE)
# b_pd_names = basename(b_pd_files) |>
#   substring(11, 12)
# if (TRUE %in% file.exists(b_pd_files)) {
#   b_pd = lapply(b_pd_files, readr::read_rds) |>
#     rlang::set_names(b_pd_names)
#   b_pd = pd_cov(b_pd)
# }
# 
# z = difference_pd(a_pd, b_pd, normalize = TRUE)
# 
# covar_labels = c("brick_mld" = "Mixed Layer Depth (m)",
#                     "brick_sbtm" = "Bottom Salinity (ppm)",
#                     "brick_sss" = "Sea Surface Salinity (ppm)",
#                     "brick_tbtm" = "Bottom Temperature (°C)",
#                     "gseal" = "Gray Seal Habitat Suitability",
#                     "hseal" = "Harbor Seal Habitat Suitability",
#                     "log_depth" = "log(Depth (m))")
# 
# dif_pd_plot = ggplot() +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
#   geom_line(data = z, aes(x = x,
#                               y = y,
#                               color = month)) +
#   scale_y_continuous(limits = c(-1, 1)) +
#   scale_color_manual(values = mon_colors) +
#   theme_classic() +
#   labs(x = NULL,
#        y = "Partial Dependence",
#        color = "Month")  +
#   facet_wrap(~ covar, scales = "free_x", labeller = labeller(covar = covar_labels))
# dif_pd_plot
# ggsave(filename = sprintf("%s_rf_pd_dif.png", cfg$version),
#        plot = dif_pd_plot,
#        path = vpath,
#        width = 11, height = 8.5, units = "in", dpi = 300)
# 

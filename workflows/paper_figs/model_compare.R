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

args = argparser::arg_parser("a tool to compile and compare one month's habitat suitability across model types",
                             name = "model_compare.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/paper_figs/p21.100960.08.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
for (f in list.files(cfg$source_path, pattern = "^.*\\.R$", full.names = TRUE)){
  source(f)
}
vpars = charlier::parse_version(cfg$version)
vpath = file.path(cfg$root_path, cfg$paper_path, "versions", vpars[["major"]], vpars[["minor"]], vpars[["release"]])
if (!dir.exists(vpath)) 
  dir.create(vpath, showWarnings = FALSE, recursive = TRUE)

charlier::start_logger(filename = file.path(vpath, "log"))
charlier::info("writing config")
charlier::write_config(cfg, filename = file.path(vpath, basename(args$config)))

nefsc_cc_bb = cofbb::get_bb("nefsc_carcharodon", "sf")

coast = rnaturalearth::ne_coastline(scale = 'large', returnclass = 'sf') |>
  sf::st_geometry() |>
  sf::st_crop(nefsc_cc_bb)

files = list.files(path = file.path(cfg$root_path, 
                                    cfg$cast_path, 
                                    cfg$cast_vers_maj, 
                                    cfg$cast_vers_min,
                                    paste0(cfg$cast_vers_maj, "." ,cfg$cast_vers_min, ".", cfg$month)), 
                   pattern = "_prediction.tif",
                   full.names = TRUE) 
b = basename(files)
model_type.abb = str_split_i(b, "_", i = 1)
full_model = c("Boosted Trees", "GAM", "GLM", "MaxNet", "Random Forest")
lut = tibble(b, model_type.abb, full_model)

z = stars::read_stars(files, along = list(model_type = full_model)) |>
  dplyr::rename("Habitat Suitability Index" = "bt_prediction.tif")

labels = c("0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1")

model_comp = ggplot() +
  geom_stars(data = z) +
  coord_map() +
  scale_fill_viridis_b(option = "mako", 
                    name = "Habitat Suitability\nIndex", 
                    limits = c(0, 1), 
                    labels = labels,
                    n.breaks = 11,
                    na.value = "gray40") +
  geom_sf(data = coast, linewidth = 0.1, color = "gray70") +
  facet_wrap(~ model_type) +
  labs(x = "",
       y = "") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"))
ggsave(filename = paste0(cfg$version, "_model_comp.png"), plot = model_comp, 
       path = file.path(vpath, "figures"), create.dir = TRUE,
       width = 11, height = 8.5, units = "in", dpi = 300, bg = "white")

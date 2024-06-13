# usage: brickman-dump.R [--] [--help] [--config CONFIG]
# 
# prepare brickman data by reading, warping, cropping and saving
# 
# flags:
#   -h, --help    show this help message and exit
# 
# optional arguments:
#   -c, --config  the name of the configuration file 
#     [default: /mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/brickman-dump/brickman-dump.yaml]

# for each year
#   for each scenario
#     read the brickman
#     transform and subset each variable
#     save to requested path

suppressPackageStartupMessages({
  library(charlier)
  library(cofbb)
  library(argparser)
  library(brickman)
  library(stars)
})

args = argparser::arg_parser("prepare brickman data by reading, warping, cropping and saving",
                             name = "brickman-dump.R",
                             hide.opts = TRUE) |>
  argparser::add_argument(arg = "--config",
                          type = "character",
                          default = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/workflows/brickman-dump/brickman-dump.yaml",
                          help = "the name of the configuration file") |>
  argparser::parse_args()

cfg = charlier::read_config(args$config)
path = file.path(cfg$root_path, cfg$outpath)
if (!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
bb = cofbb::get_bb(cfg$bbox_name, form = "sf")

for (year in cfg$year){
  if (year == "PRESENT"){
    scenario = "PRESENT"
    cat("reading:", year, scenario, "\n")
    brickman = brickman::read_brickman(scenario = scenario, 
                                       year = year, 
                                       vars = cfg$vars, 
                                       interval = cfg$interval)
    cat("warping\n")
    warp = brickman::warp_brickman(brickman, crs = sf::st_crs(bb))
    cat("cropping\n")
    sub = warp[bb]
    for (i in seq_along(cfg$vars)){
      v = cfg$vars[i]
      filename = file.path(path, sprintf("%s_%s_%s_%s.tif", scenario[1], as.character(year[1]), v, cfg$interval))
      cat("writing:", basename(filename), "\n")
      sub[i,,, drop = FALSE] |>
        stars::write_stars(filename)
    }
  } else {
    for (scenario in cfg$scenario){
      cat("reading:", year, scenario, "\n")
      brickman = brickman::read_brickman(scenario = scenario[1], 
                                         year = year[1], 
                                         vars = cfg$vars, 
                                         interval = cfg$interval)
      cat("warping\n")
      warp = brickman::warp_brickman(brickman, crs = sf::st_crs(bb))
      cat("cropping\n")
      sub = warp[bb]
      
      for (i in seq_along(cfg$vars)){
        v = cfg$vars[i]
        filename = file.path(path, sprintf("%s_%s_%s_%s.tif", scenario[1], as.character(year[1]), v, cfg$interval))
        cat("writing:", basename(filename), "\n")
        sub[i,,, drop = FALSE] |>
          stars::write_stars(filename)
      }
    }
  }
}
cat("done!\n")
if (!interactive()){
  if (!is.null(cfg$email)) {
    charlier::sendmail(to = cfg$email, subject = "brickman-dump", message = "Good news! Brickman dump is done!")
  }
  quit(save = "no", status = 0)
}
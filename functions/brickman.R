#' Function to open, warp, crop, and save brickman data
#' 
#' @param scenario chr, one of RCP85, RCP45, or PRESENT
#' @param year num, one of 2055 or 2075, ignored if scenario is PRESENT
#' @param vars chr, variable names (SST or SSS)
#' @param interval chr, either "ann" or "mon"
#' @param bb sf, bounding box from cofbb or otherwise defined
#' @param path filepath, path to where you want data saved
#' @return subsetted stars image 
subset_brickman = function(scenario = c("RCP85", "RCP45", "PRESENT")[1],
                           year = c(2055, 2075, NA)[1],
                           vars = c("SST", "SSS")[1], 
                           interval = c("mon", "ann")[1], 
                           bb = cofbb::get_bb("gom_carcharodon", form = "sf"), 
                           path = here::here("data/brickman/gom_carcharodon")){
  brickman = brickman::read_brickman(scenario = scenario[1], year = year[1], vars = vars[1], interval = interval[1])
  warp = brickman::warp_brickman(brickman, crs = sf::st_crs(bb))
  sub = warp[bb]
  
  if(scenario == "PRESENT") year = "PRESENT"
  
  if(!dir.exists(path)) ok = dir.create(path, recursive = TRUE)
  filename = file.path(path, sprintf("%s_%s_%s_%s.tif", scenario[1], as.character(year[1]), vars[1], interval[1]))
  stars::write_stars(sub, filename)
}

#' Function to read in Brickman data that has been cached
#' 
#' @param scenario chr, one of RCP85, RCP45, or PRESENT
#' @param year num, one of 2055 or 2075, ignored if scenario is PRESENT
#' @param vars chr, variable names (SST or SSS)
#' @param interval chr, either "ann" or "mon"
#' @param path filepath, path to where data is saved
#' @param anomaly logical, anomaly or true value
#' @return subsetted stars image 
load_brickman = function(scenario = c("RCP85", "RCP45", "PRESENT")[1],
                         year = c(2055, 2075, NA)[1],
                         vars = c("SST", "SSS")[1], 
                         interval = c("mon", "ann")[1], 
                         path = here::here("data/brickman/gom_carcharodon"),
                         anomaly = FALSE){
  if(FALSE){
    scenario = c("RCP85", "RCP45", "PRESENT")[1]
    year = c(2055, 2075, NA)[1]
    vars = c("SST", "SSS")[1] 
    interval = c("mon", "ann")[1]
    path = here::here("data/brickman/gom_carcharodon")
    anomaly = FALSE
  }
  
  if(scenario == "PRESENT") year = "PRESENT"
  
  filename = file.path(path, sprintf("%s_%s_%s_%s.tif", scenario[1], as.character(year[1]), vars[1], interval[1]))
  x = stars::read_stars(filename)

  if(anomaly == FALSE && scenario != "PRESENT"){
    present = load_brickman(scenario = "PRESENT", vars = vars[1], interval = interval[1], path = path)
    x = x + present
  }
  return(x)
}

#' Function to get summary brickman stats
#' 
#' @param x stars, stars object of interest
#' @return tbl, summary of stats
summary_brickman = function(x){
  m = stars::st_apply(x, "band", mean, na.rm = TRUE)
  quant = stars::st_apply(x, "band", quantile, probs = seq(from = 0.05, to = 0.95, by = 0.05), na.rm = TRUE)
  quant = quant[[1]] |> 
    t()
  colnames(quant) = paste0("q", sprintf("%0.2i", seq(from = 5, to = 95, by = 5)))
  quant = dplyr::as_tibble(quant)
  minmax = stars::st_apply(x, "band", range, na.rm = TRUE)
  minmax = minmax[[1]] |>
    t()
  colnames(minmax) = c("min", "max")
  minmax = dplyr::as_tibble(minmax)
  n = stars::st_apply(x, "band", 
                      function(v){
                        ix = is.na(v)
                        sum(!ix)
                      })
  
  dplyr::tibble(band = stars::st_get_dimension_values(x, "band"), mean = m[[1]], n = n[[1]]) |>
    dplyr::bind_cols(minmax) |>
    dplyr::bind_cols(quant)
}


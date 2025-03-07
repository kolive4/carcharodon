#' Function to open, warp, crop, and save brickman data
#' 
#' @param scenario chr, one of RCP85, RCP45, or PRESENT
#' @param year num, one of 2055 or 2075, ignored if scenario is PRESENT
#' @param vars chr, variable names (SST or SSS)
#' @param interval chr, either "ann" or "mon"
#' @param bb sf, bounding box from cofbb or otherwise defined
#' @param band_as_time logical, convert band to appropriate time/date stamp
#' @param path filepath, path to where you want data saved
#' @return subsetted stars image 
subset_brickman = function(scenario = c("RCP85", "RCP45", "PRESENT")[1],
                           year = c(2055, 2075, NA)[1],
                           vars = c("SST", "SSS")[1], 
                           interval = c("mon", "ann")[1], 
                           bb = cofbb::get_bb("gom_carcharodon", form = "sf"),
                           band_as_time = FALSE,
                           path = here::here("data/brickman/gom_carcharodon")){
  if(FALSE){
    scenario = "PRESENT"
    year = "PRESENT"
    vars = c("SST", "Tbtm")
    interval = "mon"
    bb = cofbb::get_bb("gom_carcharodon", form = "sf")
    band_as_time = TRUE
    path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon"
  }
  
  brickman = brickman::read_brickman(scenario = scenario[1], year = year[1], vars = vars, interval = interval[1])
  warp = brickman::warp_brickman(brickman, crs = sf::st_crs(bb))
  sub = warp[bb]
  
  if(scenario == "PRESENT") year = "PRESENT"
  
  if(!dir.exists(path)) ok = dir.create(path, recursive = TRUE)

  for (i in seq_along(vars)){
    v = vars[i]
    filename = file.path(path, sprintf("%s_%s_%s_%s.tif", scenario[1], as.character(year[1]), v, interval[1]))
    cat(filename, "\n")
    sub[i,,, drop = FALSE] |>
      stars::write_stars(filename)
  }
  
  if(band_as_time == TRUE) {
    time = switch(scenario,
                  "PRESENT" = seq(from = as.Date("2020-01-01"), length = 12, by = "month"),
                  "2075" = seq(from = as.Date("2075-01-01"), length = 12, by = "month") ,
                  "2055" = seq(from = as.Date("2055-01-01"), length = 12, by = "month") )
    dims = stars::st_dimensions(sub)
    sub = stars::st_set_dimensions(sub, names(dims)[3], names = "time", values = time)
  }
  
  return(sub)

}

#' Function to read in Brickman data that has been cached
#' 
#' @param scenario chr, one of RCP85, RCP45, or PRESENT
#' @param year num, one of 2055 or 2075, ignored if scenario is PRESENT
#' @param vars chr, variable names (SST or SSS)
#' @param interval chr, either "ann" or "mon"
#' @param path filepath, path to where data is saved
#' @param band_as_time logical, convert band to appropriate time/date stamp
#' @param verbose logical
#' @param anomaly logical, anomaly or true value
#' @return subsetted stars image 
load_brickman = function(scenario = c("RCP85", "RCP45", "PRESENT")[1],
                         year = c(2055, 2075, NA)[1],
                         vars = c("SST", "SSS")[1], 
                         interval = c("mon", "ann")[1], 
                         path = here::here("data/brickman/gom_carcharodon"),
                         band_as_time = FALSE,
                         verbose = FALSE,
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
  
  x = lapply(seq_along(vars), 
             function(i){
                v = vars[i]
                filename = file.path(path, 
                                     sprintf("%s_%s_%s_%s.tif", 
                                             scenario[1], 
                                             as.character(year[1]), 
                                             v, 
                                             interval[1]))
                if(verbose) cat(filename, "\n")
                x = stars::read_stars(filename)
                if(anomaly == FALSE && scenario != "PRESENT"){
                  present = load_brickman(scenario = "PRESENT", 
                                          vars = vars[1], 
                                          interval = interval[1], 
                                          path = path)
                  x = x + present
                }
                if(v == "Bathy_depth") {
                  names(x) = "Bathy_depth"
                }
                return(x)
             }
  )
  x = twinkle::bind_attrs(x)
  
  if(band_as_time == TRUE && !("Bathy_depth" %in% vars)) {
    time = switch(scenario,
                  "PRESENT" = seq(from = as.Date("2020-01-01"), length = 12, by = "month"),
                  "2075" = seq(from = as.Date("2075-01-01"), length = 12, by = "month") ,
                  "2055" = seq(from = as.Date("2055-01-01"), length = 12, by = "month") )
    dims = stars::st_dimensions(x)
    x = stars::st_set_dimensions(x, "band", names = "time", values = time)
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

#' Function to get depth restricted bathymetry
#' 
#' @param x stars, stars object for depth
#' @param covar str, name of covariate of interest
#' @param depth num, depth restriction
#' @param value NA, value beyond depth restriction
#' @return depth restricted stars object
buffer_depth = function(x, covar, depth, value = NA) {
  if(FALSE){
    x = brickman_bathymetry
    covar = cfg$static_covariates
    depth = cfg$depth_threshold
    value = NA
  }
  m = x[[covar]] |>
    as.matrix()
  ix = m > depth
  m[ix] = value
  x[[covar]] = m

  return(x)
}


#' Extract brickman points
#' 
#' @param x stars, stars object of variable data
#' @param y sf, points for extraction
#' @param ... extra arguments for st_extract, such as: time_column
#' @return tibble of data
brickman_extract = function(x, y, ...){
  if (FALSE) {
    x = hseal_layer
    y = wshark
    time_column = "month"
  }

  #' Function to extract data at points for specific covariates
  #' 
  #' @param obj stars object to pass in
  #' @param i chr, covariate name in the list (SST, Tbtm, etc.)
  #' @param points stars, points stars object at which to extract points
  #' @return stars object 
  covariate_extract = function(obj, i, points, ...) {
    if(FALSE){
      obj = hseal_layer
      i = "hseal"
      points = wshark
    }
    r = stars::st_extract(obj[i,,,], at = points, ...) |>
        sf::st_as_sf() |>
        sf::st_drop_geometry() |>
        dplyr::as_tibble()
    if(length(dim(obj)) < 3){
      r = rlang::set_names(r, as.name(i))
    } else if ((dim(obj)[3] == 12) && (ncol(r) == 12)) {
      r = rlang::set_names(r, paste(as.name(i), month.abb, sep = "_"))
    }
    return(r)
  }
  
  lapply(names(x), function(vname, obj = NULL, points = NULL){
    covariate_extract(obj, vname, points, ...) |>
      dplyr::select(1)
  }, obj = x, points = y) |>
    dplyr::bind_cols()
}


#' Function to read and simplify names for brickman covariates
#' 
#' @param filename brickman_covar_obs_bg.gpkg, observations and background points with brickman data
#' @param simplify logical, whether or not to simplify brickman covariate names
#' @param bb bounding box
#' @return dataframe with simplified brickman covariate names

read_brickman_points = function(filename = file.path(cfg$root_path, cfg$gather_data_vpath, "brickman_covar_obs_bg.gpkg"), 
                                simplify = TRUE,
                                bb = NULL) {
  
  x = sf::read_sf(filename)
  
  if(!is.null(bb)) x = sf::st_crop(x, bb)
  
  if(simplify) x = x |>
    dplyr::rename(brick_sst = PRESENT_PRESENT_SST_mon.tif) |>
    dplyr::rename(brick_tbtm = PRESENT_PRESENT_Tbtm_mon.tif) |>
    dplyr::rename(brick_mld = PRESENT_PRESENT_MLD_mon.tif) |>
    dplyr::rename(brick_sss = PRESENT_PRESENT_SSS_mon.tif) |>
    dplyr::rename(brick_sbtm = PRESENT_PRESENT_Sbtm_mon.tif) |>
    dplyr::rename(brick_u = PRESENT_PRESENT_U_mon.tif) |>
    dplyr::rename(brick_v = PRESENT_PRESENT_V_mon.tif) |>
    dplyr::rename(brick_xbtm = PRESENT_PRESENT_Xbtm_mon.tif) 
  
  return(x)
}






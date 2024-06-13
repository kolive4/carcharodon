#' Retrieve a data path
#' 
#' @export
#' @param ... character, one or more file path segments to be post-pended to \code{root}
#' @param root character, the root data directory path
#' @return character path specification
get_path <- function(..., root = here::here("data")){
  file.path(root, ...)
}


#' check if covariate file exists
#' 
#' @export
#' @param filename name of covariate file
#' @param path str, path to where file is stored
#' @return true/false
covars_exists <- function(filename,
                          path = here::here("data", "covars")){
  file = file.path(path, filename)
  file.exists(file)
} 


#' Retrieve a listing of local files
#' 
#' @export
#' @param path the path to the datasets
#' @param full.names logical return basename or full path specs?
#' @param strip character or NA, if character then strip this pattern. Ignored
#'   if full.names is \code{TRUE}
#' @return character names (possibly filenames)
list_covars <- function(path = get_path("covars"), 
                        full.names = FALSE,
                        strip = c(NA, ".tif")[2]){
  ff <- list.files(path, pattern = "^.*\\.tif", full.names = full.names)
  if (!full.names && !is.na(strip[1])){
    ff <- sub(strip, "", ff, fixed = TRUE)
  }
  ff
}


#' read stars object from cache
#' 
#' @export
#' @param filename chr, name of file
#' @param path str, path to where file is stored
#' @param ... other arguments passed to read_stars
#' @return same stars object pipe-able
read_covar <- function(filename, 
                       path = here::here("data", "covars"),
                       ...){
  stars::read_stars(file.path(path, filename), ...)
}


#' write stars object to a cache
#' 
#' @export
#' @param x stars object, 
#' @param filename chr, name of file
#' @param path str, path to where file is stored
#' @param ... other arguments passed to write_stars
#' @return same stars object pipe-able
write_covar <- function(x, 
                        filename, 
                        path = here::here("data", "covars"),
                        ...){
  stars::write_stars(x, dsn = file.path(path, filename), ...)
}


#' seasonality selection function
#' 
#' @export
#' @param x tibble of occurrence data
#' @param season chr, season you wish to select, summer, winter, spring, autumn
#' @return occurrence data for that season
seasonality <- function(x,
                        season = c('summer', 'spring', 'autumn', 'winter')[1]){
  
  # x = dplyr::mutate(x, month = lubridate::mdy(x$eventDate) |>
  #                 lubridate::month())
  
  x = dplyr::mutate(x, month = format(x$eventDate, "%m") |>
                      as.numeric())
  
  r = switch(tolower(season[1]),
             "summer" = dplyr::filter(x, .data$month %in% c(6, 7, 8)),
             "spring" = dplyr::filter(x, .data$month %in% c(3, 4, 5)),
             "autumn" = dplyr::filter(x, .data$month %in% c(9, 10, 11)),
             "winter" = dplyr::filter(x, .data$month %in% c(12, 1, 2))
  ) 
  
  return(r)
} 


# functions for raster information

#' function to filter values not within a threshold of sst(s)
#' 
#' @export
#' @param x list, list of sst values to apply to a raster
#' @param threshlow degrees celsius, lower sst threshold value
#' @param threshhigh degrees celsius, higher sst threshold value
#' @return a list of sst values within the threshold to be applied as a raster
thresh_filter <- function(x, threshlow = to_deg_C(5), threshhigh = NULL){
  if(is.null(threshhigh)){
    x = x |>
      dplyr::mutate(thresh = .data$sst >= threshlow)
  }
  else{
    x = x |>
      dplyr::mutate(thresh = dplyr::between(.data$sst, threshlow, threshhigh))
  }
  return(x)
}


#' function to plot the sst within the threshold values; takes list and filters values that do not fit in the thresh before plotting them
#' 
#' @export
#' @param x, list of sst values 
#' @param main str, main title for the plot
#' @return a map of sst values that fit within the threshold
plot_thresh <- function(x, main){
  x$sst[!x$thresh] <- NA
  plot(x["sst"], col = rev(RColorBrewer::brewer.pal(8, "YlOrRd")), main = main, reset = F)
  plot(sf::st_geometry(nwa_coast), border = "black", lwd = 4, add = T)
}

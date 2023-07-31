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


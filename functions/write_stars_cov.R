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
                        path = get_path("covars"),
                        ...){
  stars::write_stars(x, dsn = file.path(path, filename), ...)
}

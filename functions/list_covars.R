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
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

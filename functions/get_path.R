#' Retrieve a data path
#' 
#' @export
#' @param ... character, one or more file path segments to be post-pended to \code{root}
#' @param root character, the root data directory path
#' @return character path specification
get_path <- function(..., root = here::here("data")){
  file.path(root, ...)
}
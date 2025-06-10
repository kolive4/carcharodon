#' Verifies config version
#' 
#' @param files filenames for one or more configuration files
#' @param write FALSE, write the new file
#' @return returns a list of configurations
#' @examples
#' # example code
#' files = list.files(path = "workflows/modeling_workflow", pattern = "^.*\\.yaml$", full.names = TRUE)
#' cfg = verify_version(files, write = TRUE)
#' cfg
verify_version = function(files, write = FALSE) {
  cfgs = lapply(files, function(file){
    cfg = charlier::read_config(file)
    v = gsub(".yaml", "", basename(file), fixed = TRUE)
    if (cfg$version != v) {
      cat("correcting", basename(file), "\n")
      cfg$version = v
      if (write == TRUE) {
        charlier::write_config(cfg, file)
      }
    }
    return(cfg)
  })
  names(cfgs) = basename(files)
  return(cfgs)
}


#' Append config functions
#' 
#' @param files config(s) filename
#' @param extra list of items to append to a config
#' @param write option to rewrite the config
#' @return config file with added items
#' @examples
#' # example code 
#' files = list.files(path = "workflows/get_data_workflow", pattern = glob2rx("*.yaml"), full.names = TRUE)
#' extra = list(
#' fall_fish_mon = c(12, 1, 2, 3, 4, 5),
#' spring_fish_mon = c(6, 7, 8, 9, 10, 11))
#' cfgs = append_config(files, extra, write = TRUE)
#'  
append_config = function(files, extra, write = FALSE) {
  cfgs = lapply(files, function(file){
    cfg = charlier::read_config(file)
    cfg = c(cfg, extra)
    if (write == TRUE) {
      charlier::write_config(cfg, file)
    }
    return(cfg)
  })
  names(cfgs) = basename(files)
  return(cfgs)
}

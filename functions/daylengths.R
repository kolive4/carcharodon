#' Function to create a daylength stars object
#' 
#' @export
#' @param destination chr, destination path
#' @param template template stars object on which to build
#' @param day num, which day of the month to gather data
#' @return stars object with 12 months/maps
create_daylength_maps = function(destination = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/daylength",
                                 template = load_brickman(scenario = 'PRESENT', 
                                                          vars = "Bathy_depth", 
                                                          band_as_time = TRUE, 
                                                          path = file.path("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/bathy")),
                                 day = "15"){
  if(FALSE){
    destination = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/daylength"
    template = stars::read_stars("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/bathy/brick_masked.tif")
    day = "15"
  }
  
  if (!dir.exists(destination)) {
    dir.create(destination, recursive = TRUE)
  }
  
  template = rlang::set_names(template, "daylength")
  
  lats = stars::st_get_dimension_values(template, "y")
  
  start_date = sprintf("2020-01-%s", day) |>
    as.Date()
  end_date = sprintf("2020-12-%s", day) |>
    as.Date()
  dates = seq(from = start_date, to = end_date, by = "month")
  
  m = template[[1]] 
  dm = dim(m)
  index = seq_len(prod(dm))
  
  d = lapply(seq_along(dates), function(i){
    dl = geosphere::daylength(lat = lats, doy = dates[i])
    tmp = lapply(seq_along(dl), function(j){
      m[,j] = rep(dl[j], dm[1])
    })
    tmp = do.call(rbind, tmp) |>
      t()
    
    template$daylength[index] = as.vector(tmp)
    
    template
  }) |>
    twinkle::bind_bands() |>
    stars::st_set_dimensions("band", names = "time", values = dates) |>
    stars::write_stars(file.path(destination, paste0("daylength_", day, ".tif")))
  
}


#' Function to read daylength stars object
#' 
#' @export
#' @param path chr, filepath to stars object
#' @param day num, which day of the month to gather data
#' @param band_as_time logical, convert band to appropriate time/date stamp 
#' @return stars object of daylengths
read_daylength = function(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/daylength",
                          day = 15,
                          band_as_time = TRUE) {
  
  filename = paste0("daylength_", day, ".tif")
  
  x = stars::read_stars(file.path(path, filename)) |>
    rlang::set_names("daylength")
  
  if (band_as_time) {
    start_date = sprintf("2020-01-%s", day) |>
      as.Date()
    end_date = sprintf("2020-12-%s", day) |>
      as.Date()
    dates = seq(from = start_date, to = end_date, by = "month")
    
    x = x |>
      stars::st_set_dimensions("band", names = "time", values = dates)
  }
  return(x)
}

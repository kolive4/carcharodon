#' Function to create and write a velocity magnitude layer based on an east-west and north-south current layer
#' 
#' @param scenario climate scenario
#' @param year year projected
#' @return velocity magnitude stars object
write_vel_mag = function(scenario = c("RCP85", "RCP45", "PRESENT")[1], 
                         year = c(2055, 2075, NA)[1]) {
  if(FALSE) {
    scenario = "PRESENT"
    year = NA
  }
  if (scenario == "PRESENT") {
    year = "PRESENT"
  }
  monthly_brick_cov = load_brickman(scenario = scenario, 
                                      year = year,
                                      vars = c("U", "V"), 
                                      interval = "mon", 
                                      band_as_time = TRUE, 
                                      path = file.path("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon"))
  
  u_s = monthly_brick_cov[sprintf("%s_%s_U_mon.tif", scenario, year)]
  v_s = monthly_brick_cov[sprintf("%s_%s_V_mon.tif", scenario, year)]
  uv_s = (((u_s)^2) + ((v_s)^2)) |>
    sqrt()
  names(uv_s) = "vel_mag"
  uv_s = uv_s |>
    stars::write_stars(sprintf("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/%s_%s_vel_mag.tif", scenario, year))
}

#' Function to read in a created velocity magnitude stars layer
#' 
#' @param scenario climate scenario
#' @param year year for the scenario
#' @param band_as_time description
#' @return stars object
read_vel_mag = function(scenario, 
                        year, 
                        band_as_time) {
  if(FALSE) {
    scenario = "PRESENT"
  }
  if (scenario == "PRESENT") {
    year = "PRESENT"
  }
  
  x = stars::read_stars(sprintf("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/%s_%s_vel_mag.tif", scenario, year)) |>
    rename(vel_mag = sprintf("%s_%s_vel_mag.tif", scenario, year))
  
  if(band_as_time == TRUE) {
    time = switch(scenario,
                  "PRESENT" = seq(from = as.Date("2020-01-01"), 
                                  to = as.Date("2020-12-01"), 
                                  by = "month"),
                  "2075" = NULL ,
                  "2055" = NULL)
    x = stars::st_set_dimensions(x, "band", names = "band", values = time)
  }
  
  return(x)
}

#' Function to average stars objects across months
#'
#' @param x multi-layer multi-variable stars objects
#' @return averaged stars object
avg_covs = function(x, na.rm = FALSE){
  d = dim(x)
  if(length(d) < 3) {
    return(x)
  }
  
  z = st_apply(x, c(1, 2), sum, na.rm = na.rm)
  z = z / d[3]
  
  return(z)
}

#' function to reset the dimension of stars objects (intended for month reset)
#' @param x stars object
#' @param dn which dimension name to reset
#' @return description
reset_stars_start = function(x, dn) {
  if(FALSE) {
    x = a
    dn = "month"
  }
  d = stars::st_dimensions(x)
  n = abs(d[[dn]]$to - d[[dn]]$from)
  
  d[[dn]]$from = 1
  d[[dn]]$to = 1 + n
  
  st_dimensions(x) <- d
  return(x)
}
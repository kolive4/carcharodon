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
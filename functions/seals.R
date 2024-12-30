#' Function to read in seal data that has been cached
#' 
#' @param scenario chr, one of RCP85, RCP45, or PRESENT
#' @param year num, one of 2055 or 2075, ignored if scenario is PRESENT
#' @param species chr, species common names (harbor or gray)
#' @param months seq, sequence of months where data is available
#' @param path filepath, path to where data is saved
#' @param band_as_time logical, convert band to appropriate time/date stamp
#' @return subsetted stars image 
load_seal = function(scenario = c("RCP85", "RCP45", "PRESENT")[1],
                     year = c(2055, 2075, NA)[1],
                     species = c("harbor", "gray")[1],
                     months = seq(1, 12, by = 1),
                     path = here::here("workflows/forecast_workflow/versions"),
                     band_as_time = TRUE
                         ){
  if(FALSE){
    scenario = c("RCP85", "RCP45", "PRESENT")[1]
    year = c(2055, 2075, NA)[1]
    species = c("harbor", "gray")[1]
    months = seq(1, 12, by = 1)
    path = here::here("workflows/forecast_workflow/versions")
    band_as_time = TRUE
  }
  
  if(scenario == "PRESENT") year = "PRESENT"
  
  x = lapply(seq_along(months), 
         function(month){
           if (species == "harbor") {
             if (scenario == "PRESENT") {
               filename = file.path(path, "v03/0200", sprintf("v03.0200.%s", sprintf("%02d", month)), "prediction.tif")
               }
             if (year == 2055) {
               if (scenario == "RCP45") {
                 filename = file.path(path, "v03/0201", sprintf("v03.0201.%s", sprintf("%02d", month)), "prediction.tif")
                 }
               if (scenario == "RCP85") {
                   filename = file.path(path, "v03/0202", sprintf("v03.0202.%s", sprintf("%02d", month)), "prediction.tif")
               }
             }
             if (year == 2075) {
               if (scenario == "RCP45") {
                 filename = file.path(path, "v03/0203", sprintf("v03.0203.%s", sprintf("%02d", month)), "prediction.tif")
                 }
               if (scenario == "RCP85") {
                 filename = file.path(path, "v03/0204", sprintf("v03.0204.%s", sprintf("%02d", month)), "prediction.tif")
               }
             }
           }
           if (species == "gray") {
             if (scenario == "PRESENT") {
               filename = file.path(path, "v02/0200", sprintf("v02.0200.%s", sprintf("%02d", month)), "prediction.tif")
               }
             if (year == 2055) {
               if (scenario == "RCP45") {
                 filename = file.path(path, "v02/0201", sprintf("v02.0201.%s", sprintf("%02d", month)), "prediction.tif")
               }
               if (scenario == "RCP85") {
                 filename = file.path(path, "v02/0202", sprintf("v02.0202.%s", sprintf("%02d", month)), "prediction.tif")
               }
             }
             if (year == 2075) {
               if (scenario == "RCP45") {
                 filename = file.path(path, "v02/0203", sprintf("v02.0203.%s", sprintf("%02d", month)), "prediction.tif")
               }
               if (scenario == "RCP85") {
                 filename = file.path(path, "v02/0204", sprintf("v02.0204.%s", sprintf("%02d", month)), "prediction.tif")
               }
             }
           }
         x = stars::read_stars(filename)
         }
  )
  x = twinkle::bind_bands(x)
  
  if(band_as_time == TRUE) {
    time = switch(scenario,
                  "PRESENT" = seq(from = as.Date("2020-01-01"), length = 12, by = "month"),
                  "2075" = seq(from = as.Date("2075-01-01"), length = 12, by = "month") ,
                  "2055" = seq(from = as.Date("2055-01-01"), length = 12, by = "month") )
    dims = stars::st_dimensions(x)
    x = stars::st_set_dimensions(x, "band", names = "time", values = time)
  }
  
  return(x)
}

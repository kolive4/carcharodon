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
    scenario = c("RCP85", "RCP45", "PRESENT")[3]
    year = c(2055, 2075, NA)[3]
    species = c("harbor", "gray")[1]
    months = seq(1, 12, by = 1)
    path = here::here("workflows/forecast_workflow/versions")
    band_as_time = TRUE
  }
  
  if(scenario == "PRESENT") year = "PRESENT"
  
  filenames = lapply(seq_along(months), 
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
         return(filename)
         }
  )
  x = stars::read_stars(filenames, along = list(band = seq(1, 12))) |>
    stars::st_set_dimensions("band", delta = NA_real_, offset = NA_real_)

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

#' function to load seal prediction data from tidy_workflow
#' 
#' @param species chr, species common names (harbor or gray)
#' @param scenario chr, RCP scenario to pass in, always present in this case
#' @param model_type chr, which model do you want to pull predictions from
#' @param path file path where predictions are saved
#' @param band_as_time logical, convert band to appropriate time/date stamp
#' @return stars object
load_tidy_seal = function(species = c("harbor", "gray")[1],
                          bg = c("all", "one_to_two"),
                          scenario = c("PRESENT", "RCP45", "RCP85"),
                          year = c("PRESENT", "2055", "2075"),
                          model_type = c("rf", "bt", "maxent", "gam", "glm")[3],
                          path = here::here("workflows/tidy_cast/versions"),
                          band_as_time = FALSE){
  if(FALSE){
    species = "harbor"
    model_type = "maxent"
    band_as_time = TRUE
    bg = "one_to_two"
  }
  
  if (species == "gray") {
    if (bg == "all") {
      if (scenario == "PRESENT") {
        s_path = file.path(path, "t12/000860")
      } else if (scenario == "RCP45" && year == "2055") {
        s_path = file.path(path, "t12/000861")
      } else if (scenario == "RCP45" && year == "2075") {
        s_path = file.path(path, "t12/000862")
      } else if (scenario == "RCP85" && year == "2055") {
        s_path = file.path(path, "t12/000863")
      } else if (scenario == "RCP85" && year == "2075") {
        s_path = file.path(path, "t12/000864")
      }
    } else if (bg == "one_to_two") {
      if (scenario == "PRESENT") {
        s_path = file.path(path, "t22/000860")
      } else if (scenario == "RCP45" && year == "2055") {
        s_path = file.path(path, "t22/000861")
      } else if (scenario == "RCP45" && year == "2075") {
        s_path = file.path(path, "t22/000862")
      } else if (scenario == "RCP85" && year == "2055") {
        s_path = file.path(path, "t22/000863")
      } else if (scenario == "RCP85" && year == "2075") {
        s_path = file.path(path, "t22/000864")
      }
    }
  } else if (species == "harbor") {
    if (bg == "all") {
      if (scenario == "PRESENT") {
        s_path = file.path(path, "t13/000860")
      } else if (scenario == "RCP45" && year == "2055") {
        s_path = file.path(path, "t13/000861")
      } else if (scenario == "RCP45" && year == "2075") {
        s_path = file.path(path, "t13/000862")
      } else if (scenario == "RCP85" && year == "2055") {
        s_path = file.path(path, "t13/000863")
      } else if (scenario == "RCP85" && year == "2075") {
        s_path = file.path(path, "t13/000864")
      }
    } else if (bg == "one_to_two") {
      if (scenario == "PRESENT") {
        s_path = file.path(path, "t23/000860")
      } else if (scenario == "RCP45" && year == "2055") {
        s_path = file.path(path, "t23/000861")
      } else if (scenario == "RCP45" && year == "2075") {
        s_path = file.path(path, "t23/000862")
      } else if (scenario == "RCP85" && year == "2055") {
        s_path = file.path(path, "t23/000863")
      } else if (scenario == "RCP85" && year == "2075") {
        s_path = file.path(path, "t23/000864")
      }
    }
  }
  
  t = list.dirs(s_path, recursive = FALSE, full.names = TRUE)
  cast_vers = basename(t)
  
  tifs = lapply(t, function(filepath){
    filename = file.path(filepath, paste0(model_type, "_prediction.tif"))
    return(filename)
  })
  x = stars::read_stars(tifs, along = list(band = c(seq(1, 12)))) |>
    stars::st_set_dimensions("band", delta = NA_real_, offset = NA_real_)
  
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

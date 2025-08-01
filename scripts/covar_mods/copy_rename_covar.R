library(stringr)
library(tibble)
library(stars)
library(dplyr)
library(purrr)

copies = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon", 
                    pattern = glob2rx("RCP*.tif"),
                    full.names = TRUE)

file.rename(copies, 
            str_replace(copies, pattern = "RCP", "dRCP"))

now_covs = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon",
                      pattern = glob2rx("PRESENT_PRESENT*.tif"),
                      full.names = TRUE)

fut_dcov = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/brickman/gom_carcharodon",
                      pattern = glob2rx("dRCP*.tif"),
                      full.names = TRUE)


# helper: extract covariate name (e.g. "SST", "MLD", etc.) from file basename
get_cov = function(path) {
  if(FALSE){
    path = now_covs
  }
  str_match(basename(path), "_([A-Za-z]+)_mon\\.tif$")[,2] #brickman pattern
  #str_match(basename(path), "^[^_]+_[^_]+_([^\\.]+)\\.tif$")[,2] #vel_mag pattern
}

present_lookup = tibble(
  covar = get_cov(now_covs),
  path = now_covs
) |>
  deframe()

#' Function to add anomaly and present rasters
#' 
#' @param anom_path anomaly tif paths
#' @return stars object of combined present and anomaly to get true future
combine_anomaly_and_present = function(anom_path) {
  if (FALSE) {
    anom_path = fut_dcov[1]
  }
  covar = get_cov(anom_path)
  present_path = present_lookup[[covar]]
  
  if (is.null(present_path)) {
    warning(paste("No present raster found for covariate:", covar))
    return(NULL)
  }
  
  anom_cov = read_stars(anom_path)
  pres_cov = read_stars(present_path)
  
  combined_cov = anom_cov + pres_cov
  names(combined_cov) = stringr::str_sub(names(combined_cov), start = 2)
  
  cov_path = dirname(anom_path)
  combined_cov |> 
    write_stars(file.path(cov_path, names(combined_cov)))
}

fut_cov = lapply(fut_dcov, combine_anomaly_and_present)


#' Convert to as POSIXct to an 8D week number (1-46) or 7D week number (1-52)
#'
#' @export
#' @param x POSIXct or Date vector
#' @param week_length numeric, the number of days per week
#' @return numeric 1-46 or 1-52 week number
date_to_week <- function(x, week_length = c(8,7)[2]){
  if (missing(x)) x <- Sys.Date()
  J <- as.numeric(format(x, "%j"))
  (J-1) %/% week_length + 1
}


#' Function to convert dates to seasons
#' 
#' @export
#' @param x Date vector
#' @return vector of season
date_to_season <- function(x) {
  x = as.data.frame(x)
  
  x = dplyr::mutate(x, month = format(x, "%m") |>
                      as.numeric())
  
  x = dplyr::mutate(x, season = case_when(month %in% c(6, 7, 8) ~ "summer",
                                          month %in% c(9, 10, 11) ~ "autumn",
                                          month %in% c(12, 1, 2) ~ "winter",
                                          month %in% c(3, 4, 5) ~ "spring")
  )
  return(x$season)
  
}

#' Function to add a month to combined predictor variables stars object
#' 
#' @param template stars to attach to
#' @param month month of interest
#' @return attribute to add to a combined stars predictive layer
make_month_layer = function(template, month){
  template[[1]] = factor(sprintf("%0.2i", month), levels = lvl)
  names(template) = "month"
  return(template)
}


#' seasonality selection function
#' 
#' @export
#' @param x tibble of occurrence data
#' @param season chr, season you wish to select, summer, winter, spring, autumn
#' @return occurrence data for that season
seasonality <- function(x,
                        season = c('summer', 'spring', 'autumn', 'winter')[1]){
  
  # x = dplyr::mutate(x, month = lubridate::mdy(x$eventDate) |>
  #                 lubridate::month())
  
  x = dplyr::mutate(x, month = format(x$eventDate, "%m") |>
                      as.numeric())
  
  r = switch(tolower(season[1]),
             "summer" = dplyr::filter(x, .data$month %in% c(6, 7, 8)),
             "spring" = dplyr::filter(x, .data$month %in% c(3, 4, 5)),
             "autumn" = dplyr::filter(x, .data$month %in% c(9, 10, 11)),
             "winter" = dplyr::filter(x, .data$month %in% c(12, 1, 2))
  ) 
  
  return(r)
} 

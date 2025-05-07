#' Function that will iterate over covariates and months
#' 
#' @param a_pd description
#' @param b_pd 
#' @return a list of differences
difference_pd = function(a_pd, b_pd, normalize = TRUE){
  if(FALSE){
    a_pd = a_pd
    b_pd = b_pd
  }
  if (normalize) {
    a_pd = normalize_pd(a_pd)
    b_pd = normalize_pd(b_pd)
  }
  covars = intersect(a_pd$covar, b_pd$covar)
  lapply(covars, 
         function(cv){
           a = a_pd |>
             dplyr::filter(covar == cv)
           b = b_pd |>
             dplyr::filter(covar == cv)
           months = intersect(a$month, b$month)
           lapply(months, 
                  function(mon){
                    dif_pd(a |>
                             dplyr::filter(month == mon),
                           b |>
                             dplyr::filter(month == mon),
                           cv = cv,
                           m = mon)}) |>
             dplyr::bind_rows()
         }) |>
    dplyr::bind_rows()
}


#' Function to find the difference of partial dependence
#' @param a one partial dependence dataframe
#' @param b a second partial dependence dataframe
#' @param cv covar
#' @param m month
#' @return d the interpolated differenced partial dependence
dif_pd = function(a, b, cv = "foo", m = "13") {
  if(FALSE) {
    cv = "brick_sss"
    m = "10"
    a = a_pd |>
      normalize_pd() |>
      filter(covar == cv) |>
      filter(month == m)
    b = b_pd |>
      normalize_pd() |>
      filter(covar == cv) |>
      filter(month == m)
    
  }
  a_range = range(a$bin_mid)
  b_range = range(b$bin_mid)
  a_pd_range = range(a$pd)
  b_pd_range = range(b$pd)
  r_pd = c(min(c(a_pd_range[1], b_pd_range[1])), max(c(a_pd_range[2], b_pd_range[2])))
  r = c(max(c(a_range[1], b_range[1])), min(c(a_range[2], b_range[2])))
  x_out = seq(r[1], r[2], length = 20)
  
  a_s = with(a, spline(x = bin_mid, y = pd, xout = x_out))
  ix = a_s$y < r_pd[1]
  a_s$y[ix] = r_pd[1]
  iz = a_s$y > r_pd[2]
  a_s$y[iz] = r_pd[2]
  
  b_s = with(b, spline(x = bin_mid, y = pd, xout = x_out))
  ix = b_s$y < r_pd[1]
  b_s$y[ix] = r_pd[1]
  iz = b_s$y > r_pd[2]
  b_s$y[iz] = r_pd[2]
  
  d = tibble::tibble(x = a_s$x,
                     y = b_s$y - a_s$y,
                     covar = cv, 
                     month = m)
  return(d)
}

#' Function that normalizes pd values
#' @param a_pd a table of pd values
#' @return normalized pd table
normalize_pd = function(a_pd) {
  r = dplyr::group_by(a_pd, covar, month) |>
    dplyr::group_map(
      function(tbl, key){
        r = range(tbl$pd)
        mutate(tbl, pd = (pd - r[1])/(r[2] - r[1])) |>
          mutate(covar = key$covar,
                 month = key$month,
                 .before = 1)
      }
    ) |>
    dplyr::bind_rows()
  
  return(r)
}

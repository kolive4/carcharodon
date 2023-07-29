#' Create a template with required, recommended or both (default) columns.
#' 
#' @export
#' @param what character, one or more of 'required' and 'recommended'
#' @param n numeric, the number of rows to return,  less than equal to zero
#'   yields an empty tibble
#' @return tibble with n rows
template_dwc <- function(what = c("required", "recommended"), n = 0) {
  
  if (n <= 0) {
    N <- 1
  } else {
    N = n
  }
  
  char = rep(NA_character_, N)
  num = rep(NA_real_, N)
  int = rep(NA_integer_, N)
  
  x <- dplyr::tibble(
    occurrenceID = char,
    basisOfRecord = char,
    sst = num,
    depth = num,
    scientificName = char,
    eventDate = rep(Sys.Date(), N))
  
  if ("recommended" %in% tolower(what)){
    
    x <- dplyr::bind_cols(x,
                          dplyr::tibble(
                            taxonRank = char,
                            kingdom = char,
                            decimalLatitude = num,
                            decimalLongitude = num,
                            geodeticDatum = char,
                            countryCode = char,
                            individualCount = int,
                            organismQuantity = int, 
                            organismQuantityType = char))
    
  }
  
  if (n <= 0) x <- dplyr::slice(x, 0)
  
  x
}

#' Modify an input OBIS dataset to include only Darwin Core required and 
#' recommended fields.
#' 
#' @seealso \href{https://ipt.gbif.org/manual/en/ipt/2.5/occurrence-data}{Darwin Core}
#' @param x tibble of raw OBIS data
#' @param template template as a tibble
#' @return tibble with defined columns
as_dwc <- function(x, template = template_dwc(n=1)){
  
  as_date <- function(x, format = "%Y-%m-%d %H:%M:%S"){
    
  }
  
  tnames <- colnames(template)
  tclass <- sapply(template, class)
  y <- dplyr::select(x, dplyr::any_of(tnames))
  ynames <- colnames(y)
  for (nm in tnames){
    if (nm %in% ynames){
      if (tclass[[nm]] == "Date"){
        y <- dplyr::mutate(y, {{ nm }} := as.Date(.data[[nm]])) 
      } else {
        class(y[[nm]]) <- tclass[[nm]]
      }
    } else {
      y <- dplyr::mutate(y, !!nm := template[[nm]])
    }
  }
  y  
}


#' Generate a dummy template of data
#' 
#' @export
#' @param n numeric, the number of rows to create
#' @param eventDate_type character, what class should eventDate be?
#' @return tibble
species_template <- function(n = 1, eventDate_type = c("character", "date")[1]){
  x <- dplyr::tibble(
    id                 = paste("void", seq_len(n), sep = "_"),
    scientificName     = "",
    eventDate          = "",
    basisOfRecord      = "",
    decimalLongitude   = NA_real_,
    decimalLatitude    = NA_real_,
    depth              = NA_real_,
    sst                = NA_real_,
    sss                = NA_real_)
  if (tolower(eventDate_type[1]) == "date"){
    x <- dplyr::mutate(x, eventDate = Sys.Date())
  }
  x
}

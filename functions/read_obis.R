#' function to read obis with the option to fetch
#' 
#' reads file by species name, if file doesn't exist, first try to fetch it then save it to the cache
#' @export
#' @param species character, latin name of species
#' @param refresh logical, if true fetch fresh set of data
#' @param dwc logical, if TRUE trim to the recommended Darwin Core content
#' @return data frame in the form of a tibble
read_obis = function(species = "Carcharodon carcharias", 
                     refresh = FALSE,
                     dwc = TRUE){
  filename = get_path(paste0(species[1], ".csv.gz"))
  if (!file.exists(filename) || refresh == TRUE) {
    x = fetch_obis(scientificname = species)
  }
  else{
    #x <- readr::read_csv(filename, show_col_types = FALSE)
    x = tidytable::fread.(filename) |>
      dplyr::as_tibble()
  }
  if (dwc) x <- as_dwc(x)
  return(x)
}

#' Retrieve a data path
#' 
#' @export
#' @param ... character, one or more file path segments to be post-pended to \code{root}
#' @param root character, the root data directory path
#' @return character path specification
get_path <- function(..., root = here::here("data")){
  file.path(root, ...)
}


#' Convert from scientific name to file name
#' 
#' @param x chr, scientific name of species
#' @param ext chr, file extension to be added
#' @param sep chr, separator of file name (between genus and species)
#' @param path chr, path of which file is stored
#' @return character vector of file names
file_name <- function(x = 'Carcharodon carcharias', 
                      ext = '.csv.gz', 
                      sep = "_", 
                      path = get_path("obis")){
  
  x = tolower(x)
  x = gsub(" ", sep, x, fixed = TRUE)
  x = paste0(x, ext)
  x = file.path(path, x)
  x
}


#' Convert from file name to scientific_name
#' 
#' @export
#' @param x chr, name of file
#' @param ext chr, file extension to be removed
#' @param sep chr, separator of file name
#' @return character vector of scientific names
scientific_name <- function(x = 'carcharodon_carcharias.csv.gz', 
                            ext = '.csv.gz', 
                            sep = '_'){
  
  x = basename(x)
  x = sub(ext, '', x, fixed = TRUE)
  x = paste0(toupper(substring(x, 1,1)),substring(x, 2))
  x = gsub(sep," ", x, fixed = TRUE)
  x
}


#' Fetch a species from OBIS
#' 
#' @export
#' @param scientificname character, the Latin name for a species
#' @param ... other arguments for \code{\link[robis]{occurrence}}
#' @param save_file NA or a path specification to save the file
#' @param template data frame defining minimal fields 
#' @return tibble, possibly empty if a species is not found
fetch_obis <- function(scientificname = 'Carcharodon carcharias', 
                       save_file = file_name(scientificname, path = here::here("data", "obis")),
                       template = species_template(),
                       ...){
  if (length(scientificname) > 1) {
    xx = lapply(scientificname, fetch_obis) |>
      dplyr::bind_rows()
    
    return(xx)
  }
  autofill <- function(x, template = species_template()){
    xnames <- colnames(x)
    tnames <- colnames(template)
    ix <- !(tnames %in% xnames)
    if (any(ix)){
      missingnames <- tnames[ix]
      for (mn in missingnames) x[[mn]] = template[[mn]][1]
    } 
    for (nm in tnames) mode(x[[nm]]) <- mode(template[[nm]])
    x |> dplyr::select(dplyr::all_of(tnames))
  }
  
  #x <- try(robis::occurrence(scientificname = scientificname[1], fields = names(template), ...))
  x <- try(robis::occurrence(scientificname = scientificname[1], ...)) |>
    readr::write_csv(file_name(scientificname, ext = "-raw.csv.gz"))|>
    dplyr::select(dplyr::any_of(names(template)))
  if (!inherits(x, 'try-error') && nrow(x) > 0){
    x <- dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character)) |>
      autofill(template) |>
      dplyr::mutate(eventDate = format(as.Date(substring(.data$eventDate, 1, nchar("YYYY-mm-dd")), 
                                               format = "%Y-%m-%d"), 
                                       format = "%Y-%m-%d")) |>
      dplyr::filter(!grepl("void_", .data$id, fixed = TRUE))
  }
  if (!inherits(x, 'try-error') && nrow(x) > 0 && !is.na(save_file)){
    x <- readr::write_csv(x, save_file)
  }
  x
}


#' Retrieve a listing of local files
#' 
#' @export
#' @param path the path to the datasets
#' @param full.names logical return basename or full path specs?
#' @param strip character or NA, if character then strip this pattern. Ignored
#'   if full.names is \code{TRUE}
#' @return character names (possibly filenames)
list_obis <- function(path = get_path("obis"), 
                      full.names = FALSE,
                      strip = c(NA, ".csv.gz")[2]){
  ff <- list.files(path, pattern = "^.*\\.csv", full.names = full.names)
  if (!full.names && !is.na(strip[1])){
    ff <- sub(strip, "", ff, fixed = TRUE)
  }
  ff
}


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
                     dwc = TRUE,
                     form = c("table", "sf")[1],
                     ...){
  if (length(species) > 1) {
    xx = lapply(species, function(spp) {
      read_obis(species = spp, refresh = refresh, dwc = dwc, form = form, ...)
    }) |>
      dplyr::bind_rows()
    
    return(xx)
  }
  filename = file_name(species[1], ...)
  if (!file.exists(filename) || refresh == TRUE) {
    x = fetch_obis(scientificname = species) |>
      dplyr::distinct()
  }
  else{
    #x <- readr::read_csv(filename, show_col_types = FALSE)
    x = readr::read_csv(filename, 
                        col_types = readr::cols(
                          id = readr::col_character(),
                          scientificName = readr::col_character(),
                          eventDate = readr::col_date(format = ""),
                          basisOfRecord = readr::col_character(),
                          decimalLongitude = readr::col_double(),
                          decimalLatitude = readr::col_double(),
                          depth = readr::col_double(),
                          sst = readr::col_double(),
                          sss = readr::col_double()
                        )) |>
      dplyr::distinct()
  }
  if (dwc) x <- as_dwc(x)
  if (form == "sf") x <- sf::st_as_sf(x, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  return(x)
}


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
    occurrenceID       = "",
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


#' Rename one or more fields (columns) in a OBIS tibble
#'
#' @export
#' @param x tibble of data
#' @param fields named character vector with oldname = newname
#' @return renamed tibble
rename_obis <- function(x, 
                        fields = c("name" = "scientificName",
                                   "longitude" = "decimalLongitude",
                                   "latitude" = "decimalLatitude"
                        )){
  #xnames <- colnames(x)
  
  #for (nm in names(fields)){
  #  if(nm %in% xnames) x <- dplyr::rename(x, fields[[nm]] = nm)
  #}
  x <- dplyr::rename(x, !!!fields)
  x
}


#' Plot the locations of a OBIS dataset
#' 
#' @export
#' @param x tibble of OBIS data
#' @param what character, one of 'base', 'ggplot', 'leaflet', 'gist'
#' @param ... other arguments for mapr package map_* functions
plot_obis <- function(x, what = c('base', 'ggplot', 'leaflet', 'gist', 'mapView')[1], ...){
  
  if(!inherits(x, "sf")) {x <- rename_obis(x)}
  switch(tolower(what[1]),
         "base" = mapr::map_plot(x, ...),
         "ggplot" = mapr::map_ggplot(x, ...),
         "leaflet" = mapr::map_leaflet(x, ...),
         "ggplot" = mapr::map_gist(x, ...),
         "mapview" = print(mapview::mapview(x, fgb = F, georaster = F, zcol = "basisOfRecord", ...)) 
  )
  
}


#' Retrieve group stats
#' 
#' @export
#' @param x table, table to be provided to function
#' @param grouping_var character, one or more columns by which to group data
#' @return table of stats for each group

group_stats <- function(x, grouping_var = "basisOfRecord") {
  y <- dplyr::group_by(x, dplyr::across(dplyr::any_of(grouping_var))) |>
    dplyr::group_map(
      function (tbl, key) {
        r <- range(tbl$sst, na.rm = TRUE)
        dplyr::mutate(key, 
                      n = nrow(tbl), 
                      min = r[1], 
                      max = r[2], 
                      median = median(tbl$sst, na.rm = TRUE), 
                      iqr = IQR(tbl$sst, na.rm = TRUE),
                      mean = mean(tbl$sst, na.rm = TRUE), 
                      sd = sd(tbl$sst, na.rm = TRUE))
      }, .keep = FALSE) |>
    dplyr::bind_rows()
  return(y)
}

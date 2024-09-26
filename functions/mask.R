#' Read a Gebco raster product
#' 
#' @param name chr, one of 'gebco_750_mask.tif', 'gebco_mask.tif' or 'gebco.tif'
#' @param path chr the data path
#' @return stars object
read_gebco_data = function(name = c('gebco_750_mask.tif', 'gebco_mask.tif', 'gebco.tif')[3],
                           path = file.path(DATAPATH, "mapping", "gebco")){
  
  stars::read_stars(file.path(path, name[1]))
}


#' Read a coastal object
#' 
#' @param name chr, the name of the file - either a raster or a polygon
#' @param path chr, the path to the file
#' @return an sf or stars object depending upon the request
read_coast_buffer = function(name = "25nm_shoreline_buffer.gpkg",
                           path = file.path(DATAPATH, "mapping")){
  switch(name[1],
         "25nm_shoreline_buffer.gpkg" = sf::read_sf(file.path(path, name[1])),
         "25nm_shoreline_buffer.tif" = stars::read_stars(file.path(path, name[1])),
         stop("name not known:", name[1]))
}

#' Rasterize the coastal mask to a specified raster template
#' 
#' @param x sf POLYGON of the coast
#' @param template stars, the object to use as template where 1 = water and 0 = land
#' @return a stars object with templates dimensions
rasterize_coast_polygon = function(x = read_coast_buffer(),
                                   template = read_gebco_data('gebco_750_mask.tif')){
  
  on.exit(sf_use_s2(orig))
  
  orig = sf_use_s2(FALSE)
  
  sf::st_crop(template, sf::st_geometry(x), crop = FALSE) |>
    rlang::set_names("mask") |>
    dplyr::mutate(mask = replace(mask, mask <= 0, NA))
}

#' Read bathymetry data
#' @param what char one of 'etopo' or 'gebco'
#' @param bb 4 element bounding box in (left, right, bottom, top) order
#' @return a stars object cropped to bb
read_base_bathy = function(what = c("gebco", "etopo")[1], bb = c(-74.9, -65, 38.8, 46)){
  
  switch(tolower(what[1]),
       "gebco" = topotools::read_gebco(bb = bb),
       "etopo" = topotools::read_gebco(bb = bb))
}


#' Given a raster, determine the distance to shore
#' 
#' @param x stars object with one attribute called "mask"
#' @param water numeric values that define water - all other values are considered "land"
#' @return stars object that has values form 0 to some number in meters that bears the great circle
#'   distance from the ocean point to the closest land.  Values of 0 imply on land.
distance_to_shore = function(
    x = stars::read_stars(here::here("data", "mapping", "etopo", "etopo_warped_mask.tif")) |>
      rlang::set_names("mask"),
    water = 1){
  
  x = rlang::set_names(x, "mask")

  # make a LUT
  lut = dplyr::mutate(x, mask = dplyr::if_else(mask == water, NA, 1)) |>
    twinkle::make_raster_lut() |>
    rlang::set_names("closest")
  
  # transform to a table (sf style)
  # bind the values with the mask
  # ad a sequence index and a dummy distance value
  z = sf::st_as_sf(lut, as_points = TRUE) |>
    dplyr::bind_cols(as.data.frame(x, add_coordinates = FALSE)) |>
    dplyr::mutate(orig = seq_len(dplyr::n()), dist = 0, .after = 2)
   
  # logicals of water/land
  ix = z$closest != z$orig
  
  # the water points to measure from
  # the points on land closest
  from = dplyr::filter(z, ix)
  to = dplyr::slice(z, from$closest) 
  
  # compote disance in meters
  d = sf::st_distance(from, to, by_element = TRUE)
  
  # stuff them back into the table (correct order)
  z$dist[ix] = as.numeric(d)
  
  # stuff the whole thing into mask
  x$mask <- z$dist
  
  # rename the layer and return
  dist = rlang::set_names(x, "distance")
  dist
}

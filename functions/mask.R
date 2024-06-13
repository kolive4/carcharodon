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
    dplyr::mutate(mask = dplyr::replace(mask, mask <= 0, NA))
}
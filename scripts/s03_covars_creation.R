# check if covar file exists, if it doesn't pull it from obpg 
if(file.exists("uri.csv")){
  uri = readr::read_csv("uri.csv", show_col_types = FALSE)
} else {
  sst_uri <- obpg::query_obpg_climatology()
  sst_summer_uri <- obpg::query_obpg_climatology(climatology = "SCSU")
  sst_autumn_uri <- obpg::query_obpg_climatology(climatology = "SCAU")
  sst_winter_uri <- obpg::query_obpg_climatology(climatology = "SCWI")
  sst_spring_uri <- obpg::query_obpg_climatology(climatology = "SCSP")
  
  uri = dplyr::tibble(name = c("sst_uri", "sst_summer_uri", "sst_autumn_uri", "sst_winter_uri", "sst_spring_uri"), uri = c(sst_uri, sst_summer_uri, sst_autumn_uri, sst_winter_uri, sst_spring_uri)) |>
    readr::write_csv("uri.csv")
}


# Extract sst within the bounding box and plot them.
if(covars_exists("sst_climatology.tif")){
  covars = read_covar("sst_climatology.tif") |>
    rlang::set_names("sst")
} else {
  sst <- ncdf4::nc_open(uri |>
                          filter(name == "sst_uri") |>
                          dplyr::pull(uri))
  
  covars <- obpg::extract(bb, sst, varname = obpg::obpg_vars(sst)) |>
    stars::st_flip("y") |>
    write_covar(filename = "sst_climatology.tif")
  
  ncdf4::nc_close(sst)
  
}

if(covars_exists("sst_summer.tif")){
  covars_summer = read_covar("sst_summer.tif") |>
    set_names("sst")
} else {
  summer_sst <- ncdf4::nc_open(uri |>
                                 filter(name == "sst_summer_uri") |>
                                 pull(uri))
  covars_summer <- obpg::extract(bb, summer_sst, varname = obpg::obpg_vars(sst))  |>
    stars::st_flip("y") |>
    write_covar(filename = "sst_summer.tif")
  
  ncdf4::nc_close(summer_sst)
}

if(covars_exists("sst_autumn.tif")){
  covars_autumn = read_covar("sst_autumn.tif") |>
    set_names("sst")
} else {
  autumn_sst <- ncdf4::nc_open(uri |>
                                 filter(name == "sst_autumn_uri") |>
                                 pull(uri))
  
  covars_autumn <- obpg::extract(bb, autumn_sst, varname = obpg::obpg_vars(sst))  |>
    stars::st_flip("y")|>
    write_covar(filename = "sst_autumn.tif")
  
  ncdf4::nc_close(autumn_sst)
}

if(covars_exists("sst_winter.tif")){
  covars_winter = read_covar("sst_winter.tif") |>
    set_names("sst")
} else {
  winter_sst <- ncdf4::nc_open(uri |>
                                 filter(name == "sst_winter_uri") |>
                                 pull(uri))
  
  covars_winter <- obpg::extract(bb, winter_sst, varname = obpg::obpg_vars(sst))  |>
    stars::st_flip("y")|>
    write_covar(filename = "sst_winter.tif")
  
  ncdf4::nc_close(winter_sst)
}

if(covars_exists("sst_spring.tif")){
  covars_spring = read_covar("sst_spring.tif") |>
    set_names("sst")
} else {
  spring_sst <- ncdf4::nc_open(uri |>
                                 filter(name == "sst_spring_uri") |>
                                 pull(uri))
  
  covars_spring <- obpg::extract(bb, spring_sst, varname = obpg::obpg_vars(sst))  |>
    stars::st_flip("y")|>
    write_covar(filename = "sst_spring.tif")
  
  ncdf4::nc_close(spring_sst)
}

mask <- stars::read_stars("data/mapping/nwa2.binary.tif") |>
  st_warp(covars_summer) |>
  st_crop(st_bbox(covars_summer), as_points = TRUE)

global_coast <- rnaturalearth::ne_coastline(returnclass = "sf")
nwa_coast <- sf::st_crop(global_coast, bb)


plot(covars, col = rev(RColorBrewer::brewer.pal(8, "YlOrRd")), main = "Total Mission SST", reset = FALSE)
plot(sf::st_geometry(nwa_coast), border = "black", lwd = 4, add = TRUE)

plot(covars_summer, col = rev(RColorBrewer::brewer.pal(8, "YlOrRd")), main = "Summer SST", reset = F)
plot(sf::st_geometry(nwa_coast), border = "black", lwd = 4, add = T)
plot(covars_autumn, col = rev(RColorBrewer::brewer.pal(8, "YlOrRd")), main = "Autumn SST", reset = F)
plot(sf::st_geometry(nwa_coast), border = "black", lwd = 4, add = T)
plot(covars_winter, col = rev(RColorBrewer::brewer.pal(8, "YlOrRd")), main = "Winter SST", reset = F)
plot(sf::st_geometry(nwa_coast), border = "black", lwd = 4, add = T)
plot(covars_spring, col = rev(RColorBrewer::brewer.pal(8, "YlOrRd")), main = "Spring SST", reset = F)
plot(sf::st_geometry(nwa_coast), border = "black", lwd = 4, add = T)


# read in covars
envs <- c(
  climatology = read_covar("sst_climatology.tif"),
  spring = read_covar("sst_spring.tif"),
  summer = read_covar("sst_summer.tif"),
  autumn = read_covar("sst_autumn.tif"),
  winter = read_covar("sst_winter.tif")
) |>
  rlang::set_names(c("climatology", "spring", "summer", "autumn", "winter"))

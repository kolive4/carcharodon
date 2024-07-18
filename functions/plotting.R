#' Function for mapping
#' 
#' @param data coastline object from rnaturalearth
#' @param bb bounding box coordinates for cropping
#' @param ... arguments passable to geom_sf
#' @return x, ggplot layer

geom_coastline = function(coast = rnaturalearth::ne_coastline(scale = "large", returnclass = "sf"), bb = NULL, ...) {
  
  maine_coords = dplyr::tibble(name = "Maine", Latitude = 45, Longitude = -69) |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  x = list(
    ggplot2::geom_sf(data = sf::st_crop(coast, bb), aes(), colour = "black"),
    ggplot2::geom_sf_label(data = maine_coords, aes(label = name, geometry = geometry)) 
  )
  
  x
}


#' Function for plotting covariates
#'
#' @param cfg configuration file
#' @param bathy bathy data
#' @param covar covariate data
#' @param obs observation data
#' @return plots 
plot_covars = function(cfg, bathy = NULL, fish = NULL, covars = NULL, obs = NULL){
  
  if ("SST" %in% cfg$covars){
    sst_range = range(brick_covars[1,,,as.numeric(cfg$month)][[1]], na.rm = TRUE)
    sst_points = ggplot() +
      geom_stars(data = brick_covars[1,,,as.numeric(cfg$month)]) +
      scale_fill_steps(name = "Sea Surface Temperature (\u00B0C)", 
                       limits = sst_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#FEEDDE", high = "#8C2D04") +
      # geom_sf(data = mon_shark_obs, 
      #         aes(shape = basisOfRecord), 
      #         fill = "white", 
      #         show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
    print(sst_points)
    ggsave(filename = sprintf("%s_sst_points.png", cfg$version), 
           plot = sst_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300)
  }
  
  if ("Tbtm" %in% cfg$covars){
    tbtm_range = range(brick_covars[2,,,as.numeric(cfg$month)][[1]], na.rm = TRUE)
    tbtm_points = ggplot() +
      geom_stars(data = brick_covars[2,,,as.numeric(cfg$month)]) +
      scale_fill_steps(name = "Bottom Temperature (\u00B0C)", 
                       limits = tbtm_range, 
                       n.breaks = 7, 
                       low = "#DBFAF9", high = "#02877A") +
      # geom_sf(data = mon_shark_obs, 
      #         aes(shape = basisOfRecord), 
      #         fill = "white", 
      #         show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
    print(tbtm_points)
    ggsave(filename = sprintf("%s_tbtm_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = tbtm_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300)
  }
  
  if ("MLD" %in% cfg$covars){
    mld_range = range(brick_covars[3,,,as.numeric(cfg$month)][[1]], na.rm = TRUE)
    mld_points = ggplot() +
      geom_stars(data = brick_covars[3,,,as.numeric(cfg$month)]) +
      scale_fill_steps(name = "Mixed Layer Depth (m)",
                       limits = mld_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#cbc2b9", high = "#5e3719") +
      #geom_sf(data = mon_shark_obs, 
      #        aes(shape = basisOfRecord), 
      #        fill = "white", 
      #        show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
    print(mld_points)
    ggsave(filename = sprintf("%s_mld_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = mld_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300)
  }
  
  if ("SSS" %in% cfg$covars){
    sss_range = range(brick_covars[4,,,as.numeric(cfg$month)][[1]], na.rm = TRUE)
    sss_points = ggplot() +
      geom_stars(data = brick_covars[4,,,as.numeric(cfg$month)]) +
      scale_fill_steps(name = "Sea Surface Salinity (ppm)",
                       limits = sss_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#ffd2b6", high = "#c24e00") +
      # geom_sf(data = mon_shark_obs, 
      #         aes(shape = basisOfRecord), 
      #         fill = "white", 
      #         show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
    print(sss_points)
    ggsave(filename = sprintf("%s_sss_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = sss_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300)
  }
  
  if ("Sbtm" %in% cfg$covars){
    sbtm_range = range(brick_covars[5,,,as.numeric(cfg$month)][[1]], na.rm = TRUE)
    sbtm_points = ggplot() +
      geom_stars(data = brick_covars[5,,,as.numeric(cfg$month)]) +
      scale_fill_steps(name = "Bottom Salinity (ppm)",
                       limits = sbtm_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 8, 
                       low = "#fff5b5", high = "#a49c00") +
      # geom_sf(data = mon_shark_obs, 
      #         aes(shape = basisOfRecord), 
      #         fill = "white", 
      #         show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
    print(sbtm_points)
    ggsave(filename = sprintf("%s_sbtm_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = sbtm_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300)
  }
  
  if ("U" %in% cfg$covars){
    u_range = range(brick_covars[6,,,as.numeric(cfg$month)][[1]], na.rm = TRUE)
    u_points = ggplot() +
      geom_stars(data = brick_covars[6,,,as.numeric(cfg$month)]) +
      scale_fill_steps(name = "Horizontal advection (?)",
                       limits = u_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#ECC0FC", high = "#6D047D") +
      geom_sf(data = mon_shark_obs, 
              aes(shape = basisOfRecord), 
              fill = "white", 
              show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
    print(u_points)
    ggsave(filename = sprintf("%s_u_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = u_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300)
  }
  
  if ("V" %in% cfg$covars){
    v_range = range(brick_covars[7,,,as.numeric(cfg$month)][[1]], na.rm = TRUE)
    v_points = ggplot() +
      geom_stars(data = brick_covars[7,,,as.numeric(cfg$month)]) +
      scale_fill_steps(name = "Vertical advection (?)",
                       limits = v_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#D6FDD4", high = "#146A03") +
      geom_sf(data = mon_shark_obs, 
              aes(shape = basisOfRecord), 
              fill = "white", 
              show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
    print(v_points)
    ggsave(filename = sprintf("%s_v_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = v_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300)
  }
  
  
  if ("Xbtm" %in% cfg$covars){
    xbtm_range = range(brick_covars[8,,,as.numeric(cfg$month)][[1]], na.rm = TRUE)
    xbtm_points = ggplot() +
      geom_stars(data = brick_covars[8,,,as.numeric(cfg$month)]) +
      scale_fill_steps(name = "Bottom Advection (?)",
                       limits = xbtm_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#FCECD4", high = "#C49300") +
      geom_sf(data = mon_shark_obs, 
              aes(shape = basisOfRecord), 
              fill = "white", 
              show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
    print(xbtm_points)
    ggsave(filename = sprintf("%s_xbtm_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = xbtm_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300)
  }
  
  
  if ("Bathy_depth" %in% cfg$bathy_var) {
    log_bathymetry_binned_plot = ggplot() +
      geom_stars(data = log_bathy) +
      scale_fill_steps(name = "log(Depth)", 
                       n.breaks = 7, 
                       low = "#deebf7", high = "#08306b") +
      geom_sf(data = mon_shark_obs, 
              aes(shape = basisOfRecord), 
              fill = "white", 
              show.legend = "point") +
      scale_shape_manual(name = "Method", 
                         values = cfg$graphics$BOR_symbol) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
    log_bathymetry_binned_plot
    ggsave(filename = sprintf("%s_log_depth_binned.png", cfg$version), 
           plot = log_bathymetry_binned_plot, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300)
    
    if ("fish_biomass" %in% cfg$fish_var) {
      fish_biomass_plot = ggplot() +
        geom_stars(data = fish_layer) +
        scale_fill_steps(name = "Fish Biomass", 
                         n.breaks = 7, 
                         low = "#FFCFF2", high = "#B80087") +
        geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
        scale_shape_manual(name = "Method", 
                           values = cfg$graphics$BOR_symbol) +
        ggplot2::labs(caption = cfg$version) +
        theme_void() 
      fish_biomass_plot
      ggsave(filename = sprintf("%s_fish_biomass.png", cfg$version), 
             plot = fish_biomass_plot, 
             path = file.path(vpath, "figures"), 
             width = 11, height = 8.5, units = "in", dpi = 300)
      
    
  }
    
    
    
  }
}

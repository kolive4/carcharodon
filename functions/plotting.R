#' Function for mapping
#' 
#' @param data coastline object from rnaturalearth
#' @param bb bounding box coordinates for cropping
#' @param color color of coastline
#' @param ... arguments passable to geom_sf
#' @return x, ggplot layer

geom_coastline = function(coast = rnaturalearth::ne_coastline(scale = "large", returnclass = "sf"), bb = NULL, color = "white", ...) {
  
  maine_coords = dplyr::tibble(name = "Maine", Latitude = 45, Longitude = -69) |>
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  x = list(
    ggplot2::geom_sf(data = sf::st_crop(coast, bb), aes(), colour = color),
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
#' @param plot_points logical for whether or not to plot points on the plot
#' @param contour contour line data
#' @param plot_contour logical for whether or not to plot contour lines
#' @return plots 
plot_covars = function(cfg, bathy = NULL, log_bathy = NULL, fish = NULL, dfs = NULL, covars = NULL, obs = NULL, plot_points = NULL, contour = NULL, plot_contour = NULL){
  
  if ("SST" %in% cfg$covars){
    sst_range = range(covars[["sst"]], na.rm = TRUE)
    sst_points = ggplot() +
      geom_stars(data = covars["sst"]) +
      scale_fill_steps(name = "Sea Surface Temperature (\u00B0C)", 
                       limits = sst_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#FEEDDE", high = "#8C2D04") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void()
      if (plot_points) {
        sst_points = sst_points + 
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      sst_points = sst_points +
        geom_sf(data = mask_contour, color = "white")
    }
    ggsave(filename = sprintf("%s_sst_points.png", cfg$version), 
           plot = sst_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
  }
  
  if ("Tbtm" %in% cfg$covars){
    tbtm_range = range(covars[["tbtm"]], na.rm = TRUE)
    tbtm_points = ggplot() +
      geom_stars(data = covars["tbtm"]) +
      scale_fill_steps(name = "Bottom Temperature (\u00B0C)", 
                       limits = tbtm_range, 
                       n.breaks = 7, 
                       low = "#DBFAF9", high = "#02877A") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
      if (plot_points) {
        tbtm_points = tbtm_points +
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      tbtm_points = tbtm_points +
        geom_sf(data = mask_contour, color = "white")
    }
    ggsave(filename = sprintf("%s_tbtm_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = tbtm_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
  }
  
  if ("MLD" %in% cfg$covars){
    mld_range = range(covars[["mld"]], na.rm = TRUE)
    mld_points = ggplot() +
      geom_stars(data = covars["mld"]) +
      scale_fill_steps(name = "Mixed Layer Depth (m)",
                       limits = mld_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#cbc2b9", high = "#5e3719") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void()
      if (plot_points) {
        mld_points = mld_points +
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      mld_points = mld_points +
        geom_sf(data = mask_contour, color = "white")
    }
    ggsave(filename = sprintf("%s_mld_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = mld_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
  }
  
  if ("SSS" %in% cfg$covars){
    sss_range = range(covars[["sss"]], na.rm = TRUE)
    sss_points = ggplot() +
      geom_stars(data = covars["sss"]) +
      scale_fill_steps(name = "Sea Surface Salinity (ppm)",
                       limits = sss_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#ffd2b6", high = "#c24e00") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void()
      if (plot_points) {
        sss_points = sss_points +
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      sss_points = sss_points +
        geom_sf(data = mask_contour, color = "white")
    }
    ggsave(filename = sprintf("%s_sss_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = sss_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
  }
  
  if ("Sbtm" %in% cfg$covars){
    sbtm_range = range(covars[["sbtm"]], na.rm = TRUE)
    sbtm_points = ggplot() +
      geom_stars(data = covars["sbtm"]) +
      scale_fill_steps(name = "Bottom Salinity (ppm)",
                       limits = sbtm_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 8, 
                       low = "#fff5b5", high = "#a49c00") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void()
      if (plot_points) {
        sbtm_points = sbtm_points +
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      sbtm_points = sbtm_points +
        geom_sf(data = mask_contour, color = "white")
    }
    ggsave(filename = sprintf("%s_sbtm_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = sbtm_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
  }
  
  if ("U" %in% cfg$covars){
    u_range = range(covars[["u"]], na.rm = TRUE)
    u_points = ggplot() +
      geom_stars(data = covars["u"]) +
      scale_fill_steps(name = "Horizontal advection (?)",
                       limits = u_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#ECC0FC", high = "#6D047D") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void()
      if (plot_points) {
        u_points = u_points +
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      u_points = u_points +
        geom_sf(data = mask_contour, color = "white")
    }
    print(u_points)
    ggsave(filename = sprintf("%s_u_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = u_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
  }
  
  if ("V" %in% cfg$covars){
    v_range = range(covars[["v"]], na.rm = TRUE)
    v_points = ggplot() +
      geom_stars(data = covars["v"]) +
      scale_fill_steps(name = "Vertical advection (?)",
                       limits = v_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#D6FDD4", high = "#146A03") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void() 
      if (plot_points) {
        v_points = v_points +
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      v_points = v_points +
        geom_sf(data = mask_contour, color = "white")
    }
    ggsave(filename = sprintf("%s_v_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = v_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
  }
  
  
  if ("Xbtm" %in% cfg$covars){
    xbtm_range = range(covars[["xbtm"]], na.rm = TRUE)
    xbtm_points = ggplot() +
      geom_stars(data = covars["xbtm"]) +
      scale_fill_steps(name = "Bottom Advection (?)",
                       limits = xbtm_range, # if the same across months, change sst_range arg to [1,,,], if the same across scenarios/years input in cfg 
                       n.breaks = 7, 
                       low = "#FCECD4", high = "#C49300") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void()
      if (plot_points) {
        xbtm_points = xbtm_points +
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      xbtm_points = xbtm_points +
        geom_sf(data = mask_contour, color = "white")
    }
    ggsave(filename = sprintf("%s_xbtm_points.png", cfg$version), # need to find a way to separate when making multiple, can I paste in a value from the cfg that would be an identifier?
           plot = xbtm_points, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
  }
  
  if ("Bathy_depth" %in% cfg$static_vars) {
    bathymetry_binned_plot = ggplot() +
      geom_stars(data = bathy) +
      scale_fill_steps(name = "Depth", 
                       n.breaks = 7, 
                       low = "#deebf7", high = "#08306b") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void()
      if (plot_points) {
        bathymetry_binned_plot = bathymetry_binned_plot +
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      bathymetry_binned_plot = bathymetry_binned_plot +
        geom_sf(data = mask_contour, color = "white")
    }
    ggsave(filename = sprintf("%s_depth_binned.png", cfg$version), 
           plot = bathymetry_binned_plot, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
  
  if ("log_depth" %in% cfg$static_vars) {
    log_bathymetry_binned_plot = ggplot() +
      geom_stars(data = log_bathy) +
      scale_fill_steps(name = "log(Depth)", 
                       n.breaks = 7, 
                       low = "#deebf7", high = "#08306b") +
      geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
      ggplot2::labs(caption = cfg$version) +
      theme_void()
      if (plot_points) {
        log_bathymetry_binned_plot = log_bathymetry_binned_plot +
          geom_sf(data = mon_shark_obs, 
                aes(shape = basisOfRecord), 
                fill = "white", 
                show.legend = "point") +
          scale_shape_manual(name = "Method", 
                             values = cfg$graphics$BOR_symbol)
      }
    if (plot_contour) {
      log_bathymetry_binned_plot = log_bathymetry_binned_plot +
        geom_sf(data = mask_contour, color = "white")
    }
    ggsave(filename = sprintf("%s_log_depth_binned.png", cfg$version), 
           plot = log_bathymetry_binned_plot, 
           path = file.path(vpath, "figures"), 
           width = 11, height = 8.5, units = "in", dpi = 300,
           create.dir = TRUE)
    
    if ("fish_biomass" %in% cfg$static_vars) {
      fish_biomass_plot = ggplot() +
        geom_stars(data = fish) +
        scale_fill_steps(name = "Fish Biomass", 
                         n.breaks = 7, 
                         low = "#FFCFF2", high = "#B80087") +
        geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
        ggplot2::labs(caption = cfg$version) +
        theme_void()
        if (plot_points) {
          fish_biomass_plot = fish_biomass_plot +
            geom_sf(data = mon_shark_obs, 
                  aes(shape = basisOfRecord), 
                  fill = "white", 
                  show.legend = "point") +
            scale_shape_manual(name = "Method", 
                               values = cfg$graphics$BOR_symbol)
        }
      if (plot_contour) {
        fish_biomass_plot = fish_biomass_plot +
          geom_sf(data = mask_contour, color = "white")
      }
      ggsave(filename = sprintf("%s_fish_biomass.png", cfg$version), 
             plot = fish_biomass_plot, 
             path = file.path(vpath, "figures"), 
             width = 11, height = 8.5, units = "in", dpi = 300,
             create.dir = TRUE)
      
    
    }
    if ("dfs" %in% cfg$static_vars) {
      dfs_plot = ggplot() +
        geom_stars(data = dfs) +
        scale_fill_steps(name = "Distance from Shore (m)", 
                         n.breaks = 7, 
                         low = "#c7dbff", high = "#bf49ff") +
        geom_coastline(bb = cofbb::get_bb("nefsc_carcharodon", form = "bb")) +
        ggplot2::labs(caption = cfg$version) +
        theme_void()
        if (plot_points) {
          dfs_plot = dfs_plot +
            geom_sf(data = mon_shark_obs, 
                  aes(shape = basisOfRecord), 
                  fill = "white", 
                  show.legend = "point") +
            scale_shape_manual(name = "Method", 
                               values = cfg$graphics$BOR_symbol)
        }
      if (plot_contour) {
        dfs_plot = dfs_plot +
          geom_sf(data = mask_contour, color = "white")
      }
      ggsave(filename = sprintf("%s_dfs.png", cfg$version), 
             plot = dfs_plot, 
             path = file.path(vpath, "figures"), 
             width = 11, height = 8.5, units = "in", dpi = 300,
             create.dir = TRUE)
      
      
    }
    
    
    
  }
  }
}

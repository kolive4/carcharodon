#' Given a set of points and a target raster geometry, compute the density (count)
#' of points in each cell.
#' 
#' @param x sf POINT object
#' @param y stars or SpatRaster object that defines the geometry of the output
#' @param name chr, the name of the output variable
#' @param dilate num, the size of the square structuring element used for dilating the
#'   output counts (padding with zeroes).  Set to 0 to skip. 
#' @param dilate_value num, if dilation occurs, this is the value assigned to the padded cells
#' @param mask `SpatRaster` or `stars` object that defines masked areas where
#'   dilation does not occur with the value NA.  It must have the same spatial 
#'   geometry as the input \code{y}.
#' @return 
rasterize_point_density <- function(x, y, 
                                    name = "count",
                                    dilate = 0,
                                    dilate_value = 1,
                                    mask = y){
  
  if (inherits(y, "stars")){
    # if y has scrambled coords reorganize as x,y,z
    # then be sure we have just one variable and one band (ie simple 2d geometry)
    y = stars:::st_upfront(y)
    y = y[1]
    d = dim(y)
    if(length(d) > 2){
      y = dplyr::slice(y, names(d)[3], 1)  
    }
    
    # cast as point data
    v = sf::st_as_sf(y)
    # trim the points to just a "name" attribute
    x = dplyr::mutate(x, {{ name }} := 1) |>
      dplyr::select(dplyr::all_of(name))
    # aggregate by counting the instances of x in each element of y 
    # (or the polygonized-by-cell "v" version) and then cast back to 
    # the template raster
    r = aggregate(x, v, FUN = length) |>
      stars::st_rasterize(template = y, align = TRUE)
  } else {
    r = terra::rasterize(x, y, fun = "count")
  }
  
  
  if (dilate[1] > 0){
    
    if (inherits(r, 'stars')){
      raw = r[[1]][]
    } else {
      raw = as.matrix(r)
    }
    
    raw = (!is.na(raw)) * 1.0
    
    m = raw |> 
      imager::as.cimg() |>
      imager::dilate_square(dilate[1]) |>
      as.matrix()
    
    if (!is.null(mask)){
      if (inherits(mask, 'stars')){
        if (inherits(mask[[1]], "factor")) mask[[1]] <- as.numeric(mask[[1]])
        ix = is.na(mask[[1]][]) 
      } else {
        ix = is.na(as.matrix(mask))    
      }
      m[ix] <- NA
      # in case the edge cases produce unwanted zeroes (like over land)
      m[m <= 0] <- NA
    }
    
    # here we transfer the padded zeroes to the count data
    if (inherits(r, "stars")){
      s = r[[1]]
      ix <- (!is.na(s)) | (is.na(m))
      s[!ix] <- dilate_value
      r[[1]] <- s
    } else {
      s = r[[1]]
      ix <- (!is.na(s)) | (is.na(m))
      s[!ix] <- dilate_value
      r[[1]] <- s
    }
  }
  
  r
}


#' Sample (possibly weighted) time relative to a time series 
#' 
#' @param x a vector of Date values (may be unordered)
#' @param size num, the number of samples
#' @param by char, the time step to sample ("day", "month" or "year")
#' @param weighted logical, if TRUE use the distribution of \code{x} to create a sampling weight
#' @param ... any other arguments for \code{sample}
#' @return Date class vector
sample_time = function(x = read_obis(form = "sf") |>
                         dplyr::filter(date >= as.Date("2000-01-01")) |>
                         dplyr::pull(date), 
                       size = 100,
                       by = c("day", "month", "year")[2],
                       weighted = FALSE,
                       ...){
  
  d = switch(tolower(by[1]),
             "month" = format(x, "%Y-%m-01") |> as.Date(),
             "year" = format(x, "%Y-01-01") |> as.Date(),
             x)
  r = range(d)
  if(r[1] == r[2]){
    s = rep(d[1], size)
  } else {
    b = seq(from = r[1], to = r[2], by = by)
    nb = length(b)
    if (weighted){
      H = hist(d, breaks = b, plot = FALSE)
      s = sample(b[seq(from = 1, to = nb-1, by = 1)], size = size, prob = H$density, ...)
    } else {
      s = sample(b[seq(from = 1, to = nb-1, by = 1)], size = size, ...)
    }
  }
  s
}

#' Slicing function
#' 
#' @param obs obs data
#' @param dyn dynamic data
#' @param static static variables
#' @param figure_path path to save figures to
#' @param tidy_data_path path to save data to
#' @param odd_vars variables not included in final model
#' @return obs and dyn data that has been sliced
predict_by_mon = function(obs, dyn, static, mask = NULL, figure_path = NULL, tidy_data_path = NULL, odd_vars = FALSE, ...){
  u_mon = unique(obs$month)
  lapply(u_mon, function(mon){
    cat("Month is", mon, "\n")
    x = obs |>
      dplyr::filter(month == mon)
    d = dyn |>
      dplyr::slice(along = "band", index = mon)
    
    if (odd_vars == TRUE) {
      if (mon %in% c(12, 1, 2, 3, 4, 5)) {
        vars = c(depth, dfs, fall_fish)
        static = dplyr::select(static, dplyr::all_of(names(vars)))
      } else {
        vars = c(depth, dfs, spring_fish)
        static = dplyr::select(static, dplyr::all_of(names(vars)))
      }
    } else {
      static = static
    }
    
    d = twinkle::bind_attrs(list(d, static))
    figure_path = file.path(figure_path, mon)
    ok = dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)
    tidy_data_path = file.path(tidy_data_path, mon)
    ok = dir.create(tidy_data_path, recursive = TRUE, showWarnings = FALSE)
    predict_1(x, d, mask = mask, figure_path = figure_path, tidy_data_path = tidy_data_path, ...)
  })
}

#' Splitting things by month
#' 
#' @param x presliced obs
#' @param d presliced dynamic and static data 
#' @param mask mask to base thinning
#' @param thin logical, do we want to thin or not
#' @param figure_path path to save figures to
#' @param tidy_data_path path to save data to
#' @return hsi map as a stars map
predict_1 = function(x, d, mask = NULL, thin = TRUE, figure_path = NULL, tidy_data_path = NULL){
  thinned_obs = tidysdm::thin_by_cell(x, mask) |>
    thin_by_dist(dist_min = tidysdm::km2m(20))
  png(filename = file.path(figure_path, "thinned_obs.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  plot(mask, breaks = "equal", axes = TRUE, reset = FALSE)
  plot(sf::st_geometry(thinned_obs), pch = "+", add = TRUE)
  plot(coast, col = "orange", add = TRUE)
  dev.off()
  
  observation_density_raster = rasterize_point_density(x, mask, dilate = 3)
  png(filename = file.path(figure_path, "obs_density.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  plot(observation_density_raster)
  dev.off()
  
  model_input = tidysdm::sample_background(
    data = thinned_obs,
    raster = observation_density_raster,
    n = 2*nrow(thinned_obs),
    method = "bias",
    class_label = "background",
    return_pres = TRUE
  ) |>
    dplyr::mutate(time = lubridate::NA_Date_, .after = 1)
  
  ix = model_input$class == "presence"
  model_input$time[ix] = thinned_obs$month

  png(filename = file.path(figure_path, "model_input.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  plot(model_input['class'], pch = 1, cex = 0.2,reset = FALSE, axes = TRUE)
  plot(coast, col = "black", add = TRUE)
  dev.off()
  
  nback = sum(!ix)
  days_sample = sample_time(x$eventDate,
                            size = nback,
                            by = "month",
                            replace = TRUE,
                            weighted = TRUE)
  model_input$time[!ix] = days_sample
  
  preds = d
  
  input_data = stars::st_extract(preds, at = model_input)|>
    sf::st_as_sf() |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::all_of(names(preds)))
  model_input = dplyr::bind_cols(model_input, input_data) |>
    dplyr::select(-dplyr::all_of("time")) |>
    dplyr::relocate(dplyr::all_of("class"), .before = 1) #|>
    # dplyr::glimpse()
  
  model_input = model_input |>
    dplyr::select(dplyr::all_of(c("class", names(preds)))) |>
    na.omit()
  pres_vs_bg = tidysdm::plot_pres_vs_bg(model_input, class)
  png(filename = file.path(figure_path, "pres_vs_bg_preds.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(pres_vs_bg)
  dev.off()
  
  model_input |>
    dplyr::select(dplyr::all_of(c("class", names(preds)))) |>
    tidysdm::dist_pres_vs_bg(class)
  
  png(filename = file.path(figure_path, "preds_matrix.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(pairs(preds))
  dev.off()
  
  vars_uncor = tidysdm::filter_collinear(preds, cutoff = 0.7, 
                                         method = "cor_caret", verbose = TRUE)
  vars_uncor
  preds = dplyr::select(preds, dplyr::all_of(vars_uncor))
  
  model_input = dplyr::select(model_input,
                              dplyr::all_of(c("class", names(preds)))) |>
    sf::write_sf(file.path(tidy_data_path, "model_input.gpkg"))
  
  rec = recipe(head(model_input),
               formula = class ~ .)
  
  models = workflow_set(preproc = list(default = rec),
                        models = list(glm = tidysdm::sdm_spec_glm(),
                                      rf = tidysdm::sdm_spec_rf(),
                                      gbm = tidysdm::sdm_spec_boost_tree(),
                                      maxent = tidysdm::sdm_spec_maxent()),
                        cross = TRUE) |>
    option_add(control = tidysdm::control_ensemble_grid())
  
  input_cv = spatial_block_cv(data = model_input, v = 3, n = 5)
  
  folds = autoplot(input_cv)
  png(filename = file.path(figure_path, "folds.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(folds)
  dev.off()
  
  models = models |>
    workflow_map("tune_grid",
                 resamples = input_cv,
                 grid = 3,
                 metrics = tidysdm::sdm_metric_set(),
                 verbose = TRUE)
  model_eval = autoplot(models)
  png(filename = file.path(figure_path, "model_eval.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(model_eval)
  dev.off()
  
  ensemble = simple_ensemble() |>
    add_member(models, metric = "roc_auc")
  ensemble_eval = autoplot(ensemble)
  png(filename = file.path(figure_path, "ensemble_eval.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(ensemble_eval)
  dev.off()
  
  ensemble_metrics = ensemble |>
    collect_metrics()
  ok = dir.create(tidy_data_path, recursive = TRUE)
  write.csv(ensemble_metrics , file.path(tidy_data_path, "ensemble_metrics.csv"))
  
}



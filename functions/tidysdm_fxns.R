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

#' Write and read models and ensembles.
#' 
#' @param x a model or ensemble
#' @param filename chr, the name of the file to write to
#' @param ... extra keywords passed to [`readr::write_rds`] and [`readr::read_rds`].
#' @return a model or ensemble (invisibly for writing)
write_model = function(x, filename = "model.rds", ...){
  readr::write_rds(x, filename, ...)
}

#' @rdname write_model
read_model = function(filename, ...){
  readr::read_rds(filename, ...)
}

#' @rdname write_model
write_ensemble = function(x, filename = "ensemble.rds", ...){
  readr::write_rds(x, filename, ...)
}

#' @rdname write_model
read_ensemble = function(filename, ...){
  readr::read_rds(filename, ...)
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
#' @param thinned pre-thinned observation data
#' @param preds combined dynamic and static predictors
#' @param figure_path path to save figures to
#' @param tidy_data_path path to save data to
#' @param model_path path to save models to
#' @param v number of folds in the cross folding step
#' @return obs and dyn data that has been sliced
predict_by_mon = function(obs, thinned, preds, mask = NULL, figure_path = NULL, tidy_data_path = NULL, model_path = NULL, v = 10, ...){
  if(FALSE){
    mon = 8
    obs = obs
    thinned = thinned_obs
    preds = preds 
    mask = mask
    figure_path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidysdm_workflow/figures"
    tidy_data_path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/data/tidysdm"
    model_path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidysdm_workflow/models"
    v = 10
  }
  u_mon = unique(obs$month)
  lapply(u_mon, function(mon){
    cat("Month is", mon, "\n")
    x = obs |>
      dplyr::filter(month == mon)
    thinned = thinned |>
      dplyr::filter(month == mon)
    preds = preds |>
      dplyr::slice(along = "band", index = mon)
    figure_path = file.path(figure_path, mon)
    ok = dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)
    tidy_data_path = file.path(tidy_data_path, mon)
    ok = dir.create(tidy_data_path, recursive = TRUE, showWarnings = FALSE)
    model_path = file.path(model_path, mon)
    ok = dir.create(model_path, recursive = TRUE, showWarnings = FALSE)
    predict_1(x, thinned, preds, mask = mask, figure_path = figure_path, tidy_data_path = tidy_data_path, model_path = model_path, v)
  })
}


#' Splitting things by month
#' 
#' @param x presliced obs
#' @param thinned presliced thinned obs
#' @param preds presliced predictive covariate data 
#' @param mask mask to base thinning
#' @param thin logical, do we want to thin or not
#' @param figure_path path to save figures to
#' @param tidy_data_path path to save data to
#' @param model_path path to save models to
#' @param v number of folds for cross folding step
#' @return hsi map as a stars map
predict_1 = function(x, thinned, preds, mask = NULL, thin = TRUE, figure_path = NULL, tidy_data_path = NULL, model_path = NULL, v = 10){

  observation_density_raster = rasterize_point_density(x, mask, dilate = 3)
  png(filename = file.path(figure_path, "obs_density.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  plot(observation_density_raster)
  dev.off()
  
  model_input = tidysdm::sample_background(
    data = thinned,
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
  
  input_data = stars::st_extract(preds, at = model_input)|>
    sf::st_as_sf() |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::all_of(names(preds)))
  model_input = dplyr::bind_cols(model_input, input_data) |>
    dplyr::select(-dplyr::all_of("time")) |>
    dplyr::relocate(dplyr::all_of("class"), .before = 1) 
  
  model_input = model_input |>
    dplyr::select(dplyr::all_of(c("class", names(preds)))) |>
    na.omit()
  pres_vs_bg = tidysdm::plot_pres_vs_bg(model_input, class)
  png(filename = file.path(figure_path, "pres_vs_bg_preds.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(pres_vs_bg)
  dev.off()
  
  model_input |>
    dplyr::select(dplyr::all_of(c("class", names(preds)))) |>
    tidysdm::dist_pres_vs_bg(class) |>
    write.csv(file.path(figure_path, "dist_pres_vs_bg.csv"))
  
  png(filename = file.path(figure_path, "preds_matrix.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(pairs(preds))
  dev.off()
  
  vars_uncor = tidysdm::filter_collinear(preds, cutoff = 0.7, 
                                         method = "vif_cor", verbose = TRUE)
  vars_uncor
  vars_keep = c("mld", "hseal", "tbtm", "gseal", "sbtm", "log_depth")
  preds = dplyr::select(preds, dplyr::all_of(vars_keep))
  
  model_input = dplyr::select(model_input,
                              dplyr::all_of(c("class", names(preds)))) |>
    na.omit() |>
    sf::write_sf(file.path(tidy_data_path, "model_input.gpkg"))
  
  rec = recipe(head(model_input),
               formula = class ~ .)
  
  glm_models = workflow_set(preproc = list(default = rec),
                        models = list(glm = tidysdm::sdm_spec_glm(),
                                      rf = tidysdm::sdm_spec_rf(),
                                      gbm = tidysdm::sdm_spec_boost_tree(),
                                      maxent = tidysdm::sdm_spec_maxent()),
                        cross = TRUE) |>
    option_add(control = tidysdm::control_ensemble_grid())
  
  input_cv = spatial_block_cv(data = model_input, v = v, n = 5)
  
  folds = autoplot(input_cv)
  png(filename = file.path(figure_path, "folds.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(folds)
  dev.off()
  
  models = models |>
    workflow_map("tune_grid",
                 resamples = input_cv,
                 grid = 5,
                 metrics = tidysdm::sdm_metric_set(),
                 verbose = TRUE)
  model_eval = autoplot(models)
  png(filename = file.path(figure_path, "model_eval.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(model_eval)
  dev.off()
  
  glm = models |>
    dplyr::filter(wflow_id == "default_glm") |>
    write_model(filename = file.path(model_path, "glm_model.rds"))
  rf = models |>
    dplyr::filter(wflow_id == "default_rf") |>
    write_model(filename = file.path(model_path, "rf_model.rds"))
  gbm = models |>
    dplyr::filter(wflow_id == "default_gbm") |>
    write_model(filename = file.path(model_path, "gbm_model.rds"))
  maxent = models |>
    dplyr::filter(wflow_id == "default_maxent") |>
    write_model(filename = file.path(model_path, "maxent_model.rds"))
  
  tss_ensemble = simple_ensemble() |>
    add_member(models, metric = "tss_max")
  tss_ensemble_eval = autoplot(tss_ensemble)
  png(filename = file.path(figure_path, "tss_ensemble_eval.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(tss_ensemble_eval)
  dev.off()
  write_ensemble(tss_ensemble, filename = file.path(model_path, "tss_ensemble.rds"))
  
  boyce_ensemble = simple_ensemble() |>
    add_member(models, metric = "boyce_cont")
  boyce_ensemble_eval = autoplot(boyce_ensemble)
  png(filename = file.path(figure_path, "boyce_ensemble_eval.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(boyce_ensemble_eval)
  dev.off()
  write_ensemble(boyce_ensemble, filename = file.path(model_path, "boyce_ensemble.rds"))  
  
  auc_ensemble = simple_ensemble() |>
    add_member(models, metric = "roc_auc")
  auc_ensemble_eval = autoplot(auc_ensemble)
  png(filename = file.path(figure_path, "auc_ensemble_eval.png"), bg = "transparent", width = 11, height = 8.5, units = "in", res = 300)
  print(auc_ensemble_eval)
  dev.off()
  write_ensemble(auc_ensemble, filename = file.path(model_path, "auc_ensemble.rds"))  
  tss_ensemble_metrics = tss_ensemble |>
    collect_metrics()
  write.csv(tss_ensemble_metrics , file.path(model_path, "tss_ensemble_metrics.csv"))
  
  boyce_ensemble_metrics = boyce_ensemble |>
    collect_metrics()
  write.csv(boyce_ensemble_metrics , file.path(model_path, "boyce_ensemble_metrics.csv"))
  
  auc_ensemble_metrics = auc_ensemble |>
    collect_metrics()
  write.csv(auc_ensemble_metrics , file.path(model_path, "auc_ensemble_metrics.csv"))
}


plot_roc = function(x, truth, pred, title = "ROC"){
  
  #' Plot an annotated ROC with AUC
  #' 
  #' @param x table of predictive outcomes - see `predict_model`
  #' @param truth the truth column, usually `class`
  #' @param pred the prediction column, usually .pred_presense
  #' @param title chr the optional title
  #' @return ggplot2 object suitable for printing
  
  auc = yardstick::roc_auc(x, {{truth}}, {{pred}}) |>
    dplyr::pull(.estimate)
  roc_curve(x, {{truth}}, {{pred}}) |>
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(lty = 3) +
    coord_equal() +
    theme_bw() + 
    labs(x = "False Positive Rate (Specificity)",
         y = "True Positive Rate (Sensitivity)",
         title = title) + 
    ggplot2::annotate("text", x = 0.8, y= 0.05, 
                      label = sprintf("AUC: %0.3f", auc)) 
}

predict_stars = function(wflow, newdata, type = "prob", threshold = 0.5, ...){
  
  #' Predict a classification from stars object
  #' 
  #' @param wflow a workflow object
  #' @param newdata stars data
  #' @param typ chr the type of prediction, by default "prob" - see `?predict.model_fit`
  #' @param threshold num the aribitray threshold used to define the outcome
  #' @param ... other arguments for `predict.model_fit`
  #' @return a stars object
  lvls = c("presence", "background")
  predict_raster(wflow, newdata, type = type, ...) |>
    dplyr::mutate(.pred = if_else(.pred_presence >= threshold, lvls[1], lvls[2]) |>
                    factor(levels = lvls))
}

partial_dependence_plot = function(x, 
                                   share_y = "all", 
                                   filename = NULL, 
                                   v = NULL,
                                   data = NULL,
                                   outcome = "class",
                                   ...){
  
  #' Given a workflow or fitted model generate a partial dependence plot
  #' 
  #' @param x workflow with a fitted model
  #' @param share_y chr, by default "all" but see `?plot.EffectsData`
  #' @param v chr, variable names `?plot.EffectsData`. Not needed for workflow.
  #' @param data table with outcomes and predictors. Not needed for workflow.
  #' @param outcome chr, the name of the column (variable) that defines the outcome
  #' @param ... other arguments for `?plot.EffectsData`
  #' @return plot object as ggplot or plotly see `?plot.EffectsData`
  
  if (inherits(x, "workflow")){
    stopifnot(is_trained_workflow(x))
    # extract the mold
    mold = workflows::extract_mold(x)
    # build training data
    data = dplyr::bind_cols(mold$outcomes, mold$predictors)
    # extract the fitted model engine
    x = workflows::extract_fit_engine(x)
    v = colnames(mold$predictors)
  } else {
    if (is.null(data)) stop("if not proving a workflow, please provide data and v")
    nm = colnames(data)
    v = nm[!(nm %in% outcome)]
    x = x$fit
  }
  # compute the partial effects
  if(inherits(x, "maxnet")) {
    pd = effectplots::partial_dependence(x, 
                                         v = v, 
                                         data = data,
                                         prob = TRUE,
                                         type = "cloglog")
  } else if (inherits(x, "xgb.Booster")) {
    pd = effectplots::partial_dependence(x, 
                                         v = v, 
                                         data = data |>
                                           dplyr::select(-class) |>
                                           data.matrix())
  } else {
    pd = effectplots::partial_dependence(x, 
                                         v = v, 
                                         data = data)
  }
  
  # plot
  p = plot(pd, share_y = share_y, ylim = c(0, 1), ...)
  # save if requested
  if (!is.null(filename)){
    ggplot2::ggsave(filename, plot = p)
  }
  
  return(p)
}

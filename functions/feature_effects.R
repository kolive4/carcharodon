#' @describeIn feature_effects Method for ranger models.
#' @export
feature_effects.xgb.Booster <- function(
    object,
    v,
    data,
    y = NULL,
    pred = NULL,
    pred_fun = NULL,
    trafo = NULL,
    which_pred = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 13L,
    outlier_iqr = 2,
    calc_pred = TRUE,
    pd_n = 500L,
    ale_n = 50000L,
    ale_bin_size = 200L,
    ...
) {
  if (is.null(pred_fun)) {
    pred_fun <- function(model, newdata, ...) {
      xgboost:::predict.xgb.Booster(model, newdata, ...)$predictions
    }
  }
  feature_effects(
    object,
    v = v,
    data = data,
    y = y,
    pred = pred,
    pred_fun = pred_fun,
    trafo = trafo,
    which_pred = which_pred,
    w = w,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    calc_pred = calc_pred,
    pd_n = pd_n,
    ale_n = ale_n,
    ale_bin_size = ale_bin_size,
    prob = TRUE
  )
}


#' @describeIn feature_effects Default method.
#' @export
feature_effects.default <- function(
    object,
    v,
    data,
    y = NULL,
    pred = NULL,
    pred_fun = stats::predict,
    trafo = NULL,
    which_pred = NULL,
    w = NULL,
    breaks = "Sturges",
    right = TRUE,
    discrete_m = 13L,
    outlier_iqr = 2,
    calc_pred = TRUE,
    pd_n = 500L,
    ale_n = 50000L,
    ale_bin_size = 200L,
    seed = NULL,
    ...
) {
  # Input checks
  stopifnot(
    is.data.frame(data) || is.matrix(data),
    length(v) >= 1L,
    v %in% colnames(data),
    is.function(pred_fun),
    is.null(trafo) || is.function(trafo),
    pd_n >= 0L,
    ale_n >= 0L,
    ale_bin_size >= 0L
  )
  n <- nrow(data)
  nms <- colnames(data)
  stopifnot(
    n >= 2L,
    basic_check(y, n = n, nms = nms),
    basic_check(w, n = n, nms = nms)
  )
  
  if (!is.null(seed)) {
    # old <- .Random.seed
    # on.exit({.Random.seed <- old})
    set.seed(seed)
  }
  
  # Prepare y (NAs are checked after preparing w)
  if (!is.null(y)) {
    if (length(y) == 1L) {
      y <- if (is.matrix(data)) data[, y] else data[[y]]
    } else {
      y <- unname(y)
    }
    if (!is.numeric(y) && !is.logical(y)) {
      stop("'y' must be numeric or logical.")
    }
    if (!is.double(y)) {
      y <- as.double(y)
    }
  }
  
  # Prepare pred (part 1)
  if (!is.null(pred)) {
    if (NROW(pred) != n) {
      stop("'pred' must have the same length as nrow(data).")
    }
    pred <- prep_pred(pred, trafo = trafo, which_pred = which_pred)
  }
  
  # Prepare w
  if (!is.null(w)) {
    if (length(w) == 1L) {
      w <- if (is.matrix(data)) data[, w] else data[[w]]
    }
    if (!is.numeric(w) && !is.logical(w)) {
      stop("'w' must be numeric, or logical.")
    }
    wpos <- !is.na(w) & w > 0
    if (!any(wpos)) {
      stop("No positive 'w'!")
    }
    if (!all(wpos)) {
      message("Removing data with missing or non-positive weights 'w'.")
      w <- w[wpos]
      data <- collapse::ss(data, wpos)
      stopifnot(nrow(data) >= 2L)
      if (!is.null(y)) {
        y <- y[wpos]
      }
      if (!is.null(pred)) {
        pred <- pred[wpos]
      }
    }
    if (!is.double(w)) {
      w <- as.double(w)
    }
  }
  
  if (!is.null(y) && anyNA(y)) {
    # We do this check after preparing w. This allows a user to pass weights in {0, NA}
    stop("'y' can't contain NA")
  }
  
  # Prepare pred (part 2)
  if (is.null(pred) && isTRUE(calc_pred)) {
    pred <- prep_pred(
      pred_fun(object, data, ...), trafo = trafo, which_pred = which_pred
    )
  }
  
  # Check X variables
  if (is.matrix(data)) {
    stopifnot("Matrix is of wrong type" = check_v_type(data))
  } else {
    ok <- vapply(collapse::ss(data, , v), check_v_type, FUN.VALUE = logical(1L))
    if (!all(ok)) {
      stop("Unsupported data type for ", paste(v, collapse = ", "))
    }
  }
  
  # We need this subset for fast quartiles and fast check if numeric x is discrete
  ix_sub <- if (nrow(data) > 9997L) sample.int(nrow(data), 9997L)
  
  # Combine pred, y, and resid. If df, we can easier drop columns in grouped_stats()
  PYR <- list(pred = pred, y = y, resid = if (!is.null(pred) && !is.null(y)) y - pred)
  wPYR <- lengths(PYR) > 0L
  PYR <- if (any(wPYR)) collapse::qDF(PYR[wPYR])
  
  # Prepare pd_data and ale_data (list with data, w, ix)
  pd_data <- if (pd_n > 0L) .subsample(data, nmax = pd_n, w = w)
  ale_data <- if (ale_n > 0L) .subsample(data, nmax = ale_n, w = w)
  
  # Prepare breaks
  nv <- length(v)
  if (is.list(breaks)) {
    if (length(breaks) < nv) {
      br <- replicate(nv, "Sturges", simplify = FALSE)
      names(br) <- v
      br[names(breaks)] <- breaks
      breaks <- br
    }
  } else {
    breaks <- replicate(nv, breaks, simplify = FALSE)
  }
  
  # We want to pass a list/data.frame to mapply(). For high length(v)/ncol(data),
  # the approach via qDF takes significantly less memory and time.
  if (is.matrix(data)) {
    if (nv <= ceiling(2 / 3 * ncol(data))) {
      data <- lapply(v, function(i) data[, i])
    } else {
      data <- collapse::qDF(data)
    }
  }
  
  out <- mapply(
    FUN = calculate_stats,
    v,
    x = if (is.data.frame(data)) collapse::ss(data, , v) else data,
    breaks = breaks,
    right = right,
    discrete_m = discrete_m,
    outlier_iqr = outlier_iqr,
    MoreArgs = list(
      PYR = PYR,
      w = w,
      object = object,
      pred_fun = pred_fun,
      trafo = trafo,
      which_pred = which_pred,
      pd_data = pd_data,
      ale_data = ale_data,
      ale_bin_size = ale_bin_size,
      ix_sub = ix_sub,
      ...
    ),
    SIMPLIFY = FALSE
  )
  
  # Remove empty results (happens if feature is discrete and only ALE was calculated)
  ok <- lengths(out) > 0L  # non-null (has some columns)
  if (!all(ok)) {
    if (!any(ok)) {
      stop("Nothing has been calculated!")
    }
    message(
      "Dropping variables without results: ", paste(names(out)[!ok], collapse = ", ")
    )
    out <- out[ok]
  }
  structure(out, class = "EffectData")
}

#' pull data from partial dependence
#' 
#' @param x list of partial dependence dataframes
#' @return list of covariates with partial dependence for month
pd_cov = function(x) {
  if (FALSE) {
    x = rf_pd
  }
  covars = names(x[[1]])
  xx = lapply(covars,
              function(covar){
                y = lapply(names(x),
                           function(mon){
                             x[[mon]][[covar]] |>
                               dplyr::as_tibble() |>
                               dplyr::mutate(covar = covar, month = mon, .before = 1)
                           }) |>
                  dplyr::bind_rows()
                
              }) |>
    dplyr::bind_rows()
}

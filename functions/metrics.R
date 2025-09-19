#' Function to calculate percent deviance explained from the model
#' 
#' @export
#' @param probs model object with numeric values
#' @param truth testing split of data
#' @return percent deviance explained value
perdev_ex = function(probs, truth, eps = 1e-15){
  if (FALSE) {
    truth = p_maxent$class
    probs = p_maxent$.pred_presence
  }
  p = pmin(pmax(probs, eps), 1 - eps) # set up clamps on probabilities to avoid 0's and 1's in log calcs
  y = as.numeric(truth == "presence")
  
  resid_dev = -2 * sum(y * log(p) + (1-y) * log1p(-p))
  # resid_dev2 = sum(-2 * (y * log(p) + (1-y)*log(1-p))) https://library.virginia.edu/data/articles/understanding-deviance-residuals
  
  p_null = mean(y)
  null_dev = -2 * sum(y * log(p_null) + (1-y) * log1p(-p_null))
  
  pdev = 1 - resid_dev / null_dev
  return(pdev)
  
}
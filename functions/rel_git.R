#' Function to provide relative paths of png casts to a github_document version of markdown
#' 
#' @param version report version with figures
#' @param model_type model type, one of rf, bt, maxent, gam, or glm
#' @param filename cast figure filename
#' @param base_dir base directory to build out the relative path
#' @return relative path to casts suited for github_document rendering
rel_cast_path = function(version = "c21.100671.01_12", 
                    model_type = "rf", 
                    filename = "_compiled_cast.png", 
                    base_dir = "tidy_reports/versions") {
  if (FALSE) {
    version = "c21.100671.01_12"
    model_type = "rf"
    filename = "_compiled_cast.png"
    base_dir = "tidy_reports/versions"
  }
  parsed = charlier::parse_version(version)
  file.path("..", "..", "..", "..", base_dir, parsed["major"], parsed["minor"], paste0(version, "_", model_type, filename))
}


#' Function to determine which path to figures to use based on what type of doc rendered
#' 
#' @param root root directory from cfg
#' @param version workflow version to pull casts from
#' @param filename cast figure filename
#' @param model_type which model
fig_path = function(root, version, filename = "_compiled_casts.png", model_type) {
  parsed = charlier::parse_version(version)
  
  # Where the file actually is, for reading it during render
  abs_path = file.path(root, "tidy_reports", "versions", parsed["major"], parsed["minor"], paste0(version, "_", model_type, filename))
  
  # Where the file *should appear* in the final .md for GitHub
  rel_path = rel_cast_path(version, model_type)
  
  list(abs = abs_path, rel = rel_path)
}

embed_fig <- function(path_objs) {
  if (!is.list(path_objs[[1]])) {
    # Single path object
    path_objs <- list(path_objs)
  }
  
  if (knitr::is_html_output()) {
    # Use absolute paths for rendering to HTML
    abs_paths <- vapply(path_objs, function(x) x$abs, character(1))
    knitr::include_graphics(abs_paths)
  } else {
    # For GitHub: emit markdown syntax using relative paths
    rel_paths <- vapply(path_objs, function(x) x$rel, character(1))
    md_links <- paste0("![](", rel_paths, ")")
    knitr::asis_output(paste(md_links, collapse = "\n"))
  }
}
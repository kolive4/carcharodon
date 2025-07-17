#' Function to provide relative paths of png casts to a github_document version of markdown
#' 
#' @param version report version with figures
#' @param model_type model type, one of rf, bt, maxent, gam, or glm
#' @param filename cast figure filename
#' @param base_dir base directory to build out the relative path
#' @return relative path to casts suited for github_document rendering
rel_cast_path = function(version = "c21.100671.01_12", 
                    model_type = "rf", 
                    filename = "_compiled_casts.png", 
                    base_dir = "tidy_reports/versions") {
  if (FALSE) {
    version = "c21.100671.01_12"
    model_type = "rf"
    filename = "_compiled_casts.png"
    base_dir = "tidy_reports/versions"
  }
  parsed = charlier::parse_version(version)
  file.path("../../../..", base_dir, parsed["major"], parsed["minor"], paste0(version, "_", model_type, filename))
}


#' Function to determine which path to figures to use based on what type of doc rendered
#' 
#' @param root root directory from cfg
#' @param version workflow version to pull casts from
#' @param filename cast figure filename
#' @param model_type which model
fig_path = function(root, version, filename = "_compiled_casts.png", model_type) {
  if (FALSE) {
    root = cfg$root_path
    version = cfg$t_rep_now_version
    model_type = "rf"
  }
  
  parsed = charlier::parse_version(version)
  
  # Where the file actually is, for reading it during render
  abs_path = file.path(root, "tidy_reports", "versions", parsed["major"], parsed["minor"], paste0(version, "_", model_type, filename))
  
  # Where the file *should appear* in the final .md for GitHub
  rel_path = rel_cast_path(version, model_type)
  
  list(abs = abs_path, rel = rel_path)
}


embed_fig = function(path_objs, labels = NULL, width = NULL) {
  # Determine output format
  fmt = knitr::opts_knit$get("rmarkdown.pandoc.to")
  is_gfm = !is.null(fmt) && grepl("gfm", fmt, ignore.case = TRUE)
  
  # Extract relative image paths for GitHub
  rel_paths = vapply(path_objs, function(x) x$rel, character(1))
  
  if (is_gfm) {
    # If labels provided, build a markdown table header
    if (!is.null(labels)) {
      stopifnot(length(labels) == length(rel_paths))
      header = paste0("| ", paste(labels, collapse = " | "), " |")
      sep = paste0("|", paste(rep(":--:", length(labels)), collapse = "|"), "|")
      imgs = paste0("![](", rel_paths, ")")
      body = paste0("| ", paste(imgs, collapse = " | "), " |")
      md_table = paste(header, sep, body, sep = "\n")
      return(knitr::asis_output(md_table))
    } else {
      # No labels, just output images stacked
      imgs = paste0("![](", rel_paths, ")")
      return(knitr::asis_output(paste(imgs, collapse = "\n")))
    }
  } else {
    # HTML output: use include_graphics with widths if specified
    abs_paths = vapply(path_objs, function(x) x$abs, character(1))
    if (!is.null(width)) {
      return(knitr::include_graphics(abs_paths, options = list(out.width = width)))
    } else {
      return(knitr::include_graphics(abs_paths))
    }
  }
}

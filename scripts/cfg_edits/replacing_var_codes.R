library(stringr)
library(fs)

# Root folder
root_dir <- "workflows/tidy_workflow/versions/t11"

cfgs = list.files("workflows/tidy_workflow", pattern = "^t11\\.[01]005[0-5]\\.[0-2]0", full.names = TRUE)

# Find all paths (files and folders)
all_paths <- dir(root_dir, recursive = TRUE, full.names = TRUE)

# Filter those with version codes containing '003'
matching_paths <- all_paths[str_detect(all_paths, "t11\\.[01]005[0-5]\\.[0-2]0")]

# Define rename function
replace_005_with_003 <- function(path) {
  str_replace_all(path, "(?<=t11\\.[01])005", "003") |>
    str_replace_all("(?<=/)[01]005", function(x) paste0(substr(x, 1, 1), "003"))
}

# Generate new paths
renamed_paths <- replace_005_with_003(matching_paths)

# Preview changes
preview <- data.frame(from = matching_paths, to = renamed_paths)
print(preview)

# Rename directories first (sorted deepest first)
dir_matches <- matching_paths[dir_exists(matching_paths)]
dir_matches <- dir_matches[order(nchar(dir_matches), decreasing = TRUE)]
dir_renamed <- replace_005_with_003(dir_matches)

mapply(function(from, to) {
  if (!dir_exists(from)) return()
  dir_create(path_dir(to))
  file_move(from, to)
}, from = dir_matches, to = dir_renamed)

# Then rename files
file_matches <- matching_paths[file_exists(matching_paths)]
file_renamed <- replace_005_with_003(file_matches)

mapply(function(from, to) {
  dir_create(path_dir(to))
  file_move(from, to)
}, from = file_matches, to = file_renamed)

message("✅ All '003' versions renamed to '004'.")


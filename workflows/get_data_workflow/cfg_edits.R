files = list.files(path = "workflows/get_data_workflow", pattern = glob2rx("v01.*.yaml"), full.names = TRUE)
extra = list(inat_file = "inaturalist/observations-507834.csv")
cfgs = append_config(files, extra, write = TRUE)

files = list.files(path = "workflows/tidy_cast", pattern = "^t11\\.\\d{6}\\.\\d+\\.yaml$", full.names = TRUE)
cfg = verify_version(files, write = TRUE)
cfg


files = list.files(path = "workflows/tidy_workflow", pattern = "^t11\\.\\d{5}\\.\\d+\\.yaml$", full.names = TRUE)
copy_and_rename = function(file) {
  filename = basename(file)
  
  # Swap t12 → t22 and t13 → t23
  new_prefix = sub("^m12", "m13", filename)
  new_prefix = sub("^m22", "m23", new_prefix)
  
  # Construct full new path
  new_path = file.path("workflows/tidy_md", new_prefix)
  
  # Copy the file
  file.copy(from = file, to = new_path, overwrite = FALSE)
}
lapply(files, copy_and_rename)

files = list.files(path = "workflows/tidy_cast", pattern = "^t11\\.\\d{6}\\.\\d+\\.yaml$", full.names = TRUE)
swap_minor = function(file) {
  if (FALSE) {
    file = files[361]
  }
  version <- sub("\\.yaml$", "", basename(file))
  vpars = parse_version(version)
  
  # Change the first digit after '000' to a 3
  new_minor <- sub("003", "005", vpars["minor"])
  
  # Construct new filename
  new_filename <- paste0(vpars["major"], ".", new_minor, ".", vpars["release"], ".yaml")
  new_path <- file.path("workflows/tidy_cast", new_filename)
  
  # Copy to new filename
  file.copy(from = file, to = new_path, overwrite = FALSE)
}
lapply(files, swap_minor)


ref_cfg = read_config("workflows/tidy_cast/t11.000500.01.yaml")
ref_vars = ref_cfg$vars
ref_dyn = ref_cfg$dynamic_names

target_files = list.files(path = "workflows/tidy_cast", pattern = "^t11\\.[0-1]005[0-5][0-4]\\.\\d+\\.yaml$", full.names = TRUE)

overwrite_cfg_fields = function(file, ref_vars, ref_dyn){
  if (FALSE) {
    file = target_files[2]
  }
  cfg = read_config(file)
  cfg$vars = ref_vars
  cfg$dynamic_names = ref_dyn
  write_config(cfg, file)
}
lapply(target_files, overwrite_cfg_fields, ref_vars = ref_vars, ref_dyn = ref_dyn)



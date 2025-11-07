files = list.files(path = "workflows/tidy_md", pattern = "^m[3,4]1\\.\\d{5}\\.yaml$", full.names = TRUE)
cfg = verify_version(files, write = TRUE)
cfg

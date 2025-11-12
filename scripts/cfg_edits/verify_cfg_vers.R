files = list.files(path = "workflows/tidy_reports", pattern = "^c[1-4]1\\.\\d{6}\\.01_12\\.yaml$", full.names = TRUE)
cfg = verify_version(files, write = TRUE)
cfg

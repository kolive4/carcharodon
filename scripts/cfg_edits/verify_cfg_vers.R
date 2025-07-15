files = list.files(path = "workflows/tidy_cast", pattern = "^t[1-2]1\\.\\d{6}\\.\\d+\\.yaml$", full.names = TRUE)
cfg = verify_version(files, write = TRUE)
cfg
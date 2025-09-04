files = list.files(path = "workflows/tidy_workflow", pattern = "^t11\\.\\d{5}\\.\\d+\\.yaml$", full.names = TRUE)
cfg = verify_version(files, write = TRUE)
cfg
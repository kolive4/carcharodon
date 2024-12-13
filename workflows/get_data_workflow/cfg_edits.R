files = list.files(path = "workflows/get_data_workflow", pattern = glob2rx("v01.*.yaml"), full.names = TRUE)
extra = list(inat_file = "inaturalist/observations-507834.csv")
cfgs = append_config(files, extra, write = TRUE)

files = list.files(path = "workflows/forecast_workflow", pattern = glob2rx("v04.0100.*.yaml"), full.names = TRUE)
cfg = verify_version(files, write = TRUE)
cfg

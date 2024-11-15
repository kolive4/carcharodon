files = list.files(path = "workflows/forecast_workflow", pattern = glob2rx("v01.2*.yaml"), full.names = TRUE)
extra = list(fall_fish_mon = c(12, 1, 2, 3, 4, 5),
             spring_fish_mon = c(6, 7, 8, 9, 10, 11),
             which_fish = "COMBO")
cfgs = append_config(files, extra, write = TRUE)

files = list.files(path = "workflows/forecast_workflow", pattern = glob2rx("v01.070*.yaml"), full.names = TRUE)
cfg = verify_version(files, write = TRUE)
cfg

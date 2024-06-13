source("setup.R")

PATH = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon"
cfg = yaml::read_yaml(file.path(PATH, "workflows","get_data_workflow", "v0.000.yaml"))
shark_box = cofbb::get_bb(cfg$bbox_name, form = "sf")

r = subset_brickman(scenario = 'RCP45', 
                    year = 2075, 
                    vars = cfg$mon_covariates, 
                    interval = "mon", 
                    bb = shark_box, 
                    band_as_time = TRUE, 
                    path = file.path(cfg$data_path, "brickman/gom_carcharodon"))


source("../setup.R")

shark_box = cofbb::get_bb("gom_carcharodon", form = "sf")
plot_bb(shark_box)

rcp85_2055 <- subset_brickman(scenario ='RCP85', year = 2055, vars = 'SST', interval = "mon", bb = shark_box, path = here::here("data/brickman/gom_carcharodon"))
rcp45_2055 <- subset_brickman(scenario ='RCP45', year = 2055, vars = 'SST', interval = "mon", bb = shark_box, path = here::here("data/brickman/gom_carcharodon"))
rcp85_2075 <- subset_brickman(scenario ='RCP85', year = 2075, vars = 'SST', interval = "mon", bb = shark_box, path = here::here("data/brickman/gom_carcharodon"))
rcp45_2075 <- subset_brickman(scenario ='RCP45', year = 2075, vars = 'SST', interval = "mon", bb = shark_box, path = here::here("data/brickman/gom_carcharodon"))
present_brick = subset_brickman(scenario = 'PRESENT', vars = "SST", interval = "mon", bb = shark_box, path = here::here("data/brickman/gom_carcharodon"))

rcp85_2055 <- load_brickman(scenario ='RCP85', year = 2055, vars = 'SST', interval = "mon", path = here::here("data/brickman/gom_carcharodon"))
rcp45_2055 <- load_brickman(scenario ='RCP45', year = 2055, vars = 'SST', interval = "mon", path = here::here("data/brickman/gom_carcharodon"))
rcp85_2075 <- load_brickman(scenario ='RCP85', year = 2075, vars = 'SST', interval = "mon", path = here::here("data/brickman/gom_carcharodon"))
rcp45_2075 <- load_brickman(scenario ='RCP45', year = 2075, vars = 'SST', interval = "mon", path = here::here("data/brickman/gom_carcharodon"))
present_brick = load_brickman(scenario = 'PRESENT', vars = "SST", interval = "mon", path = here::here("data/brickman/gom_carcharodon"))

sum_85_55 = summary_brickman(rcp85_2055)
sum_45_55 = summary_brickman(rcp45_2055)
sum_85_75 = summary_brickman(rcp85_2075)
sum_45_75 = summary_brickman(rcp45_2075)

sum_45_55
sum_85_55
sum_45_75
sum_85_75

# minimum of the minimums is -1.46
min(sum_45_55$min)
min(sum_45_75$min)
min(sum_85_55$min)
min(sum_85_75$min)

# max of the maxs is 27.8037
max(sum_45_55$max)
max(sum_45_75$max)
max(sum_85_55$max)
max(sum_85_75$max)


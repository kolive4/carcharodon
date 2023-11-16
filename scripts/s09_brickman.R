library(brickman)

rcp85_2055 <- brickman::read_brickman(scenario ='RCP85', year = 2055, vars = 'SST', interval = "mon")
plot(rcp85_2055)

warp_rcp85_2055 = warp_brickman(rcp85_2055, crs = sf::st_crs(gom_bb))
sub = warp_rcp85_2055[bb]
plot(sub, reset = FALSE)

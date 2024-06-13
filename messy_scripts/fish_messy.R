library(stars)


fish_present_fall_multispecies = stars::read_stars("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data/fish_data/MDAT_Fish_SummaryProducts_NEFSC/commondata/raster_data/fish_biomass_nefsc_2010_2019_FALL_NEFMC_multispecies.tif")

plot(fish_present_fall_multispecies, reset = FALSE)
plot(wshark, col = "orange", pch = 16, add = TRUE)


lut_fish = make_raster_lut(fish_present_fall_multispecies)
plot(lut_fish, axes = TRUE)
plot(fish_present_fall_multispecies, axes = TRUE)
fish_mask = !is.na(fish_present_fall_multispecies)*1
plot(fish_mask)
cc = st_contour(fish_mask, breaks = 0.5)
plot(cc)

plot(lut_fish, axes = TRUE, reset = FALSE)
plot(contour, add = TRUE)
plot(st_geometry(cc), add = TRUE)
plot(wshark, add = TRUE, col = "orange", pch = 16)
plot(curated, add = TRUE, col = "orange", pch = 16)

contour = read_sf(file.path("/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon/data", cfg$polygon))


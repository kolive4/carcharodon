# This script is used to show the difference between covariates under current and RCP 8.5 2075 conditions
suppressPackageStartupMessages({
  library(stars)
  library(dplyr)
  library(brickman)
})

root_path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon"
brick_path = "data/brickman"

sst_now = stars::read_stars(file.path(root_path, brick_path, "gom_carcharodon/PRESENT_PRESENT_SST_mon.tif"))
sst_rcp85_2075 = stars::read_stars(file.path(root_path, brick_path, "gom_carcharodon/RCP85_2075_SST_mon.tif"))
sss_rcp85_2075 = stars::read_stars(file.path(root_path, brick_path, "gom_carcharodon/RCP85_2075_SSS_mon.tif"))
tbtm_rcp85_2075 = stars::read_stars(file.path(root_path, brick_path, "gom_carcharodon/RCP85_2075_Tbtm_mon.tif"))

sst_rcp85_2075 = sst_rcp85_2075 + sst_now

sst_diff = sst_rcp85_2075 - sst_now

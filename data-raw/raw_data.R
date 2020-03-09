## code to prepare `raw_data` dataset goes here

# usethis::use_data("raw_data")

library(sp)
library(rgdal)
world <- readOGR('geo/world_small/', 'TM_WORLD_BORDERS_SIMPL-0.3')
usethis::use_data(world, overwrite = T)


# Working directory
switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/fdetsch/XChange/kilimanjaro/evapotranspiration"), 
       "Windows" = setwd("D:/kilimanjaro/evapotranspiration"))

# Required packages
library(raster)

# NDVI
ndvi <- stack(list.files("myd09gq/processed/", pattern = "^NDVI", 
                         full.names = TRUE))

# SAVI
savi <- stack(list.files("myd09gq/processed/", pattern = "^SAVI", 
                         full.names = TRUE))

# F_apar
m1 <- 1.2 * 1.136
b1 <- 1.2 * (-0.04)
f.apar <- m1 * savi + b1

# F_ipar (light intercepted by the veegetated fraction of the land surface)
m2 <- 1.0
b2 <- -0.05
f.ipar <- m2 * ndvi + b2

# F_c (total fractional vegetation cover)
f.c <- f.ipar

# F_g (green fraction of the canopy that is actively transpiring)
f.g <- f.apar / f.ipar

# LAI (indication of the biophysical capacity for energy acquisition by the 
# canopy, formula taken from Norman et al., 1995)
lai <- -2 * log(f.c * (-1) + 1)


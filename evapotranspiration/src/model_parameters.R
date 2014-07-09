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

# F_apar (fraction of PAR absorbed by green vegetation cover)
m1 <- 1.2 * 1.136
b1 <- 1.2 * (-0.04)
f.apar <- m1 * savi + b1

# F_ipar (fraction of PAR intercepted by total vegetation cover)
m2 <- 1.0
b2 <- -0.05
f.ipar <- m2 * ndvi + b2

# F_c (total fractional vegetation cover)
f.c <- f.ipar

# F_g (green fraction of the canopy that is actively transpiring)
f.g <- f.apar / f.ipar

# LAI (indication of the biophysical capacity for energy acquisition by the 
# canopy, formula not taken from Norman et al., 1995 (like denoted in the paper)
# but from the paper by Fisher et al., 2008 itself)
k.par <- 0.5
lai <- (-1) * log(1 - f.c) / k.par

# T_opt (air temperature in the month when NDVI reaches annual maximum, 
# definition taken from Potter et al., 1993), based on T_max (average monthly
# maximum daily air temperature) at peak NDVI
t.max <- stack(list.files("md11a2/processed", pattern = "^RSMPL.*.tif$", 
                          full.names = TRUE))

t.opt.id <- which.max(ndvi)
t.opt.id.vec <- t.opt.id[]

t.max.mat <- as.matrix(t.max)
t.opt.vec <- sapply(1:nrow(t.max.mat), function(i) {
  t.max.mat[i, t.opt.id.vec[i]]
})

t.opt.mat <- matrix(t.opt.vec, nrow = nrow(t.max), ncol = ncol(t.max), 
                    byrow = TRUE)
t.opt <- raster(t.opt.mat, template = t.max)

# F_t (plant temperature constraint)
lambda <- t.opt
f.t <- exp(-((t.max - t.opt) / lambda)^2)


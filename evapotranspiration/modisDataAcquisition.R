# install.packages("devtools")
# library(devtools)
# install_github("Rsenal", "environmentalinformatics-marburg")
library(Rsenal)
library(MODIS)

switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/fdetsch/XChange/kilimanjaro/evapotranspiration/"), 
       "Windows" = setwd("D:/kilimanjaro/evapotranspiration/"))

MODISoptions(localArcPath = "MODIS_ARC", 
             outDirPath = "MODIS_ARC/PROCESSED")

modisDownload(modis.products = c("MYD09GA"), 
              modis.download.only = FALSE, 
              modis.outproj = "EPSG:21037", 
              begin = "2014002", 
#               end = "2002365", 
              tileH = 21, tileV = 9, 
              job = "SR1km", SDSstring = "010000000000000001000")

# 010000000000000001000 for MYD09GA
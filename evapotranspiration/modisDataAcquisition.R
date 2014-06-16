library(Rsenal)

setwd("F:/kilimanjaro/evapotranspiration")

MODISoptions(localArcPath = "MODIS_ARC", 
             outDirPath = "MODIS_ARC/PROCESSED")

modisDownload(modis.products = c("MOD13Q1", "MYD13Q1"), 
              modis.download.only = FALSE, 
              modis.outproj = "EPSG:32737", 
              tileH = 21, tileV = 9, 
              begin = "2013001", 
              job = "EVIkili", SDSstring = "011000000011")
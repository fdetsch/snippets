### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## set working directory
Orcs::setwdOS(path_ext = "phd/phd_kolloq_apr2016")

## packages
library(Rsenal)
library(rasterVis)
library(grid)

## functions
source("R/visDEM.R")


### visualization -----

# dem labeled contours
dem <- raster("../../kilimanjaro/coordinates/DEM_ARC1960_30m_Hemp.tif")
dem <- aggregate(dem, 10)
dem <- trim(projectRaster(dem, crs = "+init=epsg:4326"))

p_dem <- visDEM(dem, labcex = .7, cex = 1.6, col = "grey25", method = "edge")

# figure mann-kendall 
# rst_gimms <- stack("data/gimms_ndvi3g_dwnscl_8211_dsn.tif")
# rst_gimms <- trim(projectRaster(rst_gimms, crs = "+init=epsg:4326"))
# 
# library(gimms)
# rst_mk <- significantTau(rst_gimms, prewhitening = FALSE, 
#                          filename = "data/gimms_ndvi3g_001.tif")

rst_mk <- raster("data/gimms_ndvi3g_001.tif")

p_mk <- spplot(rst_mk, col.regions = brewer.pal(10, "BrBG"), 
               xlab = NULL, ylab = NULL, 
               colorkey = list(draw = TRUE, space = "top", width = .6, height = .6),
               at = seq(-.5, .5, .1), scales = list(draw = TRUE, cex = .8, 
                                                    y = list(rot = 90)),  
               main = list(expression(bold("Kendall's" ~ tau)), 
                           vjust = .5, hjust = .4, cex = 1)) + 
  as.layer(p_dem)

## manuscript version
tiff("img/mannkendall.tiff", height = 12, width = 12, units = "cm", res = 300, 
     compression = "lzw")
plot.new()
print(p_mk, newpage = FALSE)
dev.off()


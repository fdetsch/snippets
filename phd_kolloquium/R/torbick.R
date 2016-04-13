### environmental stuff

## clear workspace
rm(list = ls(all = TRUE))

## set working directory
Orcs::setwdOS(path_ext = "phd/phd_kolloq_apr2016")

## packages
library(Rsenal)

## functions
source("R/visDEM.R")


### data processing

## reference extent
ext <- extent(37.273, 37.407, -3.132, -3.025)

num_xmin <- xmin(ext) + .015
num_xmax <- xmax(ext) - .01
num_ymin <- ymin(ext)
num_ymax <- ymax(ext) - .01

## dem
rst_dem <- raster("../../kilimanjaro/coordinates/DEM_ARC1960_30m_Hemp.tif")
rst_dem <- aggregate(rst_dem, 4)
rst_dem <- trim(projectRaster(rst_dem, crs = "+init=epsg:4326"))
p_dem <- visDEM(rst_dem, labcex = .7, cex = 1.6, col = "grey75", 
                method = "edge")

## study area (after torbick et al. 2009, figure 3)
# rst_kili <- kiliAerial(upperLeft = c(num_ymax, num_xmin),
#                        lowerRight = c(num_ymin, num_xmax),
#                        minNumTiles = 20L, projection = "+init=epsg:4326",
#                        type = "google")
# 
# rst_kili <- writeRaster(rst_kili, "data/kili_aerial_google.tif",
#                         format = "GTiff", overwrite = TRUE)

rst_kili <- stack("data/kili_aerial_google.tif")

## gimms grid
rst_gimms <- raster("data/GIMMS3g_mk_0312_tau.tif")
rst_gimms <- trim(projectRaster(rst_gimms, crs = "+init=epsg:4326"))
spy_gimms <- rasterToPolygons(rst_gimms)
spy_gimms <- crop(spy_gimms, rst_kili, snap = "in")

## scale bar and north arrow
scale <- list("SpatialPolygonsRescale", layout.scale.bar(), scale = 0.04499312, 
              offset = c(37.34, -3.04), fill = c("transparent", "black"))
text1 = list("sp.text", c(37.34025, -3.0435), "0", cex = 1.2, font = 2, col = "black")
text2 = list("sp.text", c(37.385, -3.0435), "5 km", cex = 1.2, font = 2, col = "black")

p_bing <- spplot(rst_kili[[1]], col.regions = NA, colorkey = FALSE,
                 sp.layout = list(rgb2spLayout(rst_kili, quantiles = c(0, 1)),
                                  scale, text1, text2),
                 xlim = c(num_xmin, num_xmax), ylim = c(num_ymin, num_ymax),
                 scales = list(draw = TRUE, cex = 1, x = list(at = seq(37.3, 37.39, .03))))

p <- p_bing + 
  latticeExtra::layer(sp.polygons(spy_gimms, lwd = 1.5, lty = 3, col = "white"))
  
## write to file
tiff("img/study_area_torbick.tiff", width = 14, height = 12, units = "cm", 
     res = 300, compression = "lzw")
plot.new()
print(p, newpage = FALSE)
dev.off()

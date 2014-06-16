setwd("E:/phd/scintillometer/")

library(rgdal)
library(OpenStreetMap)

plt <- readOGR("E:/phd/kili_nov2013/map/plot/corners", "BPoles")
plt <- subset(plt, PlotID %in% c("cof2", "gra2"))
plt.ll <- spTransform(plt, CRS("+init=epsg:4326"))

plt.cntr <- readOGR("E:/phd/kili_nov2013/map/plot/corners", "AMiddlePole")
plt.cntr <- subset(plt.cntr, PlotID %in% c("cof2", "gra2"))
plt.cntr.ll <- spTransform(plt.cntr, CRS("+init=epsg:4326"))

for (i in as.character(plt.cntr.ll$PlotID)) {
  
  id <- grep(i, plt.ll$PlotID)
  
  ext <- extent(plt.ll[id, ])
  ul <- c(ymax(ext) + .001, xmin(ext) - .001)
  lr <- c(ymin(ext) - .001, xmax(ext) + .001)
  
  #   id.ul <- grep("B1", plt.ll$PoleName[id])
  #   id.lr <- grep("B8", plt.ll$PoleName[id])
  
  osm <- openmap(upperLeft = ul, lowerRight = lr, type = "bing")
  osm.utm <- projectRaster(raster(osm), crs = CRS("+init=epsg:32737"))
  
  png(paste0("out/", i, ".png"), width = 600, res = 100)
  plotRGB(osm.utm)
  points(spTransform(plt.cntr.ll, CRS("+init=epsg:32737")), col = "red")
  points(spTransform(plt.ll, CRS("+init=epsg:32737")), col = "red")
  dev.off()
  
#   # Center coordinate of final map (FOD3)
#   cntr <- data.frame(plt.cntr.ll[plt.cntr.ll$PlotID == "cof2", "PlotID"])
#   cntr <- data.frame(Lon = cntr[, 2], Lat = cntr[, 3], PlotID = cntr[, 1])
#   
#   tmp.coords <- getTileCenters(plt.rds = 5000, plt.res = 2500)
#   
#   tmp.osm <- getOsmTiles(tile.cntr = tmp.coords, 
#                          location = cntr, 
#                          plot.res = 1000, 
#                          plot.bff = 100,
#                          tmp.folder = "C:/Users/fdetsch/AppData/Local/Temp/R_raster_tmp", 
#                          path.out = "tls/osm", 
#                          type = "bing", zoom = 18)
  
}
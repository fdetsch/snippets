### Environmental settings

# Clear workspace
rm(list = ls(all = T))

# Set working directory
switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/permanent/phd/kili_nov2013/map"), 
       "Windows" = setwd("E:/phd/kili_nov2013/map"))

# Load required packages
lib <- c("OpenStreetMap", "raster", "rgdal", "doParallel", "png", "plotrix")
sapply(lib, function(x) require(x, character.only = T))

# Required functions
source("src/getTileCenters.R")
source("src/getOsmTiles.R")

# Settings
rsmpl <- F

# Parallelization
registerDoParallel(cl <- makeCluster(3))


### Data import

## Plot coordinates

plt.shp <- readOGR(dsn = "plot", layer = "Plots_MP")
plt.shp <- plt.shp[plt.shp$VALID == "Y", ]
plt.shp.utm <- spTransform(plt.shp, CRS("+init=epsg:32737"))


# ## Import additional GPS data by David
#
# dvd_1 <- foreach(i = list("12340012_L", "12340012_P", "12340012_A")) %do% {
#   tmp.shp <- readOGR(dsn = "../David", layer = i)
#   projection(tmp.shp) <- CRS("+init=epsg:4326")
#   
#   spTransform(tmp.shp, CRS("+init=epsg:32737"))
# }


## OSM data

# Center coordinate of final map (FOD3)
cntr <- data.frame(plt.shp.utm[plt.shp.utm$PLOTID == "fod3", "PLOTID"])
cntr <- data.frame(Lon = cntr[, 2], Lat = cntr[, 3], PlotID = cntr[, 1])

# Get ESRI topo and Skobbler data
jnk <- foreach(plt.rds = rep(30000, 2), plt.res = c(5000, 1000), 
               path.out = c("tls/esri-topo", "tls/skobbler"), 
               type = c("esri-topo", "skobbler")) %do% {
  tmp.coords <- getTileCenters(plt.rds, plt.res)

  tmp.osm <- getOsmTiles(tile.cntr = tmp.coords, 
                         location = cntr, 
                         plot.res = plt.res, 
                         plot.bff = 50,
                         tmp.folder = "C:/Users/fdetsch/AppData/Local/Temp/R_raster_tmp", 
                         path.out = path.out, 
                         type = type, mergeTiles = T)
}

# Merge ESRI data
fls.esri <- list.files("tls/esri-topo", pattern = ".tif$", full.names = T)
rst.esri <- foreach(i = fls.esri, .packages = lib) %dopar% stack(i)
rst.esri.mrg <- do.call(function(...) {
  merge(..., tolerance = 1, overwrite = T, format = "GTiff", 
        filename = "tls/esri-topo/esri_all")
}, rst.esri)

# Resample and merge Skobbler data
fls.skbl <- list.files("tls/skobbler", pattern = "kili_tile_.*.tif$", full.names = T)
fls.skbl <- fls.skbl[-grep("rsmpl", fls.skbl)]
rst.skbl <- foreach(i = fls.skbl, .packages = lib) %dopar% stack(i)

rst.skbl.ext <- Reduce("union", sapply(rst.skbl, extent))
template <- raster(rst.skbl.ext, crs = projection(rst.skbl[[1]]))
res(template) <- res(rst.skbl[[1]])

jnk <- if (rsmpl == T) {
  foreach(i = rst.skbl, j = fls.skbl, .packages = lib) %dopar% {
    if (!file.exists(paste(substr(j, 1, nchar(j) - 4), "rsmpl.tif", sep = "_"))) {
      crp <- crop(template, i)
      resample(i, crp, method = "ngb", 
               filename = paste(substr(j, 1, nchar(j) - 4), "rsmpl", sep = "_"), 
               format = "GTiff", overwrite = F)
    }
  }
} 

fls.skbl.rsmpl <- list.files("tls/skobbler", pattern = "rsmpl.tif$", 
                             full.names = T)
rst.skbl.rsmpl <- foreach(i = fls.skbl.rsmpl, .packages = lib) %dopar% stack(i)

rst.skbl.rsmpl.mrg <- do.call(function(...) {
  merge(..., tolerance = 1, overwrite = T, format = "GTiff", 
        filename = "tls/skobbler/skobbler_all", overlap = F)
}, rst.skbl.rsmpl)

# Intersect data from ESRI and Skobbler
rst.esri.mrg <- stack("tls/esri-topo/esri_all.tif")
rst.skbl.rsmpl.mrg <- stack("tls/skobbler/skobbler_all.tif")

rst.esri.mrg.rsmpl <- resample(rst.esri.mrg, rst.skbl.rsmpl.mrg, tolerance = 1,
                               method = "ngb", format = "GTiff", 
                               filename = "tls/esri-topo/esri_all_rsmpl")

# Replace unoccupied cells in Skobbler data with ESRI data
rst.esri.skbl <- overlay(rst.esri.mrg.rsmpl, rst.skbl.rsmpl.mrg, 
                         fun = function(x, y) {
                           y[y[] %in% 238:240] <- x[y[] %in% 238:240]
                           return(y)
                         }, filename = "tls/esri_skrobbler_mrg", format = "GTiff")

# Reproject composite raster to UTM 32S
# rst.esri.skbl.utm <- projectRaster(rst.esri.skbl, crs = projection(plt.shp.utm), 
#                                    filename = "tls/esri_skrobbler_mrg_utm", 
#                                    format = "GTiff", method = "ngb")
rst.esri.skbl.utm <- stack("tls/esri_skrobbler_mrg_utm.tif")

# Crop composite raster
plotRGB(rst.esri.skbl.utm)
points(plt.shp.utm)

crp.xtnt <- drawExtent()
rst.esri.skbl.utm.crp <- crop(rst.esri.skbl.utm, crp.xtnt, 
                              filename = "tls/esri_skrobbler_mrg_utm_crp3", 
                              format = "GTiff", method = "ngb", overwrite = T)
rst.esri.skbl.utm.crp <- stack("tls/esri_skrobbler_mrg_utm_crp3.tif")


### Plotting the official poster

# North arrow
north.arrow <- readPNG("north_arrow.png")

# Manual label arrangement
text.pos <- thigmophobe(coordinates(plt.shp.utm)[, 1], 
                        coordinates(plt.shp.utm)[, 2])
text.pos[grep("sun1", plt.shp.utm$PLOTID)] <- 1
text.pos[grep("fpd3", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fod4", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("fod5", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fpo2", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fod2", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("fpd1", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fer4", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("fer2", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fer3", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("foc5", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("flm1", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("flm3", plt.shp.utm$PLOTID)] <- 2

# PlotRGB
tiff("out/official_map.tif", width = 12167, height = 8435, units = "px", 
     compression = "lzw", pointsize = 80)
# pdf("out/official_map.pdf", pointsize = 15, width = 40, height = 30)
plotRGB(rst.esri.skbl.utm.crp, stretch = "lin", 
        maxpixels = ncell(rst.esri.skbl.utm.crp), 
        addfun = function(...) {
          points(plt.shp.utm, pch = 13, lwd = 3, cex = 2, 
                 col = brewer.pal(5, "YlOrBr")[4])
          thigmophobe.labels(coordinates(plt.shp.utm)[, 1], 
                             coordinates(plt.shp.utm)[, 2], 
                             text.pos = text.pos, offset = 1,
                             labels = plt.shp.utm$PLOTID, cex = 2, font = 2, 
                             col = brewer.pal(5, "YlOrBr")[4])
          scalebar(d = 5000, type = "bar", divs = 4, below = "km", 
                   label = c(0, 2.5, 5), xy = c(300000, 9624500), cex = 2, 
                   adj = c(.5, -1))
          rasterImage(north.arrow, 301500, 9626250, 303500, 9628750)
        })
dev.off()

# Deregister parallel backend
stopCluster(cl)
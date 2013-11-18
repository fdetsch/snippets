### Global settings

# Clear workspace
rm(list = ls(all = T))

# Set working directory
switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/permanent/phd/kili_nov2013/map"), 
       "Windows" = setwd("E:/phd/kili_nov2013/map"))

# Required packages
lib <- c("OpenStreetMap", "doParallel", "RColorBrewer", "png")
sapply(lib, function(x) require(x, character.only = T))

# Required functions
source("src/getTileCenters.R")
source("src/getOsmTiles.R")

# Parallelization
registerDoParallel(cl <- makeCluster(4))



### Data import

## Plot coordinates

plt.shp <- readOGR(dsn = "plot", layer = "Plots_MP")
plt.shp <- plt.shp[plt.shp$VALID == "Y", ]
plt.shp.utm <- spTransform(plt.shp, CRS("+init=epsg:32737"))


## Bing satellite data

# Center coordinate of final map (Kibo summit)
cntr <- data.frame(Lon = 37.353333, Lat = -3.075833, PlotID = "Kibo")
coordinates(cntr) <- ~ Lon + Lat
projection(cntr) <- "+init=epsg:4326"
cntr.utm <- spTransform(cntr, CRS("+init=epsg:32737"))

# Center coordinates of single tiles relative to overall center coordinate
tmp.coords <- getTileCenters(40000, 5000)

# Download single tiles
jnk <- getOsmTiles(tile.cntr = tmp.coords, 
                   location = cntr.utm, 
                   plot.res = 5000, 
                   tmp.folder = "C:/Users/fdetsch/AppData/Local/Temp/R_raster_tmp", 
                   path.out = "tls/bing", 
                   type = "bing", mergeTiles = T)

# List and import downloaded Bing images
bing.fls <- list.files("tls/bing", pattern = "kili_tile.*.tif$", full.names = T)
bing.fls <- bing.fls[-grep("rsmpl", bing.fls)]
bing.rst <- foreach(i = bing.fls, .packages = lib) %dopar% stack(i)

# Identify overall extent of all Bing images and set up template
bing.ext <- Reduce("union", sapply(bing.rst, extent))
template <- raster(bing.ext, crs = projection(bing.rst[[1]]))
res(template) <- res(bing.rst[[1]])

# Resample all images 
jnk <- if (rsmpl == T) {
  foreach(i = bing.rst, j = bing.fls, .packages = lib) %dopar% {
#     if (!file.exists(paste(substr(j, 1, nchar(j) - 4), "rsmpl.tif", sep = "_"))) {
      crp <- crop(template, i)
      resample(i, crp, method = "ngb", 
               filename = paste(substr(j, 1, nchar(j) - 4), "rsmpl", sep = "_"), 
               format = "GTiff", overwrite = T)
#     }
  }
} 

# # Merge single layers and reproject to UTM 32S
# bing.fls.rsmpl <- list.files("tls/bing", pattern = "rsmpl.tif$", full.names = T)
# bing.rst.rsmpl <- foreach(i = bing.fls.rsmpl, .packages = lib) %dopar% stack(i)
# bing.rst.rsmpl.mrg <- do.call(function(...) {
#   merge(..., tolerance = 1, filename = "tls/bing/bing_all", 
#         format = "GTiff", overwrite = T)
# }, bing.rst.rsmpl)
# bing.rst.rsmpl.mrg.utm <- projectRaster(bing.rst.rsmpl.mrg, 
#                                         crs = CRS("+init=epsg:32737"), 
#                                         filename = "tls/bing/bing_all_utm", 
#                                         format = "GTiff", overwrite = T, 
#                                         method = "ngb")
bing.rst.rsmpl.mrg.utm <- stack("tls/bing/bing_all_utm.tif")

# Crop data
template <- raster(extent(37, 37.72, -3.4, -2.84), crs = "+init=epsg:4326")
template.utm <- projectRaster(template, crs = "+init=epsg:32737")

bing.rst.rsmpl.mrg.utm.crp <- crop(bing.rst.rsmpl.mrg.utm, template.utm)


### Plotting

# North arrow
north.arrow <- readPNG("north_arrow.png")

# PlotRGB
tiff("out/official_map_bing.tif", width = 3824, height = 4198, units = "px", 
     compression = "lzw", pointsize = 30)
plotRGB(bing.rst.rsmpl.mrg.utm.crp, 
        maxpixels = ncell(bing.rst.rsmpl.mrg.utm.crp),
        addfun = function(...) {
          points(plt.shp.utm, pch = 21, lwd = 3, cex = 3, 
                 bg = brewer.pal(5, "YlOrBr")[4])
          scalebar(d = 5000, type = "bar", divs = 4, below = "km", 
                   label = c(0, 2.5, 5), xy = c(280000, 9622500), cex = 1.5, 
                   adj = c(.5, -1))
          rasterImage(north.arrow, 301250 - 20000, 9625500, 303750 - 20000, 9628500)
          
        })
dev.off()

# Deregister parallel backend
stopCluster(cl)
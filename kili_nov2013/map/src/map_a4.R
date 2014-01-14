### Environmental settings

# Clear workspace
rm(list = ls(all = T))

# Set working directory
switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/permanent/phd/kili_nov2013/map"), 
       "Windows" = setwd("E:/phd/kili_nov2013/map"))

# Load required packages
lib <- c("OpenStreetMap", "raster", "rgdal", "doParallel", "png", "plotrix", 
         "TeachingDemos", "RColorBrewer", "foreign")
sapply(lib, function(x) require(x, character.only = T))

# Required functions
source("src/getTileCenters.R")
source("src/getOsmTiles.R")
source("src/osmRsmplMrg.R")
source("src/getGoogleTiles.R")

# Settings
rsmpl <- T

# Parallelization
registerDoParallel(cl <- makeCluster(3))


### Data import

## Grid

grd.shp.crp.utm <- readOGR(dsn = "shp/grid", layer = "10kutm_euaf_crp_utm")


## Plot coordinates

plt.shp <- readOGR(dsn = "plot", layer = "Plots_MP")
plt.shp <- plt.shp[plt.shp$VALID == "Y", ]
plt.shp.utm <- spTransform(plt.shp, CRS("+init=epsg:32737"))


## Important places

cts.shp <- read.dbf("shp/diva/TZA.dbf", as.is = T)
cts.shp <- subset(cts.shp, F_CLASS == "P" & F_DESIG == "PPL" & 
                    ADM1 == "Kilimanjaro")
cts.shp$LAT <- as.numeric(cts.shp$LAT)
cts.shp$LONG <- as.numeric(cts.shp$LONG)
coordinates(cts.shp) <- ~ LONG + LAT
projection(cts.shp) <- CRS("+init=epsg:4326")

cts.shp.utm <- spTransform(cts.shp, CRS("+init=epsg:32737"))
cts.shp.utm <- cts.shp.utm[!duplicated(coordinates(cts.shp.utm)), ]

cts.shp.utm.big <- cts.shp.utm[cts.shp.utm$NAME %in% c("Moshi", "Himo"), ]
cts.shp.utm <- cts.shp.utm[!cts.shp.utm$NAME %in% c("Moshi", "Himo"), ]


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
tmp.coords <- getTileCenters(plt.rds = 30000, plt.res = 10000)

tmp.osm <- getOsmTiles(tile.cntr = tmp.coords, 
                       location = cntr, 
                       plot.res = 5000, 
                       plot.bff = 100,
                       tmp.folder = "C:/Users/fdetsch/AppData/Local/Temp/R_raster_tmp", 
                       path.out = "tls/skobbler_a4", 
                       type = "skobbler")

# tmp.osm <- getGoogleTiles(tile.cntr = tmp.coords, 
#                           location = cntr, 
#                           plot.res = 10000, 
#                           plot.bff = 100,
#                           path.out = "tls/google", 
#                           type = "satellite", rgb = TRUE)

# # Merge ESRI data
# rst.esri.rsmpl.mrg <- 
#   osmRsmplMrg(path = "tls/esri-topo", 
#               pattern = "kili_tile_.*.tif$", 
#               rsmpl.exe = T, 
#               path.rsmpl = "tls/esri-topo",
#               pattern.rsmpl = "kili_tile_.*rsmpl.tif$",
#               n.cores = 4, 
#               filename = "tls/esri-topo/esri_all.tif", overwrite = T)

# fls.esri <- list.files("tls/esri-topo", pattern = "kili_tile_.*.tif$", full.names = T)
# rst.esri <- foreach(i = fls.esri, .packages = lib) %dopar% stack(i)
# 
# rst.esri.ext <- Reduce("union", sapply(rst.esri, extent))
# template <- raster(rst.esri.ext, crs = projection(rst.esri[[1]]))
# res(template) <- res(rst.esri[[1]])
# 
# jnk <- if (rsmpl == T) {
#   foreach(i = rst.esri, j = fls.esri, .packages = lib) %dopar% {
#     crp <- crop(template, i)
#     resample(i, crp, method = "ngb", 
#              filename = paste(substr(j, 1, nchar(j) - 4), "rsmpl", sep = "_"), 
#              format = "GTiff", overwrite = F)
#   }
# } 
# 
# fls.esri.rsmpl <- list.files("tls/esri-topo", pattern = "kili_tile_.*rsmpl.tif$", 
#                              full.names = T)
# rst.esri.rsmpl <- foreach(i = fls.esri.rsmpl, .packages = lib) %dopar% stack(i)
# 
# rst.esri.rsmpl.mrg <- do.call(function(...) {
#   merge(..., tolerance = 1, overwrite = T, format = "GTiff", 
#         filename = "tls/esri-topo/esri_all")
# }, rst.esri.rsmpl)
# rst.esri.mrg <- stack("tls/esri-topo/esri_all.tif")

# Resample and merge Skobbler data
rst.skbl.rsmpl.mrg <- 
  osmRsmplMrg(path = "tls/skobbler_a4", 
              pattern = "kili_tile_.*.tif$", 
              rsmpl.exe = T, 
              path.rsmpl = "tls/skobbler_a4", 
              pattern.rsmpl = "kili_tile_.*rsmpl.tif$", 
              n.cores = 4, 
              file.rsmpl.mrg = "tls/skobbler_a4/skobbler_all.tif")

# fls.skbl <- list.files("tls/skobbler", pattern = "kili_tile_.*.tif$", full.names = T)
# if (length(grep("rsmpl", fls.skbl)) > 0)
#   fls.skbl <- fls.skbl[-grep("rsmpl", fls.skbl)]
# rst.skbl <- foreach(i = fls.skbl, .packages = lib) %dopar% stack(i)
# 
# rst.skbl.ext <- Reduce("union", sapply(rst.skbl, extent))
# template <- raster(rst.skbl.ext, crs = projection(rst.skbl[[1]]))
# res(template) <- res(rst.skbl[[900]])
# 
# jnk <- if (rsmpl == T) {
#   foreach(i = rst.skbl, j = fls.skbl, .packages = lib) %dopar% {
#     if (!file.exists(paste(substr(j, 1, nchar(j) - 4), "rsmpl.tif", sep = "_"))) {
#       crp <- crop(template, i)
#       resample(i, crp, method = "ngb", 
#                filename = paste(substr(j, 1, nchar(j) - 4), "rsmpl", sep = "_"), 
#                format = "GTiff", overwrite = F)
#     }
#   }
# } 
# 
# fls.skbl.rsmpl <- list.files("tls/skobbler", pattern = "kili_tile_.*rsmpl.tif$", 
#                              full.names = T)
# rst.skbl.rsmpl <- foreach(i = fls.skbl.rsmpl, .packages = lib) %dopar% stack(i)
# 
# rst.skbl.rsmpl.mrg <- do.call(function(...) {
#   merge(..., tolerance = 1, overwrite = T, format = "GTiff", 
#         filename = "tls/skobbler/skobbler_all_hd", overlap = F)
# }, rst.skbl.rsmpl)
# 
# Intersect data from ESRI and Skobbler
rst.esri.rsmpl.mrg <- stack("tls/esri-topo_a4/esri_all.tif")
rst.skbl.rsmpl.mrg <- stack("tls/skobbler_a4/skobbler_all.tif")

rst.esri.rsmpl.mrg.rsmpl <- 
  resample(rst.esri.rsmpl.mrg, rst.skbl.rsmpl.mrg, tolerance = 1, method = "ngb", 
           filename = "tls/esri-topo_a4/esri_all_rsmpl_mrg.tif", overwrite = T)
rst.esri.rsmpl.mrg.rsmpl <- stack("tls/esri-topo/esri_all_rsmpl_mrg.tif")


# Replace unoccupied cells in Skobbler data with ESRI data
# rst.esri.skbl <- 
#   overlay(rst.esri.rsmpl.mrg.rsmpl, rst.skbl.rsmpl.mrg, fun = function(x, y) {
#     y[y[] %in% 238:240] <- x[y[] %in% 238:240]
#     return(y)
#   }, filename = "tls/esri_skrobbler_rsmpl_mrg_a4.tif", overwrite = T)
rst.esri.skbl <- stack("tls/esri_skrobbler_rsmpl_mrg.tif")

# Reproject composite raster to UTM 32S
# rst.esri.skbl.utm <- projectRaster(rst.esri.skbl, crs = projection(plt.shp.utm), 
#                                    filename = "tls/esri_skrobbler_mrg_utm_a4.tif", 
#                                    method = "ngb", overwrite = T)
rst.esri.skbl.utm <- stack("tls/esri_skrobbler_mrg_utm.tif")

# # Crop composite raster
# plotRGB(rst.esri.skbl.utm)
# points(plt.shp.utm)
# 
# crp.xtnt <- extent(stack("tls/esri_skrobbler_mrg_utm_crp3.tif"))
# rst.esri.skbl.utm.crp <- crop(rst.esri.skbl.utm, crp.xtnt, 
#                               filename = "tls/esri_skrobbler_mrg_utm_crp_a4", 
#                               format = "GTiff", method = "ngb", overwrite = T)
rst.esri.skbl.utm.crp <- stack("tls/esri_skrobbler_mrg_utm_crp_a4.tif")


### Plotting the official poster

# # North arrow
# north.arrow <- readPNG("north_arrow.png")

# Manual label arrangement
text.pos <- thigmophobe(coordinates(plt.shp.utm)[, 1], 
                        coordinates(plt.shp.utm)[, 2])
text.pos[grep("sun1", plt.shp.utm$PLOTID)] <- 1
text.pos[grep("fpd3", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fod4", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("fod5", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fpo1", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fpo2", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fod2", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("fpd1", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fer4", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("fer2", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("fer3", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("foc1", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("foc5", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("flm1", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("flm3", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("sav4", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("mai1", plt.shp.utm$PLOTID)] <- 3
text.pos[grep("cof3", plt.shp.utm$PLOTID)] <- 3
text.pos[grep("gra1", plt.shp.utm$PLOTID)] <- 2
text.pos[grep("gra3", plt.shp.utm$PLOTID)] <- 4
text.pos[grep("gra4", plt.shp.utm$PLOTID)] <- 3
text.pos[grep("hom1", plt.shp.utm$PLOTID)] <- 1
text.pos[grep("hom4", plt.shp.utm$PLOTID)] <- 1
text.pos[grep("hel1", plt.shp.utm$PLOTID)] <- 2

text.pos.cts <- thigmophobe(coordinates(cts.shp.utm)[, 1], 
                            coordinates(cts.shp.utm)[, 2])
text.pos.cts[grep("Mwika West", cts.shp.utm$NAME)] <- 1

# PlotRGB
tiff("out/official_map_wcts_grd_a4.tif", width = 3036, height = 2113, units = "px", 
     compression = "lzw", pointsize = 20)
# pdf("out/official_map.pdf", pointsize = 15, width = 40, height = 30)
plotRGB(rst.esri.skbl.utm.crp, stretch = "lin", 
        maxpixels = ncell(rst.esri.skbl.utm.crp), 
        addfun = function(...) {
          # Grid lines
          lines(crop(grd.shp.crp.utm, rst.esri.skbl.utm.crp), 
                col = "grey60", lwd = 3)
          shadowtext(x = rep(seq(300000, 350000, 10000), 2), 
                     y = c(rep(9663400, 6), rep(9625100, 6)), 
                     labels = as.character(seq(300000, 350000, 10000)), 
                     cex = .9, font = 2, srt = 90, col = "grey60", bg = "white", 
                     pos = rep(2, 6))
          shadowtext(x = c(rep(299000, 4), rep(356000, 4)), 
                     y = rep(seq(9630000, 9660000, 10000), 2), 
                     labels = as.character(seq(9630000, 9660000, 10000)), 
                     cex = .9, font = 2, col = "grey60", bg = "white", 
                     pos = rep(3, 6))
          # Important places
          shadowtext(x = coordinates(cts.shp.utm)[, 1], 
                     y = coordinates(cts.shp.utm)[, 2], 
                     labels = cts.shp.utm$NAME, cex = 1.5, font = 3, 
                     col = "black", bg = "white", pos = text.pos.cts)
          # Research plots
          points(plt.shp.utm, 
                 pch = ifelse(plt.shp.utm$FOCALPLOT == "Y", 24, 22), lwd = 5, 
                 cex = ifelse(plt.shp.utm$FOCALPLOT == "Y", 3, 2), 
                 col = "white", 
                 bg = ifelse(plt.shp.utm$FOCALPLOT == "Y", "steelblue", "black"))
          shadowtext(x = coordinates(plt.shp.utm)[, 1], 
                     y = coordinates(plt.shp.utm)[, 2], 
                     labels = plt.shp.utm$PLOTID, cex = 2, font = 2, 
                     pos = text.pos, col = "black", bg = "white", 
                     r = 0.2, offset = ifelse(plt.shp.utm$FOCALPLOT == "Y", 4, 1))
          # Cities (Moshi, Himo)
          shadowtext(x = coordinates(cts.shp.utm.big)[, 1], 
                     y = coordinates(cts.shp.utm.big)[, 2], 
                     labels = cts.shp.utm.big$NAME, cex = 2, font = 4,  
                     col = "black", bg = "white", pos = c(4, 1), offset = 1)
          scalebar(d = 5000, type = "bar", divs = 4, below = "km", 
                   label = c(0, 2.5, 5), xy = c(301000, 9624500), cex = 2, 
                   adj = c(.5, -1))
#           rasterImage(north.arrow, 301500, 9626250, 303500, 9628750)
        })
dev.off()

# Deregister parallel backend
stopCluster(cl)
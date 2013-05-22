### Environmental settings

# Clear workspace
rm(list = ls(all = TRUE))

# Required packages
library(raster)
library(rgdal)
# library(parallel)
# library(OpenStreetMap)
library(dismo)
library(doParallel)
# library(rgeos)

# Paths and filenames
path.wd <- "E:/kilimanjaro/plot_landuse/" # Windows external HDD
path.src <- "src"
path.tls <- "tls"
path.crp <- "crp"
path.mrg <- "mrg"
# path.out <- "out"

file.coords <- "station_master.csv"
# file.cloudy <- "osm_cloudy.txt"

# Working directory
setwd(path.wd)

# Required functions
source(paste(path.src, "getTileCenters.R", sep = "/"))
source(paste(path.src, "getOsmTiles.R", sep = "/"))
source(paste(path.src, "getGoogleTiles.R", sep = "/"))


### Parallelization

# Initialize parallelization
n.cores <- detectCores()
clstr <- makePSOCKcluster(n.cores)


### Plot coordinates

# Import data and select complete cases only
data.coords <- read.csv(file.coords, header = TRUE, stringsAsFactors = FALSE)[,c("PlotID", "Lon", "Lat")]
data.coords <- data.coords[complete.cases(data.coords),]

# Set coordinates and coordinate reference system (CRS) of SpatialPointsDataFrame
coordinates(data.coords) <- c("Lon", "Lat")
projection(data.coords) <- CRS("+proj=longlat +datum=WGS84")

# Mercator plot coordinates
data.coords.mrc <- spTransform(data.coords, 
                               CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

# Distance between plot and final image boundary
plt.rds <- 2000
# Extent of single OSM tiles
plt.res <- 250

# Generate list containing relative coordinates of single tile centers
tile.cntr <- getTileCenters(plt.rds = plt.rds, 
                            plt.res = plt.res)


### OSM 

## Parallelized import, rasterization and merging of single OSM tiles per research plot

# # Export necessary variables and required packages/functions to cluster
# clusterExport(clstr, c("data.coords.mrc", "tile.cntr", "path.wd", "path.src", "path.tls", "plt.res"))
# clusterEvalQ(clstr, c(setwd(path.wd), # Working directory
#                       library(raster), library(rgdal), # Required packages
#                       source(paste(path.src, "getOsmTiles.R", sep = "/")))) # Required functions

# Loop through all research plots
lapply(seq(nrow(data.coords.mrc)), function(a) {
  
  # Duplicate Mercator plot coordinates
  plot.coords.mrc <- data.frame(data.coords.mrc)[a,]
    
  # Get OSM tiles
  tmp.osm <- getOsmTiles(tile.cntr = tile.cntr, 
                         plot.coords.mrc = plot.coords.mrc, 
                         plot.res = plt.res)
  
  # Rasterize and append single OSM tiles
  for (i in seq(tmp.osm)) {
    
    if (i == 1) {
      rst.osmtile <- lapply(seq(tmp.osm[[i]]$tiles), function(j) {
        raster(tmp.osm[[i]]$tiles[[j]])
      })
    } else {
      tmp <- lapply(seq(tmp.osm[[i]]$tiles), function(j) {
        raster(tmp.osm[[i]]$tiles[[j]])
      })
      rst.osmtile <- append(rst.osmtile, tmp)
    }
    
  }
  
  # Identify coarsest resolution
  rst.osmtile.res <- unique(unlist(lapply(seq(rst.osmtile), function(i) {
    res(rst.osmtile[[i]])
  })))
  
  # Choose raster images with coarsest resolution only
  rst.osmtile <- rst.osmtile[which(unlist(lapply(seq(rst.osmtile), function(i) {
        res(rst.osmtile[[i]])[1] == max(rst.osmtile.res)
  })))]
  
  # Export single tiles
  lapply(seq(rst.osmtile), function(i) {
    
    # Create subdirectory if neccessary
    if (!file.exists(paste(path.tls, data.coords.mrc$PlotID[a], sep = "/")))
      dir.create(paste(path.tls, data.coords.mrc$PlotID[a], sep = "/"))
    # Write raster file to HDD
    writeRaster(rst.osmtile[[i]], filename = paste(path.tls, "/", data.coords.mrc$PlotID[a], "/", data.coords.mrc$PlotID[a], "_osm_tile_", i, sep = ""), 
                format = "GTiff", overwrite = TRUE)
    
  })
 
})
          
#   # Merge and export low-resolution raster to file
#   rst.osmtile.merge_lres <- do.call("merge", rst.osmtile[which(unlist(lapply(seq(rst.osmtile), function(i) {
#     res(rst.osmtile[[i]])[1] == max(rst.osmtile.res)
#   })))])
#   writeRaster(rst.osmtile.merge_lres, filename = paste(path.mrg, "/", data.coords.mrc$PlotID[a], "_osm_lres_mrg.tif", sep = ""), overwrite = TRUE)
#   
# }) # End of parLapply

# Merge single tiles to complete raster
dirs.osmtile <- dir(path.tls, full.names = TRUE)

clusterExport(clstr, c("dirs.osmtile", "path.mrg"))
clusterEvalQ(clstr, library(raster))

lapply(seq(dirs.osmtile), function(z) {
  
  tmp.files <- list.files(dirs.osmtile[z], pattern = "osm", full.names = TRUE)
  tmp.files.sh <- list.files(dirs.osmtile[z], pattern = "osm", full.names = FALSE)
  
  if (!file.exists(paste(path.mrg, "/", substr(tmp.files.sh[1], 1, 8), "_mrg.tif", sep = ""))) {
    tmp.rst <- lapply(seq(tmp.files), function(i) {
      stack(tmp.files[i])
    })
    
    tmp.rst.mrg <- do.call("merge", tmp.rst)
    
    writeRaster(tmp.rst.mrg, paste(path.mrg, "/", substr(tmp.files.sh[1], 1, 8), "_mrg", sep = ""), format = "GTiff", overwrite = TRUE)
  }
          
})

### Google Maps

# Plots with too high cloud occurence in OSM
plot.cloudy <- readLines(file.cloudy)
plot.cloudy <- plot.cloudy[which(nchar(plot.cloudy) == 4)]
# plot.cloudy <- c("hom5")

data.coords.mrc.cloudy <- subset(data.coords.mrc, data.coords.mrc$PlotID %in% plot.cloudy)

# Download from Google Maps server (parallel)
clstr <- makeCluster(n.cores)
registerDoParallel(clstr)

foreach(a = seq(nrow(data.coords.mrc.cloudy)), .packages = c("raster", "rgdal")) %dopar%
  getGoogleTiles(tile.cntr = tile.cntr, 
                 plot.coords.mrc = data.frame(data.coords.mrc.cloudy)[a,], 
                 plot.res = plt.res, 
                 path.tls = path.tls)

stopCluster(clstr)

# Plots with available Google Maps
plot.dsm <- unique(substr(list.files(path.tls, pattern = "dsm", recursive = TRUE, full.names = TRUE), 5, 8))
plot.dsm.mrg <- substr(list.files(path.mrg, pattern = "dsm", recursive = TRUE, full.names = TRUE), 5, 8)

plot.dsm <- plot.dsm[!plot.dsm %in% plot.dsm.mrg]

clstr <- makePSOCKcluster(n.cores)
tmp.rst.crp <- lapply(seq(plot.dsm), function(i) {
  
  tmp.files <- list.files(paste(path.tls, plot.dsm[i], sep = "/"), pattern = paste(plot.dsm[i], "dsm", "tif", sep = ".*"), full.names = TRUE)
  tmp.files.sh <- list.files(paste(path.tls, plot.dsm[i], sep = "/"), pattern = paste(plot.dsm[i], "dsm", "tif", sep = ".*"), full.names = FALSE)
  
  clusterExport(clstr, c("path.wd", "tmp.files"), envir = environment())
  clusterEvalQ(clstr, c(setwd(path.wd), library(raster)))
  
  tmp.rst <- parLapply(clstr, seq(tmp.files), function(j) {
    
    stack(tmp.files[j])
    
  })
  
  if (!file.exists(paste(path.crp, plot.dsm[i], sep = "/")))
    dir.create(paste(path.crp, plot.dsm[i], sep = "/"))
  
  clusterExport(clstr, c("tmp.rst", "path.crp", "plot.dsm", "tmp.files.sh", "i"), envir = environment())
  tmp.rst.crp <- parLapply(clstr, seq(tmp.rst), function(k) {
          
    crop(tmp.rst[[k]], extent(xmin(tmp.rst[[k]]), xmax(tmp.rst[[k]]), ymin(tmp.rst[[k]]) + 20, ymax(tmp.rst[[k]])), 
         filename = paste(path.crp, "/", plot.dsm[i], "/", substr(tmp.files.sh[k], 1, nchar(tmp.files.sh[k]) - 4), "_crp.tif", sep = ""), overwrite = TRUE)
    
  })
 
  # Merge and save cropped RasterBricks
  tmp.rst.mrg <- do.call(function(...) {merge(..., tolerance = 1)}, tmp.rst.crp)
  tmp.rst.mrg.rprj <- projectRaster(tmp.rst.mrg, crs = "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  writeRaster(tmp.rst.mrg.rprj, paste(path.mrg, "/", plot.dsm[i], "_dsm_mrg_epsg-32737_123", sep = ""), format = "GTiff", overwrite = TRUE)
  
})

  
})

# Stop cluster
stopCluster(clstr)


### Get Kilimanjaro map via 'dismo' package
tmp <- brick("mrg/sav0_osm_lres_mrg.tif")
plotRGB(tmp)
plot(gBuffer(data.coords.mrc[1,], width = 1500), col = "gray", add = TRUE)
points(data.coords.mrc[1,], bg = "yellow", pch = 21)
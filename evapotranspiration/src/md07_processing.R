# Packages
lib <- c("raster", "rgdal", "doParallel", "zoo", "ncdf")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

# Workspace
setwd("/media/XChange/kilimanjaro/evapotranspiration/")

# Functions
source("src/barometricFormula.R")

# Parallelization
registerDoParallel(cl <- makeCluster(4))

# Kili extent and derived raster template
kili <- data.frame(x = c(37, 37.72), y = c(-3.4, -2.84), id = c("ll", "ur"))
coordinates(kili) <- c("x", "y")
projection(kili) <- CRS("+init=epsg:4326")

template.ncol <- floor((xmax(extent(kili)) - xmin(extent(kili))) / .05)
template.nrow <- floor((ymax(extent(kili)) - ymin(extent(kili))) / .05)
template.rst.ll <- raster(ncols = template.ncol, nrows = template.nrow, 
                          ext = extent(kili), crs = "+init=epsg:4326")
template.rst.utm <- projectRaster(template.rst.ll, crs = "+init=epsg:21037")

# Template for resampling to 250 m
template.rsmpl <- raster("myd09gq/processed/NDVI_AGG_CC_CRP_MYD09GQ.A2013001.sur_refl_b02_1.tif")
template.rsmpl <- projectRaster(template.rsmpl, crs = CRS("+init=epsg:21037"))

# Pressure levels
p <- c(5, 10, 20, 30, 50, 70, 100, 150, 200, 250, 
       300, 400, 500, 620, 700, 780, 850, 920, 950, 1000)

# File metadata
filenames <- list.files("md07/raw006", pattern = "^MOD07_L2.*.hdf$", 
                        full.names = TRUE, recursive = TRUE)

foreach(filename = filenames, .packages = lib) %dopar% {
  filename.sh <- basename(filename)
  
  file.info <- GDALinfo(filename, returnScaleOffset = FALSE)
  file.sds <- attr(file.info, "subdsmdata")
  
  # LonLat SDS
  lat <- file.sds[grep("Latitude", file.sds)-1[1]]
  lat <- sapply(strsplit(lat, "="), "[[", 2)
  lat.info <- GDALinfo(lat)
  lat.scale.offset <- attr(lat.info, "ScaleOffset")
  lat.rst <- (raster(lat) - lat.scale.offset[2]) * lat.scale.offset[1]
  lat.rng <- range(lat.rst[])
  
  lon <- file.sds[grep("Longitude", file.sds)-1[1]]
  lon <- sapply(strsplit(lon, "="), "[[", 2)
  lon.info <- GDALinfo(lon)
  lon.scale.offset <- attr(lon.info, "ScaleOffset")
  lon.rst <- (raster(lon) - lon.scale.offset[2]) * lon.scale.offset[1]
  lon.rng <- range(lon.rst[])
  
  if (min(lon.rst[], na.rm = TRUE) < -180 | max(lon.rst[], na.rm = TRUE) > 180 |
        min(lat.rst[], na.rm = TRUE) < -90 | max(lat.rst[], na.rm = TRUE) > 90)
    return(NULL)
  
  lat.lon.sp <- data.frame(x = lon.rst[], y = lat.rst[])
  coordinates(lat.lon.sp) <- ~ x + y
  projection(lat.lon.sp) <- "+init=epsg:4326"
  
  x <- (xmin(extent(kili)) > xmin(extent(lat.lon.sp)) & 
          xmin(extent(kili)) < xmax(extent(lat.lon.sp))) |
    (xmax(extent(kili)) < xmax(extent(lat.lon.sp)) & 
       xmax(extent(kili)) > xmin(extent(lat.lon.sp)))
  
  y <- (ymin(extent(kili)) > ymin(extent(lat.lon.sp)) & 
          ymin(extent(kili)) < ymax(extent(lat.lon.sp))) |
    (ymax(extent(kili)) < ymax(extent(lat.lon.sp)) & 
       ymax(extent(kili)) > ymin(extent(lat.lon.sp)))
  
  if (all(x, y)) {
    
    sds.names <- c("Solar_Zenith", "Skin_Temperature", "Surface_Pressure", 
                   "Retrieved_Temperature_Profile", "Retrieved_Moisture_Profile", 
                   "Water_Vapor")
    
    rst.md07 <- lapply(sds.names, function(i) {
      
      outdir <- "md07/processed/"
      outname <- paste0(substr(filename.sh, 1, nchar(filename.sh)-4), "_", i)
      
      sds <- file.sds[grep(i, file.sds)[1]]
      index <- as.numeric(strsplit(strsplit(sds, "=")[[1]][1], "_")[[1]][2])
      sds <- sapply(strsplit(sds, "="), "[[", 2)
      sds.info <- GDALinfo(sds)
      sds.mdata <- attr(sds.info, "mdata")
      sds.scale.offset <- sapply(c("^scale_factor", "^add_offset"), function(j) {
        prm <- sds.mdata[grep(j, sds.mdata)]
        return(as.numeric(strsplit(prm, "=")[[1]][2]))
      })
      
      if (sds.info[["bands"]] == 1) {
        sds.rst <- raster(readGDAL(sds, as.is = TRUE, silent = TRUE))
        sds.rst <- (sds.rst - sds.scale.offset[2]) * sds.scale.offset[1]
        sds.rst <- writeRaster(sds.rst, paste0(outdir, outname), 
                               format = "GTiff", overwrite = TRUE)
        
        # Append current parameter to Lat/Lon 'SpatialPoints'
        lat.lon.spdf <- SpatialPointsDataFrame(lat.lon.sp, 
                                               data = data.frame(sds.rst[]))
        
        # Crop Lat/Lon 'SpatialPointsDataFrame' with Kili extent
        lat.lon.spdf.crp <- crop(lat.lon.spdf, kili)
        
        # Remove NA rows
        if (is.null(lat.lon.spdf.crp))
          return(NULL)
        
        # Reproject to EPSG:21037
        lat.lon.spdf.crp <- spTransform(lat.lon.spdf.crp, CRS("+init=epsg:21037"))
        
        lat.lon.spdf.crp <- lat.lon.spdf.crp[complete.cases(lat.lon.spdf.crp@data), ]
        
        if (nrow(lat.lon.spdf.crp) > 0) {
          lat.lon.rst.crp <- 
            rasterize(lat.lon.spdf.crp, template.rst.utm, field = names(lat.lon.spdf.crp), 
                      fun = mean, filename = paste0(outdir, "CRP_", outname),
                      format = "GTiff", overwrite = TRUE)
          lat.lon.rst.crp.rsmpl <- 
            resample(lat.lon.rst.crp, template.rsmpl, 
                     filename = paste0(outdir, "RSMPL_CRP_", outname), 
                     format = "GTiff", overwrite = TRUE)
        }
        
      } else {
        sds.rst <- stack(readGDAL(sds, as.is = TRUE, silent = TRUE))
        for (j in 1:sds.info[["bands"]])
          sds.rst[[j]] <- (sds.rst[[j]] - sds.scale.offset[2]) * sds.scale.offset[1]
        sds.rst <- writeRaster(sds.rst, paste0(outdir, outname), 
                               format = "GTiff", overwrite = TRUE, bylayer = FALSE)
        
        # Append current parameter to Lat/Lon 'SpatialPoints'
        lat.lon.spdf <- SpatialPointsDataFrame(lat.lon.sp, 
                                               data = data.frame(sds.rst[]))
        
        # Crop Lat/Lon 'SpatialPointsDataFrame' with Kili extent
        lat.lon.spdf.crp <- crop(lat.lon.spdf, kili)

        # Remove NA rows
        if (is.null(lat.lon.spdf.crp))
          return(NULL)
        
        # Reproject to EPSG:21037
        lat.lon.spdf.crp <- spTransform(lat.lon.spdf.crp, CRS("+init=epsg:21037"))
        
        cc <- sapply(1:nrow(lat.lon.spdf.crp), function(j) {
          any(!is.na(lat.lon.spdf.crp@data[j, ]))
        })
        lat.lon.spdf.crp <- lat.lon.spdf.crp[cc, ]
        
        # 'SpatialPointsDataFrame' to raster conversion
        if (nrow(lat.lon.spdf.crp) > 0) {
          
          lat.lon.rst.crp <- stack(lapply(1:sds.info[["bands"]], function(j) {
            rst <- rasterize(lat.lon.spdf.crp, template.rst.utm, 
                             field = names(lat.lon.spdf.crp@data)[j], fun = mean)
          }))
          
          lat.lon.rst.crp <- 
            writeRaster(lat.lon.rst.crp, format = "GTiff", overwrite = TRUE, 
                        filename = paste0(outdir, "CRP_", outname), 
                        bylayer = FALSE)

          lat.lon.rst.crp.rsmpl <- resample(lat.lon.rst.crp, template.rsmpl)
          
          lat.lon.rst.crp.rsmpl <- 
            writeRaster(lat.lon.rst.crp.rsmpl, format = "GTiff", overwrite = TRUE, 
                        filename = paste0(outdir, "RSMPL_CRP_", outname), 
                        bylayer = FALSE)
        }
      }
      
      return(NULL)
    })
  }
  
  return(NULL)
}


## Reprocess dew point temperatures

outdir <- "md07/processed/"

# Process single scenes, i.e. morning and evening separately
rmp.fls <- list.files(outdir, pattern = "^CRP_.*Retrieved_Moisture_Profile.tif$", 
                      full.names = TRUE)

dpt.rst <- foreach(i = rmp.fls, .packages = lib) %dopar% {
  filename.sh <- basename(i)
  outname <- paste0("DPT_", filename.sh)
  
  tmp.rst <- stack(i)
  calc(tmp.rst, fun = function(x, ...) {
    if (all(is.na(x))) return(NA) else return(max(x, ...))
  }, na.rm = TRUE, filename = paste0(outdir, outname), 
       overwrite = TRUE)
}

# Daily aggregation (fun = max)
dpt.fls <- list.files(outdir, pattern = "^DPT_.*.tif$", full.names = TRUE)
dpt.rst <- stack(dpt.fls)

indices <- as.numeric(as.factor(substr(basename(dpt.fls), 19, 25)))
dpt.rst.aggdly <- 
  stackApply(dpt.rst, indices, fun = max, overwrite = TRUE, 
             filename = paste0(outdir, "AGGDLY"), bylayer = TRUE, 
             suffix = substr(basename(dpt.fls[!duplicated(indices)]), 1, 
                             nchar(basename(dpt.fls[!duplicated(indices)]))-4))

# Monthly aggregation (fun = median)
dpt.fls.dly <- list.files(outdir, pattern = "^AGGDLY_DPT.*.tif$", 
                          full.names = TRUE)
dpt.rst.dly <- stack(dpt.fls.dly)

indices <- as.numeric(as.factor(as.yearmon(substr(basename(dpt.fls.dly), 26, 32), "%Y%j")))
dpt.rst.aggmly <- 
  stackApply(dpt.rst.dly, indices, fun = median, 
             overwrite = TRUE, filename = paste0(outdir, "AGGMLY"), 
             bylayer = TRUE, 
             suffix = substr(basename(dpt.fls.dly[!duplicated(indices)]), 1, 
                             nchar(basename(dpt.fls[!duplicated(indices)])-4)))

dpt.fls.aggmly <- list.files("md07/processed/", pattern = "AGGMLY.*DPT.*.tif$", 
                             full.names = TRUE)
dpt.rst.aggmly <- lapply(dpt.fls.aggmly, stack)


## Reprocess air temperatures

# Process single scenes, i.e. morning and evening separately
rtp.fls <- list.files(outdir, pattern = "^CRP_.*Retrieved_Temperature_Profile.tif$", 
                      full.names = TRUE)

ta.rst <- foreach(i = rtp.fls, .packages = lib) %dopar% {
  filename.sh <- basename(i)
  outname <- paste0("TA_", filename.sh)
  
  tmp.rst <- stack(i)
  calc(tmp.rst, fun = function(x, ...) {
    if (all(is.na(x))) return(NA) else return(max(x, ...))
  }, na.rm = TRUE, filename = paste0(outdir, outname), 
       overwrite = TRUE)
}

# Daily aggregation (fun = max)
ta.fls <- list.files(outdir, pattern = "^TA_.*.tif$", full.names = TRUE)
ta.rst <- stack(ta.fls)

indices <- as.numeric(as.factor(substr(basename(ta.fls), 18, 24)))
ta.rst.aggdly <- 
  stackApply(ta.rst, indices, fun = max, overwrite = TRUE, format = "GTiff",
             filename = paste0(outdir, "AGGDLY"), bylayer = TRUE, 
             suffix = substr(basename(ta.fls[!duplicated(indices)]), 1, 
                             nchar(basename(ta.fls[!duplicated(indices)]))-4))

# Monthly aggregation (fun = median)
ta.fls.dly <- list.files(outdir, pattern = "^AGGDLY_TA.*.tif$", 
                         full.names = TRUE)
ta.rst.dly <- stack(ta.fls.dly)

indices <- as.numeric(as.factor(as.yearmon(substr(basename(ta.fls.dly), 25, 31), "%Y%j")))
ta.rst.aggmly <- 
  stackApply(ta.rst.dly, indices, fun = median, format = "GTiff", bylayer = TRUE,
             overwrite = TRUE, filename = paste0(outdir, "AGGMLY"), 
             suffix = substr(basename(ta.fls.dly[!duplicated(indices)]), 1, 
                             nchar(basename(ta.fls.dly[!duplicated(indices)]))-4))

ta.fls.aggmly <- list.files("md07/processed/", pattern = "AGGMLY.*TA.*.tif$", 
                            full.names = TRUE)
ta.rst.aggmly <- lapply(ta.fls.aggmly, stack)


# ECMWF temperature data from different pressure levels
ta.nc <- open.ncdf("ecmwf/netcdf-atls04-20140725100507-9022-0428.nc")
ta.levels <- ta.nc$var$t$dim[[3]]$vals

ta <- readGDAL("ecmwf/netcdf-atls04-20140725100507-9022-0428.nc", 
               as.is = TRUE, p4s = "+init=epsg:4326")
ta <- brick(ta)
ta <- (ta * ta.nc$var$t$scaleFact) + ta.nc$var$t$addOffset

ta.rst <- foreach(k = ta.levels, l = 1:17, .packages = lib) %dopar% {
  
  rst <- ta[[seq(l, nlayers(ta), 17)]]
  rst.utm <- projectRaster(rst, crs = CRS("+init=epsg:21037"), format = "GTiff",
                           filename = paste0("ecmwf/air_temperature_2013_", 
                                             formatC(k, width = 4, flag = 0)), 
                           overwrite = TRUE, unstack = FALSE)
  crop(rst.utm, template.rst.utm, overwrite = TRUE, unstack = FALSE, snap = "out",
       filename = paste0("ecmwf/CRP_air_temperature_2013_", 
                         formatC(k, width = 4, flag = 0)), format = "GTiff")
}

ta.rst.rsmpl <- foreach(i = ta.rst, .packages = lib) %dopar% {
  rst.rsmpl <- 
    resample(i, template.rsmpl, 
             filename = paste0("ecmwf/RSMPL_", 
                               unique(sapply(strsplit(names(i), "\\."), "[[", 1))), 
             format = "GTiff", overwrite = TRUE, bylayer = FALSE)
  
  return(rst.rsmpl)
}

ta.fls.rsmpl <- list.files("ecmwf/", pattern = "^RSMPL_.*temperature.*.tif$", 
                           full.names = TRUE)
ta.rst.rsmpl <- lapply(ta.fls.rsmpl, stack)

ta.rst.rsmpl.dly <- 
  foreach(i = 1:nlayers(ta.rst.rsmpl[[1]]), .packages = lib) %dopar% {
    foreach(j = ta.rst.rsmpl, .combine = "stack") %do% {
      j[[i]]
    }
  }


## Include geopotential heights

# Surface air pressure via geopotential heights
p.nc <- open.ncdf("ecmwf/netcdf-atls04-20140722100225-5449-0149.nc")
p.levels <- p.nc$var$z$dim[[3]]$vals

gp <- readGDAL("ecmwf/netcdf-atls04-20140722100225-5449-0149.nc", 
               as.is = TRUE, p4s = "+init=epsg:4326")
gp <- brick(gp)
gp <- ((gp * p.nc$var$z$scaleFact) + p.nc$var$z$addOffset) / 9.80665

gp.rst <- foreach(k = p.levels, l = 1:17, .packages = lib) %dopar% {
  
  rst <- gp[[seq(l, nlayers(gp), 17)]]
  rst.utm <- projectRaster(rst, crs = CRS("+init=epsg:21037"), format = "GTiff",
                           filename = paste0("ecmwf/surface_pressure_2013_", 
                                             formatC(k, width = 4, flag = 0)), 
                           overwrite = TRUE, unstack = FALSE)
  crop(rst.utm, template.rst.utm, overwrite = TRUE, unstack = FALSE, snap = "out",
       filename = paste0("ecmwf/CRP_surface_pressure_2013_", 
                         formatC(k, width = 4, flag = 0)), format = "GTiff")
}

gp.fls <- list.files("ecmwf/", pattern = "^CRP_.*pressure.*.tif$", full.names = TRUE)
gp.rst <- lapply(gp.fls, stack)

# Resample DEM and geopotential heights to NDVI resolution (250 m)
gp.rst.rsmpl <- foreach(i = gp.rst, .packages = lib) %dopar% {
  rst.rsmpl <- 
    resample(i, template.rsmpl, 
             filename = paste0("ecmwf/RSMPL_", 
                               unique(sapply(strsplit(names(i), "\\."), "[[", 1))), 
             format = "GTiff", overwrite = TRUE, bylayer = FALSE)
  
  return(rst.rsmpl)
}

gp.fls.rsmpl <- list.files("ecmwf/", pattern = "^RSMPL.*pressure.*.tif$", 
                           full.names = TRUE)
gp.rst.rsmpl <- foreach(i = gp.fls.rsmpl, .packages = lib) %dopar% stack(i)

gp.rst.rsmpl.dly <- 
  foreach(i = 1:nlayers(gp.rst.rsmpl[[1]]), .packages = lib) %dopar% {
    foreach(j = gp.rst.rsmpl, .combine = "stack") %do% {
      j[[i]]
    }
  }

# # Not run (takes very long)
# outnames <- paste0("ecmwf/DLY_", 
#                    substr(basename(gp.fls.rsmpl), 1, 
#                           nchar(basename(gp.fls.rsmpl)) - 9), 
#                    formatC(1:length(gp.rst.rsmpl.dly), width = 3, flag = 0))
# gp.rst.rsmpl.dly <- 
#   foreach(i = gp.rst.rsmpl.dly, j = as.list(outnames), .packages = lib) %dopar% {
#     writeRaster(i, j, format = "GTiff", overwrite = TRUE, bylayer = FALSE)
#   }

# Import and resample DEM
dem <- raster("dem/DEM_ARC1960_30m_Hemp.tif")
dem.rsmpl <- resample(dem, gp.rst.rsmpl.dly[[1]])
dem.rsmpl <- crop(dem.rsmpl, template.rst.utm, snap = "out")

# Calculate surface pressure (sp) from DEM, geopotential heights and 
# temperature profiles according to barometric formula
sp.template <- gp.rst.rsmpl.dly[[1]][[1]]
sp.template[] <- NA

outnames <- paste0("ecmwf/DLY_RSMPL_CRP_surface_pressure_2013", 
                   formatC(1:length(gp.rst.rsmpl.dly), width = 3, flag = 0))

sp.rst <- foreach(i = gp.rst.rsmpl.dly, j = ta.rst.rsmpl.dly, 
                  k = as.list(outnames), .packages = lib, .verbose = TRUE,
                  .combine = "stack", .export = "barometricFormula") %dopar% {
  
  sp.tmp <- sp.template
  
  gp.mat.rsmpl.dly <- as.matrix(i)
  ta.mat.rsmpl.dly <- as.matrix(j)

  sp.tmp[] <- sapply(1:nrow(gp.mat.rsmpl.dly), function(k) {
    z <- dem.rsmpl[k]
    gp <- gp.mat.rsmpl.dly[k, ]
    ta <- ta.mat.rsmpl.dly[k, ]
    
    if (is.na(z) | any(is.na(gp)))
      return(NA)
    
    barometricFormula(z = z, 
                      gp = gp, 
                      ta = ta, 
                      p.levels = p.levels)
    
  })
  
  sp.tmp <- writeRaster(sp.tmp, k, format = "GTiff", overwrite = TRUE)
  
  return(sp.tmp)
}

# Deregister parallel backend
stopCluster(cl)



### Test
# Deviation in boundary coordinates between SDS layers "Latitude"/"Longitude"
# and coordinates from SDS-based metadata 
i <- 136
sds.rst[i]
xy <- xyFromCell(sds.rst, i)
plot(sds.rst)
points(xy)
rst <- sds.rst
rst[][-i]  <- NA
shp <- rasterToPolygons(rst)
(ymax(extent(shp)) - ymax(file.ext)) * 111.32

i <- 4186
sds.rst[i]
south <- TRUE
while(south & !is.na(i)) {
  adj <- i
  i <- adjacent(sds.rst, i, pairs = FALSE)[4]
}
i <- adj
sds.rst[i]
xy <- xyFromCell(sds.rst, i)
plot(sds.rst)
points(xy)
rst <- sds.rst
rst[][-i]  <- NA
shp <- rasterToPolygons(rst)
(ymin(extent(shp)) - ymin(file.ext)) * 111.32
# Order of pressure levels in temperature/moisture profiles (5, 10, 20, ... hPa)
p <- c(5, 10, 20, 30, 50, 70, 100, 150, 200, 250, 
       300, 400, 500, 620, 700, 780, 850, 920, 950, 1000)
x <- sapply(1:nlayers(sds.rst), function(i) median(sds.rst[[i]][], na.rm = TRUE))
plot(p ~ x, type = "b", ylim = c(1000, 0))
axis(4)
# gdalUtils
gdal_translate(filename, dst_dataset = "md07/processed/tmp.tif", 
               sd_index = 8, output_Raster = FALSE)
plot((rst - sds.scale.offset[2]) * sds.scale.offset[1])

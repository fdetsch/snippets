switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/fdetsch/XChange/kilimanjaro/evapotranspiration/"), 
       "Windows" = setwd("D:/kilimanjaro/evapotranspiration/"))

lib <- c("raster", "rgdal", "doParallel", "zoo")
sapply(lib, function(x) require(x, character.only = TRUE))

registerDoParallel(cl <- makeCluster(3))

# Kili extent
template.ext.ll <- extent(37, 37.72, -3.4, -2.84)
template.rst.ll <- raster(ext = template.ext.ll)
template.rst.utm <- projectExtent(template.rst.ll, crs = "+init=epsg:32737")

# Import daytime Terra and Aqua LST
fls.lst.day <- list.files("MODIS_ARC/PROCESSED/TMP1km", 
                          pattern = "Day_1km.tif$", full.names = TRUE)
fls.lst.day <- fls.lst.day[order(substr(basename(fls.lst.day), 10, 16))]
doy <- substr(basename(fls.lst.day), 10, 16)

rst.lst.day <- 
  stack(foreach(i = fls.lst.day, .packages = lib) %dopar% {
    crop((raster(i) * 0.02 - 273.15), template.rst.utm, 
         filename = paste0("md11a2/processed/CRP_", basename(i)), 
         overwrite = TRUE)
  })

# Calculate daily maximum daytime temperatures from Terra and Aqua (usually 
# Aqua higher than Terra, but Terra will be taken in case of missing Aqua cells)
time.range <- seq(as.Date("2013-01-01"), as.Date("2013-12-31"), 1)
indices <- as.numeric(as.factor(doy))
start.day <- sapply(unique(indices), function(i) which(indices == i)[1])
rst.lst.day.mrg <- 
  stackApply(rst.lst.day, indices, fun = max, na.rm = TRUE, 
             filename = "md11a2/processed/MRG", 
             overwrite = TRUE, format = "GTiff", bylayer = TRUE, 
             suffix = gsub("MOD11A1", "MD11A1", names(rst.lst.day)[start.day]))

# Calculate monthly mean daytime temperatures
indices <- as.numeric(as.factor(as.yearmon(time.range)))
start.month <- sapply(unique(indices), function(i) which(indices == i)[1])
rst.lst.mth <- 
  stackApply(rst.lst.day.mrg, indices, fun = function(...) mean(...), 
             na.rm = TRUE, filename = "md11a2/processed/AGG1MTH", 
             overwrite = TRUE, format = "GTiff", bylayer = TRUE, 
             suffix = names(rst.lst.day.mrg)[start.month])

rst.lst.mth <- stack(list.files("md11a2/processed", pattern = "^AGG1MTH.*.tif$", 
                                full.names = TRUE))

# Resample 1km LST raster do 250m (SAVI, NDVI, LAI, etc.) resolution
rst.lst.mth.rsmpl <- 
  round(disaggregate(rst.lst.mth, fact = 4, method = "bilinear", 
               filename = "md11a2/processed/RSMPL", overwrite = TRUE, 
               format = "GTiff", bylayer = TRUE, suffix = names(rst.lst.mth)), 
        digits = 1)

writeRaster(rst.lst.mth.rsmpl, filename = "md11a2/processed/RSMPL", 
            bylayer = TRUE, overwrite = TRUE, format = "GTiff", 
            suffix = names(rst.lst.mth.rsmpl))

# Deregister parallel backend
stopCluster(cl)

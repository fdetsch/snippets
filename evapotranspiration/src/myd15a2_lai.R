switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/fdetsch/XChange/kilimanjaro/evapotranspiration/"), 
       "Windows" = setwd("D:/kilimanjaro/evapotranspiration/"))

lib <- c("raster", "rgdal", "doParallel")
sapply(lib, function(x) require(x, character.only = TRUE))

# Required functions
source("src/number2binary.R")

registerDoParallel(cl <- makeCluster(4))

# Kili extent
template.ext.ll <- extent(37, 37.72, -3.4, -2.84)
template.rst.ll <- raster(ext = template.ext.ll)
template.rst.utm <- projectExtent(template.rst.ll, crs = "+init=epsg:32737")

# Import LAI and corresponding QC layers
rst.lai <- 
  foreach(i = c("Lai_1km.tif$", "Lai_QC.tif$", "Extra_QC.tif$"), 
          j = c(.1, 1, 1)) %do% {
    fls.lai <- list.files("MODIS_ARC/PROCESSED/LAI1km", pattern = i, 
                          full.names = TRUE)
    foreach(k = fls.lai, .packages = lib, .combine = "stack") %dopar% {
      crop(raster(k) * j, template.rst.utm, 
           filename = paste0("myd15a2/processed/CRP_", basename(k)),  
           overwrite = TRUE)
    }
  }

# Quality check: Lai_QC
rst.lai.cc <- 
  stack(foreach(i = 1:nlayers(rst.lai[[1]]), .packages = lib) %dopar% {
    overlay(rst.lai[[1]][[i]], rst.lai[[2]][[i]], 
            fun = function(x, y) {
              index <- sapply(y[], function(i) {
                if (!is.na(i)) {
                  # 8-bit string
                  bit <- number2binary(i, 8)
                  # Cloud state
                  state <- paste(bit[c(4, 5)], 
                                 collapse = "") %in% c("00", "11", "10")
                  
                  return(state)
                } else {
                  return(FALSE)
                }
              })
              x[!index] <- NA
              return(x)
            }, filename = paste0("myd15a2/processed/CC_", 
                                 names(rst.lai[[1]])[i]), 
            overwrite = TRUE, format = "GTiff")
  })

# Quality check: Extra_QC
rst.lai.cc.cc <- 
  stack(foreach(i = 1:nlayers(rst.lai.cc), .packages = lib) %dopar% {
    overlay(rst.lai.cc[[i]], rst.lai[[3]][[i]], 
            fun = function(x, y) {
              index <- sapply(y[], function(i) {
                if (!is.na(i)) {
                  # 8-bit string
                  bit <- number2binary(i, 8)
                  
                  # Snow/ice detected
                  snow <- bit[6] == 0
                  # Cirrus detected
                  cirrus <- bit[4] == 0
                  # Internal cloud mask
                  clouds <- bit[3] == 0
                  # Cloud shadow
                  shadow <- bit[2] == 0
                  
                  return(all(snow, cirrus, clouds, shadow))
                } else {
                  return(FALSE)
                }
              })
              x[!index] <- NA
              return(x)
            }, filename = paste0("myd15a2/processed/CC_", 
                                 names(rst.lai.cc[[i]])), 
            overwrite = TRUE, format = "GTiff")
  })

# Overlay quality-controlled LAI 
rst.lai.cc.cc.agg <- calc(rst.lai.cc.cc, mean, na.rm = TRUE)

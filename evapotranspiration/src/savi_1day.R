### Environmental settings

# Workspace clearance
rm(list = ls(all = TRUE))

# Working directory
switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/fdetsch/XChange/kilimanjaro/evapotranspiration"), 
       "Windows" = setwd("D:/kilimanjaro/evapotranspiration"))

# Required packages
lib <- c("raster", "rgdal", "doParallel", "zoo")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

# Required functions
source("src/number2binary.R")

# Parallelization
registerDoParallel(cl <- makeCluster(4))


### Data processing

## Cropping

# Kili extent
template.ext.ll <- extent(37, 37.72, -3.4, -2.84)
template.rst.ll <- raster(ext = template.ext.ll)
template.rst.utm <- projectExtent(template.rst.ll, crs = "+init=epsg:32737")

# Crop (and resample) band 1, band 2 and QA layer by Kili extent
rst.b1.b2.qa <- 
  foreach(i = c("MYD09GQ.*b01", "MYD09GQ.*b02", "MYD09GA.*state_1km")) %do% {
    
    # List available files
    tmp.fls <- list.files("MODIS_ARC/PROCESSED/", 
                          pattern = i, recursive = TRUE, full.names = TRUE)
    
    # Crop
    tmp.rst <- foreach(j = tmp.fls, .packages = lib) %dopar% {
      crop(raster(j), template.rst.utm, 
           filename = paste0("myd09gq/processed/CRP_", basename(j)),  
           overwrite = TRUE)
    }
    
    # Disaggregate raster data with coarser resolution, i.e. MYD09GA
    if (i == "MYD09GA.*state_1km")
      tmp.rst <- foreach(j = tmp.rst, .packages = lib) %dopar% {
        disaggregate(j, fact = 4, 
                     filename = paste0("myd09gq/processed/DAG_", names(j)), 
                     format = "GTiff", overwrite = TRUE)
      }
    
    # Stack and return cropped RasterLayers
    tmp.stck <- stack(tmp.rst)
    return(tmp.stck)
  }

# Import cropped and disaggregated raster data
rst.b1.b2.qa <- 
  foreach(i = c("^CRP_MYD09GQ.*b01", "^CRP_MYD09GQ.*b02", 
                "^DAG.*MYD09GA.*state_1km"), .packages = lib) %dopar% {
    
    tmp.fls <- list.files(ifelse(i != "^DAG.*MYD09GA.*state_1km", 
                                 "myd09gq/processed/",
                                 "myd09ga/processed/"),
                          pattern = paste(i, ".tif", sep = ".*"), 
                          recursive = TRUE, full.names = TRUE)
    return(stack(tmp.fls))
  }
  

## Quality control:
## Reject all cloud contaminated cells in MYD09GQ bands 1 and 2 based on 
## corresponding cloud information in MYD09GA

# Loop through bands 1 and 2
rst.b1.b2.cc <- foreach(i = c(1, 2)) %do% {
  # Loop through layers of currend band RasterStack
  stack(foreach(j = 1:nlayers(rst.b1.b2.qa[[i]]), .packages = lib) %dopar%
    overlay(rst.b1.b2.qa[[i]][[j]], rst.b1.b2.qa[[3]][[j]], 
            fun = function(x, y) {
              index <- sapply(y[], function(i) {
                if (!is.na(i)) {
                  # 16-bit string
                  bit <- number2binary(i, 16)
                  # Cloud state
                  state <- paste(bit[c(15, 16)], 
                                 collapse = "") %in% c("00", "11", "10")
                  # # Shadow
                  # shadow <- bit[14] == 0
                  # Cirrus
                  cirrus <- paste(bit[c(7, 8)], 
                                  collapse = "") %in% c("00", "01")
                  # Intern cloud algorithm
                  intcl <- bit[6] == 0
                  # Snow mask
                  snow <- bit[4] == 0
                  # Adjacent clouds
                  adjcl <- bit[3] == 0
                  
                  return(all(state, snow, cirrus, intcl, adjcl))
                } else {
                  return(FALSE)
                }
              })
              x[!index] <- NA
              return(x)
            }, filename = paste0("myd09gq/processed/CC_", 
                                 names(rst.b1.b2.qa[[i]][[j]])), 
            overwrite = TRUE, format = "GTiff"))
}

# Import cloud-corrected raster data
rst.b1.b2.cc <- 
  foreach(i = c("^CC_.*MYD09GQ.*b01", "^CC_.*MYD09GQ.*b02"), 
          .packages = lib) %dopar% {
            fls <- list.files("myd09gq/processed", pattern = i, full.names = TRUE)
            return(stack(fls))
}


## Aggregation on monthly values

indices <- as.numeric(as.factor(as.yearmon(seq(as.Date("2013-01-01"), 
                                               as.Date("2013-02-28"), 1))))

rst.b1.b2.agg <- 
  foreach(i = c(1, 2), .packages = lib) %dopar% {
    foreach(j = unique(indices), .combine = "stack") %do% {
      sub <- rst.b1.b2.cc[[i]][[grep(j, indices)]]
      calc(sub, fun = function(x) {
        if (all(is.na(x))) return(NA) else return(round(median(x, na.rm = TRUE)))
      }, filename = paste0("myd09gq/processed/AGG_", names(sub)[1]), 
      format = "GTiff", overwrite = TRUE)
    }
  }

rst.b1.b2.agg <- 
  foreach(i = c("^AGG.*b01", "^AGG.*b02")) %do% {
    fls <- list.files("myd09gq/processed", pattern = i, full.names = TRUE)
    return(stack(fls))
  }

### Results

ndvi <- (rst.b1.b2.agg[[2]]-rst.b1.b2.agg[[1]]) / 
  (rst.b1.b2.agg[[2]]+rst.b1.b2.agg[[1]])
ndvi <- round(ndvi, digits = 2)
for (i in 1:nlayers(ndvi))
  ndvi[[i]][ndvi[[i]][] > 1] <- 1
writeRaster(ndvi, "myd09gq/processed/NDVI", bylayer = TRUE, format = "GTiff", 
            suffix = names(ndvi), overwrite = TRUE)

savi <- 1.5 * (rst.b1.b2.agg[[2]]-rst.b1.b2.agg[[1]]) / 
  (rst.b1.b2.agg[[2]]+rst.b1.b2.agg[[1]]+0.5)
savi <- round(savi, digits = 2)
writeRaster(savi, "myd09gq/processed/SAVI", bylayer = TRUE, format = "GTiff", 
            suffix = names(savi), overwrite = TRUE)

# Deregister parallel backend
stopCluster(cl)
### Environmental settings

# Workspace clearance
rm(list = ls(all = TRUE))

# Working directory
switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/fdetsch/XChange/kilimanjaro/evapotranspiration"), 
       "Windows" = setwd("D:/kilimanjaro/evapotranspiration"))

# Required packages
lib <- c("raster", "rgdal", "doParallel")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

# Required functions
source("src/number2binary.R")

# Parallelization
registerDoParallel(cl <- makeCluster(3))



### Data processing

## Cropping

# Kili extent
template.ext.ll <- extent(37, 37.72, -3.4, -2.84)
template.rst.ll <- raster(ext = template.ext.ll)
template.rst.utm <- projectExtent(template.rst.ll, crs = "+init=epsg:32737")

# Crop band 1, band 2 and QA layer by Kili extent
rst.b1.b2.qa <- 
  foreach(i = c("MYD09Q1.*b01", "MYD09Q1.*b02", "MYD09Q1.*qc")) %do% {
    
    tmp.fls <- list.files("MODIS_ARC/PROCESSED/", 
                          pattern = i, recursive = TRUE, full.names = TRUE)[1:8]
    
    # Crop raster data
    tmp.rst <- foreach(j = tmp.fls, .packages = lib) %dopar% {
      crop(raster(j), template.rst.utm, 
           filename = paste0("myd09q1/processed/CRP_", basename(j)),  
           overwrite = TRUE)
    }
    
    # Stack and return cropped RasterLayers
    tmp.stck <- stack(tmp.rst)
    return(tmp.stck)
  }

## Quality control, step 1:
## Reject all cloud contaminated cells based on corresponding quality 
## information included in MYD09Q1 (8-day)

rst.b1.b2.qc <- foreach(i = c(1, 2)) %do% {
  stack(foreach(j = 1:nlayers(rst.b1.b2.qa[[i]]), .packages = lib) %dopar%
    overlay(rst.b1.b2.qa[[i]][[j]], rst.b1.b2.qa[[3]][[j]], 
            fun = function(x, y) {
              index <- sapply(y[], function(k) {
                modland <- paste(number2binary(k, 16)[c(15, 16)], 
                                 collapse = "") %in% c("00", "01")
                clstate <- paste(number2binary(k, 16)[c(13, 14)], 
                                 collapse = "") %in% c("00", "11")
                b1.qual <- !paste(number2binary(k, 16)[9:12], 
                                  collapse = "") %in% c("1011", "1101", "1110", "1111")
                b2.qual <- !paste(number2binary(k, 16)[5:8], 
                                  collapse = "") %in% c("1011", "1101", "1110", "1111")
                
                return(all(modland, clstate, b1.qual, b2.qual))
              })
      x[!index] <- NA
    return(x)
  }, filename = paste0("myd09q1/processed/QC_", names(rst.b1.b2.qa[[i]][[j]])), 
  overwrite = TRUE, format = "GTiff"))
}

## Quality control, step 2:
## Reject all cloud contaminated cells based on corresponding quality 
## information included in MYD09GA (1-day, aggregated on 8-day)

fls.ga <- list.files("myd09ga/processed/", pattern = "^AGG.*.tif$", 
                     full.names = TRUE)[1:8]
rst.ga <- stack(fls.ga)

rst.b1.b2.cc <- foreach(i = c(1, 2)) %do% {
  foreach(j = 1:nlayers(rst.b1.b2.qc[[i]]), k = 1:nlayers(rst.ga), 
          .packages = lib, .combine = "stack") %dopar%
    overlay(rst.b1.b2.qc[[i]][[j]], rst.ga[[k]], 
            fun = function(x, y) {
              x[y[] == 0] <- NA
              return(x)
            }, filename = paste0("myd09q1/processed/CC_", names(rst.b1.b2.qc[[i]][[j]])), 
            overwrite = TRUE, format = "GTiff")
}

## Aggregation on monthly values

indices <- as.numeric(as.factor(as.yearmon(seq(as.Date("2013-01-01"), 
                                               as.Date("2013-02-28"), 8))))

rst.b1.b2.agg <- 
  foreach(i = c(1, 2), .packages = lib) %dopar% {
    foreach(j = unique(indices), .combine = "stack") %do% {
      sub <- rst.b1.b2.cc[[i]][[grep(j, indices)]]
      calc(sub, fun = function(x) {
        if (all(is.na(x))) return(NA) else return(round(median(x, na.rm = TRUE)))
      }, filename = paste0("myd09q1/processed/AGG_", names(sub)[1]), 
      format = "GTiff", overwrite = TRUE)
    }  
  }


### Results

savi <- 1.5 * (rst.b1.b2.agg[[2]]-rst.b1.b2.agg[[1]]) / 
  (rst.b1.b2.agg[[2]]+rst.b1.b2.agg[[1]]+0.5)
writeRaster(savi, "myd09q1/processed/SAVI", bylayer = TRUE, format = "GTiff", 
            suffix = names(savi), overwrite = TRUE)

# Deregister parallel backend
stopCluster(cl)
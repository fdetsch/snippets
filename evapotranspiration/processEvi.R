switch(Sys.info()[["sysname"]], 
       "Windows" = setwd("F:/kilimanjaro/evapotranspiration/"), 
       "Linux" = setwd("/media/XChange/kilimanjaro/evapotranspiration"))

lib <- c("MODIS", "doParallel")
sapply(lib, function(x) require(x, character.only = TRUE))

registerDoParallel(cl <- makeCluster(4))


## Geographic extent

kili <- data.frame(x = c(37, 37.72), y = c(-3.4, -2.84), id = c("ll", "ur"))
coordinates(kili) <- c("x", "y")
projection(kili) <- CRS("+init=epsg:4326")
kili <- spTransform(kili, CRS("+init=epsg:32737"))


## EVI layers

# Import MYD13Q1 SDS layers relevant for whittaker filter
rst <- foreach(x = c("EVI", "VI_Quality", "composite_day_of_the_year", 
                "pixel_reliability"), .packages = lib) %dopar% {
  fls <- list.files("MODIS_ARC/PROCESSED/EVIkili/", 
                    pattern = paste0("MYD.*", x, ".tif$"), full.names = TRUE)
  
  rst.crp <- stack(lapply(fls, function(y) {
    tmp.rst <- raster(y)
    crop(tmp.rst, kili, 
         filename = paste0("myd13q1/processed/CRP_", basename(y)), 
         format = "GTiff", overwrite = TRUE, bylayer = TRUE)
  }))
  
  if (x == "EVI")
    rst.crp <- rst.crp * 0.0001
  
  return(rst.crp)
}

# Remove clouds based on provided 'pixel reliability' layer
fls <- list.files("myd13q1/processed/", pattern = "^CRP_.*_EVI.tif$")

rst[[1]] <- overlay(rst.crp[[1]], rst.crp[[4]], 
                    fun = function(x, y) {
                      x[!y[] %in% 0:2] <- NA
                      return(x)
                    }, 
                    filename = paste0("myd13q1/processed/QA_", basename(fls.crp)), 
                    format = "GTiff", overwrite = TRUE, bylayer = TRUE)
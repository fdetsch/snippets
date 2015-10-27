## working directory
library(Orcs)
setwdOS(path_ext = "programming/r/chirps")

## packages
library(chirps)
library(rworldmap)
library(doParallel)

## parallelization
supcl <- makeCluster(3)
registerDoParallel(supcl)

chirps_files_gz <- list.files("/media/permanent/programming/r/chirps/data",
                              pattern = ".gz$", full.names = TRUE)

chirps_files <- extractChirps(chirps_files_gz, remove = FALSE, overwrite = TRUE)

data("countriesCoarse")
spy_iran <- subset(countriesCoarse, ADMIN == "Iran")

## crop images
overwrite <- TRUE
lst_chirps_crp <-
  foreach(i = chirps_files, .packages = c("raster", "rgdal")) %dopar% {
    # output filename
    filename <- paste0("data/crp/CRP_", basename(i))
    # if file does not exist or overwrite is TRUE, crop image
    if (!file.exists(filename) | overwrite) {
      rst <- raster(i)
      crop(rst, spy_iran, filename = filename,
           format = "GTiff", overwrite = TRUE)
    # otherwise return existing cropped image
    } else {
      raster(filename)
    }
  }

## reject invalid values
lst_chirps_scl <-
  foreach(i = lst_chirps_crp, .packages = c("raster", "rgdal")) %dopar% {
    # output filename
    filename <- paste0("data/scl/SCL_", names(i), ".tif")
    # if file does not exist or overwrite is TRUE, rescale image
    if (!file.exists(filename) | overwrite) {
      rst <- i
      rst[rst[] < 0] <- 0
      writeRaster(rst, filename = filename,
                  format = "GTiff", overwrite = TRUE)
      # otherwise return existing cropped image
    } else {
      raster(filename)
    }
  }

## resample
## deregister parallel backend
stopCluster(supcl)

# Required packages
library(doParallel)

# Working directory
setwd("E:/repositories/julendat/src/julendat/rmodules")

# Load function 'gfWrite'
source("gfWrite.R")

# Data sets
# stations <- c("E:/kilimanjaro/temperature_network/data/noaa_gsod_airport_1973_2013_reformat.csv", 
#               "E:/kilimanjaro/temperature_network/data/noaa_gsod_arusha_1973_2013_reformat.csv",
#               "E:/kilimanjaro/temperature_network/data/noaa_gsod_moshi_1973_2013_reformat.csv", 
#               "E:/kilimanjaro/temperature_network/data/noaa_gsod_nairobi_1973_2013_reformat.csv")

stations <- c("E:/kilimanjaro/temperature_network/out/noaa_gsod_airport_1973_2013_reformat_gf_li_ssa.csv", 
              "E:/kilimanjaro/temperature_network/out/noaa_gsod_arusha_1973_2013_reformat_gf.csv",
              "E:/kilimanjaro/temperature_network/out/noaa_gsod_moshi_1973_2013_reformat_gf.csv", 
              "E:/kilimanjaro/temperature_network/out/noaa_gsod_nairobi_1973_2013_reformat_gf_li_ssa.csv")

# Parallelization
registerDoParallel(clstr <- makeCluster(4))

# Gap-fill single data sets
foreach (i = seq(stations)) %dopar% {
  
  # Execute gfWrite
  gfWrite(files.dep = stations[i],
          files.indep = stations[-c(i, 3)],
          filepath.output = paste("E:/kilimanjaro/temperature_network/out/", 
                                  substr(basename(stations[i]), 1, nchar(basename(stations[i])) - 7), 
                                  "_gf2.csv", sep = ""),
          filepath.coords = NULL,
          quality.levels = NULL,
          gap.limit = 3650, 
          na.limit = .95,
          time.window = 3650,
          n.plot = 10,
          prm.dep = "TEMP", 
          prm.indep = NA, 
          family = gaussian, 
          plevel = sprintf("%04.0f", 100), 
          end.datetime = Sys.Date())
  
}

# Deregister parallel backend
stopCluster(clstr)
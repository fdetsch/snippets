# Environmental stuff
rm(list = ls(all = TRUE))

switch(Sys.info()[["sysname"]], 
       "Windows" = setwd("E:/"), 
       "Linux" = setwd("/media/permanent/"))

lib <- c("randomForest", "ggplot2", "latticeExtra")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

source("phd/scintillometer/src/slsMergeDailyData.R")

srunWorkspaces <- dir("SRun/", pattern = "workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

fls <- list.files(pattern = "_mrg_rf_agg01h.csv", recursive = TRUE, 
                  full.names = TRUE)

dat <- do.call("rbind", lapply(fls, function(i) {
  plt <- sapply(strsplit(basename(i), "_"), "[[", 1)
  dat <- read.csv(i)
  return(data.frame(plotid = plt, dat))
}))
dat$datetime <- strptime(dat$datetime, format = "%Y-%m-%d %H:%M")

ggplot(aes(x = datetime, y = waterET), data = dat) + 
  geom_histogram(stat = "identity") + 
  facet_wrap(~ plotid, ncol = 2, scales = "free_x") + 
  geom_hline(aes(y = 0), colour = "darkgrey") + 
  labs(x = "Time [h]", y = "Evapotranspiration [mm/h]") + 
  theme_bw()
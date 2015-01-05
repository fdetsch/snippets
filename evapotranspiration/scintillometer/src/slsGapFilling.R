### Environmental stuff

rm(list = ls(all = TRUE))

switch(Sys.info()[["sysname"]], 
       "Windows" = setwd("F:/kilimanjaro/evapotranspiration"), 
       "Linux" = setwd("/media/envin/XChange/kilimanjaro/evapotranspiration"))

lib <- c("randomForest", "ggplot2", "latticeExtra")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

source("scintillometer/src/slsMergeDailyData.R")

srunWorkspaces <- dir("scintillometer/SRun/", pattern = "workspace_SLS", 
                      recursive = FALSE, full.names = TRUE)


### Data processing 

ls_rf_agg10m <- lapply(srunWorkspaces, function(i) {
  plt <- sapply(strsplit(basename(i), "_"), "[[", 3)
  
  fls <- list.files(paste0(i, "/data/retrieved_SPU-111-230"), pattern = ".mnd$", 
                    full.names = TRUE)
  
  # Merge daily .mnd files
  dat <- slsMergeDailyData(files = fls, 
                           equal.columns = ifelse(plt %in% c("gra1", "fer0"), FALSE, TRUE))
  
  # Create continuous time series
  time.seq <- strptime(dat$datetime[!is.na(dat$datetime)], 
                       format = "%Y-%m-%d %H:%M:%S")
  time.seq <- strftime(seq(min(time.seq), max(time.seq), 60), 
                       format = "%Y-%m-%d %H:%M:%S")
  
  dat2 <- merge(data.frame(datetime = time.seq), 
                dat[, c("datetime", "tempUp", "tempLw", "dwnRad", "upwRad", 
                        "humidity", "soilHeatFlux", "pressure", 
                        "precipRate", "waterET")], 
                by = 1, all.x = TRUE)
  dat2$datetime <- strptime(dat2$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  if (!file.exists(paste0(i, "/data/out")))
    dir.create(paste0(i, "/data/out"))
  write.csv(dat2, paste0(i, "/data/out/", plt, "_mrg.csv"), row.names = FALSE)

  # 1h aggregation of gappy time series
  dat2_agg1h <- aggregate(dat2[, 2:ncol(dat2)], by = list(substr(dat2[, 1], 1, 13)), 
                          FUN = function(x) round(median(x, na.rm = TRUE), 2))
  dat2_agg1h[, 1] <- paste0(dat2_agg1h[, 1], ":00")
  names(dat2_agg1h)[1] <- "datetime"
  dat2_agg1h$datetime <- strptime(dat2_agg1h$datetime, format = "%Y-%m-%d %H:%M")
  
  write.csv(dat2_agg1h, paste0(i, "/data/out/", plt, "_mrg_agg01h.csv"), 
            row.names = FALSE)
  
  # 10m aggregation of gappy time series
  dat2_agg10m <- aggregate(dat2[, 2:ncol(dat2)], by = list(substr(dat2[, 1], 1, 15)), 
                    FUN = function(x) round(median(x, na.rm = TRUE), 2))
  dat2_agg10m[, 1] <- paste0(dat2_agg10m[, 1], "0")
  names(dat2_agg10m)[1] <- "datetime"
  
  write.csv(dat2_agg10m, paste0(i, "/data/out/", plt, "_mrg_agg10m.csv"), 
            row.names = FALSE)
  
  # Selection of training data
  index <- rowSums(is.na(dat2[, 2:(ncol(dat2)-1)])) == 0 & 
    is.na(dat2[, ncol(dat2)])
  
  dat2.train <- dat2[complete.cases(dat2[, -1]), 2:ncol(dat2)]
  dat2.test <- dat2[index, ]
  
  dat2.rf <- randomForest(waterET ~ ., data = dat2.train)
  
  # Random forest-based ET prediction
  pred <- predict(dat2.rf, dat2.test)
  dat3 <- dat2
  dat3[index, "waterET"] <- pred
  dat3$waterET <- as.numeric(as.character(dat3$waterET))
  
  write.csv(dat3, paste0(i, "/data/out/", plt, "_mrg_rf.csv"), row.names = FALSE)
  
  # 1h aggregation of gap-filled time series
  dat4 <- aggregate(dat3[, 2:ncol(dat3)], by = list(substr(dat3[, 1], 1, 13)), 
                    FUN = function(x) round(median(x, na.rm = TRUE), 2))
  dat4[, 1] <- paste0(dat4[, 1], ":00")
  names(dat4)[1] <- "datetime"
  dat4$datetime <- strptime(dat4$datetime, format = "%Y-%m-%d %H:%M")
  
  
  write.csv(dat4, paste0(i, "/data/out/", plt, "_mrg_rf_agg01h.csv"), 
            row.names = FALSE)
  
  # 10m aggregation of gap-filled time series
  dat5 <- aggregate(dat3[, 2:ncol(dat3)], by = list(substr(dat3[, 1], 1, 15)), 
                    FUN = function(x) round(median(x, na.rm = TRUE), 2))
  dat5[, 1] <- paste0(dat5[, 1], "0")
  names(dat5)[1] <- "datetime"
  
  write.csv(dat5, paste0(i, "/data/out/", plt, "_mrg_rf_agg10m.csv"), 
            row.names = FALSE)

  return(dat5)
})
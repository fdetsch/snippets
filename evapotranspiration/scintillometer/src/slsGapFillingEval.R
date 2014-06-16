### Environmental stuff

rm(list = ls(all = TRUE))

switch(Sys.info()[["sysname"]], 
       "Windows" = setwd("F:/kilimanjaro/evapotranspiration"), 
       "Linux" = setwd("/media/XChange/kilimanjaro/evapotranspiration"))

lib <- c("randomForest", "ggplot2", "latticeExtra")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

source("scintillometer/src/slsMergeDailyData.R")

srunWorkspaces <- dir("scintillometer/SRun/", pattern = "workspace_SLS", 
                      recursive = FALSE, full.names = TRUE)


### Data processing

lapply(srunWorkspaces, function(i) {
  
  plt <- sapply(strsplit(basename(i), "_"), "[[", 3)
  
  fls <- list.files(paste0(i, "/data/retrieved_SPU-111-230"), pattern = ".mnd$", 
                    full.names = TRUE)
  
  # Merge daily .mnd files
  dat <- slsMergeDailyData(files = fls, 
                           equal.columns = ifelse(plt == "gra1", FALSE, TRUE))

  # Create continuous time series
  time.seq <- strptime(dat$datetime[!is.na(dat$datetime)], 
                       format = "%Y-%m-%d %H:%M:%S")
  time.seq <- strftime(seq(min(time.seq), max(time.seq), 60), 
                       format = "%Y-%m-%d %H:%M:%S")
  
  # Subset columns relevant for randomForest algorithm
  dat2 <- merge(data.frame(datetime = time.seq), 
                dat[, c("datetime", "tempUp", "tempLw", "dwnRad", "upwRad", 
                        "humidity", "soilHeatFlux", "pressure", 
                        "precipRate", "waterET")], 
                by = 1, all.x = TRUE)
  dat2$datetime <- strptime(dat2$datetime, format = "%Y-%m-%d %H:%M:%S")

  # 5m aggregation (temporal resolution of wind speed measurements, 
  # check internal logger configuration for aggregation procedure)
  agg.5m <- aggregate(dat2[, 2:ncol(dat2)], 
                      by = list(rep(1:(nrow(dat2)/5), each = 5)), 
                      FUN = function(x) round(mean(x, na.rm = TRUE), 1), 
                      simplify = FALSE)
  agg.5m <- data.frame(datetime = dat2[seq(1, nrow(dat2), 5), 1], 
                       agg.5m[, 2:ncol(agg.5m)])    
  
  # Complete records for randomForest evaluation
  dat2 <- dat2[complete.cases(dat2), ]
  dat2$waterET <- factor(dat2$waterET)

  
  ## Random forest
  
  valid <- FALSE
  niter <- 1
  while (!valid) {
    set.seed(niter)
    index <- sample(1:nrow(dat2), 1000)
    
    train <- dat2[-index, ]
    
    if (all(levels(train$waterET) %in% 
              unique(levels(train$waterET)[train$waterET]))) {
      valid <- TRUE
    } else {
      niter <- niter + 1
    }
  }

  test <- dat2[index, ]
  
  rf <- randomForest(waterET ~ ., data = train)
  
  pred <- predict(rf, test)
  
  if (!file.exists(paste0(i, "/data/eval")))
    dir.create(paste0(i, "/data/eval"))
  
  png(paste0(i, "/data/eval/", plt, "_rf_eval.png"), width = 20, height = 20, 
      units = "cm", pointsize = 16, res = 300)
  print(xyplot(as.numeric(levels(pred)[pred]) ~ 
                 as.numeric(levels(test$waterET)[test$waterET]), 
               main = plt, col = "grey75", xlab = "Measured ET [mm/h]", 
               ylab = "Predicted ET [mm/h]") + 
          layer(panel.ablineq(lm(y ~ x), r.squared = TRUE, rotate = TRUE, at = .8, 
                              pos = 1)))
  dev.off()
  
})
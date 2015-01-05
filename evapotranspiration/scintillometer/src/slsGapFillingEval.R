### Environmental stuff

rm(list = ls(all = TRUE))

switch(Sys.info()[["sysname"]], 
       "Windows" = setwd("D:/kilimanjaro/evapotranspiration"), 
       "Linux" = setwd("/media/fdetsch/XChange/kilimanjaro/evapotranspiration"))

lib <- c("randomForest", "ggplot2", "latticeExtra", "RColorBrewer", "foreach")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

source("scintillometer/src/slsMergeDailyData.R")

srunWorkspaces <- dir("scintillometer/SRun/", pattern = "workspace_SLS", 
                      recursive = FALSE, full.names = TRUE)


### Data processing

col <- colorRampPalette(brewer.pal(9, "OrRd"))

foreach(i = srunWorkspaces, j = seq_along(srunWorkspaces)) %do% {
  
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
  #   dat2$datetime <- strptime(dat2$datetime, format = "%Y-%m-%d %H:%M:%S")
  
#   # 5m aggregation (temporal resolution of wind speed measurements, 
#   # check internal logger configuration for aggregation procedure)
#   agg.5m <- aggregate(dat2[, 2:ncol(dat2)], 
#                       by = list(rep(1:(nrow(dat2)/5), each = 5)), 
#                       FUN = function(x) round(mean(x, na.rm = TRUE), 1), 
#                       simplify = FALSE)
#   agg.5m <- data.frame(datetime = dat2[seq(1, nrow(dat2), 5), 1], 
#                        agg.5m[, 2:ncol(agg.5m)])    
  
  # Complete records for randomForest evaluation
  dat2 <- dat2[complete.cases(dat2), ]
#   dat2$waterET <- factor(dat2$waterET)
  
  
  ## Random forest
  
  valid <- FALSE
  niter <- 1
#   while (!valid) {
    set.seed(niter)
    index <- sample(1:nrow(dat2), 1000)
    
    train <- dat2[-index, -1]
    
#     if (all(levels(train$waterET) %in% 
#               unique(levels(train$waterET)[train$waterET]))) {
#       valid <- TRUE
#     } else {
#       niter <- niter + 1
#     }
#   }
  
  test <- dat2[index, -1]
  
  rf <- randomForest(waterET ~ ., data = train)
  
  pred <- predict(rf, test)
  pred <- round(pred, 3)
  
  # Statistics
  pred.vals <- pred
  obs.vals <- test$waterET
  ME <- mean(pred.vals - obs.vals, na.rm = TRUE)
  MAE <- mean(abs(pred.vals - obs.vals), na.rm = TRUE)
  RMSE <- sqrt(mean((pred.vals - obs.vals)^2, na.rm = TRUE))
  R <- cor(pred.vals, obs.vals, use = "complete.obs")
  Rsq <- R * R
  
  if (!file.exists(paste0(i, "/data/eval")))
    dir.create(paste0(i, "/data/eval"))
  
  png(paste0(i, "/data/eval/", plt, "_rf_eval2.png"), width = 30, height = 25, 
      units = "cm", pointsize = 20, res = 300)
  print(xyplot(pred.vals ~ obs.vals, cex = 1.2,
#                main = plt, 
               aspect = "iso", 
               scales = list(x = list(cex = 1.2), y = list(cex = 1.2)), 
               xlim = c(-0.15, range(range(pred.vals), range(obs.vals))[2] + .05), 
               ylim = c(-0.15, range(range(pred.vals), range(obs.vals))[2] + .05),
               xlab = list(label = expression(ET["meas"] ~ (mm/h)), cex = 1.5), 
               ylab = list(label = expression(ET["pred"] ~ (mm/h)), cex = 1.5),
               panel = function(x, y, ...) {
#                  panel.smoothScatter(x, y, colramp = col, nrpoints = 0)
                 #                                      nbin = c(length(levels(test$waterET)) + 1, 
                 #                                               length(levels(pred)) + 1))
                 panel.xyplot(x, y, col = "grey65", cex = 1.2)
               }) + 
          layer(panel.abline(a = 0, b = 1, col = "grey25", lty = 2)) + 
          layer(panel.ablineq(lm(y ~ x), r.squared = TRUE, x = .8, y = 0, 
                              cex = 1.5, lwd = 2)))
  dev.off()
  
  statistics <- data.frame(PLOTID = plt, 
                           ME = round(ME, 3), 
                           MAE = round(MAE, 3), 
                           RMSE = round(RMSE, 3), 
                           RSQ = round(Rsq, 3))
  write.table(statistics, file = "scintillometer/out/rf_statistics.csv", 
              append = ifelse(j == 1, FALSE, TRUE), 
              col.names = ifelse(j == 1, TRUE, FALSE), 
              row.names = FALSE, sep = ",", quote = FALSE)
}
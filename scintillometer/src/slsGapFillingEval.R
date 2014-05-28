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

lapply(srunWorkspaces, function(i) {
  
  plt <- sapply(strsplit(basename(i), "_"), "[[", 3)
  
  fls <- list.files(paste0(i, "/data/retrieved_SPU-111-230"), pattern = ".mnd$", 
                    full.names = TRUE)
  
  # Merge daily .mnd files
  dat <- slsMergeDailyData(files = fls, 
                           equal.columns = FALSE)
  
  # Subset columns relevant for randomForest algorithm
  dat2 <- dat[, c("tempUp", "tempLw", "dwnRad", "upwRad", "humidity",
                  "soilHeatFlux", "pressure", "precipRate", "waterET")]
  dat2 <- dat2[complete.cases(dat2), ]
  dat2$waterET <- factor(dat2$waterET)
  
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
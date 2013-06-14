### Environmental stuff

# Required packages
library(zoo)
library(Rssa)
library(foreach)

# Required functions
source("E:/repositories/julendat/src/julendat/rmodules/as.ki.data.R")
source("E:/repositories/julendat/src/julendat/rmodules/gfGapLength.R")
source("E:/kilimanjaro/temperature_network/src/gfNogapLength.R")

# Working directory
path.wd <- "E:/kilimanjaro/temperature_network"
setwd(path.wd)


### Data processing

# List and import data sets
fls.ki <- lapply(fls <- list.files("out", pattern = "reformat_gf.csv$", full.names = TRUE), 
                 as.ki.data)

# Identify lengths of measurement gaps
fls.ki.na <- lapply(fls.ki, function(i) {
  tmp.ki.na <- which(is.na(i@Parameter[["TEMP"]]))
  
  do.call("rbind", gfGapLength(data.dep = i, 
              pos.na = tmp.ki.na, 
              gap.limit = 999999, 
              end.datetime = Sys.Date()))
})


## Linear interpolation over small (n <= 5) measurement gaps

for (h in seq(fls.ki)) {
  # Time series
  tmp.ts <- zoo(fls.ki[[h]]@Parameter[["TEMP"]], 
                order.by = as.Date(fls.ki[[h]]@Datetime))
  # Rolling mean (window width = 11)
  tmp.ts.rm <- rollapply(data = tmp.ts, width = 11, fill = list(NA, NULL, NA), partial = TRUE, 
                         function(...) mean(..., na.rm = TRUE))
  # Sufficiently small gaps
  tmp.ki.na <- fls.ki.na[[h]][which(fls.ki.na[[h]][,3] <= 5), ]
  tmp.ki.na.small_gap <- do.call("c", lapply(seq(nrow(tmp.ki.na)), function(i) {
    seq(tmp.ki.na[i, 1], tmp.ki.na[i, 2])
  }))
  # Replace identified gaps by rolling mean
  tmp.ts[tmp.ki.na.small_gap] <- tmp.ts.rm[tmp.ki.na.small_gap]
  fls.ki[[h]]@Parameter[["TEMP"]] <- tmp.ts
}

# # Get updated gap information
# fls.ki.na <- lapply(fls.ki, function(i) {
#   tmp.ki.na <- which(is.na(i@Parameter[["TEMP"]]))
#   
#   gfGapLength(data.dep = i, 
#               pos.na = tmp.ki.na, 
#               gap.limit = 999999, 
#               end.datetime = Sys.Date())
# })


## Forecasting

fls.ki.ssa <- lapply(fls.ki[c(1,2,4)], function(i) {
  
  tmp.rev <- rev(as.numeric(i@Parameter[["TEMP"]]))
  tmp.rev.ts <- zoo(tmp.rev, order.by = as.Date(index(i@Parameter[["TEMP"]])))
  
  tmp.ki.rev <- i
  tmp.ki.rev@Parameter[["TEMP"]] <- as.numeric(tmp.rev.ts)
  
  # Identify lengths of measurement gaps
  tmp.ki.rev.na <- which(is.na(tmp.ki.rev@Parameter[["TEMP"]]))
  ki.rev.na <- do.call(function(...) {
    tmp <- rbind(...)
    names(tmp) <- c("start", "end", "span")
    return(tmp)}, gfGapLength(data.dep = tmp.ki.rev,
                              pos.na = tmp.ki.rev.na, 
                              gap.limit = 999999, 
                              end.datetime = Sys.Date()))
  
  while (length(ki.rev.na) > 0) {
    
    # Identify lengths of continuous measurements
    ki.rev.nona <- do.call("rbind", gfNogapLength(gap.lengths = ki.rev.na, 
                                                  data.dep = tmp.ki.rev))
    
    # Deconstruct continuous measurement series
    tmp.ssa <- ssa(tmp.ki.rev@Parameter[["TEMP"]][ki.rev.nona[1,1]:ki.rev.nona[1,2]], L = 365)
    # Forecast the next gap
    tmp.ki.rev@Parameter[["TEMP"]][ki.rev.na[1,1] : ki.rev.na[1,2]] <-
      forecast(tmp.ssa, groups = list(seq(nlambda(tmp.ssa))), len = ki.rev.na[1,3])$mean
    
    # Update lengths of measurement gaps
    tmp.ki.rev.na <- which(is.na(tmp.ki.rev@Parameter[["TEMP"]]))
    if (length(tmp.ki.rev.na) > 0) {
      ki.rev.na <- do.call(function(...) {
        tmp <- rbind(...)
        names(tmp) <- c("start", "end", "span")
        return(tmp)}, gfGapLength(data.dep = tmp.ki.rev,
                                  pos.na = tmp.ki.rev.na, 
                                  gap.limit = 999999, 
                                  end.datetime = Sys.Date()))
    } else {
      ki.rev.na <- list()
    }
    
  }
  
  # Replace gappy by filled time series
  tmp <- rev(as.numeric(tmp.ki.rev@Parameter[["TEMP"]]))
  i@Parameter[["TEMP"]] <- as.numeric(tmp)
  
  return(i)
  
})


## Plotting

data <- lapply(fls.ki.ssa, function(i) {
  return(zoo(i@Parameter[["TEMP"]], order.by = as.Date(i@Datetime)))
})

foreach(a = c("airport", "arusha", "nairobi"), b = 1:3, c = c(1,2,4)) %do% {
  jpeg(filename = paste("E:/kilimanjaro/temperature_network/out/noaa_gsod", a, "1973_2013_reformat_gf_li_ssa.jpg", sep = "_"), 
       width = 1400, height = 600)
  plot(data[[b]], col = "red", xlab = "Time [d]", ylab = "Temperature [°C]")
  lines(fls.ki[[c]]@Parameter[["TEMP"]])
  legend("topleft", col = c("black", "red"), c("julendat", "ssa"), lty = 1)
  dev.off()
}


## Output storage

lapply(seq(fls.ki.ssa), function(i) {
  write.csv(data.frame("Datetime" = fls.ki.ssa[[i]]@Datetime, 
                       "Timezone" = rep(fls.ki.ssa[[i]]@Timezone, length(fls.ki.ssa[[i]]@Datetime)), 
                       "Aggregationtime" = rep(fls.ki.ssa[[i]]@Aggregationtime, length(fls.ki.ssa[[i]]@Datetime)), 
                       "PlotId" = fls.ki.ssa[[i]]@PlotId$Shortname, 
                       "EpPlotId" = fls.ki.ssa[[i]]@EpPlotId, 
                       "StationId" = fls.ki.ssa[[i]]@StationId$Shortname, 
                       "Processlevel" = rep(fls.ki.ssa[[i]]@Processlevel, length(fls.ki.ssa[[i]]@Datetime)), 
                       "Qualityflag" = fls.ki.ssa[[i]]@Qualityflag, 
                       "TEMP" = fls.ki.ssa[[i]]@Parameter$TEMP), 
            paste(substr(fls[-3], 1, nchar(fls[-3]) - 4)[i], "li_ssa.csv", sep = "_"), row.names = FALSE)
})


# ########################################
# 
# ### Evaluation of 'rollapply()' function
# set.seed(10)
# tmp.smpl <- sample(seq(tmp.ts), 5000)
# tmp.ts.eval <- tmp.ts[tmp.smpl]
# tmp.ts[tmp.smpl] <- NA
# tmp.ts.eval.rm <- rollapply(data = tmp.ts, width = 11, fill = list(NA, NULL, NA), function(...) mean(..., na.rm = TRUE), partial = TRUE)
# 
# tmp.ts.eval <- na.omit(cbind(tmp.ts.eval, tmp.ts.eval.rm[tmp.smpl]))
# plot(tmp.ts.eval[,2] ~ tmp.ts.eval[,1])
# cor(tmp.ts.eval[,2], tmp.ts.eval[,1])
# 
# ########################################
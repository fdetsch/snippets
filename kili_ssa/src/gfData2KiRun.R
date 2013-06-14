# Required libraries
library(doParallel)
library(zoo)

# Required functions
source("E:/kilimanjaro/temperature_network/src/gfData2KiFormat.R")

# Parallelization
registerDoParallel(clstr <- makeCluster(4))

# Plots and output tables per station
foreach(i = c("airport", "arusha", "moshi", "nairobi"), .packages = c("zoo", "Hmisc")) %dopar% {
  tmp <- gfData2KiFormat(file = paste("E:/kilimanjaro/temperature_network/data/noaa_gsod", i, "1970_2013.txt", sep = "_"), 
                         header = TRUE, sep = ",", na.strings = c("*", "**", "***", "****", "*****", "******"), 
                         date.column = "YEARMODA",
                         date.format = "%Y%m%d",
                         prm.column = "TEMP", 
                         temp2celsius = TRUE, 
                         time.step = "day", 
                         timezone = "eat",
                         aggtime = "diurnal", 
                         plot.id = substr(i, 1, 4))
  
  # Number of hourly measurements per day
  tmp.sub <- subset(tmp, !is.na(tmp$TEMP))
  tmp.sub.nmonth <- aggregate(tmp.sub$TEMP, by=list(as.yearmon(tmp.sub$Datetime)), length)
  colnames(tmp.sub.nmonth) <- c("Date", "MonthlyObservations")
  jpeg(paste("E:/kilimanjaro/temperature_network/data/noaa_gsod", i, "1973_2013.jpg", sep = "_"), 
       width = 866, height = 596)
  plot(tmp.sub.nmonth, type="h", main = paste("Daily measurements per month (1973-2013) \n-", capitalize(i), "-"), 
       xlab = "Time", ylab = "Monthly observations")
  dev.off()
  
  write.csv(tmp, paste("E:/kilimanjaro/temperature_network/data/noaa_gsod", i, "1973_2013_reformat.csv", sep = "_"), 
            row.names = FALSE)
}

# Deregister parallel backend
stopCluster(clstr)
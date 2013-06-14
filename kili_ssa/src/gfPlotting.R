# Required packages
library(zoo)

# Generate time series plots for each station
for (i in c("airport", "arusha", "moshi", "nairobi")) {
  
  file.in <- paste("E:/kilimanjaro/temperature_network/out/noaa_gsod", i, "1973_2013_reformat_gf.csv", sep = "_")
  file.out <- paste("E:/kilimanjaro/temperature_network/out/noaa_gsod", i, "1973_2013_reformat_gf2.csv", sep = "_")
  
  data <- lapply(c(file.in, file.out), function(i) {
    tmp <- read.table(i, header = TRUE, sep = ",", 
                      stringsAsFactors = FALSE, na.strings = "NA")[,c("Datetime", "TEMP")]
    return(zoo(tmp$TEMP, order.by = as.Date(tmp$Datetime)))
  })
  
  jpeg(filename = paste("E:/kilimanjaro/temperature_network/out/noaa_gsod", i, "1973_2013_reformat_gf2.jpg", sep = "_"), 
       width = 1400, height = 600)
  plot(data[[2]], col = "red", xlab = "Time [d]", ylab = "Temperature [°C]")
  lines(data[[1]])
  legend("topleft", col = c("red", "black"), c("julendat", "julendat /w ssa"), lty = 1)
  
  dev.off()
  
}
gfData2KiFormat <- function(file,
                            date.column,
                            date.format = "%Y%m%d%H%M",
                            prm.column, 
                            temp2celsius = FALSE, 
                            time.step = "day",
                            timezone = NA,
                            aggtime = "-999",
                            plot.id = NA,
                            ep.plot.id = "xxx",
                            station.id = NA,
                            proc.level = -999,
                            qual.flag = NA,
                            ...) {
  
  # Required package
  stopifnot(require(zoo))
  
  # Set system timezone to GMT
  Sys.setenv(tz = "GMT")
  
  # Import data
  gsof <- read.table(file, ...)

  # Convert date column to Date format
  gsof.initial.time <- gsof[, date.column]
  
  # Convert temperature from Fahrenheit to Celsius (optional)
  if (temp2celsius) 
    gsof[, prm.column] <- round((gsof[, prm.column] - 32.0) / 1.8, digits = 1)
  
  # Convert parameter column into continuoustime-series object
  gsof.ts.prm <- zoo(gsof[, prm.column], strptime(gsof[, date.column], date.format))
#   gsof.ts.prm.agg <- aggregate(gsof.ts.prm, cut(time(gsof.ts.prm), time.step), mean)
#   index(gsof.ts.prm.agg) <- as.POSIXlt(index(gsof.ts.prm.agg), tz = "GMT")
  gsof.ts.date <- zoo(,seq(as.POSIXlt(paste(substr(start(gsof.ts.prm), 1, 4), "01", "01", sep = "-")), end(gsof.ts.prm), by = time.step))
  index(gsof.ts.date) <- as.POSIXlt(index(gsof.ts.date))
  gsof.ts <- merge(gsof.ts.prm, gsof.ts.date, all = TRUE)
  
  # Merge and return created data
  gsof.ts <- data.frame(if (time.step == "day") paste(index(gsof.ts), "12:00:00") else index(gsof.ts),
                        rep(timezone, length(gsof.ts)),
                        rep(aggtime, length(gsof.ts)),
                        rep(plot.id, length(gsof.ts)),
                        rep(ep.plot.id, length(gsof.ts)),
                        rep(station.id, length(gsof.ts)),
                        rep(proc.level, length(gsof.ts)),
                        rep(qual.flag, length(gsof.ts)),
                        as.numeric(gsof.ts), stringsAsFactors = FALSE)
  names(gsof.ts) <- c("Datetime", "Timezone", "Aggregationtime", "PlotId", 
                      "EpPlotId", "StationId", "Processlevel", "Qualityflag", prm.column)
  
  # Set system timezone back to CET
  Sys.setenv(tz = "CET")
  
  return(gsof.ts)
  
}

# # Call
# tmp <- gfData2Ts(file = "E:/kilimanjaro/temperature_network/data/noaa_hourly_global_airport_1973_2013.csv", 
#                  header = TRUE, sep = ",", na.strings = c("*", "**", "***", "****", "*****", "******"), 
#                  date.column = "YR..MODAHRMN", 
#                  prm.column = "TEMP", 
#                  temp2celsius = TRUE, 
#                  time.step = "hour")
# head(tmp)
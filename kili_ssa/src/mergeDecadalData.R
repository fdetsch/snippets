for (i in c("airport", "arusha", "moshi", "nairobi")) {
  
  tmp.fls <- list.files("E:/kilimanjaro/temperature_network/data", pattern = paste(i, ".txt$", sep = ".*"), full.names = TRUE)
  
  date.span <- cbind(sapply(strsplit(substr(basename(tmp.fls), 1, nchar(basename(tmp.fls)) - 4), "_"), "[", 5), 
                     sapply(strsplit(substr(basename(tmp.fls), 1, nchar(basename(tmp.fls)) - 4), "_"), "[", 6))
  
  tmp.dat <- do.call("rbind", lapply(tmp.fls, function(j) {
    read.table(j, header = TRUE, fill = TRUE)
  }))
  
  write.csv(tmp.dat, paste(unique(dirname(tmp.fls)), "/noaa_hourly_global_", i, "_", min(date.span[,1]), "_", max(date.span[,2]), ".csv", sep = ""), 
            quote = FALSE, row.names = FALSE)
  
}
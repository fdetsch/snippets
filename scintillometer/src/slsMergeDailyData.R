slsMergeDailyData <- function(files, 
                              equal.columns = TRUE,
                              ...) {
  
  stopifnot(require(foreach))
  
  dat <- foreach(i = files, j = seq(files)) %do% {
    
    # Import headers
    hdr <- readLines(i)
    index <- grep("#", hdr)
    hdr <- hdr[index]
    hdr <- sapply(strsplit(hdr, " # "), "[[", 2)
    
    # Import data
    tmp <- read.table(i, header = FALSE, skip = index[length(index)] + 1, 
                      sep = "\t", na.strings = "*", stringsAsFactors = FALSE)
    names(tmp) <- hdr
    
    tmp <- tmp[2:nrow(tmp), ]
    
    # Reformat datetime 
    tmp.date <- substr(tmp[, 1], 13, 22)
    tmp.time <- substr(tmp[, 1], 24, 31)
    tmp$datetime <- strftime(strptime(paste(tmp.date, tmp.time), 
                                      format = "%Y-%m-%d %H:%M:%S"))
    
    return(tmp)
  }
  
  if (equal.columns) {
    dat <- do.call("rbind", dat) 
  } else {
    dat <- rbind.fill(dat)
  }
  
  dat <- dat[order(dat$datetime), ]
  
  return(dat)
}
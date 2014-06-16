switch(Sys.info()[["sysname"]], 
       "Windows" = setwd("E:/"), 
       "Linux" = setwd("/media/permanent/"))

setwd("SRun/workspace_SLS_gra2_20140304/")

library(foreach)

fls <- list.files("data/retrieved_SPU-111-230", pattern = ".dgn$", 
                  full.names = TRUE)

dat <- foreach(i = fls, j = seq(fls)) %do% {
  # Import headers
  hdr <- readLines(i)
  index <- grep("#", hdr)
  hdr <- hdr[index]
  hdr <- sapply(strsplit(hdr, " # "), "[[", 2)
  
  # Import data
  tmp <- read.table(i, header = FALSE, skip = index[length(index)] + 1, 
                    sep = "\t", na.strings = "*", stringsAsFactors = FALSE)
  names(tmp) <- hdr
  
  # Reformat datetime 
  tmp.date <- substr(tmp[, 1], 13, 22)
  tmp.time <- substr(tmp[, 1], 24, 31)
  tmp$datetime <- strftime(strptime(paste(tmp.date, tmp.time), 
                                    format = "%Y-%m-%d %H:%M:%S"))
  
  return(tmp)
}

tst <- do.call("rbind", dat)
# tst <- rbind.fill(dat)
tst <- tst[order(tst$datetime), ]

for (i in c("XA", "YA", "XB", "YB")) {
  col <- paste0("channelFlags", i)
  print(sort(table(tst[, col])))
}

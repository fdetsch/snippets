library(sp)

load("data/breweries91.rda")

dat <- breweries91@data

toUTF8 <- function(x, from = "UTF-8", to = "UTF-8", ...) {
  iconv(x, from = from, to = to, ...)
}

nonascii <- c(1:2, 4, 6)
dat[, nonascii] <- apply(dat[, nonascii], 2, FUN = toUTF8)
coordinates(dat) <- coordinates(breweries91)
proj4string(dat) <- proj4string(breweries91)

breweries91 <- dat
save(breweries91, file = "data/breweries91.rda")

### see also https://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
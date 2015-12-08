library(sp)

load("data/gadmCHE.rda")

dat <- gadmCHE@data

# toUTF8 <- function(x, from = "UTF-8", to = "UTF-8", ...) {
#   iconv(x, from = from, to = to, ...)
# }
#
# nonascii <- c(1:2, 4, 6)
# dat[, nonascii] <- apply(dat[, nonascii], 2, FUN = toUTF8)
# coordinates(dat) <- coordinates(breweries91)
# proj4string(dat) <- proj4string(breweries91)
#
# breweries91 <- dat
# save(breweries91, file = "data/breweries91.rda")

replaceUmlauts <- function(x) {

  library(foreach)

  nonascii <- grep("I_WAS_NOT_ASCII", iconv(x, "UTF-8", "ASCII",
                                            sub = "I_WAS_NOT_ASCII"))

  if (length(nonascii) > 0) {
    for (i in nonascii) {
      jnk <- foreach(j = c("ä", "ö", "ü", "ß", "â", "è", "ë", "é"), k = c("ae", "oe", "ue", "ss", "a", "e", "e", "e")) %do% {
        x[i] <- gsub(j, k, x[i])
      }
    }
  }

  return(x)
}

replaceUmlauts <- function(x) {

  library(foreach)

  nonascii <- grep("I_WAS_NOT_ASCII", iconv(levels(x), "UTF-8", "ASCII",
                                            sub = "I_WAS_NOT_ASCII"))

  if (length(nonascii) > 0) {
    for (i in nonascii) {
      jnk <- foreach(j = c("ä", "ö", "ü", "ß", "â", "è", "ë", "é"), k = c("ae", "oe", "ue", "ss", "a", "e", "e", "e")) %do% {
        x[i] <- gsub(j, k, levels(x)[i])
      }
    }
  }

  return(x)
}

dat <- apply(dat, 2, FUN = replaceUmlauts)

dat[31, 6] <- "<a href='http://www.xn--meisterbru-y5a.de/mod/main.php' target=\"_blank\">www.meisterbr%C3%A4u.de</a>"

dat <- data.frame(dat)
dat$zipcode <- as.integer(as.character(dat$zipcode))
dat$founded <- as.integer(as.character(dat$founded))

dat$ID_0 <- as.integer(as.character(dat$ID_0))
dat$ID_1 <- as.integer(as.character(dat$ID_1))
dat$ID_2 <- as.integer(as.character(dat$ID_2))

coordinates(dat) <- coordinates(gadmCHE)
proj4string(dat) <- proj4string(gadmCHE)

gadmCHE@data <- dat
save(gadmCHE, file = "data/gadmCHE.rda")

gadmCHE@data[, c(1, 3, 4, 6, 8:12)] <-
  apply(gadmCHE@data[, c(1, 3, 4, 6, 8:12)], 2, FUN = function(x) {
    iconv(x, "UTF-8", "ASCII", sub = "I_WAS_NOT_ASCII")
  })



rm(list = ls(all = TRUE))
load("data/gadmCHE.rda")
lapply(1:ncol(gadmCHE@data), function(i) {
  grep("I_WAS_NOT_ASCII", iconv(gadmCHE@data[, i], "UTF-8", "ASCII", sub="I_WAS_NOT_ASCII"))
})

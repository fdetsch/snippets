### update collections per product -----

load("inst/external/collections.RData")

## retrieve online information
cls <- sapply(names(MODIScollection), function(product) {
  clc <- try(getCollection(product, forceCheck = TRUE, newest = FALSE), silent = TRUE)
  Sys.sleep(1L)
  return(clc)
})

## write retrieved collections into data table
library(qpcR)
dat <- do.call(qpcR:::cbind.na, 
        lapply(1:length(cls), function(i) {
          if (!inherits(cls[[i]], "try-error")) {
            val <- unlist(cls[[i]])
            val <- as.integer(val)
            
            # if required, order collections (e.g., "004", "041", "005")
            tns <- val >= 10 & val < 100
            if (any(tns)) {
              val[tns] <- val[tns] / 10
              ids <- order(val)
              val[tns] <- val[tns] * 10
              val <- val[ids]
            }
            
            data.frame(val)
          }
        })
)

names(dat) <- names(MODIScollection)

## write to file
MODIScollection <- dat
save(MODIScollection, file = "inst/external/collections.RData")


### add new sources (e.g., LPDAAC) to products list -----

load("inst/external/MODIS_Products.RData")

## loop over products
for (product in c("MOD14", "MYD14")) {
  id <- grep(paste0("^", product, "$"), MODIS_Products$PRODUCT)
  MODIS_Products$SOURCE[[id]] <- c("LPDAAC", "LAADS")
}

## write to file
save(MODIS_Products, file = "inst/external/MODIS_Products.RData")

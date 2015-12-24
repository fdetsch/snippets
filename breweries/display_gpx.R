library(plotKML)
library(sp)

## .gpx track to 'SpatialLinesDataFrame'
sln <- plotKML::readGPX("~/Downloads/7_Brauereien.gpx")$tracks[[1]][[1]]

sln <- sp::SpatialLines(
  list(sp::Lines(
    list(sp::Line(sln[, 1:2])), 
    ID = "7 Brauereien auf einen Streich")), 
  proj4string = CRS("+init=epsg:4326")
)

## 'SpatialLines' to 'SpatialLinesDataFrame'
sln_utm <- spTransform(sln, CRS = CRS("+init=epsg:32632"))
dat <- data.frame("len" = round(rgeos::gLength(sln_utm) / 1000, 2))
rownames(dat) <- "7 Brauereien auf einen Streich"

slndf <- sp::SpatialLinesDataFrame(sln, data = dat)

## display track
library(mapview)
mapview(slndf)

## track length
rgeos::gLength(spTransform(sln, CRS = CRS("+init=epsg:32632")))
# [1] 16915.59

## display in google earth
plotKML(slndf, col = "blue", lwd = 2)

## required packages and data
library(ggplot2)
library(fwmap)
library(raster)
data("diamonds")

## replicate 'diamonds' dataset
big <- diamonds[rep(seq_len(nrow(diamonds)), 10), ]
big$cut <- as.character(big$cut)
big$color <- as.character(big$color)
big$clarity <- as.character(big$clarity)

## set random coordinates
big$x <- rnorm(nrow(big), 10, 3)
big$y <- rnorm(nrow(big), 50, 3)
coordinates(big) <- ~ x + y
projection(big) <- "+init=epsg:4326"

fwmap(big)

## speed check
library(microbenchmark)
microbenchmark(
  fwmap:::all2JSON(out.matrix[1:10000, ]), # 'Rcpp' version
  apply(out.matrix[1:10000, ], 1, fwmap:::one2JSON), # alternate 'Rcpp' version
  coords2JSON(out.matrix[1:10, ]), # returns list
  jsonlite::toJSON(out.matrix[1:10, ]), # 'jsonlite' version
  times = 10L
)

## similarity check
identical(
  json1 <- fwmap:::all2JSON(out.matrix[1:1000, ]), 
  json2 <- as.character(jsonlite::toJSON(out.matrix[1:1000, ]))
)

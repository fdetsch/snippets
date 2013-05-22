getOsmTiles <- function(tile.cntr, 
                        plot.coords.mrc,
                        plot.res,
                        ...) {
  
  #########################################################################################
  # Parameters are as follows:
  #
  # tile.cntr (numeric list):     List containing numeric coordinates (lon, lat) relative 
  #                               to the location of the research plot under investigation.
  # plot.coords.mrc (data.frame): Data frame containing information about current plot. 
  # plot.res (numeric):           Desired resolution (size) of each tile.
  # ...:                          Further arguments to be passed on to the function.
  #
  #########################################################################################
  
  # Required packages
  stopifnot(require(raster))
  stopifnot(require(OpenStreetMap))
  
  # Loop through single tile centers of current research plot
  tmp.osm <- lapply(seq(tile.cntr), function(z) {
    
    # Set center of current tile
    tmp.coords.mrc <- plot.coords.mrc
    tmp.coords.mrc$Lon <- tmp.coords.mrc$Lon + tile.cntr[[z]][,1]
    tmp.coords.mrc$Lat <- tmp.coords.mrc$Lat + tile.cntr[[z]][,2]
    coordinates(tmp.coords.mrc) <- c("Lon", "Lat")
    projection(tmp.coords.mrc) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
    
    # Set extent of current tile
    tmp.coords.mrc$left <- coordinates(tmp.coords.mrc)[,1] - plot.res
    tmp.coords.mrc$top <- coordinates(tmp.coords.mrc)[,2] + plot.res
    tmp.coords.mrc$right <- coordinates(tmp.coords.mrc)[,1] + plot.res
    tmp.coords.mrc$bottom <- coordinates(tmp.coords.mrc)[,2] - plot.res
    
    # Boundary coordinates in Mercator
    tmp.bndry.tl.mrc <- data.frame(tmp.coords.mrc)[,c("PlotID", "left", "top")]
    coordinates(tmp.bndry.tl.mrc) <- c("left", "top")
    proj4string(tmp.bndry.tl.mrc) <- proj4string(tmp.coords.mrc)
    
    tmp.bndry.br.mrc <- data.frame(tmp.coords.mrc)[,c("PlotID", "right", "bottom")]
    coordinates(tmp.bndry.br.mrc) <- c("right", "bottom")
    proj4string(tmp.bndry.br.mrc) <- proj4string(tmp.coords.mrc)
    
    # Reproject boundrary coordinates to LatLon
    tmp.bndry.tl <- spTransform(tmp.bndry.tl.mrc, CRS("+proj=longlat +datum=WGS84"))
    tmp.bndry.br <- spTransform(tmp.bndry.br.mrc, CRS("+proj=longlat +datum=WGS84"))
    
    # Get BING image of the given extent
    openmap(upperLeft = c(as.numeric(coordinates(tmp.bndry.tl)[,2]), as.numeric(coordinates(tmp.bndry.tl)[,1])), 
            lowerRight = c(as.numeric(coordinates(tmp.bndry.br)[,2]), as.numeric(coordinates(tmp.bndry.br)[,1])), 
            type = "bing")
    
  })
  
  # Return output
  return(tmp.osm)
  
}
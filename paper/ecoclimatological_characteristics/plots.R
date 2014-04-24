# Working directory
switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/media/XChange/publications/paper/ecoclimatological_characteristics/"), 
       "Windows" = setwd("D:/publications/paper/ecoclimatological_characteristics"))

# Required packages
lib <- c("raster", "rgdal", "sp", "doParallel", "ggplot2", "OpenStreetMap", 
         "dismo")
sapply(lib, function(x) require(x, character.only = TRUE))

# Open node cluster for parallel processing
registerDoParallel(cl <- makeCluster(3))

# ASTER data
# fls.ast <- list.files("data/ASTER/", pattern = ".rst$", full.names = TRUE)
# rst.ast <- foreach(i = fls.ast, .packages = lib) %dopar% {
#   tmp.rst <- stack(i)
#   tmp.rst[which(rowSums(getValues(tmp.rst)) == 0)] <- NA
#   return(tmp.rst)
# }
# 
# rst.ast.mrg <- Reduce(function(...) merge(..., tolerance = 1), rst.ast)
# writeRaster(rst.ast.mrg, "data/ASTER/AST_ALL", format = "GTiff", 
#             overwrite = TRUE)

rst.ast.mrg <- stack("data/ASTER/AST_ALL.tif")

# Classified RST and RDC files
fls.rst <- list.files("data/", pattern = "_final.rst$", full.names = TRUE)
fls.rdc <- list.files("data/", pattern = "_final.RDC$", full.names = TRUE)

# Extract land-use classes for color scheme selection
lu <- foreach(i = fls.rdc, .combine = "rbind") %do% {
  txt <- readLines(i)
  txt <- txt[grep("code", txt)]
  
  txt.sub <- gsub(" ", "", txt)
  txt.spl <- strsplit(substr(txt.sub, 5, nchar(txt.sub)), ":")
  
  data.frame(luv = sapply(txt.spl, "[[", 1), 
             luc = sapply(txt.spl, "[[", 2))
}

# Remove duplicated records
lu <- lu[!duplicated(tolower(lu[, 2])), ]

lu <- lu[order(as.character(lu[, 2])), ]

# Define color scheme for 'ggplot'
col <- c("Bare_soil" = "grey75", #bare_soil
         "Clearing" = "red", #clearing
         "Clouds" = "white", #clouds
         "Coffee" = "bisque4", #coffee
         "Cropland" = "bisque", #cropland
         "Erica" = "darkorange2", #erica
         "Erica_valley" = "darkorange4", #erica_valley
         "Field" = "chocolate4", #field
         "Forest_lower_montane" = "chartreuse4", #forest_lower_montane
         "Forest_not_classified" = "darkgreen", #forest_not_classified
         "Forest_Ocotea" = "aquamarine1", #forest_ocotea
         "Forest_Ocotea_disturbed" = "aquamarine3", #forest_ocotea_disturbed
         "Forest_Ocotea_disturbed_valley" = "aquamarine4", #forest_ocotea_disturbed_valley
         "Forest_Podocarpus" = "darkolivegreen1", #forest_podocarpus
         "Forest_Podocarpus_disturbed" = "darkolivegreen4", #forest_podocarpus_disturbed
         "Forest_Podocarpus_valley" = "darkolivegreen3", #forest_podocarpus_valley
         "Grassland" = "chartreuse1", #grassland
         "Grassland_Savanna" = "burlywood1", #grassland_savanna
         "Grassland_subalpine" = "antiquewhite1", #grassland_subalpine
         "Grassland_trees" = "chartreuse3", #grassland_trees
         "Helicrysum" = "khaki1", #helicrysum
         "Homegarden" = "darkred", #homegarden
         "Settlement" = "darkorchid1", #settlement
         "Shadow" = "black", #shadow
         "Unclassified" = "darkmagenta", #unclassified
         "Water_body" = "deepskyblue") #water_body

# Create integer raster layers for further processing
rst <- foreach(i = fls.rst) %do% {
  rst <- raster(i)
  val <- getValues(rst, format = "matrix")
  
  raster(val, template = rst)
}

plt <- read.csv("Complete_Plots_midPoint_coordinates_update24032014.csv", 
                header = TRUE, nrows = 83)[, c("PlotID", "Easting", "Northing")]
coordinates(plt) <- ~ Easting + Northing
projection(plt) <- CRS("+init=epsg:21037")
plt <- spTransform(plt, CRS(projection(rst[[1]])))

# Plot widths and heights for image storage
wh <- list(c(22, 17), c(22, 30), c(22, 17), c(22, 30), c(22, 25), 
           c(22, 20), c(22, 18), c(22, 19), c(22, 20), c(22, 17), 
           c(22, 20), c(22, 19))
wh <- list(c(12.19, 8.28)*2, c(22, 30), c(12.28, 8.44)*2, c(22, 30), c(12.2, 11.9)*2, 
           c(11, 8.65)*2, c(11.3, 8.3)*2, c(11.3, 8.15)*2, c(10.92, 8.63)*2, c(13.4, 8.8)*2, 
           c(10.9, 8.4)*2, c(11.7, 8.4)*2)



# Plot names for image storage
fn <- as.list(substr(basename(fls.rst), 1, 
                     nchar(basename(fls.rst)) - 4))

foreach(i = rst, j = fn, k = wh, .packages = lib) %dopar% {
  
  plt.crp <- crop(plt, i)
  plt.crp <- data.frame(plt.crp)
  
  # Prepare data for 'ggplot' application
  rst.pt <- rasterToPoints(i)
  rst.df <- data.frame(rst.pt)
  colnames(rst.df) = c("x", "y", "luv")
  
  # Merge land-use integer values with class names
  rst.df.mrg <- merge(rst.df, lu, all.x = TRUE, sort = FALSE)
  
  # Reorder factor levels for 'ggplot' legend
  rst.df.mrg$luc <- factor(rst.df.mrg$luc, 
                           levels = sort(levels(rst.df.mrg$luc)))
  
  # 'ggplot'
  png(paste0("out/", j, ".png"), width = k[1], height = k[2], units = "cm", 
      pointsize = 12, res = 300)
  print(ggplot() + 
          geom_tile(aes(x = x, y = y, fill = luc), data = rst.df.mrg) + 
          geom_point(aes(x = Easting, y = Northing), data = plt.crp, 
                     shape = 24, size = 7, fill = "black") + 
          geom_point(aes(x = Easting, y = Northing), data = plt.crp, 
                     shape = 24, size = 5, fill = "grey75") + 
          scale_fill_manual("Land-cover types", values = col) + 
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) + 
          theme_bw() + 
          theme(legend.background = element_rect(fill = "gray95")))
  dev.off()
  
#   # Corresponding ASTER image
#   rst.ast <- crop(rst.ast.mrg, i, format = "GTiff", overwrite = TRUE, 
#                   filename = paste0("out/", substr(j, 1, 9), "CRP"))
#   
#   png(paste0("out/", substr(j, 1, 9), "CRP.png"), width = k[1], height = k[2], 
#       units = "cm", pointsize = 12, res = 300)
#   plotRGB(rst.ast)
#   dev.off()
#   
#   # Corresponding OSM image
#   i.ll <- projectExtent(i, crs = CRS("+init=epsg:4326"))
#   
#   osm <- openmap(upperLeft = c(ymax(i.ll), xmin(i.ll)), 
#                  lowerRight = c(ymin(i.ll), xmax(i.ll)), type = "bing", 
#                  minNumTiles = 30)
#   rst.osm <- projectRaster(raster(osm), crs = CRS(projection(i)), method = "ngb")
#   writeRaster(rst.osm, filename = paste0("out/", substr(j, 1, 5), "OSM"), 
#               format = "GTiff", overwrite = TRUE)
#   
#   png(paste0("out/", substr(j, 1, 5), "OSM.png"), width = k[1], height = k[2], 
#       units = "cm", pointsize = 12, res = 300)
#   plotRGB(rst.osm)
#   dev.off()

  # Corresponding Google images
  i.ll <- projectExtent(i, crs = CRS("+init=epsg:4326"))
    
  rst.dsm <- gmap(extent(i.ll), type = "satellite", scale = 2, rgb = TRUE)
  rst.dsm <- projectRaster(rst.dsm, crs = CRS(projection(i)), method = "ngb")
  writeRaster(rst.dsm, filename = paste0("out/", substr(j, 1, 5), "DSM"), 
              format = "GTiff", overwrite = TRUE)

  png(paste0("out/", substr(j, 1, 5), "DSM.png"), width = k[1], height = k[2], 
      units = "cm", pointsize = 12, res = 300)
  plotRGB(rst.dsm)
  dev.off()

}

# Deregister parallel backend
stopCluster(cl)
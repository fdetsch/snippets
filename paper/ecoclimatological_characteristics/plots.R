# Working directory
setwd("D:/publications/paper/ecoclimatological_characteristics")

# Required packages
lib <- c("raster", "rgdal", "sp", "doParallel", "ggplot2")
sapply(lib, function(x) require(x, character.only = TRUE))

# Open node cluster for parallel processing
registerDoParallel(cl <- makeCluster(3))

# RST and RDC files
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
#   lu$col <- c("azure", #bare_soil
#               "red", #clearing
#               "white", #clouds
#               "bisque4", #coffee
#               "chartreuse4", #cropland
#               "darkorange2", #erica
#               "darkorange4", #erica_valley
#               "chocolate4", #field
#               "chartreuse4", #forest_lower_montane
#               "darkgreen", #forest_not_classified
#               "aquamarine1", #forest_ocotea
#               "aquamarine3", #forest_ocotea_disturbed
#               "aquamarine4", #forest_ocotea_disturbed_valley
#               "darkolivegreen1", #forest_podocarpus
#               "darkolivegreen4", #forest_podocarpus_disturbed
#               "darkolivegreen3", #forest_podocarpus_valley
#               "chartreuse1", #grassland
#               "burlywood1", #grassland_savanna
#               "antiquewhite1", #grassland_subalpine
#               "chartreuse3", #grassland_trees
#               "beige", #helicrysum
#               "darkred", #homegarden
#               "bisque", #settlement
#               "black", #shadow
#               "darkmagenta", #unclassified
#               "blue") #water_body

col <- c("Bare_soil" = "azure", #bare_soil
         "Clearing" = "red", #clearing
         "Clouds" = "white", #clouds
         "Coffee" = "bisque4", #coffee
         "Cropland" = "chartreuse4", #cropland
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
         "Helicrysum" = "beige", #helicrysum
         "Homegarden" = "darkred", #homegarden
         "Settlement" = "bisque", #settlement
         "Shadow" = "black", #shadow
         "Unclassified" = "darkmagenta", #unclassified
         "Water_body" = "blue") #water_body

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

foreach(i = rst, j = as.list(substr(basename(fls.rst), 1, 
                                    nchar(basename(fls.rst)) - 4)), 
        .packages = lib) %dopar% {
          
          plt.crp <- crop(plt, i)
          plt.crp <- data.frame(plt.crp)
          
          # Prepare data for 'ggplot' application
          rst.pt <- rasterToPoints(i)
          rst.df <- data.frame(rst.pt)
          colnames(rst.df) = c("x", "y", "luv")
          
          # Merge land-use integer values with class names
          rst.df.mrg <- merge(rst.df, lu, all.x = TRUE, sort = FALSE)
          
          #   # Reorder factor levels for 'ggplot' legend
          #   rst.df.mrg$Class_name <- factor(rst.df.mrg$Class_name, 
          #                                    levels = c("Grassland", "Grassland_Savanna", 
          #                                               "Cropland", "Field", 
          #                                               "Homegarden", "Forest_not_classified", 
          #                                               "Settlement", "Water_body", 
          #                                               "Clouds", "Shadow"))
          
          # 'ggplot'
          png(paste0("out/", j, ".png"), width = 22, height = 17, units = "cm", 
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
          
        }
}
library(GSODTools)
library(Rsenal)

Orcs::setwdOS(path_ext = "phd/phd_kolloq_apr2016")

dat <- gsodstations[grep("MAKINDU|KILIMANJARO|MOSHI", gsodstations$STATION.NAME), ]
coordinates(dat) <- ~ LON + LAT
proj4string(dat) <- "+init=epsg:4326"

# rst <- kiliAerial(upperLeft = c(ymax(dat) + .1, xmin(dat) - .1), 
#                   lowerRight = c(ymin(dat) - .1, xmax(dat) + .1), 
#                   projection = "+init=epsg:4326")
# 
# writeRaster(rst, "data/kili_aerial_makindu.tif", format = "GTiff", 
#             overwrite = TRUE)

rst <- stack("data/kili_aerial_makindu.tif")

p <- spplot(rst[[1]], col.regions = "transparent", colorkey = FALSE, 
            scales = list(draw = TRUE), 
            sp.layout = list(rgb2spLayout(rst, c(0, 1)), 
                             list("sp.points", dat, col = "white", pch = 17, cex = 2.5), 
                             list("sp.points", dat, pch = 17, cex = 2,
                                  col = c("grey50", "black", "black"))))

tiff("img/study_area_makindu.tiff", width = 14, height = 22, units = "cm", 
     res = 300, compression = "lzw")
print(p)
dev.off()
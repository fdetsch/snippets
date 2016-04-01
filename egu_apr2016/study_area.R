library(Rsenal)
library(grid)
library(lattice)

Orcs::setwdOS(path_ext = "phd/egu_2016/poster")

source("R/visKili.R")

# rgb <- kiliAerial(projection = "+init=epsg:4326", minNumTiles = 20L)
# rgb <- trim(rgb)
# rgb <- writeRaster(rgb, "data/study_area.tif", format = "GTiff", 
#                    overwrite = TRUE, bylayer = FALSE)

rgb <- stack("data/study_area.tif")

## scale bar and north arrow
scale <- list("SpatialPolygonsRescale", layout.scale.bar(), scale = 0.08998623, 
              offset = c(37.065, -3.38), fill = c("transparent", "black"))
text1 = list("sp.text", c(37.065, -3.3625), "0", cex = 1.2, font = 2)
text2 = list("sp.text", c(37.16, -3.3625), "10 km", cex = 1.2, font = 2)

arrow <- list("SpatialPolygonsRescale", layout.north.arrow(type = 1), 
              offset = c(37.02, -3.4), scale = .06, fill = "black", lwd = 2)

p <- spplot(rgb[[1]], col.regions = "transparent", colorkey = FALSE, 
            sp.layout = list(rgb2spLayout(rgb, c(0, 1)), 
                             scale, text1, text2, arrow, 
                             list("sp.text", loc = c(37.63, -3.4), 
                                  txt = "\uA9 OpenStreetMap contributors", 
                                  font = 2, cex = 1.2, col = "grey80")), 
            scales = list(draw = TRUE, cex = 1.5))

p_topo <- visKili(ext = rgb, lwd = 5)

tiff("img/study_area.tiff", width = 32, height = 26, units = "cm", 
     pointsize = 18, compression = "lzw", res = 500)
print(p)

## add topographic map
downViewport(trellis.vpname("figure"))
vp_rect <- viewport(x = .755, y = .725, height = .275, width = .25, 
                    just = c("left", "bottom"))
pushViewport(vp_rect)
print(p_topo, newpage = FALSE)

# add equator label
downViewport(trellis.vpname("figure"))
grid.text(x = .05, y = .35, just = c("left", "bottom"), label = "Eq.", 
          gp = gpar(cex = 1.2))

dev.off()

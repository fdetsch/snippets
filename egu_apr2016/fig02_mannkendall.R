### environmental stuff

## clear workspace
rm(list = ls(all = TRUE))

## packages
lib <- c("grid", "Rsenal", "foreach", "latticeExtra", "ggplot2", 
         "Orcs", "gimms")
Orcs::loadPkgs(lib)

## functions
source("R/visKili.R")
source("R/visDEM.R")
source("R/visMannKendall.R")
source("R/visDensity.R")

## folders
ch_dir_extdata <- "/media/fdetsch/XChange/kilimanjaro/ndvi_comparison/data/rst/"
ch_dir_outdata <- "/media/fdetsch/XChange/kilimanjaro/ndvi_comparison/out/"

### data processing

## digital elevation model (dem)
ch_fls_dem <- paste0(ch_dir_extdata, "../dem/DEM_ARC1960_30m_Hemp.tif")
rst_dem <- raster(ch_fls_dem)
rst_dem <- aggregate(rst_dem, fact = 10)
rst_dem <- projectRaster(rst_dem, crs = "+init=epsg:4326")
p_dem <- visDEM(rst_dem, labcex = .6, cex = 1.6, col = "black")

## reference extent
fls_rgb <- "/media/permanent/phd/egu_2016/poster/data/study_area.tif"
rst_rgb <- stack(fls_rgb)

num_xmin <- xmin(rst_rgb)
num_xmax <- xmax(rst_rgb)
num_ymin <- ymin(rst_rgb)
num_ymax <- ymax(rst_rgb)

## gimms grid
rst_gimms <- raster(paste0(ch_dir_outdata, "/GIMMS3g_mk_0312_tau.tif"))
rst_gimms <- projectRaster(rst_gimms, crs = "+init=epsg:4326")
spy_gimms <- rasterToPolygons(rst_gimms)

## non-vegetated modis pixels
modis_nonveg <- readRDS("data/modis_nonvegetated.rds")

## mann-kendall trend tests (2003-2012; p < 0.05)
st_year <- "2003"
nd_year <- "2012"

products <- list("GIMMS3g", 
                 "MOD13Q1.005", "MYD13Q1.005", 
                 "MOD13Q1.006", "MYD13Q1.006")

# ## breaks (works only when mann-kendall trend layers already exist)
# fls_mk <- list.files(ch_dir_outdata, pattern = "mk_0312_tau05", 
#                      full.names = TRUE)
# lst_mk <- lapply(fls_mk, raster)
# 
# sapply(lst_mk, function(i) {
#   cat("Minimum:", minValue(i), "\tMaximum:", maxValue(i), "\n")
#   return(invisible(NULL))
# })

## create and visualize mann-kendall trend layers
lst_p_mk <- lapply(c(.05, .001), function(p_value) {
  # status message
  cat("Processing p =", p_value, "\n")
  
  if (p_value == 0.05) {
    labels <- list(expression(bold("a) NDVI"["3g"])), 
                   expression(bold("b) NDVI"["Terra-C5"])), 
                   expression(bold("c) NDVI"["Aqua-C5"])), 
                   expression(bold("d) NDVI"["Terra-C6"])), 
                   expression(bold("e) NDVI"["Aqua-C6"])))
  } else {
    labels <- list(expression(bold("x) NDVI"["3g"])), 
                   expression(bold("a) NDVI"["Terra-C5"])), 
                   expression(bold("b) NDVI"["Aqua-C5"])), 
                   expression(bold("c) NDVI"["Terra-C6"])), 
                   expression(bold("d) NDVI"["Aqua-C6"])))
  }
  
  foreach(i = products, txt = labels, 
          .export = ls(envir = environment())) %do% {
    
    # list avl files  
    fls_ndvi <- if (i == "GIMMS3g") {
      rearrangeFiles(dsn = paste0(ch_dir_extdata, i), pattern = "^DSN_.*.tif$", 
                     pos = c(4, 6, 11) + 27, full.names = TRUE, 
                     recursive = TRUE)
    } else {
      list.files(paste0(ch_dir_extdata, i), pattern = "^DSN_.*.tif$", 
                 full.names = TRUE, recursive = TRUE)
    }
    
    # import temporal subset
    st <- grep(ifelse(i == "GIMMS3g", "03jan", st_year), fls_ndvi)[1]
    nd <- grep(ifelse(i == "GIMMS3g", "12dec", nd_year), fls_ndvi)
    nd <- nd[length(nd)]
            
    fls_ndvi <- fls_ndvi[st:nd]
    rst_ndvi <- stack(fls_ndvi)
    
    if (i != "GIMMS3g")
      rst_ndvi[modis_nonveg] <- NA
      
    p <- visMannKendall(rst = rst_ndvi, main = expression("Kendall's" ~ tau),
                        xlab = "", ylab = "", keycex = 1.2,
                        p_value = p_value, crs = "+init=epsg:4326",
                        filename = paste0(ch_dir_outdata, i, "_mk_0312.tif"), 
                        at = seq(-.55, .55, .01), 
                        format = "GTiff", 
                        xlim = c(num_xmin, num_xmax), 
                        ylim = c(num_ymin, num_ymax), 
                        scales = list(draw = TRUE, cex = 1.2, 
                                      y = list(at = seq(-2.9, -3.3, -.2))), 
                        rewrite = FALSE)
   
    # add contour lines and text 
    p <- p + 
      latticeExtra::layer(sp.polygons(spy_gimms, lty = 3, col = "grey75"), 
                          data = list(i = i)) + 
      latticeExtra::layer(sp.text(loc = c(37.04, -2.86), txt = txt, font = 2, 
                                  cex = 1.2, adj = c(.1, 1), col = "black"), 
                          data = list(txt = txt)) 
    
    p <- envinmrRasterPlot(p, rot = 0, height = .6, width = .6, key.cex = 1.2)
    
    return(p)
  }
})
  
  
################################################################################
### visualization ##############################################################
################################################################################

## combination final figure, p < 0.05
p_mk_comb <- latticeCombineGrid(lst_p_mk[[1]][c(4, 5, 2, 3, 1)], 
                                layout = c(2, 3), as.table = FALSE)

p_mk_comb <- p_mk_comb + 
  latticeExtra::as.layer(p_dem)

## density plot
p_dens <- visDensity(p = .05, dsn = ch_dir_outdata, combined = FALSE)

# standalone tiff version
tiff("/media/permanent/phd/egu_2016/poster/img/figure02.tiff", width = 30, 
     height = 40, units = "cm", res = 500, compression = "lzw")
plot.new()

print(p_mk_comb, newpage = FALSE)

# add key caption
downViewport(trellis.vpname("figure"))
grid.text(bquote(bold("Kendall's " ~ tau)), x = 0.5, y = 1.12, 
          just = c("centre", "top"), gp = gpar(font = 2, cex = 1.5))

# add density plot #1
upViewport(n = 0)
downViewport(trellis.vpname("figure"))
vp_dens1 <- viewport(x = .4925, y = .340, height = .13, width = .2, 
                     just = c("center", "bottom"), name = "dens1.vp")
pushViewport(vp_dens1)
print(p_dens[[1]], newpage = FALSE)
grid.rect(gp = gpar(col = "black", fill = "transparent", cex = 1.1))

# add density plot #2
upViewport()
vp_dens2 <- viewport(x = .4925, y = .004, height = .13, width = .2, 
                     just = c("center", "bottom"), name = "dens2.vp")
pushViewport(vp_dens2)
print(p_dens[[2]], newpage = FALSE)
grid.rect(gp = gpar(col = "black", fill = "transparent", cex = 1.1))

dev.off()

################################################################################
## combination final figure, p < 0.001
################################################################################
p_mk_comb <- latticeCombineGrid(lst_p_mk[[2]][2:5], layout = c(2, 2))

p_mk_comb <- p_mk_comb + 
  latticeExtra::as.layer(p_dem)

p_dens <- visDensity(p = 0.001, dsn = ch_dir_outdata, combined = FALSE)

# standalone tiff version
tiff("/media/permanent/phd/egu_2016/poster/img/figure04.tiff", width = 30, 
     height = 26, units = "cm", res = 500, compression = "lzw")
plot.new()

print(p_mk_comb, newpage = FALSE)

# add key caption
downViewport(trellis.vpname("figure"))
grid.text(bquote(bold("Kendall's " ~ tau)), x = 0.5, y = 1.18, 
          just = c("centre", "top"), gp = gpar(font = 2, cex = 1.5))

# add density plot #1
upViewport(n = 0)
downViewport(trellis.vpname("figure"))
vp_dens1 <- viewport(x = .4925, y = .341 * 3/2, height = .195, width = .2, 
                     just = c("center", "bottom"), name = "dens1.vp")
pushViewport(vp_dens1)
print(p_dens[[1]], newpage = FALSE)
grid.rect(gp = gpar(col = "black", fill = "transparent", cex = 1.1))

# add density plot #2
upViewport()
vp_dens2 <- viewport(x = .4925, y = .004 * 3/2, height = .195, width = .2, 
                     just = c("center", "bottom"), name = "dens2.vp")
pushViewport(vp_dens2)
print(p_dens[[2]], newpage = FALSE)
grid.rect(gp = gpar(col = "black", fill = "transparent", cex = 1.1))

dev.off()


## set working directory
Orcs::setwdOS(path_ext = "phd/phd_kolloq_apr2016")

## load packages
library(Rsenal)
library(RColorBrewer)
library(lattice)
library(grid)


## data import

rst_eot <- stack("data/gimms_ndvi3g_dwnscl_8211.tif")
rst_eot_ll <- trim(projectRaster(rst_eot, crs = "+init=epsg:4326"))

# ch_dt_scl <- substr(basename(ch_fls_scl), 16, 21)
# 
# ch_fls_dsn <- list.files("kilimanjaro/ndvi/whittaker_myd13q1/", 
#                          pattern = "^DSN_SCL_AGGMAX_WHT", full.names = TRUE)
# ch_dt_dsn <- substr(basename(ch_fls_dsn), 20, 25)
# 
# int_id <- which(ch_dt_scl %in% ch_dt_dsn)
# ch_fls_scl <- ch_fls_scl[int_id]
# 
# rst_scl <- stack(ch_fls_scl)
# rst_scl_ll <- projectRaster(rst_scl, crs = "+init=epsg:4326")
# rst_scl_ll <- trim(rst_scl_ll)
# 
# rst_dsn <- stack(ch_fls_dsn)
# rst_dsn_ll <- projectRaster(rst_dsn, crs = "+init=epsg:4326")
# rst_dsn_ll <- trim(rst_dsn_ll)

## dem
rst_dem <- raster("../../kilimanjaro/coordinates/DEM_ARC1960_30m_Hemp.tif")
rst_dem <- aggregate(rst_dem, 4)
rst_dem_ll <- projectRaster(rst_dem, crs = "+init=epsg:4326")

## monthly averages
rst_mv <- raster::stack(rep(lapply(1:12, function(i) {
  raster::calc(rst_eot[[seq(i, raster::nlayers(rst_eot), 12)]], fun = mean)
}), raster::nlayers(rst_eot) / 12))
rst_mv_ll <- trim(projectRaster(rst_mv, crs = "+init=epsg:4326"))

## jan 1998
dt_sq <- seq(as.Date("1982-01-01"), as.Date("2011-12-31"), "month")
ch_dt_sq <- strftime(dt_sq, format = "%Y%m")
int_id_feb <- grep("199801", ch_dt_sq)

## visualization
col.regions <- colorRampPalette(brewer.pal(9, "Greens"))
rst_dem_ll <- crop(rst_dem_ll, rst_mv_ll)

rst_eot_ll[[int_id_feb]][rst_eot_ll[[int_id_feb]][] > 1] <- 1
p_raw <- spplot(rst_eot_ll[[int_id_feb]], 
                scales = list(draw = TRUE, cex = .7, 
                              x = list(at = seq(37.1, 37.6, .5)), 
                              y = list(at = seq(-3, -3.4, -.2))), 
                col.regions = col.regions(100), at = seq(-.1, 1, .05), 
                colorkey = list(space = "left", width = .6, height = .5), 
                xlab = NULL, ylab = NULL, 
                main = list(expression(bold("NDVI"[EOT])), cex = 1),
                sp.layout = list(
                  list("sp.lines", rasterToContour(rst_dem_ll), col = "grey75", 
                       lwd = .5), 
                  list("sp.text", loc = c(37.04, -3.36), txt = "a)", 
                       col = "black", font = 2, cex = .75)
                ))

p_mv <- spplot(rst_mv_ll[[1]], colorkey = FALSE,
               scales = list(draw = TRUE, cex = .7, 
                             x = list(at = seq(37.1, 37.6, .5)), 
                             y = list(at = seq(-3, -3.4, -.2))), 
               col.regions = col.regions(100), at = seq(-.1, 1, .05),
               xlab = NULL, ylab = NULL, 
               sp.layout = list(
                 list("sp.lines", rasterToContour(rst_dem_ll), col = "grey75", 
                      lwd = .5), 
                 list("sp.text", loc = c(37.04, -3.36), txt = "b)", 
                      col = "black", font = 2, cex = .75)
               ))

p_raw_mv <- latticeCombineGrid(list(p_raw, p_mv), layout = c(2, 1))
p_raw_mv <- envinmrRasterPlot(p_raw_mv, width = .6, height = .6, key.cex = .8)

col.div <- colorRampPalette(brewer.pal(11, "BrBG"))
p_anom <- spplot(rst_eot_ll[[int_id_feb]] - rst_mv_ll[[1]], 
                 colorkey = list(space = "bottom", width = .6, height = .6),  
                 scales = list(draw = TRUE, cex = .7, 
                               x = list(at = seq(37, 37.6, .2), alternating = 1), 
                               y = list(at = seq(-3, -3.4, -.2), rot = 90, alternating = 3)), 
                 col.regions = col.div(100), 
                 xlab = NULL, ylab = NULL, at = seq(-.4, .4, .05), 
                 sp.layout = list(
                   list("sp.lines", rasterToContour(rst_dem_ll), col = "grey75", 
                        lwd = .5), 
                   list("sp.text", loc = c(37.04, -3.36), txt = "c)", 
                        col = "black", font = 2, cex = .75)
                 ))

## manuscript version
old_theme <- lattice.options()
lattice.options(
  layout.widths = list(
    left.padding = list(x = 0, units = "points"), 
    right.padding = list(x = 0, units = "points")
  ), 
  layout.heights = list(
    top.padding = list(x = 0, units = "points"), 
    bottom.padding = list(x = 0, units = "points")
  )
)

tiff("img/deseason.tiff", width = 16, height = 20, units = "cm", res = 300, 
     compression = "lzw")
grid.newpage()

# raw data
vp_raw <- viewport(x = 0, y = .6, just = c("left", "bottom"), 
                   width = 1, height = .4)
pushViewport(vp_raw)
print(p_raw_mv, newpage = FALSE)

# anomalies
downViewport(trellis.vpname("page"))

vp_dsn <- viewport(x = 0, y = -1.35, just = c("left", "bottom"), 
                   width = 1, height = 1.5)
pushViewport(vp_dsn)
print(p_anom, newpage = FALSE)

## right legend title
downViewport(trellis.vpname("figure"))

grid.text(expression(bold(Delta ~ "NDVI")), just = c("left", "bottom"),
          x = .45, y = -.275, gp = gpar(cex = 1))

dev.off()


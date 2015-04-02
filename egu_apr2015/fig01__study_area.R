## environmental stuff

# packages
library(Rsenal)
library(latticeExtra)
library(grid)

# path: output storage
ch_dir_out <- "/media/permanent/phd/egu_2015/poster/fig/"

# path: research plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"


## data

# plots included in sls field campaign
ch_plt_sls <- c("sav5", "sav4", 
                "mai4", "mai1", 
                "cof3", "cof2", 
                "gra1", "gra2", 
                "fer0", "fed1", 
                "hel1")

# bing aerial image
rst_kili <- kiliAerial(minNumTiles = 40, rasterize = TRUE)
spl_kili <- rgb2spLayout(rst_kili, alpha = .9)

# research plots
spp_plt <- readOGR(dsn = ch_dir_crd, layer = ch_fls_crd)
spp_plt_amp <- subset(spp_plt, PoleType == "AMP")

int_plt_sls <- spp_plt_amp$PlotID %in% ch_plt_sls
spp_plt_amp_sls <- spp_plt_amp[int_plt_sls, ]


## visualization

# extent expansion
ext_plt_amp_sls <- extent(spp_plt_amp_sls)

num_xmin <- xmin(ext_plt_amp_sls) - 10000
num_xmax <- xmax(ext_plt_amp_sls) + 5000
num_xlim <- c(num_xmin, num_xmax)

num_ymin <- ymin(ext_plt_amp_sls) - 5000
num_ymax <- ymax(ext_plt_amp_sls) + 10000
num_ylim <- c(num_ymin, num_ymax)

# point coordinates
mat_crd <- coordinates(spp_plt_amp_sls)

# bing aerial including point locations
p_bing <- spplot(spp_plt_amp_sls, zcol = "PlotID", 
                 scales = list(draw = TRUE, cex = 1.25), 
                 col.regions = "white", cex = 1.75, pch = 20,
                 auto.key = FALSE, xlim = num_xlim, ylim = num_ylim, 
                 sp.layout = spl_kili) + 
  layer(sp.points(spp_plt_amp_sls, cex = 1.25, pch = 20, col = "black"))


## visualization

# figure
png(paste0(ch_dir_out, "fig01__study_area.png"), width = 30, height = 24, 
    units = "cm", pointsize = 21, res = 300)

# viewport for visualization of study area
vp1 <- viewport(x = 0, y = 1, height = .9, width = 1, 
                just = c("left", "top"), name = "vp_plot")

grid.newpage()
pushViewport(vp1)

# bing image incl point locations
print(p_bing, newpage = FALSE)

# insertion of shadow text
downViewport(trellis.vpname(name = "figure"))

offsetGridText(x = mat_crd, labels = spp_plt_amp_sls$PlotID, stext = TRUE,
               xlim = num_xlim, ylim = num_ylim, offset = .0175, 
               gp = gpar(fontsize = 25, fontfamily = "Arial"))

# figure caption
upViewport(0)

vp2 <- viewport(x = 0, y = 0, height = .1, width = 1, 
                just = c("left", "bottom"), name = "vp_caption")

pushViewport(vp2)

grid.text(label = "Figure 1: Overview of the study area and the sampling sites.", 
          x = .5, y = .9, just = c("centre", "centre"), 
          gp = gpar(fontfamily = "Arial"))

dev.off()


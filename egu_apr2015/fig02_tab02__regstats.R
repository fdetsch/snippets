## environmental stuff

rm(list = ls(all = TRUE))

# packages
library(latticeExtra)
library(gridExtra)

# path: output storage
ch_dir_out <- "/media/permanent/phd/egu_2015/poster/fig/"

# path: data and functions
ch_dir_repo <- "/media/permanent/repositories/paper_kilimanjaro_scintillometer/"

# functions
source(paste0(ch_dir_repo, "R/slsPlots.R"))


## data

load(paste0(ch_dir_repo, "data/reg_stats_whr.RData"))


## visualization

ch_sls_plt <- slsPlots()

fc_rf_plt <- df_rf_scores_stats$plot
ch_rf_plt <- as.character(fc_rf_plt)

int_id_plt <- match(ch_rf_plt, ch_sls_plt)
ls_rf_scores_dryssn_vis <- ls_rf_scores_dryssn_vis[int_id_plt]

# add plot names to figures
ls_rf_scores_dryssn_vis <- lapply(seq(ls_rf_scores_dryssn_vis), function(i) {
  update(ls_rf_scores_dryssn_vis[[i]], main = paste0("\t", ch_sls_plt[i]))
})

png(paste0(ch_dir_out, "fig02__reg_stats.png"), width = 35, height = 20, 
    units = "cm", pointsize = 15, res = 300)
do.call(function(...) grid.arrange(..., ncol = 4, as.table = TRUE), 
        ls_rf_scores_dryssn_vis)
dev.off()

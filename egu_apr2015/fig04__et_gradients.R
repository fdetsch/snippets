# packages
library(lubridate)
library(plotrix)
library(ggplot2)

# functions
ch_dir_fun <- "../paper_kilimanjaro_scintillometer/R/"
source(paste0(ch_dir_fun, "slsPlots.R"))
source(paste0(ch_dir_fun, "slsAvlFls.R"))
source(paste0(ch_dir_fun, "slsDiurnalVariation.R"))
source(paste0(ch_dir_fun, "slsAggregate.R"))

# path: output storage
ch_dir_out <- "/media/permanent/phd/egu_2015/poster/fig/"

# sls plots and referring files
ch_sls_plt <- slsPlots()

df_sls_fls <- slsAvlFls()
df_sls_fls_rs <- subset(df_sls_fls, season == "r")

# 60-min et rates (mm/h)
ls_sls_dv_01h <- lapply(1:nrow(df_sls_fls_rs), function(i) {
  tmp_df <- slsDiurnalVariation(fn = df_sls_fls_rs$mrg_rf[i], agg_by = 60, 
                                FUN = function(...) mean(..., na.rm = TRUE), 
                                path_fun = ch_dir_fun)
  data.frame(plot = df_sls_fls_rs$plot[i], habitat = df_sls_fls_rs$habitat[i],
             season = df_sls_fls_rs$season[i], tmp_df)
})
df_sls_dv_01h <- do.call("rbind", ls_sls_dv_01h)

# maximum hourly et rates per plot
library(dplyr)
df_sls_dv_01h %>%
  group_by(plot) %>% 
  filter(waterET == max(waterET)) %>%
  data.frame() %>%
  arrange(waterET)


# diurnal et amounts (mm)
ls_sls_dv_01d <- lapply(ls_sls_dv_01h, function(i) {
  tmp.df <- slsAggregate(fn = i, agg_by = 24, include_time = FALSE,
                         FUN = function(...) round(sum(..., na.rm = TRUE), 1))
  data.frame(plot = unique(i$plot), habitat = unique(i$habitat), 
             season = unique(i$season), tmp.df)
})
df_sls_dv_01d <- do.call("rbind", ls_sls_dv_01d)

df_sls_dv_01d %>%
  group_by(habitat) %>%
  summarise(waterET_mu = mean(waterET)) %>%
  data.frame() -> df_sls_dv_01d_agg

# reorder habitat factor levels
df_sls_dv_01d_agg$habitat_new <- factor(df_sls_dv_01d_agg$habitat, 
                                       levels = rev(c("mai", "sav", "cof", "gra", 
                                                      "fed", "fer", " ", "hel")))

df_sls_dv_01d_agg$x <- "17:00"
df_sls_dv_01d_agg$y <- 0.65

# daytime subset
ch_sls_dv_01h_hr <- substr(as.character(df_sls_dv_01h$datetime), 1, 2)
int_sls_dv_01h_hr <- as.integer(ch_sls_dv_01h_hr)
int_sls_dv_01h_dt <- int_sls_dv_01h_hr >= 4 & int_sls_dv_01h_hr < 20
df_sls_dv_01h_dt <- df_sls_dv_01h[int_sls_dv_01h_dt, ]

# standard errors
df_sls_dv_01h_dt %>% 
  group_by(habitat, datetime) %>% 
  summarise(waterET_mu = mean(waterET), waterET_se = std.error(waterET)) %>%
  data.frame() -> df_sls_dv_01h_dt

df_sls_dv_01h_dt$time <- strptime(df_sls_dv_01h_dt$datetime, format = "%H:%M:%S")
df_sls_dv_01h_dt$time_fac <- factor(format(df_sls_dv_01h_dt$time, format = "%H:%M"))

# reorder habitat factor levels
df_sls_dv_01h_dt$habitat_new <- factor(df_sls_dv_01h_dt$habitat, 
                                       levels = rev(c("mai", "sav", "cof", "gra", 
                                                      "fed", "fer", " ", "hel")))

# x-axis labels
ch_lvl <- levels(df_sls_dv_01h_dt$time_fac)
ch_lbl <- rep("", length(ch_lvl))

ls_lvl <- strsplit(ch_lvl, ":")
ch_lvl_hr <- sapply(ls_lvl, "[[", 1)
int_lvl_hr <- as.integer(ch_lvl_hr)
int_lvl_hr_odd <- int_lvl_hr %% 2 != 0
ch_lvl_min <- sapply(ls_lvl, "[[", 2)

int_lvl_hr_odd_full <- ch_lvl_min == "00" & int_lvl_hr_odd
ch_lbl[int_lvl_hr_odd_full] <- ch_lvl[int_lvl_hr_odd_full]
names(ch_lbl) <- ch_lvl

# visualization
limits <- aes(ymax = waterET_mu + waterET_se, ymin = waterET_mu - waterET_se)

p <- ggplot(aes(x = time_fac, y = waterET_mu), data = df_sls_dv_01h_dt) + 
  geom_hline(aes(yintercept = 0), colour = "grey75") + 
  geom_histogram(stat = "identity", position = "dodge", 
                 colour = "black", fill = "black") +
  facet_wrap(~ habitat_new, ncol = 2, drop = FALSE) + 
  geom_errorbar(limits, position = "dodge", linetype = "dashed", 
                width = .2, colour = "grey60") + 
  geom_text(aes(x = x, y = y, label = paste0("  ", waterET_mu, " mm/d")), 
            data = df_sls_dv_01d_agg) + 
  scale_x_discrete(labels = ch_lbl) + 
  guides(colour = FALSE, fill = FALSE) + 
  labs(x = "\nTime (hours)", y = "Evapotranspiration (mm) \n") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        strip.text = element_text(size = 12))

png(paste0(ch_dir_out, "/fig04__et_gradients.png"), width = 25, 
    height = 25, units = "cm", res = 300, pointsize = 21)
print(p) 
dev.off()

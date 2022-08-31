#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_hysteresis_dimensions.R ####
# ---------------------------------------------------------------------------- #
# Purpose: process data to make figures of hysteresis plots
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #

require(dplyr)
require(tidyr)

#### Get the data ####

load('data/input_data/climate_zones/df_climspace_t2xsm.RData') # <---- df_climspace
load('data/final_data/figures_for_paper/hysteresis_data_ready4fig.RData', verbose = T)

df_2dim <- df_p_all %>%
  transmute(t2.clim.bin = t2.clim.bin,
            sm.clim.bin = sm.clim.bin,
            t2.clim = as.numeric(t2.clim.bin),
            sm.clim = as.numeric(sm.clim.bin),
            xrngN = xrange_s / sd(df_r_all$x),
            yrngN = yrange_s / sd(df_r_all$y),
            areaB = xrange_s * yrange_s,
            areaL = areaL,
            areaM = max(areaB), # max(xrange_s) * max(yrange_s),
            hystI = areaL / max(areaL),
            biasI = (xrngN - yrngN)/(xrngN + yrngN))



#### Prepare Map based on bins ####

load('data/input_data/climate_zones/df_climspace_t2xsm.RData') # <---- df_climspace
#df_climspace <- df_climspace %>% select(x, y, clim.x, clim.y) %>% rename(clim.t2 = clim.x, clim.sm = clim.y)

x.bin.width <- 4
y.bin.width <- 0.04

# make bin labels
x.bin.numlbls <- seq(floor(min(df_climspace$clim.t2)),
                     ceiling(max(df_climspace$clim.t2)) + x.bin.width, 
                     x.bin.width)
y.bin.numlbls <- seq(floor(min(df_climspace$clim.sm)), 
                     ceiling((max(df_climspace$clim.sm) + y.bin.width) * 100)/100,
                     y.bin.width)

# make bin breaks
x.bin.breaks <- c(x.bin.numlbls - (x.bin.width/2), Inf)
y.bin.breaks <- c(y.bin.numlbls - (y.bin.width/2), Inf)

df_climspace_bin <- df_climspace %>%
  mutate(t2.clim.bin = cut(clim.t2,
                           breaks = x.bin.breaks,
                           labels = x.bin.numlbls)) %>%
  mutate(sm.clim.bin = cut(clim.sm,
                           breaks = y.bin.breaks,
                           labels = y.bin.numlbls))


df_hyst_map <- df_climspace_bin %>% 
  left_join(df_2dim, by = c('t2.clim.bin', 'sm.clim.bin'))


#### Export the data ####

save('df_hyst_map', file = 'data/final_data/figures_for_paper/hysteresis_map_ready4fig.RData')







# 
# require(ggplot2)
# 
# ggplot(df_map) +
#   geom_raster(aes(x = x, y = y, fill = clim.t2)) +
#   scale_fill_viridis(option = "F")
# 
# ggplot(df_map) +
#   geom_raster(aes(x = x, y = y, fill = clim.sm)) +
#   scale_fill_viridis(option = "G")
# 
# ggplot(df_map) +
#   geom_raster(aes(x = x, y = y, fill = hystI)) +
#   scale_fill_viridis(option = "F")
# 
# ggplot(df_map) +
#   geom_raster(aes(x = x, y = y, fill = biasI)) +
#   scale_fill_viridis(option = "F")
# 
# ggplot(df_2dim, aes(x = t2.clim, y = sm.clim)) +
#   geom_raster(aes(fill = biasI)) + 
#   scale_fill_viridis(option = "F")
# 
# ggplot(df_2dim, aes(x = t2.clim, y = sm.clim)) +
#   geom_raster(aes(fill = hystI)) + 
#   scale_fill_viridis(option = "B")
# 
# ggplot(df_2dim, aes(x = t2.clim, y = sm.clim)) +
#   geom_raster(aes(fill = areaB)) + 
#   scale_fill_viridis(option = "G")
# 
# ggplot(df_2dim, aes(x = t2.clim, y = sm.clim)) +
#   geom_raster(aes(fill = areaL)) + 
#   scale_fill_viridis(option = "G")

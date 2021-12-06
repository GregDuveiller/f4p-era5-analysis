#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### calc_hysteresis_dimensions.R ####
# ---------------------------------------------------------------------------- #
# Purpose: process data to make figures of hysteresis plots
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #

require(dplyr)
require(tidyr)
require(ggplot2)



load('data/figures_for_paper/hysteresis_data_ready4fig.RData', verbose = T)

df_2dim <- df_p_all %>%
  transmute(t2.clim = as.numeric(t2.clim.bin),
            sm.clim = as.numeric(sm.clim.bin),
            xrngN = xrange_s / sd(df_r_all$x),
            yrngN = yrange_s / sd(df_r_all$y),
            areaB = xrange_s * yrange_s,
            areaL = areaL,
            hystI = areaL / areaB,
            biasI = (xrngN - yrngN)/(xrngN + yrngN))

#  select(t2.clim.bin, sm.clim.bin ) %>%


ggplot(df_2dim, aes(x = t2.clim, y = sm.clim)) +
  geom_raster(aes(fill = biasI)) + 
  scale_fill_viridis(option = "F")

ggplot(df_2dim, aes(x = t2.clim, y = sm.clim)) +
  geom_raster(aes(fill = hystI)) + 
  scale_fill_viridis(option = "B")

ggplot(df_2dim, aes(x = t2.clim, y = sm.clim)) +
  geom_raster(aes(fill = areaL)) + 
  scale_fill_viridis(option = "G")

#!/usr/local/bin/Rscript
################################################################################
# Purpose: make diagnostic plot to evaluate agreement of single variables
# Project: f4p-era5-analysis
# Authors: G.Duveiller
################################################################################


library(dplyr)
library(tidyr)

library(ggplot2)

load('data/inter_data/df_comb_obs_vs_sim/df_comb___LST.RData', verbose = T)  # <--- df_comb


# get function for index of agreement...
source('../../tools/agreement-index/calculate-agr-metrics.R')

# overall agreement
agr <- get.Agr.Metrics(df_comb$obs, df_comb$sim)

# agreement in space (per pixel)
sp_agr <- df_comb %>%
  filter(x == -37.125, y == 83.375) %>%
  group_by(y, x) %>%
  summarise(L_sp = get.Agr.Metrics(obs, sim))


# agreement in time (per climzone)





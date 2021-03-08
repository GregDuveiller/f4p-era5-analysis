#!/usr/local/bin/Rscript
################################################################################
# Purpose: make diagnostic plot of agreement of single variables
# Project: f4p-era5-analysis
# Authors: G.Duveiller
################################################################################


library(ggplot2)
library(patchwork)
library(scales)
library(dplyr)


varname <- 'LST'
varname <- 'albedo_wsa_vis'

load( paste0('data/inter_data/df_single_var_agreement/df_single_var_agr_', varname,'.RData'))
# 'agr', 'freq', 'sp_agr', 'temp_agr',


gBin2d <- ggplot(freq) +
    geom_tile(aes(x = obs_val, y = sim_val, fill = freq)) + 
    geom_abline(colour = 'grey50') + 
    scale_fill_viridis_c(option = 'A')




ggplot(sp_agr) + 
  geom_tile(aes(x = x, y = y, fill =agre$L)) +  
  scale_fill_viridis_c(option = 'A')



ggplot(temp_agr) + 
  geom_line(aes(x = time, y = agre$L, colour = cz_name))


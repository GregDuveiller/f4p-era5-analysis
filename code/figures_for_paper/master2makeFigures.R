#!/usr/local/bin/Rscript
################################################################################
# Purpose: master script to generate all figures in the paper
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
################################################################################

library(ggplot2)
library(patchwork)


#### Setup...####

fig.path <- 'paper/figures/'
dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.fmt <- 'png'


#### Calls to scripts ... ####




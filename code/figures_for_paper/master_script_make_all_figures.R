#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### master_script_make_all_figures.R ####
# ---------------------------------------------------------------------------- #
# Purpose: master script to generate all figures in the paper
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #

library(ggplot2)
library(patchwork)


#### Setup...####

fig.path <- 'paper/figures/'
dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.fmt <- 'png'

#### Figure for the initial bias maps ####


#### Figure with the hysteresis demo plots ####
source('code/figures_for_paper/plot_hyst_demo.R')


#### Figure for the full hysteresis climate space ####
source('code/figures_for_paper/plot_hyst_climspace.R')


#### Figure for the interannual correlation maps ####


#### Figure for the distributions of the biases for selected climate zones ####


#### Figure for heat wave maps ####




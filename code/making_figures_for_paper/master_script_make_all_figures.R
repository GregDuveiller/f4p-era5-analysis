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

gry_land <- 'grey50'
gry_meer <- 'grey30'



#### Figure for the initial bias maps ####
source('code/making_figures_for_paper/plot_bias_summaries.R')

#### Figure with the hysteresis demo plots ####
source('code/making_figures_for_paper/plot_hyst_demo.R')

#### Figure for the full hysteresis climate space ####
source('code/making_figures_for_paper/plot_hyst_climspace.R')

#### Figure for the hysteresis maps ####
source('code/making_figures_for_paper/plot_hyst_summary_map.R')

#### Figure for the inter-annual correlation maps ####
source('code/making_figures_for_paper/plot_interannual_correlation.R')

#### Figure for the distributions of the biases for selected climate zones ####
source('code/making_figures_for_paper/plot_bias_against_temp_anomalies.R')

#### Figure for heat wave maps ####
source('code/making_figures_for_paper/plot_heatwaves.R')




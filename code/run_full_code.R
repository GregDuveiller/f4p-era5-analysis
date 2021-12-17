#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### run_full_code.R ####
# ---------------------------------------------------------------------------- #
# Purpose: Run through git code start to finish
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #

# set working directory to local git 
setwd() # setwd('/media/mark/HD/Mark/Mark_COPERNICUS/scripts/git_paperTest/f4p-era5-analysis')


# setup directory paths
source('setup_data_folder_structure.R')

# create sim-obs paired dataframes from satellite/ERA5 data 
source('code/combine_sim_with_obs/harvest___CombDFs___LAI.R')
source('code/combine_sim_with_obs/harvest___CombDFs___LST.R')
source('code/combine_sim_with_obs/harvest___CombDFs___ET.R')
source('code/combine_sim_with_obs/harvest___CombDFs___Albedo_MCD43C3.R')
source('code/combine_sim_with_obs/harvest___CombDFs___Albedo_MCD43GF.R')

# Global bias and correlation figures
source('code/harvesting_data_for_final_figures/prepare_data_for_bias_and_cor_summaries.R')
source('code/making_figures_for_paper/plot_bias_summaries.R')
source('code/making_figures_for_paper/plot_interannual_correlation.R')


# # Climate hystersis figures
#     prepare_data_per_clim_bin.R   prepare_hysteresis_dimensions.R  
# prepare_climzones.R                            prepare_hysteresis_per_bin.R     
# 
# # Bias with temperature anomaly figures
# prepare_bias_vs_temp_anomalies.R
# 
# # Heatwave figures
# prepare_gislayers_for_maps.R
# prepare_time_series_4_hw.R
# prepare_data_for_heatwave_maps.R
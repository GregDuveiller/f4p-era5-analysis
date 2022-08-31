#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### run_full_code.R ####
# ---------------------------------------------------------------------------- #
# Purpose: This script suggests a run through order for reproducing the figures 
# of the paper, however it is recommended to run the scripts in separate shells
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
# source('code/combine_sim_with_obs/harvest___CombDFs___Albedo_MCD43GF.R')

# prepare climate zone dataframe
source('code/harvesting_data_for_final_figures/prepare_climzones.R')
# prepare European maps for heatwaves and temperature anomaly
source('code/harvesting_data_for_final_figures/prepare_gislayers_for_maps.R')

# Global bias and correlation figures
source('code/harvesting_data_for_final_figures/prepare_data_for_bias_and_cor_summaries.R')
source('code/making_figures_for_paper/plot_bias_summaries.R')                                   # average temporal bias represented in space
source('code/making_figures_for_paper/plot_interannual_correlation.R')                          # temporal correlation between LST and LAI represented in space

# Climate hystersis figures
source('code/harvesting_data_for_final_figures/prepare_data_per_clim_bin.R')
source('code/harvesting_data_for_final_figures/prepare_hysteresis_per_bin.R')
source('code/harvesting_data_for_final_figures/prepare_hysteresis_dimensions.R')
source('code/making_figures_for_paper/plot_hyst_demo.R')                                      # hysteresis demonstration
source('code/making_figures_for_paper/plot_hyst_climspace.R')                                   # full hysteresis climate space
source('code/making_figures_for_paper/plot_hyst_summary_map.R')                                 # hysteresis maps


# Europe LST/LAI bias with temperature figure
source('code/harvesting_data_for_final_figures/prepare_bias_vs_temp_anomalies.R')
source('code/making_figures_for_paper/plot_bias_against_temp_anomalies.R')                      # plot the LST/LAI bias as a function of temperature anomaly

# Europe bias during heatwave figure
source('code/harvesting_data_for_final_figures/prepare_time_series_4_hw.R')
source('code/harvesting_data_for_final_figures/prepare_data_for_heatwave_maps.R')
source('code/harvesting_data_for_final_figures/prepare_heatwave_summary_stats.R')
# source('code/making_figures_for_paper/plot_heatwave_locations.R')                             # simple plot of the locations of heatwaves
source('code/making_figures_for_paper/plot_heatwaves.R')                                        # plot the heatwave bias shift statistics and map


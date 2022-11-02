#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### master_script_harvest_all.R ####
# ---------------------------------------------------------------------------- #
# Purpose: master script to harvest all data to make figures for the paper
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #

# prepare data for climzones
source("code/harvesting_data_for_final_figures/prepare_climzones.R")
source("code/harvesting_data_for_final_figures/prepare_data_per_clim_bin.R") 

# prepare data for hysteresis analysis
source("code/harvesting_data_for_final_figures/prepare_hysteresis_per_bin.R")
source("code/harvesting_data_for_final_figures/prepare_hysteresis_dimensions.R")

# prepare data for ...
source("code/harvesting_data_for_final_figures/prepare_data_for_bias_and_cor_summaries.R")
source("code/harvesting_data_for_final_figures/prepare_bias_vs_temp_anomalies.R")  

# prepare data for heatwave analysis
source("code/harvesting_data_for_final_figures/prepare_gislayers_for_maps.R")
source("code/harvesting_data_for_final_figures/prepare_data_for_heatwave_maps.R")
source("code/harvesting_data_for_final_figures/prepare_heatwave_summary_stats.R")




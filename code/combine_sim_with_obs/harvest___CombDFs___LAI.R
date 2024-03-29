#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### harvest___CombDFs___LAI.R ####
# ---------------------------------------------------------------------------- #
# Purpose: match the RS and ERA5 reanalysis data sources in single dataframe
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #

library(dplyr)
library(tidyr)

# input data
dpath_r_dataframes <- 'data/input_data/r_data_frames' 
#output data
out_path <- 'data/inter_data/df_comb_obs_vs_sim'
dir.create(path = out_path, recursive = T, showWarnings = F)
source('code/combine_sim_with_obs/function___get_df_comb_lai.R')

spres <- '025'

## LAI  ----
get_df_comb_lai(
  target_var = 'LAI',
  spres = spres,
  src_obs = 'theia',
  src_sim = 'ERA5sl',
  path_obs = paste0(dpath_r_dataframes,''), # path extension to dataframes within dataframe dir, e.g: /theia/dataframe_res025
  path_sim = paste0(dpath_r_dataframes,''), # path extension to dataframes within dataframe dir, e.g: /CDS/LAI_ERA5sl/dataframe_res025
  out_path = out_path
)

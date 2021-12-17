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
source('code/combine_sim_with_obs/function___get_df_comb_std.R')

spres <- '025'

## LST  ----
get_df_comb_std(
  target_var = 'LST',
  spres = spres,
  src_obs = 'GEE',
  src_sim = 'ERA5l',
  target_var_sim = 'SKT',
  path_obs = paste0(dpath_r_dataframes,''), # path extension to dataframes within dataframe dir , e.g: '/GEE/LST_MODIS/dataframe_res025'
  path_sim = paste0(dpath_r_dataframes,''), # path extension to dataframes within dataframe dir , e.g: '/CDS/SKT_ERA5l/dataframe_res025'
  varDFname_obs = 'LST_max5_mean',
  varDFname_sim = 'skt_top5avg',
  out_path = out_path
)
  
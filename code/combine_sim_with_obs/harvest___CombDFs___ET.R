#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### harvest___CombDFs___ET.R ####
# ---------------------------------------------------------------------------- #
# Purpose: match the RS and ERA5 reanalysis data sources in single dataframe
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #

library(dplyr)
library(tidyr)


# input data
dat_path <- '/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3'
#output data
out_path <- 'data/inter_data/df_comb_obs_vs_sim'
dir.create(path = out_path, recursive = T, showWarnings = F) # MP
source('code/combine_sim_with_obs/function___get_df_comb_std.R')

spres <- '025'

## LST  ----
get_df_comb_std(
  target_var = 'E',
  spres = spres,
  src_obs = 'GLEAM',
  src_sim = 'ERA5l',
  target_var_sim = 'ET',
  path_obs = paste0(dat_path,'/GLEAM/dataframe_res', spres),
  path_sim = paste0(dat_path,'/CDS/ET_ERA5l/dataframe_res', spres),
  varDFname_obs = 'E',
  varDFname_sim = 'e',
  out_path = out_path
)

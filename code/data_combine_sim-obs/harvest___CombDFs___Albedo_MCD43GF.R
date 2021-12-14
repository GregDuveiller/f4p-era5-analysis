#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### harvest___CombDFs___albedo_MC43GF.R ####
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
out_path <- 'data/combine_sim-obs'
dir.create(path = out_path, recursive = T, showWarnings = F) # MP
source('code/data_combine_sim-obs/function___get_df_comb_std.R')

spres <- '025'


## ALBEDO - white sky visible ----
get_df_comb_std(
  target_var = 'albedo',
  xtrLbl_obs = 'wsa_vis',
  spres = spres,
  src_obs = 'NASA',
  src_sim = 'ERA5l',
  path_obs = paste0(dat_path,'/NASA_LPDAAC/albedo_MCD43GF/dataframe_res', spres, '/QA_all'),
  path_sim = paste0(dat_path,'/CDS/albedo_ERA5sl/dataframe_res', spres),
  varDFname_obs = 'wsa_vis_QAall',
  varDFname_sim = 'aluvd',
  out_path = out_path
  )

# other types of albedo 

# ## ALBEDO - white sky near infrared ----
# get_df_comb_std(
#   target_var = 'albedo',
#   xtrLbl_obs = 'wsa_nir',
#   spres = spres,
#   src_obs = 'NASA',
#   src_sim = 'ERA5l',
#   path_obs = paste0(dat_path,'/NASA_LPDAAC/albedo_MCD43GF/dataframe_res', spres, '/QA_all'),
#   path_sim = paste0(dat_path,'/CDS/albedo_ERA5sl/dataframe_res', spres),
#   varDFname_obs = 'wsa_nir_QAall',
#   varDFname_sim = 'alnid',
#   out_path = out_path
# )
# 
# ## ALBEDO - black sky visible ----
# get_df_comb_std(
#   target_var = 'albedo',
#   xtrLbl_obs = 'bsa_vis',
#   spres = spres,
#   src_obs = 'NASA',
#   src_sim = 'ERA5l',
#   path_obs = paste0(dat_path,'/NASA_LPDAAC/albedo_MCD43GF/dataframe_res', spres, '/QA_all'),
#   path_sim = paste0(dat_path,'/CDS/albedo_ERA5sl/dataframe_res', spres),
#   varDFname_obs = 'bsa_vis_QAall',
#   varDFname_sim = 'aluvp',
#   out_path = out_path
# )
# 
# ## ALBEDO - black sky near infrared ----
# get_df_comb_std(
#   target_var = 'albedo',
#   xtrLbl_obs = 'bsa_nir',
#   spres = spres,
#   src_obs = 'NASA',
#   src_sim = 'ERA5l',
#   path_obs = paste0(dat_path,'/NASA_LPDAAC/albedo_MCD43GF/dataframe_res', spres, '/QA_all'),
#   path_sim = paste0(dat_path,'/CDS/albedo_ERA5sl/dataframe_res', spres),
#   varDFname_obs = 'bsa_nir_QAall',
#   varDFname_sim = 'alnip',
#   out_path = out_path
# )

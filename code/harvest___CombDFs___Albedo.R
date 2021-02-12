### prototype for a matching script (between RS and ReAnalysis)

library(dplyr)
library(tidyr)

dat_path <- '/eos/jeodpp/data/projects/COPERNICUSII/data'
out_path <- '/eos/jeodpp/data/projects/COPERNICUSII/transfer/greg_workspace'

spres <- '025'

source('code/function___get_df_comb_std.R')

## ALBEDO - white sky visible ----
get_df_comb_std(
  target_var = 'albedo',
  xtrLbl_obs = 'wsa_vis',
  spres = spres,
  src_obs = 'NASA',
  src_sim = 'ERA5l',
  path_obs = paste0(dat_path,'/data/NASA_LPDAAC/albedo_MCD43GF/dataframe_res', spres, '/QA_all'),
  path_sim = paste0(dat_path,'/data/CDS/albedo_ERA5sl/dataframe_res', spres),
  varDFname_obs = 'wsa_vis_QAall',
  varDFname_sim = 'aluvd',
  out_path = out_path
  )

## ALBEDO - white sky visible ----
get_df_comb_std(
  target_var = 'albedo',
  xtrLbl_obs = 'bsa_nir',
  spres = spres,
  src_obs = 'NASA',
  src_sim = 'ERA5l',
  path_obs = paste0(dat_path,'/data/NASA_LPDAAC/albedo_MCD43GF/dataframe_res', spres, '/QA_all'),
  path_sim = paste0(dat_path,'/data/CDS/albedo_ERA5sl/dataframe_res', spres),
  varDFname_obs = 'wsa_nir_QAall',
  varDFname_sim = 'alnid',
  out_path = out_path
)

## ALBEDO - black sky visible ----
get_df_comb_std(
  target_var = 'albedo',
  xtrLbl_obs = 'bsa_vis',
  spres = spres,
  src_obs = 'NASA',
  src_sim = 'ERA5l',
  path_obs = paste0(dat_path,'/data/NASA_LPDAAC/albedo_MCD43GF/dataframe_res', spres, '/QA_all'),
  path_sim = paste0(dat_path,'/data/CDS/albedo_ERA5sl/dataframe_res', spres),
  varDFname_obs = 'wsa_vis_QAall',
  varDFname_sim = 'aluvp',
  out_path = out_path
)

## ALBEDO - black sky visible ----
get_df_comb_std(
  target_var = 'albedo',
  xtrLbl_obs = 'bsa_nir',
  spres = spres,
  src_obs = 'NASA',
  src_sim = 'ERA5l',
  path_obs = paste0(dat_path,'/data/NASA_LPDAAC/albedo_MCD43GF/dataframe_res', spres, '/QA_all'),
  path_sim = paste0(dat_path,'/data/CDS/albedo_ERA5sl/dataframe_res', spres),
  varDFname_obs = 'wsa_nir_QAall',
  varDFname_sim = 'alnip',
  out_path = out_path
)

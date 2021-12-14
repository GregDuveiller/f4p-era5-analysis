    ### prototype for a matching script (between RS and ReAnalysis)

library(dplyr)
library(tidyr)

dat_path <- '/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3'
out_path <- '/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3/greg_workspace'
source('code/combine_sim-obs/function___get_df_comb_std.R')

spres <- '025'

## ALBEDO - white sky visible ----
get_df_comb_std(
  target_var = 'albedo',
  xtrLbl_obs = 'wsa_vis',
  spres = spres,
  src_obs = 'MCD43C3',
  src_sim = 'ERA5l',
  path_obs = paste0(dat_path,'/NASA_LPDAAC/albedo_MCD43C3/dataframe_res', spres),
  path_sim = paste0(dat_path,'/CDS/albedo_ERA5sl/dataframe_res', spres),
  varDFname_obs = 'Albedo_WSA_vis',
  varDFname_sim = 'aluvd',
  out_path = out_path
  )

# other types of albedo

# ## ALBEDO - white sky near infrared ----
# get_df_comb_std(
#   target_var = 'albedo',
#   xtrLbl_obs = 'wsa_nir',
#   spres = spres,
#   src_obs = 'MCD43C3',
#   src_sim = 'ERA5l',
#   path_obs = paste0(dat_path,'/NASA_LPDAAC/albedo_MCD43C3/dataframe_res', spres),
#   path_sim = paste0(dat_path,'/CDS/albedo_ERA5sl/dataframe_res', spres),
#   varDFname_obs = 'Albedo_WSA_nir',
#   varDFname_sim = 'alnid',
#   out_path = out_path
# )
# 
# ## ALBEDO - black sky visible ----
# get_df_comb_std(
#   target_var = 'albedo',
#   xtrLbl_obs = 'bsa_vis',
#   spres = spres,
#   src_obs = 'MCD43C3',
#   src_sim = 'ERA5l',
#   path_obs = paste0(dat_path,'/NASA_LPDAAC/albedo_MCD43C3/dataframe_res', spres),
#   path_sim = paste0(dat_path,'/CDS/albedo_ERA5sl/dataframe_res', spres),
#   varDFname_obs = 'Albedo_BSA_vis',
#   varDFname_sim = 'aluvp',
#   out_path = out_path
# )
# 
# ## ALBEDO - black sky near infrared ----
# get_df_comb_std(
#   target_var = 'albedo',
#   xtrLbl_obs = 'bsa_nir',
#   spres = spres,
#   src_obs = 'MCD43C3',
#   src_sim = 'ERA5l',
#   path_obs = paste0(dat_path,'/NASA_LPDAAC/albedo_MCD43C3/dataframe_res', spres),
#   path_sim = paste0(dat_path,'/CDS/albedo_ERA5sl/dataframe_res', spres),
#   varDFname_obs = 'Albedo_BSA_nir',
#   varDFname_sim = 'alnip',
#   out_path = out_path
# )

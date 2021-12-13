### prototype for a matching script (between RS and ReAnalysis)

library(dplyr)
library(tidyr)

dat_path <- '/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3'
out_path <- '/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3/greg_workspace'
source('/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/code/combine_sim-obs/function___get_df_comb_std.R')


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

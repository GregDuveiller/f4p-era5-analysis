### prototype for a matching script (between RS and ReAnalysis)

library(dplyr)
library(tidyr)

dat_path <- '/eos/jeodpp/data/projects/COPERNICUSII/data'
out_path <- '/eos/jeodpp/data/projects/COPERNICUSII/transfer/greg_workspace'

spres <- '025'

source('code/function___get_df_comb_std.R')

## LST  ----
get_df_comb_std(
  target_var = 'LST',
  spres = spres,
  src_obs = 'GEE',
  src_sim = 'ERA5l',
  target_var_sim = 'SKT',
  path_obs = paste0(dat_path,'/GEE/LST_MODIS/dataframe_res', spres),
  path_sim = paste0(dat_path,'/CDS/SKT_ERA5l/dataframe_res', spres),
  varDFname_obs = 'LST_max5_mean',
  varDFname_sim = 'skt_top5avg',
  out_path = out_path
)

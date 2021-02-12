### prototype for a matching script (between RS and ReAnalysis)

library(dplyr)
library(tidyr)

dat_path <- '/eos/jeodpp/data/projects/COPERNICUSII/data'
out_path <- '/eos/jeodpp/data/projects/COPERNICUSII/transfer/greg_workspace'

spres <- '025'

source('code/function___get_df_comb_lai.R')

## LAI  ----
get_df_comb_std(
  target_var = 'LAI',
  spres = spres,
  src_obs = 'theia',
  src_sim = 'ERA5l',
  path_obs = paste0(dat_path,'/theia/dataframe_res025'),
  path_sim = paste0(dat_path,'/CDS/LAI_ERA5sl/dataframe_res025'), 
  out_path = out_path
)

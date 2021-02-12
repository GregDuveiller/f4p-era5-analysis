### prototype for a matching script (between RS and ReAnalysis)

library(dplyr)
library(tidyr)

dat_path <- '/eos/jeodpp/data/projects/COPERNICUSII/data'
out_path <- '/eos/jeodpp/data/projects/COPERNICUSII/transfer/greg_workspace'

spres <- '025'

source('code/function___get_df_comb_std.R')

## Soil Moisture  ----
get_df_comb_std(
  target_var = 'SM',
  spres = spres,
  src_obs = 'sat',
  src_sim = 'SM',
  path_obs = paste0(dat_path,'/CDS/SM_satellite/dataframe_res', spres),
  path_sim = paste0(dat_path,'/CDS/SM_ERA5l/dataframe_res', spres),
  varDFname_obs = 'sm',
  varDFname_sim = 'swvl1',
  out_path = out_path
)

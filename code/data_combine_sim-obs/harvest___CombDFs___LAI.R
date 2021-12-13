### prototype for a matching script (between RS and ReAnalysis)

library(dplyr)
library(tidyr)

dat_path <- '/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3'
out_path <- '/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3/greg_workspace'
source('/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/code/combine_sim-obs/function___get_df_comb_std.R')

spres <- '025'


## LAI  ----
get_df_comb_lai(
  target_var = 'LAI',
  spres = spres,
  src_obs = 'theia',
  src_sim = 'ERA5sl',
  path_obs = paste0(dat_path,'/theia/dataframe_res025'),
  path_sim = paste0(dat_path,'/CDS/LAI_ERA5sl/dataframe_res025'), 
  out_path = out_path
)

#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_data_for_corr_summaries.R ####
# ---------------------------------------------------------------------------- #
# Purpose: prepare the data to generate maps summarizing the bias correlation
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #



#### Initialization ####

library(dplyr)
require(tidyr)


#### Set-up of parameters ####

# chose variable names. LAI should be first and comparison variable second
varname_1 <- 'LAI' # 'albedo_wsa_vis' # 'E' # 'SM'  # 'LST' #'albedo_wsa_nir' albedo_wsa_vis albedo_bsa_nir albedo_bsa_vis #LAI
varname_2 <- 'LST'
dpath <- 'data/inter_data/df_single_var_summaries/'


#### Load the data ####

# load the agreement metrics from prepare___variables.R

varname_2 <- 'LST'
load(paste0(dpath, 'df_single_var_agr__sp_agr_monthS__', varname_2,'.RData'))  
sp_agr_monthS_var2  <- sp_agr_monthS


#### Prepare the data ####

df_dum <- data.frame(
  x = sp_agr_monthS_var2$x,
  y = sp_agr_monthS_var2$y,
  monthS = sp_agr_monthS_var2$monthS,
  r = sp_agr_monthS_var2$agre_bVARvsbLAI$r)

df_LSTb_LAIb_corr <- df_dum %>% 
  filter(monthS %in% c(1,7)) %>%
  select(x, y, monthS, r)




#### Export the data ####

output_path <- 'data/figures_for_paper/'
if(!dir.exists(output_path)) {dir.create(paste0(output_path), recursive = T)}

save('df_LSTb_LAIb_corr', file = paste0(output_path, 'data_for_corr_summary_maps', '.RData') ) 


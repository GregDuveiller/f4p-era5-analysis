#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_data_for_bias_summaries.R ####
# ---------------------------------------------------------------------------- #
# Purpose: prepare the data to generate maps summarizing the  biases
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #



#### Initialization ####

library(dplyr)
require(tidyr)

rm(list = ls()) 


#### Set-up of parameters ####

# chose variable names. LAI should be first and comparison variable second
varname_1 <- 'LAI' # 'albedo_wsa_vis' # 'E' # 'SM'  # 'LST' #'albedo_wsa_nir' albedo_wsa_vis albedo_bsa_nir albedo_bsa_vis #LAI
varname_2 <- 'LST'
base_dir <- 'data/inter_data/df_single_var_summaries/'


#### Load and prepare the data ####

# load the agreement metrics from prepare___variables.R
dpath <- base_dir
load( paste0(dpath, 'df_single_var_agr__sp_agr_overall__', varname_1,'.RData'))  ; sp_agr_overall_var1 <- sp_agr_overall       # temporal agreement metrics for each pixel across all months/years
load( paste0(dpath, 'df_single_var_agr__sp_agr_monthS__', varname_1,'.RData'))   ; sp_agr_monthS_var1  <- sp_agr_monthS         # temporal agreement metrics for each month (i.e. temporal agreement of a given month across years)
load( paste0(dpath, 'df_single_var_agr__temp_agr_gen__', varname_1,'.RData'))    ; temp_agr_gen_var1   <- temp_agr_gen          # spatial agreeement at a given moneth (for each climatezone)
# load( paste0(dpath, 'df_single_var_agr__agr__', varname_1,'.RData'))             ; agr_var1            <- agr                 # overall agreement metrics, all pixels, all times
# load( paste0(dpath, 'df_single_var_agr__freq__', varname_1,'.RData'))            ; freq_var1           <- freq                # binned histogram of frequency of values (all space and time)

load( paste0(dpath, 'df_single_var_agr__sp_agr_overall__', varname_2,'.RData'))  ; sp_agr_overall_var2 <- sp_agr_overall
load( paste0(dpath, 'df_single_var_agr__sp_agr_monthS__', varname_2,'.RData'))   ; sp_agr_monthS_var2  <- sp_agr_monthS
load( paste0(dpath, 'df_single_var_agr__temp_agr_gen__', varname_2,'.RData'))    ; temp_agr_gen_var2   <- temp_agr_gen
# load( paste0(dpath, 'df_single_var_agr__agr__', varname_2,'.RData'))             ; agr_var2            <- agr
# load( paste0(dpath, 'df_single_var_agr__freq__', varname_2,'.RData'))            ; freq_var2           <- freq
# 'agr', 'freq', 'sp_agr', 'temp_agr_gen', 'temp_agr_det'

# set start and end of timeseries data
start_date <- '2003-01-15' ; end_date <- '2018-12-31'
temp_agr_gen_var1 <- temp_agr_gen_var1 %>% filter(cz_major_zone != 'O')
temp_agr_gen_var1 <- temp_agr_gen_var1 %>% filter(time > as.Date(x = start_date ) & time < as.Date(x = end_date ) )
temp_agr_gen_var2 <- temp_agr_gen_var2 %>% filter(cz_major_zone != 'O')
temp_agr_gen_var2 <- temp_agr_gen_var2 %>% filter(time > as.Date(x = start_date ) & time < as.Date(x = end_date ) )
# colnames(agr_var1) <- paste0("agre$", colnames(agr_var1))
# colnames(agr_var2) <- paste0("agre$", colnames(agr_var2))



#### Export the data ####

output_path <- 'data/figures_for_paper/'
if(!dir.exists(output_path)) {dir.create(paste0(output_path), recursive = T)}

save(list = ls(), file = paste0(output_path, 'data_for_summary_maps', '.RData') ) 


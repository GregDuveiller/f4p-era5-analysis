#!/usr/local/bin/Rscript
################################################################################
# Purpose: harvest info to make diagnostic plot of agreement of single variables
# Project: f4p-era5-analysis
# Authors: G.Duveiller
################################################################################


library(dplyr)
library(tidyr)
library(here)

# get function for index of agreement...
source('../../tools/agreement-index/calculate-agr-metrics.R')

dir.create(path = 'data/inter_data/df_single_var_agreement', recursive = T, showWarnings = F)



# agreement in time (per climzone)
load('data/inter_data/ancillary_info/df_KG_climatezones.RData')  # <---- df_cz
df_cz <- df_cz %>% mutate(cz_major_zone = substr(cz_name, 1, 1)) %>%
  select(-cz_ID, -cz_colours)




# varname = 'LAI'
varname_list <- c('SM', 'LAI', 'LST', 'albedo_wsa_nir', 'albedo_wsa_vis', 'albedo_bsa_nir', 'albedo_bsa_vis')
varname_list <- c('LAI', 'SM', 'LST', 'albedo_wsa_vis')
varname_list <- c('albedo_wsa_nir', 'albedo_wsa_vis', 'albedo_bsa_nir', 'albedo_bsa_vis')


# Flag to remove polar areas and sea ice (useful for albedo with MODIS)
rm_polar_and_sea <- T


for( varname in varname_list){

print(paste0('|> working on ', varname, '...'))

load(paste0('data/inter_data/df_comb_obs_vs_sim/df_comb___', varname,'.RData'))  # <--- df_comb



# should possibly be applied before/outside this script
if(rm_polar_and_sea == T){
  df_comb <- df_comb %>% 
    right_join(y = df_cz %>% filter(cz_major_zone %in% LETTERS[1:4]), 
               by = c('x', 'y')) %>%
    select(-cz_name, -cz_major_zone)
  varname <- paste0(varname,'_cleaner')  # <-- could name it more approapriately
}

df_comb <- df_comb %>% filter(!is.na(obs)) 




df_comb <- df_comb %>% 
  mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
  mutate(monthS = ifelse(monthS == 0, 12, monthS))  %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-'))) %>%
  left_join(df_cz, by = c('x', 'y'))

#LAI_obs_mu LAI_obs_sd LAI_rea_mu LAI_rea_sd  LAI_dif_mu LAI_dif_sd



temp_agr_det <- df_comb %>%
  group_by(cz_name, time) %>%
  summarise(N = sum(!is.na(obs)),
            agre = get.Agr.Metrics(obs, sim),
            obs_mu = mean(obs, na.rm = T),
            obs_sd = sd(obs, na.rm = T),
            sim_mu = mean(sim, na.rm = T),
            sim_sd = sd(sim, na.rm = T),
            dif_mu = mean(sim - obs, na.rm = T),
            dif_sd = sd(sim - obs, na.rm = T))

temp_agr_gen <- df_comb %>%
  group_by(cz_major_zone, time) %>%
  summarise(N = sum(!is.na(obs)),
            agre = get.Agr.Metrics(obs, sim),
            obs_mu = mean(obs, na.rm = T),
            obs_sd = sd(obs, na.rm = T),
            sim_mu = mean(sim, na.rm = T),
            sim_sd = sd(sim, na.rm = T),
            dif_mu = mean(sim - obs, na.rm = T),
            dif_sd = sd(sim - obs, na.rm = T)) 

save('agr', 'freq', 'sp_agr', 'temp_agr_gen', 'temp_agr_det',
     file = paste0('data/inter_data/df_single_var_agreement/df_single_var_agr_',varname,'.RData'))

print(paste0('<| all done for ', varname, '!'))
}

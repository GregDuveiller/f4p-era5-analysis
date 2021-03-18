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

# varname = 'LAI'
varname_list <- c('SM', 'LAI', 'LST', 'albedo_wsa_nir', 'albedo_wsa_vis', 'albedo_bsa_nir', 'albedo_bsa_vis')
varname_list <- c('LAI', 'SM', 'LST', 'albedo_wsa_vis')


for( varname in varname_list){

print(paste0('|> working on ', varname, '...'))

load(paste0('data/inter_data/df_comb_obs_vs_sim/df_comb___', varname,'.RData'))  # <--- df_comb

df_comb <- df_comb %>% filter(!is.na(obs)) 
# get overall agreement
agr <- get.Agr.Metrics(df_comb$obs, df_comb$sim)

nbins <- 100

min_val <- min(round(min(df_comb$obs), digits = 12), round(min(df_comb$sim), digits = 12))
max_val <- max(round(max(df_comb$obs), digits = 12), round(max(df_comb$sim), digits = 12))

bins <- seq(min_val, max_val, length = nbins)
centroids <- bins[1:(nbins-1)] + (diff(bins)/2)

freq <-  as.data.frame(table(
  findInterval(df_comb$obs, bins, rightmost.closed = T, all.inside = T), 
  findInterval(df_comb$sim, bins, rightmost.closed = T, all.inside = T)))

freq[,4] <- centroids[freq[,1]]
freq[,5] <- centroids[freq[,2]]

colnames(freq) <- c('obs_bin', 'sim_bin', 'freq', 'obs_val', 'sim_val')





# # agreement in space (per pixel)

sp_agr <- df_comb %>%
  group_by(y, x) %>%
  summarise(agre = get.Agr.Metrics(obs, sim),
            rmsd = mean(sqrt((obs - sim)^2)),
            bias = mean(obs - sim))





# agreement in time (per climzone)
load('data/inter_data/ancillary_info/df_KG_climatezones.RData')  # <---- df_cz
df_cz <- df_cz %>% mutate(cz_major_zone = substr(cz_name, 1, 1)) %>%
  select(-cz_ID, -cz_colours)

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

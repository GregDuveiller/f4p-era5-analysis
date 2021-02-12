# script regrouping dataframes of similar variabels together
# could be "generalized", but could also stay by variables in case there are too
# many specific cases...

# init ----

library(ggplot2)
library(dplyr)
library(tidyr)

# set output path
out.path <- 'results/vardf4comparison'

# get some general ancillary info
load('results/ancillary_info/df_KG_climatezones.RData') #  'df_cz' and 'df_lgd'


# processing ----

## LAI ----

LAI_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'theia_025', full.names = T)

load('data/r_data_frames/df_LAI_ERA5sl_025_2010.RData', verbose = T)

df_LAIra <- df %>% 
  mutate(LAI = cvh * lai_hv + cvl * lai_lv) %>%
  select(x, y, month, LAI) %>%
  rename(LAI_reanalysis = LAI) %>%
  # need to rotate cold months in Southern Hemisphere
  mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  

df_LAI_comb <- data.frame()

for(ifile in LAI_files){
  
  load(ifile); df_LAIrs <- df
  
  df_LAI <- df_LAIrs %>% 
    #filter(`LAI-RMSE` < 0.5, `LAI-NOBS` > 5) %>%
    select(x, y, year, month, LAI) %>%
    rename(LAI_observations = LAI) %>%
    filter(LAI_observations >= 1) %>%  # to filter out effects of low veg
    # need to rotate cold months in Southern Hemisphere
    mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
    mutate(monthS = ifelse(monthS == 0, 12, monthS)) %>% 
    left_join(df_LAIra, by = c("x", "y", "month", "monthS")) %>% 
    mutate(LAI_difference = LAI_reanalysis - LAI_observations) %>%
    left_join(df_cz,  by = c("x", "y")) %>%
    group_by(year, monthS, cz_name) %>%
    summarise(LAI_obs_mu = mean(LAI_observations, na.rm = T),
              LAI_obs_sd = sd(LAI_observations, na.rm = T),
              LAI_rea_mu = mean(LAI_reanalysis, na.rm = T),
              LAI_rea_sd = sd(LAI_reanalysis, na.rm = T),
              LAI_dif_mu = mean(LAI_difference, na.rm = T),
              LAI_dif_sd = sd(LAI_difference, na.rm = T))
  
  df_LAI_comb <- bind_rows(df_LAI_comb, df_LAI) 
}

df_LAI_comb <- df_LAI_comb %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-')))

print("LAI: finished")

## LST ----

LST_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'LST_GEE', full.names = T) #"df_i_025"

SKT_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'SKT_ERA5l', full.names = T)

df_LST_comb <- data.frame()

for(ifile in LST_files){
  
  load(ifile); df_LSTrs <- df_i_025
  
  load(gsub(ifile, pattern = 'LST_GEE', replacement = 'SKT_ERA5l'))
  df_SKTra <- df
  
  df_LST <- df_LSTrs %>% 
    #filter(`LST_count` > 5) %>%
    select(x, y, year, month, LST_max5_mean) %>%
    # need to rotate cold months in Southern Hemisphere
    mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
    mutate(monthS = ifelse(monthS == 0, 12, monthS)) %>% 
    left_join(df_SKTra %>%
                # need to rotate cold months in Southern Hemisphere
                mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month),
                       monthS = ifelse(monthS == 0, 12, monthS)), 
              by = c("x", "y", "month", "monthS", "year")) %>% 
    mutate(LST_difference = skt_top5avg - LST_max5_mean) %>%
    left_join(df_cz,  by = c("x", "y")) %>%
    group_by(year, monthS, cz_name) %>%
    summarise(LST_obs_mu = mean(LST_max5_mean, na.rm = T),
              LST_obs_sd = sd(LST_max5_mean, na.rm = T),
              LST_rea_mu = mean(skt_top5avg, na.rm = T),
              LST_rea_sd = sd(skt_top5avg, na.rm = T),
              LST_dif_mu = mean(LST_difference, na.rm = T),
              LST_dif_sd = sd(LST_difference, na.rm = T))
  
  df_LST_comb <- bind_rows(df_LST_comb, df_LST) 
}

df_LST_comb <- df_LST_comb %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-')))

print("LST: finished")

## ET ----

SAT_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'GLEAM', full.names = T) #"df_i_025"

ERA_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'ET_ERA5l', full.names = T)

df_ET_comb <- data.frame()

for(ifile in SAT_files){
  
  load(ifile); df_SAT <- df
  
  load(gsub(ifile, pattern = 'GLEAM', replacement = 'ERA5l')); df_ERA <- df
  
  df_ET <- df_SAT %>% 
    select(x, y, year, month, Et) %>%
    # need to rotate cold months in Southern Hemisphere
    mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
    mutate(monthS = ifelse(monthS == 0, 12, monthS)) %>% 
    left_join(df_ERA %>%
                # need to rotate cold months in Southern Hemisphere
                mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month),
                       monthS = ifelse(monthS == 0, 12, monthS)), 
              by = c("x", "y", "month", "monthS", "year")) %>% 
    mutate(ET_difference = e - Et) %>%
    left_join(df_cz,  by = c("x", "y")) %>%
    group_by(year, monthS, cz_name) %>%
    summarise(ET_obs_mu = mean(Et, na.rm = T),
              ET_obs_sd = sd(Et, na.rm = T),
              ET_rea_mu = mean(e, na.rm = T),
              ET_rea_sd = sd(e, na.rm = T),
              ET_dif_mu = mean(ET_difference, na.rm = T),
              ET_dif_sd = sd(ET_difference, na.rm = T))
  
  df_ET_comb <- bind_rows(df_ET_comb, df_ET) 
}

df_ET_comb <- df_ET_comb %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-')))
# NOTE: This time is indicative, and based on the Northern Hemisphere

print("ET: finished")


## SM ----

SAT_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'SM_sat', full.names = T) #"df_i_025"

ERA_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'SM_ERA5l', full.names = T)


df_SM_comb <- data.frame()

for(ifile in SAT_files){
  
  load(ifile); df_SMrs <- df
  
  load(gsub(ifile, pattern = 'SM_sat', replacement = 'SM_ERA5l')); df_SMra <- df
  
  df_SM <- df_SMrs %>% 
    #filter(`nobs` > 5) %>%
    select(x, y, year, month, sm) %>%
    # need to rotate cold months in Southern Hemisphere
    mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
    mutate(monthS = ifelse(monthS == 0, 12, monthS)) %>% 
    left_join(df_SMra %>%
                # need to rotate cold months in Southern Hemisphere
                mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month),
                       monthS = ifelse(monthS == 0, 12, monthS)), 
              by = c("x", "y", "month", "monthS", "year")) %>% 
    mutate(SM_difference = swvl1 - sm) %>%
    left_join(df_cz,  by = c("x", "y")) %>%
    group_by(year, monthS, cz_name) %>%
    summarise(SM_obs_mu = mean(sm, na.rm = T),
              SM_obs_sd = sd(sm, na.rm = T),
              SM_rea_mu = mean(swvl1, na.rm = T),
              SM_rea_sd = sd(swvl1, na.rm = T),
              SM_dif_mu = mean(SM_difference, na.rm = T),
              SM_dif_sd = sd(SM_difference, na.rm = T))
  
  df_SM_comb <- bind_rows(df_SM_comb, df_SM) 
}

df_SM_comb <- df_SM_comb %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-')))
# NOTE: This time is indicative, and based on the Northern Hemisphere

print("SM: finished")


# export ----
dir.create(out.path, recursive = T, showWarnings = F)
save('df_LAI_comb', file = paste0(out.path, '/df_LAI_comb.Rdata'))
save('df_LST_comb', file = paste0(out.path, '/df_LST_comb.Rdata'))
save('df_ET_comb', file = paste0(out.path, '/df_ET_comb.Rdata'))
save('df_SM_comb', file = paste0(out.path, '/df_SM_comb.Rdata'))


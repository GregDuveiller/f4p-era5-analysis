#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_time_series_4_hw.R ####
# ---------------------------------------------------------------------------- #
# Purpose: prepare the data to generate timeseries of heatwaves and effect on biases
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #

library(dplyr)
library(tidyr)
library(sf)

load('data/final_data/figures_for_paper/hw_gislayers.RData')   # <---- hw_polygons


# input files are the 'df_comb___var.RData' produced by Greg's script
input_dir <- 'data/inter_data/df_comb_obs_vs_sim/'


output_path <- 'data/final_data/figures_for_paper/'
print(paste0('output_path is : ', output_path ))
if(!dir.exists(output_path)) {dir.create(paste0(output_path), recursive = T)}


df_hw <- data.frame(hwname = c("HW03", "HW10", "HW18a", "HW18b"),
                    hwyear = c(2003, 2010, 2018, 2018),
                    hwmonth = c(8, 7, 7, 7))


get_ts_4_hw <- function(varname){
  
  varname_short <- ifelse( grepl('albedo', varname, fixed=TRUE), 'Albedo', varname )
  load(paste0(input_dir, 'df_comb___', varname, '.RData'))  # <--- df_comb
  
  get_hw_spatial_average <- function(hwname, df_comb){
    
    hw_bbox <- hw_polygons %>% filter(hwname == !!hwname) %>% st_bbox()
    
    df_subsetHW <- df_comb %>%
      filter(x >= hw_bbox$xmin & x <= hw_bbox$xmax) %>%
      filter(y >= hw_bbox$ymin & y <= hw_bbox$ymax)
    
    df_subset_ts <- df_subsetHW  %>%
      group_by(year, month) %>%
      summarise(obs = mean(obs, na.rm = T), sim = mean(sim, na.rm = T)) %>%
      mutate(time = as.Date(paste(year, month, '15', sep = '-'), '%Y-%m-%d'),
             hwname = hwname)
    
    return(df_subset_ts)
  }
  
  df_subset_ts <- bind_rows(
    get_hw_spatial_average(hwname = 'HW03', df_comb),
    get_hw_spatial_average(hwname = 'HW10', df_comb),
    get_hw_spatial_average(hwname = 'HW18a', df_comb),
    get_hw_spatial_average(hwname = 'HW18b', df_comb)
  )
  
  rm(df_comb)
  
  start_year <- 2003; end_year <- 2018
  
  hw_clim_ts <- df_subset_ts %>% 
    filter(year >= start_year & year <= end_year ) %>%
    group_by(month, hwname) %>%
    summarise(obs = mean(obs, na.rm = T),
              sim = mean(sim, na.rm = T)) %>%
    pivot_longer(cols = c('obs', 'sim'), 
                 names_to = 'source', values_to = paste0(varname_short,'_clim'))
  
  
  ts_all <- df_subset_ts %>% 
    inner_join(df_hw, by = c('hwname' = 'hwname', 'year' = 'hwyear')) %>%
    pivot_longer(cols = c('obs', 'sim'), 
                 names_to = 'source', values_to = paste0(varname_short, '_year')) %>% 
    left_join(hw_clim_ts, by = c('month', 'hwname', 'source')) %>%
    pivot_longer(cols = c(paste0(varname_short,'_year'), paste0(varname_short,'_clim')),
                 names_to = 'type', values_to = varname_short)
  
  return(ts_all)}


ts_LAI <- get_ts_4_hw(varname = 'LAI')
ts_LST <- get_ts_4_hw(varname = 'LST')
# ts_Albedo <- get_ts_4_hw(varname = 'Albedo')
ts_Albedo <- get_ts_4_hw(varname = 'albedo_wsa_vis')
ts_E <- get_ts_4_hw(varname = 'E')







#### Export the data ####

save('ts_LAI', 'ts_LST', 'ts_Albedo', 'ts_E',
     file = 'data/final_data/figures_for_paper/hw_ts.RData')



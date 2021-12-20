#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_heatwave_summary_stats.R ####
# ---------------------------------------------------------------------------- #
# Purpose: prepare the data to generate summaries of heatwaves
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #



#### Initialization ####

library(dplyr)
require(tidyr)
require(sf)




#### Load necessary data ####

# load shapefiles of heatwaves
load('data/final_data/figures_for_paper/hw_gislayers.RData')   # <---- hw_polygons 
load('data/final_data/figures_for_paper/hw_maps.RData')   # <---- df_all 




#### Summarise spatially per heatwave ####


get_HW_spvals <- function(hwname){
  hw_bbox <- hw_polygons %>% filter(hwname == !!hwname) %>% st_bbox()
  
  if(hwname %in% c('HW18a', 'HW18b')){hwyear = 2018} 
  if(hwname == 'HW10'){hwyear = 2010}
  if(hwname == 'HW03'){hwyear = 2003}
  
  df_barsHW <- df_all %>%
    filter(x >= hw_bbox$xmin & x <= hw_bbox$xmax) %>%
    filter(y >= hw_bbox$ymin & y <= hw_bbox$ymax) %>%
    filter(hwyear == !!hwyear) %>%
    group_by(variable) %>%
    summarise(obs_hw_mu = mean(obs, na.rm = T),
              sim_hw_mu = mean(sim, na.rm = T),
              obs_cl_mu = mean(obs_mean, na.rm = T),
              sim_cl_mu = mean(sim_mean, na.rm = T),
              bias_hw_mu = mean(diff_simSobs, na.rm = T), 
              bias_cl_mu = mean(diff_simSobs_mean, na.rm = T),
              bias_shift_mu = mean(diff_simSobsSmean, na.rm = T)) %>%
    mutate(hwname = hwname,
           hwyear = hwyear)
  
  return(df_barsHW)
}

df_barsHW_all <- bind_rows(
  get_HW_spvals('HW03'),
  get_HW_spvals('HW10'), 
  get_HW_spvals('HW18a'), 
  get_HW_spvals('HW18b')) %>%
  pivot_longer(!variable & !hwyear & !hwname, names_to = 'metric')



#### Export the data ####

output_path <- 'data/final_data/figures_for_paper/'
if(!dir.exists(output_path)) {dir.create(paste0(output_path), recursive = T)}

save('df_barsHW_all', file = paste0(output_path, 'hw_stats', '.RData') ) 

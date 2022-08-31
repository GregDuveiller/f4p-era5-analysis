#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_data_per_clim_bin.R ####
# ---------------------------------------------------------------------------- #
# Purpose: process data to make figures of hysteresis plots
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #

require(dplyr)
require(tidyr)



#### Prepare the climate bins ####

# load KG climzones (only needed for some spatial filtering)
load('data/inter_data/ancillary_info/df_KG_climatezones.RData')  # <---- df_cz
df_cz <- df_cz %>% dplyr::select(-cz_ID, -cz_colours)


load('data/input_data/climate_zones/df_climspace_t2xsm.RData') # <---- df_climspace
#df_climspace <- df_climspace %>% select(x, y, clim.x, clim.y) %>% rename(clim.t2 = clim.x, clim.sm = clim.y)

x.bin.width <- 4
y.bin.width <- 0.04

# make bin labels
x.bin.numlbls <- seq(floor(min(df_climspace$clim.t2)),
                     ceiling(max(df_climspace$clim.t2)) + x.bin.width, 
                     x.bin.width)
y.bin.numlbls <- seq(floor(min(df_climspace$clim.sm)), 
                     ceiling((max(df_climspace$clim.sm) + y.bin.width) * 100)/100,
                     y.bin.width)

# make bin breaks
x.bin.breaks <- c(x.bin.numlbls - (x.bin.width/2), Inf)
y.bin.breaks <- c(y.bin.numlbls - (y.bin.width/2), Inf)

df_climspace_bin <- df_climspace %>%
  mutate(t2.clim.bin = cut(clim.t2,
                           breaks = x.bin.breaks,
                           labels = x.bin.numlbls)) %>%
  mutate(sm.clim.bin = cut(clim.sm,
                           breaks = y.bin.breaks,
                           labels = y.bin.numlbls))


#### Get the data averaged by bins ####

# Flag to remove polar areas and sea ice (useful for albedo with MODIS)
rm_polar_and_sea <- F

get.data.per.bin <- function(varname, rm_polar_and_sea = F){
  
  load(paste0('data/inter_data/df_comb_obs_vs_sim/df_comb___', varname,'.RData'))  # <--- df_comb
  
  # should possibly be applied before/outside this script
  if(rm_polar_and_sea == T){
    
    df_comb <- df_comb %>% 
      right_join(y = df_cz %>% filter(cz_major_zone %in% LETTERS[1:4]), 
                 by = c('x', 'y')) %>%
      dplyr::select(-cz_name, -cz_major_zone)
    varname <- paste0(varname,'_cleaner')  # <-- could name it more approapriately
  }
  
  # filter out no data
  df_comb <- df_comb %>% filter(!is.na(obs)) 
  
  
  df_comb <- df_comb %>% 
    mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
    mutate(monthS = ifelse(monthS == 0, 12, monthS))  %>% 
    mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-'))) %>%
    left_join(df_climspace_bin, by = c('x', 'y'))
  
  
  agr_per_clim_bin <- df_comb  %>%
    group_by(t2.clim.bin, sm.clim.bin, time) %>%
    summarise(N = sum(!is.na(obs)),
              obs_mu = mean(obs, na.rm = T),
              obs_sd = sd(obs, na.rm = T),
              sim_mu = mean(sim, na.rm = T),
              sim_sd = sd(sim, na.rm = T),
              dif_mu = mean(sim - obs, na.rm = T),
              dif_sd = sd(sim - obs, na.rm = T))
  
  
  return(agr_per_clim_bin)
}

df_LAI_comb <- get.data.per.bin(varname = 'LAI', rm_polar_and_sea = F)
df_LST_comb <- get.data.per.bin(varname = 'LST', rm_polar_and_sea = F)


#### Save the data ####

output_path <- 'data/inter_data/df_per_clim_bin'
dir.create(path = output_path, recursive = T, showWarnings = F)

save('df_climspace_bin', file = 'data/final_data/figures_for_paper/df_climspace_bin.RData')
save('df_LAI_comb', file = paste0(output_path, '/df_LAI_per_clim_bin.RData'))
save('df_LST_comb', file = paste0(output_path, '/df_LST_per_clim_bin.RData'))


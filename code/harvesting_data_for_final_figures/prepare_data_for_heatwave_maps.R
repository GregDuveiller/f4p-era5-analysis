#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_data_for_heatwave_maps.R ####
# ---------------------------------------------------------------------------- #
# Purpose: prepare the data to generate maps of heatwaves and effect on biases
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #



#### Initialization ####

library(dplyr)
require(tidyr)



#### Set-up of parameters ####

# input files are the 'df_comb___var.RData' produced by Greg's script
input_dir <- 'data/inter_data/df_comb_obs_vs_sim/'

# list of variables to run over
var_list <- c('LAI', 'LST',  'E', 'Albedo')  # "Albedo" is "albedo_wsa_vis_MCD43C3"

# here select the year, month, spatial extent of heatwave in question.
# select also time period to compare it to (e.g. 2003-2018).
start_year <- 2003; end_year <- 2018  # select the range of years to average over - all years available
v_lon_min <- -12; v_lon_max <- 58; v_lat_min <- 36 ;v_lat_max <- 71 # limits to contain all 3 hws

# create a df key containing the name, year and month of each heatwave at maximum
df_hw <- data.frame(row.names = c("hw2003", "hw2010", "hw2018"),
                    year = c(2003, 2010, 2018),
                    month = c(8, 7, 7))
# Russian heatwave https://earthobservatory.nasa.gov/images/45069/heatwave-in-russia  
# https://en.wikipedia.org/wiki/2018_European_heat_wave - no specific month, generally May-July.
# But July is hottest and prob most out of kilter with LAI map (through browning)

# load shapefiles of heatwaves
load('data/final_data/figures_for_paper/hwAll_gislayers.RData')   # <---- hw_polygons 



#### Collect and calc all spatial data ####

# create a single dataframe of all heatwaves and all variables binded 
df_all <- data.frame()

for( i in 1:dim(df_hw)[1] ){
  case_study <- row.names(df_hw)[i]
  v_year <- df_hw[case_study, 'year'] ; v_month <- df_hw[case_study, 'month']
  print( paste0(case_study, ' : ', v_year, ', ', v_month ) )
  
  
  
  # Script overview:
  # For each variable, load full dataset and reduce to the area/time of interest, df
  # Get statistics of average conditions in df 
  # Get statistics of heatwave relative to average conditions in df
  
  df_vars <- data.frame()
  
  for (i in 1:length(var_list)){
    variable_i <- var_list[i] ; print(variable_i)
    input_file <- paste0('df_comb___',variable_i,'.RData')
    load(paste0(input_dir, input_file))
    
    df <- df_comb %>% filter(year >= start_year & year <= end_year & month == v_month) %>%
      filter(x >= v_lon_min & x <= v_lon_max) %>%
      filter(y >= v_lat_min & y <= v_lat_max) 
    
    rm(df_comb)
    
    #   # convert temperatures kelvin to celcius
    # if(input_file == 'df_comb___LST.RData' ){ print('convert kelvin to celcius') 
    #   df <- df %>% mutate(obs = obs - 273.15) ; df <- df %>% mutate(sim = sim - 273.15) }
    

    # separate year in question from the long term average
    df_year    <- df %>% filter(year == v_year)
    df_average <- df %>% filter(year != v_year)   #  <-------- not actually sure if we should do this
    
    # get ERA5 - obs difference
    df_year <- df_year %>% mutate(diff_simSobs = sim - obs)           # difference between ERA5 - sat
    df_average <- df_average %>% mutate(diff_simSobs = sim - obs)     # difference between ERA5 - sat
    
    # these columns are averages over all years (i.e. use when calculating the z-score etc of the year in question)
    df_average_cols <- df_average %>%
      group_by(x, y, month) %>%
      summarise(obs_mean          = mean(obs, na.rm = T)                  , sim_mean =  mean(sim, na.rm = T),                # Long term mean sat, mean sim
                obs_sd            = sd(obs, na.rm = T)                    , sim_sd   =  sd(sim, na.rm = T),                  # Long term sd sat, sd sim
                diff_simSobs_mean = mean(diff_simSobs, na.rm = T),                                                           # (ERA5-satellite) LT mean
                sd_diff_simSobs   = sd(diff_simSobs, na.rm = T) )                                                            # get the s.d. also to see normal variation in ERA5-satellite
    
    # select the relevant - long term average columns to join with the specific year data
    df_year <- df_year %>% left_join(df_average_cols)
    
    # these columns are specific to the year in question (i.e. can look at specific years to compare with the year in question)
    df_year <- df_year %>%
      # group_by(x, y, month) %>%
      mutate(diff_obsSmean       = obs - obs_mean      , diff_simSmean = sim - sim_mean ) %>%      # diff sat - Long term mean # diff ERA5 - LT mean
      mutate(z_score_obs         = diff_obsSmean/obs_sd, z_score_sim = diff_simSmean/sim_sd )  %>% # z-score of obs and sim from LT mean
      mutate(scaled_diff_simSobs = (diff_simSobs)/sd_diff_simSobs)   %>%                           # difference between (ERA5-sat) scaled by the variation in ERA5-satellite
      mutate(diff_simSobsSmean   = (diff_simSobs - diff_simSobs_mean) ) %>%                        # difference between (ERA5-sat) and (sat-ERA5 LT mean)
      mutate(z_score_simSobs     = diff_simSobsSmean/sd_diff_simSobs )                             # z-score of (ERA5-sat) in heatwave  - is this month an especially large ERA5-satellite variation
    
    df_year$variable <- variable_i
    if (0 %in% dim(df_vars)){ df_vars <- df_year }
    else{ df_vars <- rbind(df_vars, df_year ) }
    
    
  } # END LOOP OVER VARIABLES
  
  df_vars$hw <- case_study
  if (0 %in% dim(df_all)){ df_all <- df_vars }
  else{ df_all <- rbind(df_all, df_vars ) }
  df_all$variable <- factor(df_all$variable)
  df_all$hw <- factor(df_all$hw)
  
} # END LOOP OVER HEATWAVES



#### Summarise spatially per heatwave ####


get_HW_spvals <- function(hwname){
  hw_bbox <- hw_polygons %>% filter(hw == hwname) %>% st_bbox()
  
  df_barsHW <- df_all %>%
    filter(x >= hw_bbox$xmin & x <= hw_bbox$xmax) %>%
    filter(y >= hw_bbox$ymin & y <= hw_bbox$ymax) %>%
    filter(hw == hwname) %>%
    group_by(variable) %>%
    summarise(obs_hw_mu = mean(obs, na.rm = T),
              sim_hw_mu = mean(sim, na.rm = T),
              obs_cl_mu = mean(obs_mean, na.rm = T),
              sim_cl_mu = mean(sim_mean, na.rm = T),
              bias_hw_mu = mean(diff_simSobs, na.rm = T), 
              bias_cl_mu = mean(diff_simSobs_mean, na.rm = T),
              bias_shift_mu = mean(diff_simSobsSmean, na.rm = T)) %>%
    mutate(hw = hwname)
}

df_barsHW_all <- bind_rows(
  get_HW_spvals('hw2003'),
  get_HW_spvals('hw2010'), 
  get_HW_spvals('hw2018')) %>%
  pivot_longer(!variable & !hw, names_to = 'metric')




#### Export the data ####

output_path <- 'data/final_data/figures_for_paper/'
if(!dir.exists(output_path)) {dir.create(paste0(output_path), recursive = T)}

save('df_all', 'df_barsHW_all', file = paste0(output_path, 'hwAll_varAll_stats', '.RData') ) 


# original name : prepare_caseStudies.R
#
# ########################################################
# Title         : 
# Description   : This code aims to produce dataframes to plot a) values of the variables b) z-score for the variables c) raw value above long term mean etc
#                 For different input months and x,y coordinate bounds. 
#                 Select particular name/month inputs for the heatwave in question. The same bounding box is selected for each.
#                  to produce the data for a particular heatwave, select, e.g. hw2003, and comment out the others
#                 
# Inputs	      : Greg's df_comb harvested plotting scripts.
#                 
# Outputs	      : dataframe - see path
#                 
#                 
# Options	      : 
# Date          : 07/11/21
# Version       : 1.3 - this is updated for a new dataframe and to form a better plotting framework and to be useable in GIT
# Licence       : N/A
# Authors       : Mark Pickering
# Maintainer    : Mark Pickering
# Notes		      : 
#                 
#                 
# Example use   : 
#                 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################
rm(list = ls())
start_time <- Sys.time() ; print(start_time)
# find date:
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

######     GLOBAL VARS  TO CHECK AND CHANGE   #####

# input files are the 'df_comb___var.RData' produced by Greg's script
input_dir <- 'data/inter_data/df_comb_obs_vs_sim/'

# list of variables to run over
var_list <- c('LAI', 'LST',  'E', 'albedo_wsa_vis') # albedo_bsa_nir albedo_bsa_vis albedo_wsa_nir 'SM',

######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%
require(tidyr)

###################################################
######       I/O                                ###
###################################################

output_path <- 'data/figures_for_paper/'
print(paste0('output_path is : ', output_path ))
if(!dir.exists(output_path)) {dir.create(paste0(output_path), recursive = T)}

###################################################
######     CASE STUDY parameters              #####
###################################################
# here select the year, month, spatial extent of heatwave in question.
# select also time period to compare it to (e.g. 2003-2018).
# just comment out (or uncomment) code relating 

# Paper heatwave selection used in for 2003, 2010, 2018 European heatwaves only - covers all areas
# v_lon_max 53 takes us to caspian, 58 to Aral
start_year <- 2003 ; end_year <- 2018 # select the range of years to average over - all years available
v_lon_min <- -12 ; v_lon_max <- 58  ; v_lat_min <- 36 ; v_lat_max <- 71 # limits to contain all 3 hws
# v_lon_min <- -12 ; v_lon_max <- 24  ; v_lat_min <- 36 ; v_lat_max <- 60  # previously used extent

# create a df key containing the name, year and month of each heatwave at maximum
# # Russian heatwave https://earthobservatory.nasa.gov/images/45069/heatwave-in-russia  
# https://en.wikipedia.org/wiki/2018_European_heat_wave - no specific month, generally May-July. But July is hottest and prob most out of kilter with LAI map (through browning)
df_hw <- data.frame(row.names = c("hw2003", "hw2010", "hw2018"),
                    year = c(2003, 2010, 2018),
                    month = c(8, 7, 7))

# loop over the case studies
# create a single dataframe of all heatwaves and all variables binded 
df_all <- data.frame()

for( i in 1:dim(df_hw)[1] ){
  case_study <- row.names(df_hw)[i]
  v_year <- df_hw[case_study, 'year'] ; v_month <- df_hw[case_study, 'month']
  print( paste0(case_study, ' : ', v_year, ', ', v_month ) )



###################################################
######     RUN SCRIPT OVER VARIABLES          #####

# Script overview:
# For each variable, load full dataset and reduce to the area/time of interest, df
# Get statistics of average conditions in df 
# Get statistics of heatwave relative to average conditions in df

df_vars <- data.frame()
  
for (i in 1:length(var_list)){
  variable_i <- var_list[i] ; print(variable_i)
  input_file <- paste0('df_comb___',variable_i,'.RData')
  load(paste0(input_dir, input_file))
  
  ###################################################
  ######     APPLY DF CLEANING REDUCTIONS       #####

  if( grepl('albedo', input_file)  && case_study == 'hw2018'){ print('No 2018 albedo; skip ') ; next}
  df <- df_comb %>% filter(year >= start_year & year <= end_year & month == v_month) %>%
    filter(x >= v_lon_min  & x <= v_lon_max) %>%
    filter( y >= v_lat_min & y <= v_lat_max ) 
  rm(df_comb)
  # convert temperatures kelvin to celcius
  if(input_file == 'df_comb___LST.RData' ){ print('convert kelvin to celcius') 
    df <- df %>% mutate(obs = obs - 273.15) ; df <- df %>% mutate(sim = sim - 273.15) }
  
  
  ###################################################
  ######     APPLY DF CALCULATIONS E.G. ZSCORE  #####
  
  # separate year in question from the long term average
  df_year    <- df %>% filter(year == v_year)
  df_average <- df %>% filter(year != v_year)
  
  # get satellite - ERA5 difference
  df_year <- df_year %>% mutate(diff_simSobs = sim - obs)     # difference between sat - ERA5
  df_average <- df_average %>% mutate(diff_simSobs = sim - obs)     # difference between sat - ERA5
  
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
    mutate(diff_obsSmean       = obs - obs_mean                  , diff_simSmean = sim - sim_mean ) %>%      # diff sat - Long term mean # diff ERA5 - LT mean
    mutate(z_score_obs         = diff_obsSmean/obs_sd            , z_score_sim = diff_simSmean/sim_sd )  %>% # z-score of obs and sim from LT mean
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

print(paste0('saving: ', output_path) )
save(df_all, file = paste0(output_path, 'hwAll_varAll_stats', '.RData') ) 

###################################################
######     FINALISE                           #####
###################################################
# timing
end_time <- Sys.time()
print(end_time - start_time)



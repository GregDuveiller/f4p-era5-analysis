# original name : prepare_biasWtemp.R
#
# ########################################################
# Title         : 
# Description   : This code aims to produce dataframes for producing temperature bias with temperature anomaly figures
#                 
# Inputs	      : df_comb___LAI.RData etc produced by Greg initially
#                 
# Outputs	      : figures
#                 /media/mark/HD/Mark/Mark_COPERNICUS/figures/COPERNICUSII_V3/
#                 
# Options	      : 
# Date          : 07/12/21
# Version       : 1.3 - this is updated for a new dataframe and to form a better plotting framework
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
start_time <- Sys.time()
print(start_time)
# find date:
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]


######     GLOBAL VARS                        #####
start_year <- 2003 ; end_year <- 2018   # select time period to consider
v_month <- c(8)                      # select months we want to consider in analysis
v_lon_min <- -12 ; v_lon_max <- 58  ; v_lat_min <- 36 ; v_lat_max <- 71 # reduce area of interest



######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%

# library(raster) # package for raster manipulation
# library(lattice)    # enables levelplots
# library(rasterVis)  # enables levelplots
# #library(lubridate)   # enables date manipulation

# require(ggplot2)
# require(magrittr)
require(here)
require(tidyr)

# require(scales)
# library(viridis)
# library(RColorBrewer)

# # for the maps
# library(sp)
# library(rgdal)
# library(sf)         # (Greg maps) for adding maps to ggplot

# library(maptools)
# library(rgeos)
# library(classInt)
# library(RColorBrewer)
# library(nlme)      # for lmList

# load basic functions, e.g. maps
# root_script <- '/home/mark/Documents/work_jrc/work_d1/work_SIF_V2_ancova/part_1_landcover_only/200901_v1_anova/scripts/'
root_script <- '/home/mark/ownCloud/copernicus/scripts/git_mp_fork/f4p-era5-analysis/code/'  # <---- df_cz


###################################################
######       I/O                                ###
###################################################

output_root  <- '/media/mark/HD/Mark/Mark_COPERNICUS/figures/COPERNICUSII_V3/'
output_path <- paste0(output_root, full_date, '_', 'biasWtemp','/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)}


###################################################
######     LOAD DATA - PART1 - LARGE FILES    #####
###################################################
# load LAI data - then reduce to cz of interest, and add the LST
# to consider for paper (within Europe)
v_cz_in <- c('Dfc', 'Dfb', 'Csa', 'Csb', 'Cfa', 'Cfb') # 'Dfc' 'Dfb' 'Csa' 'Csb' 'Cfa' 'Cfb'

# load clim zones 
load('/home/mark/ownCloud/copernicus/scripts/KG_class/Map_KG-Global/df_KG_climatezones.RData')  # <---- df_cz
summary(df_cz,30)
df_cz <- as.data.frame(df_cz)
df_cz <- df_cz[,c(1,2,4)]
# df_cz <- df_cz %>% mutate(cz_major_zone = substr(cz_name, 1, 1)) %>% select(-cz_ID, -cz_colours)

input_dir <- '/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3/greg_workspace/MP_workspace/'
input_file <- 'df_comb___LAI.RData'

load(paste0(input_dir, input_file))
df_comb <- left_join(df_comb, df_cz)
head(df_comb) ; dim(df_comb) ; summary(df_comb)

df_comb <- df_comb %>% 
  filter(x >= v_lon_min  & x <= v_lon_max) %>%
  filter( y >= v_lat_min & y <= v_lat_max ) 

df <- df_comb %>% mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month) ) %>%
  mutate(monthS = ifelse(monthS == 0, 12, monthS))

df <-  df %>% filter(year >= start_year & year <= end_year & monthS %in%  v_month) %>%
  filter( cz_name %in% v_cz_in)
        

rm(df_comb)

# df <- df_comb %>% filter(cz_name == cz_select)
df <- df[,c(1,2,7,3,4,8,5,6)]

input_file <- 'df_comb___LST.RData'
load(paste0(input_dir, input_file))
# convert temperatures kelvin to celcius
if(input_file == 'df_comb___LST.RData' ){ 
  print('convert kelvin to celcius')
  df_comb <- df_comb %>% mutate(obs = obs - 273.15)
  df_comb <- df_comb %>% mutate(sim = sim - 273.15)
}
df <- left_join(df, df_comb, by = c('x','y','year',"month"), suffix = c('.LAI', '.LST'))
# df <- df %>% filter(!is.na(obs.LST))

# input_file <- 'df_comb___SM.RData'
# load(paste0(input_dir, input_file))
# colnames(df_comb)[5:6] <- c('obs.SM', 'sim.SM')
# df <- left_join(df, df_comb, by = c('x','y','year',"month"))

input_file <- 'df_comb___E.RData'
load(paste0(input_dir, input_file))
colnames(df_comb)[5:6] <- c('obs.E', 'sim.E')
df <- left_join(df, df_comb, by = c('x','y','year',"month"))

# Use the MCD43C3 version as covers 2018 and we are only looking at summer in any case
input_file <- 'df_comb___albedo_wsa_vis_MCD43C3.RData'
load(paste0(input_dir, input_file))
colnames(df_comb)[5:6] <- c('obs.albedo_wsa_vis', 'sim.albedo_wsa_vis')
df <- left_join(df, df_comb, by = c('x','y','year',"month"))

# input_file <- 'df_comb___albedo_bsa_nir.RData'
# load(paste0(input_dir, input_file))
# colnames(df_comb)[5:6] <- c('obs.albedo_bsa_nir', 'sim.albedo_bsa_nir')
# df <- left_join(df, df_comb, by = c('x','y','year',"month"))
# 
# input_file <- 'df_comb___albedo_bsa_vis.RData'
# load(paste0(input_dir, input_file))
# colnames(df_comb)[5:6] <- c('obs.albedo_bsa_vis', 'sim.albedo_bsa_vis')
# df <- left_join(df, df_comb, by = c('x','y','year',"month"))
# 
# input_file <- 'df_comb___albedo_wsa_nir.RData'
# load(paste0(input_dir, input_file))
# colnames(df_comb)[5:6] <- c('obs.albedo_wsa_nir', 'sim.albedo_wsa_nir')
# df <- left_join(df, df_comb, by = c('x','y','year',"month"))


head(df)
summary(df)
rm(df_comb)

###################################################
######     PREPARE DATAFRAME                  #####
###################################################

# we want to shift southern hemisphere by 6 months to make sure we are looking at summers - done above
# we take one month/one season
# for LAI and LST we get the columns: ERA5-sat difference, ERA5-sat diff as a percentage of sat, the multiyear average, obs/sim - multiyear average, obs/sim - multiyear average as perc, 
# we make the suggested plots: 
#     x = LST - LT_avg                ; y = LST(ERA5-sat diff)              ; z = frequency                   # This shows whether LST ERA5-sat difference increases in anomalous warm periods
#     x = (LST - LT_avg) as perc      ; y = LST(ERA5-sat diff) as perc      ; z = frequency                   # This shows whether LST ERA5-sat difference increases in anomalous warm periods (standardised)
#     x = LST - LT_avg                ; y = LAI(ERA5-sat diff)              ; z = frequency                   # This shows whether LAI ERA5-sat difference increases in anomalous warm periods
#     x = (LST - LT_avg) as perc      ; y = LAI(ERA5-sat diff) as perc      ; z = frequency                   # This shows whether LAI ERA5-sat difference increases in anomalous warm periods (standardised)
#     x = LST - LT_avg                ; y = LST(ERA5-sat diff)              ; z = LAI(ERA5-sat diff)          # This shows whether LST ERA5-sat difference increases in anomalous warm periods, and whether average LAI difference is a function
#     x = (LST - LT_avg) as perc      ; y = LST(ERA5-sat diff) as perc      ; z = LAI(ERA5-sat diff) as perc  # This shows whether LST ERA5-sat difference increases in anomalous warm periods, and whether average LAI difference is a function (standardised)

# get multiyear average for each pixel per month
df_avg <- df %>% group_by(x,y, monthS) %>%
  summarise( obs_mean.LAI = mean(obs.LAI, na.rm = T),
             sim_mean.LAI = mean(sim.LAI, na.rm = T),
             obs_mean.LST = mean(obs.LST, na.rm = T),
             sim_mean.LST = mean(sim.LST, na.rm = T),
             # obs_mean.SM  = mean(obs.SM , na.rm = T),
             # sim_mean.SM  = mean(sim.SM , na.rm = T),
             obs_mean.E   = mean(obs.E  , na.rm = T),
             sim_mean.E   = mean(sim.E  , na.rm = T),
             # obs_mean.albedo_bsa_nir = mean(obs.albedo_bsa_nir, na.rm = T),
             # sim_mean.albedo_bsa_nir = mean(sim.albedo_bsa_nir, na.rm = T),
             # obs_mean.albedo_wsa_nir = mean(obs.albedo_wsa_nir, na.rm = T),
             # sim_mean.albedo_wsa_nir = mean(sim.albedo_wsa_nir, na.rm = T),
             # obs_mean.albedo_bsa_vis = mean(obs.albedo_bsa_vis, na.rm = T),
             # sim_mean.albedo_bsa_vis = mean(sim.albedo_bsa_vis, na.rm = T),
             obs_mean.albedo_wsa_vis = mean(obs.albedo_wsa_vis, na.rm = T),
             sim_mean.albedo_wsa_vis = mean(sim.albedo_wsa_vis, na.rm = T)
  )

df <- left_join(df, df_avg, by = c('x','y',"monthS"))
summary(df)

# NOTE: percentage change in 'C temperature doesn't make sense (unless K)
df <- df %>% mutate(diff_sim_obs.LAI  =  (sim.LAI - obs.LAI),                      # raw difference ERA5-sat LAI
                    diff_obs_mean.LAI =  (obs.LAI - obs_mean.LAI),                 # raw difference obs - LT mean LAI
                    diff_sim_mean.LAI =  (sim.LAI - sim_mean.LAI),                 # raw difference sim - LT mean LAI
                    
                    diff_sim_obs.LST  =  (sim.LST - obs.LST),                      # raw difference ERA5-sat LST
                    diff_obs_mean.LST =  (obs.LST - obs_mean.LST),                 # raw difference obs - LT mean LST
                    diff_sim_mean.LST =  (sim.LST - sim_mean.LST),                 # raw difference sim - LT mean LST
                    
                    # diff_sim_obs.SM  =  (sim.SM - obs.SM),                      # raw difference ERA5-sat SM
                    # diff_obs_mean.SM =  (obs.SM - obs_mean.SM),                 # raw difference obs - LT mean SM
                    # diff_sim_mean.SM =  (sim.SM - sim_mean.SM),                 # raw difference sim - LT mean SM
                    
                    diff_sim_obs.E  =  (sim.E - obs.E),                      # raw difference ERA5-sat E
                    diff_obs_mean.E =  (obs.E - obs_mean.E),                 # raw difference obs - LT mean E
                    diff_sim_mean.E =  (sim.E - sim_mean.E),                 # raw difference sim - LT mean E
                    
                    diff_sim_obs.albedo_wsa_vis  =  (sim.albedo_wsa_vis - obs.albedo_wsa_vis),                      # raw difference ERA5-sat albedo_wsa_vis
                    diff_obs_mean.albedo_wsa_vis =  (obs.albedo_wsa_vis - obs_mean.albedo_wsa_vis),                 # raw difference obs - LT mean albedo_wsa_vis
                    diff_sim_mean.albedo_wsa_vis =  (sim.albedo_wsa_vis - sim_mean.albedo_wsa_vis),                 # raw difference sim - LT mean albedo_wsa_vis
                    
                    
                    # diff_sim_obs.albedo_bsa_nir  =  (sim.albedo_bsa_nir - obs.albedo_bsa_nir),                      # raw difference ERA5-sat albedo_bsa_nir
                    # diff_obs_mean.albedo_bsa_nir =  (obs.albedo_bsa_nir - obs_mean.albedo_bsa_nir),                 # raw difference obs - LT mean albedo_bsa_nir
                    # diff_sim_mean.albedo_bsa_nir =  (sim.albedo_bsa_nir - sim_mean.albedo_bsa_nir),                 # raw difference sim - LT mean albedo_bsa_nir
                    # 
                    # diff_sim_obs.albedo_bsa_vis  =  (sim.albedo_bsa_vis - obs.albedo_bsa_vis),                      # raw difference ERA5-sat albedo_bsa_vis
                    # diff_obs_mean.albedo_bsa_vis =  (obs.albedo_bsa_vis - obs_mean.albedo_bsa_vis),                 # raw difference obs - LT mean albedo_bsa_vis
                    # diff_sim_mean.albedo_bsa_vis =  (sim.albedo_bsa_vis - sim_mean.albedo_bsa_vis),                 # raw difference sim - LT mean albedo_bsa_vis
                    # 
                    # diff_sim_obs.albedo_wsa_nir  =  (sim.albedo_wsa_nir - obs.albedo_wsa_nir),                      # raw difference ERA5-sat albedo_wsa_nir
                    # diff_obs_mean.albedo_wsa_nir =  (obs.albedo_wsa_nir - obs_mean.albedo_wsa_nir),                 # raw difference obs - LT mean albedo_wsa_nir
                    # diff_sim_mean.albedo_wsa_nir =  (sim.albedo_wsa_nir - sim_mean.albedo_wsa_nir),                 # raw difference sim - LT mean albedo_wsa_nir
                    
                    # diffP_sim_obs.LAI =  ifelse(obs.LAI <= 0, NA, (sim.LAI - obs.LAI)/obs.LAI ),              # perc difference ERA5-sat LAI
                    # diffP_sim_obs.LST =  ifelse(obs.LST <= 0, NA, (sim.LST - obs.LST)/obs.LST ),              # perc difference ERA5-sat LST
                    # diffP_obs_mean.LAI = ifelse(obs_mean.LAI ==0, NA, (obs.LAI - obs_mean.LAI)/obs_mean.LAI),   # perc difference obs - LT mean LAI
                    # diffP_sim_mean.LAI = ifelse(sim_mean.LAI ==0, NA, (sim.LAI - sim_mean.LAI)/sim_mean.LAI),   # perc difference sim - LT mean LAI
                    # diffP_obs_mean.LST = ifelse(obs_mean.LST ==0, NA, (obs.LST - obs_mean.LST)/obs_mean.LST),   # perc difference obs - LT mean LST
                    # diffP_sim_mean.LST = ifelse(sim_mean.LST ==0, NA, (sim.LST - sim_mean.LST)/sim_mean.LST)    # perc difference sim - LT mean LST
)

summary(df) ; dim(df)

save(df, file =paste0(output_path, 'df_Euro_selectedCZ_aug.RData') )


###################################################
######     DIVIDE INTO QUANTILES              #####
###################################################


# input a data frame and make a divide columns q_col_name_x and q_col_name_y, into quantiles (0:quantile_max)/quantile_div in the column, q_col_name_quantile.
# For each quantile of q_col_name_quantile, we will have the mean_x mean_y (and up/down ribbons), mean Q and xy correlation/slope/lm parameters
# can also get ribbon column which shows an 'error bound' of where the data lies with e.g. ribbon_size = +/-0.25 for a bound containing 50% of data
# here we group by quantile && climate zone
f_summarise_byQuantile <- function(df_in, v_quantiles , q_col_name_x , q_col_name_y , q_col_name_quantile, ribbon_size = 0.0 ){
  
  # if only number 1 entered for quantiles then set v_quantiles such that there is only a single quantile (i.e an average of all data is taken)
  if(v_quantiles == 1){v_quantiles <- c(0,1)}
  
  df_in <- df_in %>% filter(!is.na(!!as.symbol(q_col_name_y)))
  df_in <- df_in %>% filter(!is.na(!!as.symbol(q_col_name_x)))
  df_in <- df_in %>% filter(!is.na(!!as.symbol(q_col_name_quantile)))
  df_quantile <- df_in %>% mutate( quantile = as.integer(cut(!!as.symbol(q_col_name_quantile), quantile(!!as.symbol(q_col_name_quantile), probs= v_quantiles, na.rm = FALSE), include.lowest=TRUE)))
  #ggplot(df_quantile, aes(quantile)) + geom_histogram(fill='blue', alpha=0.5) + scale_x_continuous(limits = c(0,11), oob = scales::squish)
  
  #average the quantiles
  # get the mean value of each quantile, as well as ribbon values (if_required) - these ribbon values are the upper and lower e.g. 25% around the mean - can be treated as an uncertainty wrapping e.g. 50% of data points
  df_quantile <- df_quantile %>% group_by(quantile) %>% 
    summarise(
      mean_value_X  = median( !!as.symbol(q_col_name_x) , na.rm= T),               # mean value of x
      # ribbon_value_X_up  = quantile( !!as.symbol(q_col_name_x) , probs = (0.5+ribbon_size) , na.rm= T ), # upper quantile of ribbon_size around the mean_x
      # ribbon_value_X_down  = quantile( !!as.symbol(q_col_name_x) , probs = (0.5-ribbon_size) , na.rm= T ), # upper quantile of ribbon_size around the mean_x
      mean_value_Y  = median( !!as.symbol(q_col_name_y) , na.rm= T),
      # ribbon_value_Y_up  = quantile( !!as.symbol(q_col_name_y) , probs = (0.5+ribbon_size) , na.rm= T ), # upper quantile of ribbon_size around the mean_x
      # ribbon_value_Y_down  = quantile( !!as.symbol(q_col_name_y) , probs = (0.5-ribbon_size) , na.rm= T ), # upper quantile of ribbon_size around the mean_x
      mean_value_Q  = mean( !!as.symbol(q_col_name_quantile), na.rm= T),
      # cor_XY = cor(!!as.symbol(q_col_name_x) ,!!as.symbol(q_col_name_y) ),
      # lm_c   = lm( formula = !!as.symbol(q_col_name_y) ~ !!as.symbol(q_col_name_x) )$coefficients[1],
      # lm_m   = lm( formula = !!as.symbol(q_col_name_y) ~ !!as.symbol(q_col_name_x) )$coefficients[2]
    )
  
  return(df_quantile)
}

v_variables <- c('diff_sim_obs.LST')  # this is the list of variables we want to compare along the y_axis (e.g. LST, E, albedo)

x_variable = 'diff_sim_obs.LAI'          # This is the x-axis varaible (generally LAI ERA5-sat)
v_quant <- (0:10)/10  # this gives 10ths
variable_quantile <- 'diff_obs_mean.LST' # this is the column that we get the quantiles of (e.g. quantiles of temperature relative to long term mean)


df_all <- data.frame() # this takes the quantile values of each LAI-var bias
df_all_avg <- data.frame() # this gets the average bias across all quantiles

######
# loop over climate zones and summarise by quantile the LAI and var biases
for (cz_i in v_cz_in){ # cz_i <- v_cz_in[4]
  print(cz_i)
  df_temp <- df %>% filter(cz_name == cz_i)
  #loop over the climate variables (LST, SM etc)
  for (v_i in v_variables){ # v_i <- v_variables[1]
    print(v_i)
    df_sum <- f_summarise_byQuantile(df_temp, v_quantiles = v_quant, q_col_name_x = x_variable , q_col_name_y = v_i , q_col_name_quantile = variable_quantile, ribbon_size = 0.25 )  # get the 
    df_avg <- f_summarise_byQuantile(df_temp, v_quantiles = 1, q_col_name_x = x_variable , q_col_name_y = v_i , q_col_name_quantile = variable_quantile, ribbon_size = 0.25 )        # get the average of all data
    df_sum$cz <- cz_i     ;    df_avg$cz <- cz_i
    df_sum$y_var <- v_i   ;    df_avg$y_var <- v_i
    df_all <- rbind(df_all, df_sum)
    df_all_avg <- rbind(df_all_avg, df_avg)
    
  } # end loop over variables
} # end climate loop
df_all$cz <- factor(df_all$cz)
df_all_avg$cz <- factor(df_all_avg$cz)
df_all$y_var <- factor(df_all$y_var)

# so now there is a dataframe where - for each CZ in Europe and for each LST anomaly quantile (with mean mean LST anomaly temperature), we have:  the mean LAI bias, mean LST bias
# this is done for all August European pixels from 2003-2018
df_all <- as.data.frame(df_all)
summary(df_all)

# # OPTIONAL - centre the dataframe so we remove the average August ERA5-sat bias and only see the variation due to heatwaves
# # centre the dataframe by removing the average bias to view only the bias due to heatwaves
# names(df_all_avg)[1:11] <- paste0(names(df_all_avg)[1:11], '.avg')
# df_all <- df_all %>% left_join(df_all_avg)
# df_all <- df_all %>% mutate( mean_value_Y_centred = mean_value_Y - mean_value_Y.avg,
#                              mean_value_X_centred = mean_value_X - mean_value_X.avg,
#                              mean_value_Q_centred = mean_value_Q - mean_value_Q.avg
# )
# summary(df_all)

#### final step - relabel the axes (these are pre-programmed and should fit the step above)
df_all <- df_all %>%
  reshape(idvar=c('quantile','cz'), varying = c('mean_value_X', 'mean_value_Y'), v.names=c("value"), timevar='y_var', times=c("LAI bias [m2/m2]","LST bias [C]"), new.row.names = 1:1000, direction="long") 


names(df_all)[1] <- 'LST_anomaly_quantile'
names(df_all)[2] <- 'LST_anomaly'
names(df_all)[3] <- 'CZ'
names(df_all)[4] <- 'y_var'
names(df_all)[5] <- 'Bias'
#df_all[6] <- NULL

#load('/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/data/COP_tempAnomaly/data/df_LSTLAI_tempAnomalyQuantile_aug.RData')

save(df_all, file =paste0(output_path, 'df_LSTLAI_tempAnomalyQuantile_aug.RData') )

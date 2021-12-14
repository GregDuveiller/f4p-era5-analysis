#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_bias_summaries.R ####
# ---------------------------------------------------------------------------- #
# Purpose: prepare the data to generate maps summarizing the  bias and correlation in bias (part 1)
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #

###################################################
######     INITIALISE                         #####
###################################################

rm(list = ls())
start_time <- Sys.time() ; print(start_time)
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ") ; full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

library(dplyr)
library(tidyr)
library(here)

###################################################
######     GLOBAL PARAMETERS                  #####
###################################################

# Flag to remove polar areas and sea ice (useful for albedo with MODIS)
rm_polar_and_sea <- T
varname <- 'LST' ## varname_list <- c( 'LAI', 'LST', 'E',  'albedo_wsa_vis') 
v_monthS <- c(1,7) # months of interest



###################################################
######     I/O                                #####
###################################################


input_dir <- 'data/inter_data/df_comb_obs_vs_sim/'
output_path <- 'data/final_data/figures_for_paper/'

# input_dir <- '/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3/greg_workspace/MP_workspace/' # where df_comb___var.RData are located
# output_path <- paste0('/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/data/figures_for_paper/')
# output_path <- paste0('/media/mark/HD/Mark/Mark_COPERNICUS/data/COPERNICUSII_V3/greg_workspace/MP_',full_date ,'/')
dir.create(path = output_path, recursive = T, showWarnings = F) # MP

# load clim zones 
load('data/inter_data/ancillary_info/df_KG_climatezones.RData')  # <---- df_cz
#load('/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/data/input_data/CZ/df_KG_climatezones.RData')  # <---- df_cz

df_cz <- df_cz %>% mutate(cz_major_zone = substr(cz_name, 1, 1)) %>%
  select(-cz_ID, -cz_colours)

###################################################
######     RUN CODE                           #####
###################################################

###################################################
######   get temp agreement (in sp) - monthS  #####
# get the spatial agreement in terms of monthS

###################################################
#########          LAI                    #########

load(paste0(input_dir, 'df_comb___LAI.RData'))  # <--- df_comb 

df_comb <- df_comb %>% filter(!is.na(obs)) 

df_comb <- df_comb %>% filter(!is.na(obs))  %>%
  mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
  mutate(monthS = ifelse(monthS == 0, 12, monthS))  %>% 
  # mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-'))) %>%
  left_join(df_cz, by = c('x', 'y'))

# remove polar and ice CZ
if(rm_polar_and_sea == T){ df_comb <- df_comb %>% filter(cz_major_zone %in% LETTERS[1:4]) }

# select only certain months to consider
df_comb <- df_comb %>% filter(monthS %in% v_monthS ) 

df_comb <- df_comb %>% 
  mutate( bias = sim - obs)

######   get temp agreement (in space) - monthS  #####
df_LAI_bias <- df_comb %>%
  group_by(y, x, monthS) %>%
  summarise(# agre = get.Agr.Metrics(obs, sim),
            # rmsd = mean(sqrt((obs - sim)^2)),
            # diff_simSobs = mean(sim - obs, na.rm = T)
              diff_simSobs = mean(bias, na.rm = T)) 

# save('df_LAI_bias',
#      file = paste0(output_path, 'df_single_var_agr__sp_agr_monthS_LAI.RData'))  

df_comb_LAI <- df_comb
rm(df_comb)

###################################################
#########          other var              #########

load(paste0(input_dir, 'df_comb___', varname,'.RData'))  # <--- df_comb

df_comb <- df_comb %>% filter(!is.na(obs))  %>%
  mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
  mutate(monthS = ifelse(monthS == 0, 12, monthS))  %>% 
  # mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-'))) %>%
  left_join(df_cz, by = c('x', 'y'))

# remove polar and ice CZ
if(rm_polar_and_sea == T){ df_comb <- df_comb %>% filter(cz_major_zone %in% LETTERS[1:4]) }
    
# select only certain months to consider
df_comb <- df_comb %>% filter(monthS %in% v_monthS ) 

df_comb <- df_comb %>% 
  mutate( bias = sim - obs )

######   get temp agreement (in space) - monthS  #####
df_LST_bias <- df_comb %>%
  group_by(y, x, monthS) %>%
  summarise(# agre = get.Agr.Metrics(obs, sim),
            # rmsd = mean(sqrt((obs - sim)^2)),
            # diff_simSobs = mean(sim - obs, na.rm = T)
              diff_simSobs = mean(bias, na.rm = T)) 

save('df_LST_bias', 'df_LAI_bias',
     file = paste0(output_path, 'data_for_bias_summary_maps.RData'))  

df_comb_LST <- df_comb

###################################################
######   get temp agreement (in sp) - monthS  #####
# get the temporal agreement in the correlation in the bias LAI vs bias variable terms of monthS

# reduce dataframe columns
df_comb_LAI <- df_comb_LAI[,c(1,2,3,7,8,9,10)]
df_comb_LST <- df_comb_LST[,c(1,2,3,7,8,9,10)]

names(df_comb_LAI)[names(df_comb_LAI) == 'bias'] <- 'bias.LAI'

df_comb_cor <- inner_join(df_comb_LAI, df_comb_LST, by = c('x','y','monthS', 'year', 'cz_name', 'cz_major_zone') )
df_comb_cor <- df_comb_cor %>% filter(year >= 2003 & year <= 2018)

df_LSTb_LAIb_corr <- df_comb_cor %>%
  group_by(y, x, monthS) %>%
  summarise(r = cor(bias, bias.LAI)
    #agre = get.Agr.Metrics(diff_simSobs.LAI, diff_simSobs),
    #agre_bias = get.Agr.Metrics(bias, bias.LAI) # now get the agreement metrics of bias(sat-obs) in VAR vs LAI (i.e. temporal correlation in the bais etc)
  )
df_LSTb_LAIb_corr <- df_LSTb_LAIb_corr %>% filter(!is.na(r))

save('df_LSTb_LAIb_corr',
     file = paste0(output_path, 'data_for_corr_summary_maps.RData'))  



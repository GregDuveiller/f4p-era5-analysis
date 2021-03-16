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

varname = 'albedo_wsa_vis'


for( varname in c('albedo_wsa_vis','LST', 'SM', 'LAI')){

print(paste0('|> working on ', varname, '...'))

load(paste0('data/inter_data/df_comb_obs_vs_sim/df_comb___', varname,'.RData'))  # <--- df_comb


# get overall agreement
agr <- get.Agr.Metrics(df_comb$obs, df_comb$sim)

nbins <- 100

min_val <- floor(min(min(df_comb$obs), min(df_comb$sim)))
max_val <- ceiling(max(max(df_comb$obs), max(df_comb$sim)))

bins <- seq(min_val, max_val, length = nbins)
centroids <- bins[1:(nbins-1)] + (diff(bins)/2)

freq <-  as.data.frame(table(
  findInterval(df_comb$obs, bins, rightmost.closed = T, all.inside = T), 
  findInterval(df_comb$sim, bins, rightmost.closed = T, all.inside = T)))

freq[,4] <- centroids[freq[,1]]
freq[,5] <- centroids[freq[,2]]

colnames(freq) <- c('obs_bin', 'sim_bin', 'freq', 'obs_val', 'sim_val')

# freq2D <- diag(nbins)*0
# freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]


# agreement in space (per pixel)
sp_agr <- df_comb %>%
  group_by(y, x) %>%
  summarise(agre = get.Agr.Metrics(obs, sim),
            rmsd = mean(sqrt((obs - sim)^2)),
            bias = mean(obs - sim))

# require(raster)
# r <- raster(nrows = 720, ncols = 1440, 
#             xmn = -180, xmx = 180, ymn = -90, ymx = 90, vals = NA)
# cells <- cellFromXY(object = r, xy = as.matrix(sp_agr[,c('x','y')]))
# r[cells] <- as.integer(sp_agr$agre$L * 10000)
# writeRaster(r, filename = 'data/input_data/sp_agr_map_LST.tif', datatype = 'INT2U')


# agreement in time (per climzone)
load('data/inter_data/ancillary_info/df_KG_climatezones.RData')  # <---- df_cz
df_cz <- df_cz %>% mutate(cz_major_zone = substr(cz_name, 1, 1)) %>%
  select(-cz_ID, -cz_colours)

df_comb <- df_comb %>% 
  mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
  mutate(monthS = ifelse(monthS == 0, 12, monthS))  %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-'))) %>%
  left_join(df_cz, by = c('x', 'y'))

temp_agr_det <- df_comb %>%
  group_by(cz_name, time) %>%
  summarise(agre = get.Agr.Metrics(obs, sim),
            rmsd = mean(sqrt((obs - sim)^2)),
            bias = mean(obs - sim))

temp_agr_gen <- df_comb %>%
  group_by(cz_major_zone, time) %>%
  summarise(agre = get.Agr.Metrics(obs, sim),
            rmsd = mean(sqrt((obs - sim)^2)),
            bias = mean(obs - sim)) 

save('agr', 'freq', 'sp_agr', 'temp_agr_gen', 'temp_agr_det',
     file = paste0('data/inter_data/df_single_var_agreement/df_single_var_agr_',varname,'.RData'))

print(paste0('<| all done for ', varname, '!'))
}

#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_hysteresis_per_bin.R ####
# ---------------------------------------------------------------------------- #
# Purpose: process data to make figures of hysteresis plots
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #

require(dplyr)
require(tidyr)
require(sf)


#### Get the necessary data ####

# load/prepare dataframe with data...
load('data/inter_data/df_per_clim_bin/df_LAI_per_clim_bin.RData') # <---- df_LAI_comb
load('data/inter_data/df_per_clim_bin/df_LST_per_clim_bin.RData') # <---- df_LST_comb


#### Get the data averaged by bins ####


by_vctr <- c("time", "t2.clim.bin", "sm.clim.bin")

df_r_all <- left_join(by = all_of(by_vctr), suffix = c("_LAI", "_LST"),
                      x = df_LAI_comb, y = df_LST_comb) %>%
  select(by_vctr, dif_mu_LAI, dif_mu_LST, N_LAI, N_LST) %>%   # 
  rename(y = dif_mu_LST, x = dif_mu_LAI, ny = N_LST, nx = N_LAI) %>%
  filter(!is.na(y) & !is.na(x)) %>% 
  filter(nx > 30 & ny > 30) %>%
  filter(sm.clim.bin != 0) %>%
  mutate(year = as.numeric(format(time,'%Y'))) %>%
  mutate(monthS = as.numeric(format(time, '%m')))


#### Fit the hysteresis curves ####

# define function to make plots per 2d bin
fit.hyst <- function(t2.bin.num, sm.bin.num, harmonic_n = 3){
  
  # filter here for a section of the climspace
  df <- df_r_all %>%
    filter(t2.clim.bin == levels(df_r_all$t2.clim.bin)[t2.bin.num]) %>%  
    filter(sm.clim.bin == levels(df_r_all$sm.clim.bin)[sm.bin.num])    
  
  if(dim(df)[1] == 0){
    # print('No data for this bin')
    return(df_p = NULL)}
  
  # calculate monthly averages
  df_m <- df %>%
    group_by(monthS) %>%
    summarize(y_mu = mean(y),
              x_mu = mean(x),
              y_sd = sd(y),
              x_sd = sd(x))
  
  # fit the hysteresis curve decomposing x and y 
  # (we do it on the raw data instead of the means, but result is identical)
  y <- rep(df$y, times = 3)
  x <- rep(df$x, times = 3)
  t <- c(df$monthS - 12, df$monthS, df$monthS + 12)
  per <- 12
  
  # simple linear fit (as general indicator... not clear if useful)
  lmfit <- lm(y ~ x)
  
  # Harmonic fit on both x and y... 
  if(harmonic_n == 3){   # third order harmonic fit ...
    tyfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
    txfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
  }
  if(harmonic_n == 2){   # second harmonic fit ...
    tyfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
    txfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
  }   
  # # more complicated fits...
  # tyfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t)+sin(8*pi/per*t)+cos(8*pi/per*t))
  # txfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t)+sin(8*pi/per*t)+cos(8*pi/per*t)) 
  
  # make vectors with the fitted coefficients of the harmonic fits
  beta_y <- tyfit$coefficients; names(beta_y) <- paste0('b', 'y', 0:(length(tyfit$coefficients)-1))
  beta_x <- txfit$coefficients; names(beta_x) <- paste0('b', 'x', 0:(length(txfit$coefficients)-1))
  
  #assemble smoothed time series in a dedicated dataframe
  df_s <- data.frame(t = seq(0.5,12.5,0.1))
  df_s$y <- predict.lm(tyfit, df_s)
  df_s$x <- predict.lm(txfit, df_s)
  df_s$t2.clim.bin <- unique(df$t2.clim.bin)
  df_s$sm.clim.bin <- unique(df$sm.clim.bin)  
  
  # make new matrix for polygon, and ensure closure
  mat_coords <- matrix(data = c(df_s$x, df_s$x[1], df_s$y, df_s$y[1]),
                       byrow = F, ncol = 2)
  
  # transform loop to sf object
  poly_loop <- st_polygon(x = list(mat_coords), dim = 'XY')
  
  # ensure geometry is valid (needed for abs calculation of the area)
  if(st_is_valid(poly_loop) != T) poly_loop <- st_make_valid(poly_loop)
  
  # calculate the area of each loop
  loop_area <- st_area(poly_loop)
  
  
  # prepare dataframe with metrics describing the curves
  df_p <- data.frame(
    t2.clim.bin = levels(df_r_all$t2.clim.bin)[t2.bin.num],
    sm.clim.bin = levels(df_r_all$sm.clim.bin)[sm.bin.num],
    xrange_s = abs(diff(range(df_s$x))),
    yrange_s = abs(diff(range(df_s$y))),
    x_sd_s = sd(df_s$x, na.rm = T),
    y_sd_s = sd(df_s$y, na.rm = T),
    areaL = loop_area,
    areaE = 0.5 * sum(diff(df_s$y, lag = 2) * df_s$x[2:(length(df_s$x)-1)]),
    t(beta_y), t(beta_x),
    slope = lmfit$coefficients[2],
    y_int = lmfit$coefficients[1],
    lmRMS = sqrt(mean((lmfit$residuals)^2))
  )
  
  return(list(df_p = df_p, df_s = df_s))
}

# loop thru it all
df_p_all <- data.frame(NULL)
df_s_all <- data.frame(NULL)

for(i in 1:length(levels(df_r_all$t2.clim.bin))){
  for(j in 1:length(levels(df_r_all$sm.clim.bin))){
    out <- fit.hyst(i, j)
    
    df_p_all <- bind_rows(df_p_all, out$df_p)
    df_s_all <- bind_rows(df_s_all, out$df_s)
    
  }
}

#### Export the data ####

save('df_r_all', 'df_p_all','df_s_all', file = 'data/figures_for_paper/hysteresis_data_ready4fig.RData')



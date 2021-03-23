#!/usr/local/bin/Rscript
################################################################################
# Purpose: get some anomalies from other work
# Project: f4p-era5-analysis
# Authors: G.Duveiller
################################################################################



library(here)
library(dplyr)
library(tidyr)


df_path <- '../lulcc-bph-chgclim/data/input_data/era5_df'
yrRange <- 2003:2019

# continous climspace

binVarX <- 't2'
binVarY <- 'sm'

getclim <- function(var2process){
  
  ## get the T2M data (from dataframes) ----
  if(var2process == 't2'){
    df_all <- data.frame() 
    for(iYear in yrRange){
      fname <- paste0(df_path, '/df_T2M_ERA5l_025_', iYear, '.RData')
      load(file = fname) # <-- 'df' 
      df_sel <- df %>%
        rename(var_value = t2m) %>%
        mutate(var_value = var_value - 273.15,
               lat = round(y, digits = 3),
               lon = round(x, digits = 3)) %>%
        # filter(lon >= reg_ext@xmin, lon <= reg_ext@xmax,
        #        lat >= reg_ext@ymin, lat <= reg_ext@ymax) %>%
        dplyr::select(-x, -y)
      
      df_all <- bind_rows(df_all, df_sel)
    }
    
  }
  
  ## get the SM data (from dataframes) ----
  if(var2process == 'sm'){
    df_all <- data.frame() 
    for(iYear in yrRange){
      fname <- paste0(df_path, '/df_SM_ERA5l_025_', iYear, '.RData')
      load(file = fname) # <-- 'df' 
      df_sel <- df %>%
        rename(var_value = swvl1) %>%
        mutate(lat = round(y, digits = 3),
               lon = round(x, digits = 3)) %>%
        # filter(lon >= reg_ext@xmin, lon <= reg_ext@xmax,
        #        lat >= reg_ext@ymin, lat <= reg_ext@ymax) %>%
        dplyr::select(-x, -y,)
      
      df_all <- bind_rows(df_all, df_sel)
    }
    
    
    
  }
  
  # get mean
  df_mean <- df_all %>%
    group_by(lon, lat) %>%
    summarize(clim = mean(var_value), .groups = 'keep')
  
  return(df_mean)
}



list_var <- list(t2 = list(var_name = 't2', var_tag = 't2_ERA5l_025',
                           bin.width = 1,  bin.min = -10, bin.max = 30), 
                 sm = list(var_name = 'sm', var_tag = 'sm_ERA5l_025',
                           bin.width = 0.01,  bin.min = 0.1, bin.max = 0.5),
                 et = list(var_name = 'et', var_tag = 'et_GLEAM_025',
                           bin.width = 5, bin.min = -15, bin.max = 150),
                 al = list(var_name = 'al', var_tag = 'al_MODIS_025',
                           bin.width = 0.01, bin.min = 0.02, bin.max = 0.36))  

detVarX <- list_var[binVarX][[binVarX]]
detVarY <- list_var[binVarY][[binVarY]]


yrRangeL <- '2003to2019'

df_clim_VarX <- getclim(binVarX)
df_clim_VarY <- getclim(binVarY)


## bring in all together in 2D grid ----

# make bin labels
x.bin.numlbls <- seq(detVarX$bin.min, detVarX$bin.max, detVarX$bin.width)
y.bin.numlbls <- seq(detVarY$bin.min, detVarY$bin.max, detVarY$bin.width)

# make bin breaks
x.bin.breaks <- c(x.bin.numlbls - (detVarX$bin.width/2), Inf)
y.bin.breaks <- c(y.bin.numlbls - (detVarY$bin.width/2), Inf)



df_climspace <- inner_join(df_clim_VarX, df_clim_VarY, by = c('lat', 'lon'),
                           suffix = c(".x", ".y")) %>%
  rename(x = lon, y = lat) %>%
  mutate(x.clim.bin = cut(clim.x, 
                          breaks = x.bin.breaks, 
                          labels = x.bin.numlbls)) %>% 
  mutate(y.clim.bin = cut(clim.y, 
                          breaks = y.bin.breaks, 
                          labels = y.bin.numlbls)) %>%
  tibble()

fname <- paste0('df_climspace_', binVarX, 'x', binVarY, '.RData')
fpath <- 'data/inter_data/ancillary_info'
save('df_climspace', file = paste0(fpath, '/', fname)) 


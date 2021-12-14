# original name : createNC_LAI_theia.R
#
# ########################################################
# Title         : 
# Description   : This code takes the .nc files created from ncks-converted .h5 files and writes them as netcdf files with the same names
#                 and a valid date/lonlat coords and re-scaled variables
# Inputs	      : /mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/COPERNICUSII/data/theia/data/raw_download_nc 
#                 
# Outputs	      : netcdf
#                 /mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/COPERNICUSII/data/theia/data/netcdf_base
#                 
# Options	      : 
# Date          : 02/07/20
# Version       : 1.1
# Licence       : N/A
# Authors       : Mark Pickering
# Maintainer    : Mark Pickering
# Notes		      : 
#                 
# Example use   : 
# ########################################################

######     INITIALISE                         #####
rm(list = ls())
start_time <- Sys.time()
print(start_time)

######     GLOBAL VARS                        #####
script_name <-'createNC_LAI_theia.R'           # name of the script
script_info <-'LAItheia_h5-nc'           # first attempt

######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%
library(raster) # package for raster manipulation
library(lattice)    # enables levelplots
library(rasterVis)  # enables levelplots
library(ncdf4)
#library(lubridate)   # enables date manipulation
#require(ggplot2)
#require(magrittr)
#require(here)
#require(tidyr)

library("rgdal",lib="~/R") #  personally installed library(gdal) #


######     SET I/O PATHS                      #####
# find date:
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

# base paths
root_data   <- '/mnt/cidstorage/cidportal/data/cid-bulk22/Shared/projectData/COPERNICUSII/data/theia/data/'
input_path  <- paste0(root_data, '3_raw_download_nc/')
output_path <- paste0(root_data,'netcdf_baseData/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)}
if(! dir.exists(paste0(output_path,'scripts/'))) {dir.create(paste0(output_path,'scripts/'),recursive=T)}

# copy snapshot of r_script and headers to output directory for future reference
#root_script <- here()
#file.copy(paste0(root_script,'/',script_name),(paste0(output_path,'scripts/','copy_',full_date,'_',full_time,'_',script_name)))

######     SET FUNCTIONS                      #####

######     LOOP OVER FILES                    #####
nc_files <- list.files(path=input_path, pattern="*.nc", full.names=FALSE, recursive=FALSE)

for(file in nc_files){
  #file <- 'THEIA_GEOV2_R01_AVHRR_LAI_20190125.nc' # hardcode
  print(file)
  
  # We want to open the file - extract the raster stack, and the date, then rewrite the raster as a netcdf
  # from http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html#create-and-write-a-netcdf-file
  
  
  # extract the date from the filename
  file_date <- strsplit(strsplit(file, "_")[[1]][6], ".nc")[[1]][1]
  file_year <- substr(file_date,1,4) ; file_mon <- substr(file_date,5,6) ; file_day <- substr(file_date,7,8)
  #chron(time,origin=c(file_mon, file_day, file_year))
  chron_date <- dates( paste0(file_mon,'/',file_day,'/',file_year))
  chron_time <- times("00:00:00")
  chron_date_time <- chron(dates= chron_date, times = chron_time)
  chron_date_1900 <- paste0("01/01/1900") ; chron_time_1900 <- times("00:00:00")
  chron_1900 <- chron(dates= chron_date_1900, times = chron_time_1900)
  date_time_hours_since_1900 <- (chron_date_time - chron_1900) * 24
  
  
  # create lon/lat for 0.05 deg
  lon <- as.array(seq(-179.975,179.975,0.05))
  nlon <- 7200
  lat <- as.array(seq(-89.975,89.975,0.05))
  nlat <- 3600
  
  # extract attributes from the existing netcdf
  ncin <- nc_open(paste0(input_path,file))
  
  # extract LAI values
  var_1_name <- 'LAI'
  var_1_arr <- ncvar_get(ncin,var_1_name)
  var_1_arr <- t(apply(var_1_arr, 1, rev)) # image needs to flip
  #stack_var_1 <- stack(paste0(input_path,file), varname = 'LAI') ;   plot(stack_LAI_sat_data)
  #raster_var_1 <- raster(paste0(input_path,file), varname = var_1_name) # it's one dimensional
  var_1_units <- ncatt_get(ncin, paste0(var_1_name) ,"units")                   # manually set
  var_1_fillvalue <- 255 #ncatt_get(ncin, paste0(var_1_name) ,"_FillValue")  # manually set - seems to be 255
  #var_1_sf <-   ncatt_get(ncin, paste0(var_1_name) ,"SCALING_FACTOR")

  var_1_arr[var_1_arr==var_1_fillvalue] <- NA
  #image(lon,lat,var_1_arr, col=rev(brewer.pal(10,"RdBu")))  # image array
  # typeof(var_1_arr)   #var_1_arr <- as.double(var_1_arr)
  #var_1_arr <- (var_1_arr) / var_1_sf$value
  
  #LAI-NOBS
  var_2_name <- 'LAI-NOBS'
  var_2_arr <- ncvar_get(ncin,var_2_name)
  var_2_arr <- t(apply(var_2_arr, 1, rev)) # image needs to flip
  var_2_units <- ncatt_get(ncin, paste0(var_2_name) ,"units")                   # manually set
  var_2_fillvalue <- 255 #ncatt_get(ncin, paste0(var_2_name) ,"_FillValue")  # manually set - seems to be 255
  #var_2_sf <-   ncatt_get(ncin, paste0(var_2_name) ,"SCALING_FACTOR") ncvar_get - takes into account sf & offset
  
  var_2_arr[var_2_arr==var_2_fillvalue] <- NA
  #var_2_arr <- (var_2_arr) / var_2_sf$value

  #LAI-QFLAG
  var_3_name <- 'LAI-QFLAG'
  var_3_arr <- ncvar_get(ncin,var_3_name)
  var_3_arr <- t(apply(var_3_arr, 1, rev)) # image needs to flip
  var_3_units <- ncatt_get(ncin, paste0(var_3_name) ,"units")                   # manually set
  var_3_fillvalue <- 255 #ncatt_get(ncin, paste0(var_3_name) ,"_FillValue")  # manually set - seems to be 255
  #var_3_sf <-   ncatt_get(ncin, paste0(var_3_name) ,"SCALING_FACTOR")
  
  var_3_arr[var_3_arr==var_3_fillvalue] <- NA
  #var_3_arr <- (var_3_arr) / var_3_sf$value
  
  #LAI-RMSE
  var_4_name <- 'LAI-RMSE'
  var_4_arr <- ncvar_get(ncin,var_4_name)
  var_4_arr <- t(apply(var_4_arr, 1, rev)) # image needs to flip
  var_4_units <- ncatt_get(ncin, paste0(var_4_name) ,"units")                   # manually set
  var_4_fillvalue <- 255 #ncatt_get(ncin, paste0(var_4_name) ,"_FillValue")  # manually set - seems to be 255
  #var_4_sf <-   ncatt_get(ncin, paste0(var_4_name) ,"SCALING_FACTOR")
  
  var_4_arr[var_4_arr==var_4_fillvalue] <- NA
  #var_4_arr <- (var_4_arr) / var_4_sf$value
  
  
  # get global attributes - only hist available
  history <- ncatt_get(ncin,0,"history")
  
  
  ##### create and write a new file
  # create and write the netCDF file -- ncdf4 version
  # define dimensions
  londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
  latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
  tunits <- "hours since 1900-01-01 00:00:00.0" #"days since 1970-01-01 00:00:00.0 -0:00"
  timedim <- ncdim_def("time",tunits,as.double(date_time_hours_since_1900))
  
  # define variables
  fillvalue <- 32767
  LAI_def <- ncvar_def(var_1_name,"m**2 m**-2",list(londim,latdim,timedim),fillvalue,var_1_name,prec="single",compression=9)
  LAINOBS_def <- ncvar_def(var_2_name," ",list(londim,latdim,timedim),fillvalue,var_2_name,prec="single",compression=9)
  LAIQFLAG_def <- ncvar_def(var_3_name," ",list(londim,latdim,timedim),fillvalue,var_3_name,prec="single",compression=9)
  LAIRMSE_def <- ncvar_def(var_4_name,"m**2 m**-2",list(londim,latdim,timedim),fillvalue,var_4_name,prec="single",compression=9)
  
  # create file
  ncfname <- paste0(output_path,file)
  ncout <- nc_create(ncfname,list(LAI_def,LAINOBS_def,LAIQFLAG_def,LAIRMSE_def),force_v4=TRUE)
  
  # add variables
  ncvar_put(ncout,LAI_def,var_1_arr)
  ncvar_put(ncout,LAINOBS_def,var_2_arr)
  ncvar_put(ncout,LAIQFLAG_def,var_3_arr)
  ncvar_put(ncout,LAIRMSE_def,var_4_arr)
  
  # put additional attributes into dimension and data variables
  ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")

  # add global atts
  history <- paste("Mpick add date/axes", date(), sep=", ")
  ncatt_put(ncout,0,"history",history)
  #ncout  # print data
  nc_close(ncout) # write data
}



# 
# ######     SET FUNCTIONS                      #####
# date_to_col <- function(df, inc_year=TRUE, inc_month=TRUE){
#   # function takes a dataframe with Z column a date and extracts the month and year component
#   if(inc_year)df <- df %>% mutate(year = lubridate::year(Z))
#   if(inc_month){ df <- df %>% mutate( month = lubridate::month(Z) ) }
#   df$Z <- NULL
#   return(df)
# }
# 
# stack_to_df <- function(stack, col_name, stat_type='data'){
#   # takes the netcdf stacks converts to df and applys simple processings:
#   # adding date columns (dependent on stat_type = data, mean, std), rounding x/y column, re-naming column to col_name
# 
#   df <- as.data.frame(stack, xy = T, na.rm = F, long = T)
#   df <- date_to_col(df)
#   if(stat_type=='data'){  df <- df[c('x','y','year','month','value')] }
#   else{ df <- df[c('x','y','month','value')] }
#   colnames(df)[ which(names(df) == 'value')] <- col_name
#   df$x <- round(df$x, digits = 3)           ; df$y <- round(df$y, digits = 3)
# 
#   return(df)
# }
# 
# 
# ######     CREATE DATAFRAME BY YEAR SIFPK       #####
# for(year in start_year:end_year){
# 
#   print(paste0('year is: ', year))
# 
#   ####### LAI satellite ######
#   print('LAI_satellite')
# 
#   # LAI - set paths
#   LAI_sat_filename <- paste0('sat-LAI.y.',year,'.nc')
#   LAI_sat_data_path <- paste0(root_data,LAI_sat_ext, LAI_sat_filename)
#   LAI_sat_mean_path <- paste0(root_data,LAI_sat_ext, 'sat-LAI.99-13.monMean.nc')
#   LAI_sat_std_path <-  paste0(root_data,LAI_sat_ext, 'sat-LAI.99-13.monSTD.nc')
# 
#   # specific
#   #root_data <- '/mnt/cidstorage/cidportal/data/cid-bulk22/Shared/projectData/COPERNICUSII/data/theia/data/test/'
#   #LAI_sat_filename <- paste0('THEIA_GEOV2_R01_AVHRR_LAI_20051205_gunzip_ncksConvert.nc')
#   #LAI_sat_data_path <- paste0(root_data,LAI_sat_ext, LAI_sat_filename)
#   #stack_LAI_sat_data <- stack(LAI_sat_data_path, varname = 'LAI')
#   #plot(stack_LAI_sat_data)
#   
#   # LAI - get stacks
#   stack_LAI_sat_data <- stack(LAI_sat_data_path, varname = 'LAI')
#   stack_LAI_sat_mean_data <- stack(LAI_sat_mean_path, varname = 'LAI')
#   stack_LAI_sat_std_data <- stack(LAI_sat_std_path, varname = 'LAI')
# 
#   # LAI - get df
#   df_LAI_sat_data <- stack_to_df(stack_LAI_sat_data, 'LAI_sat')
#   df_LAI_sat_mean <- stack_to_df(stack_LAI_sat_mean_data, 'LAI_sat_mean', stat_type = 'mean')
#   df_LAI_sat_std <- stack_to_df(stack_LAI_sat_std_data, 'LAI_sat_std', stat_type = 'std')
# 
#   # Join dfs
#   df_LAI_satellite <-  left_join(df_LAI_sat_data,df_LAI_sat_mean)
#   df_LAI_satellite <-  left_join(df_LAI_satellite, df_LAI_sat_std)
#   dim(df_LAI_satellite) ; head(df_LAI_satellite)
#   rm(df_LAI_sat_data, df_LAI_sat_mean, df_LAI_sat_std)
#   rm(stack_LAI_sat_data, stack_LAI_sat_mean_data, stack_LAI_sat_std_data)
# 
#   ####### LAI modelled  ######
#   print('LAI modelled')
# 
#   # Get file
#   LAI_mod_filename <- paste0('mod-LAI.2010.nc') # only one year needed as fixed
#   LAI_mod_data_path <- paste0(root_data,LAI_mod_ext,LAI_mod_filename)
# 
#   # Get stacks
#   stack_LAI_mod_cvh <- stack(LAI_mod_data_path, varname = 'cvh') # veg cover high
#   stack_LAI_mod_cvl <- stack(LAI_mod_data_path, varname = 'cvl') # veg cover low
#   stack_LAI_mod_tvh <- stack(LAI_mod_data_path, varname = 'tvh') # type veg high
#   stack_LAI_mod_tvl <- stack(LAI_mod_data_path, varname = 'tvl') # type veg low
#   stack_LAI_mod_lai_hv <- stack(LAI_mod_data_path, varname = 'lai_hv') # LAI high
#   stack_LAI_mod_lai_lv <- stack(LAI_mod_data_path, varname = 'lai_lv') # LAI low
# 
# 
# 
#   # get df
#   df_LAI_mod_cvh <- stack_to_df(stack_LAI_mod_cvh, 'mod_vcover_high', stat_type = 'annual')
#   df_LAI_mod_cvl <- stack_to_df(stack_LAI_mod_cvl, 'mod_vcover_low', stat_type = 'annual')
#   df_LAI_mod_tvh <- stack_to_df(stack_LAI_mod_tvh, 'mod_vtype_high', stat_type = 'annual')
#   df_LAI_mod_tvl <- stack_to_df(stack_LAI_mod_tvl, 'mod_vtype_low', stat_type = 'annual')
#   df_LAI_mod_lai_hv <- stack_to_df(stack_LAI_mod_lai_hv, 'LAI_mod_high', stat_type = 'annual')
#   df_LAI_mod_lai_lv <- stack_to_df(stack_LAI_mod_lai_lv, 'LAI_mod_low', stat_type = 'annual')
# 
#   # Join dfs
#   df_LAI_modelled <-  df_LAI_mod_cvh %>% left_join(df_LAI_mod_cvl) %>%
#     left_join(df_LAI_mod_tvh) %>% left_join(df_LAI_mod_tvl) %>%
#     left_join(df_LAI_mod_lai_hv) %>% left_join(df_LAI_mod_lai_lv)
#   dim(df_LAI_modelled); head(df_LAI_modelled); summary(df_LAI_modelled)
# 
#   rm( df_LAI_mod_cvh, df_LAI_mod_cvl, df_LAI_mod_tvh, df_LAI_mod_tvl, df_LAI_mod_lai_hv, df_LAI_mod_lai_lv)
#   rm(stack_LAI_mod_cvh, stack_LAI_mod_cvl, stack_LAI_mod_tvh, stack_LAI_mod_tvl, stack_LAI_mod_lai_hv, stack_LAI_mod_lai_lv)
# 
#   ####### SM satellite  ######
#   print('SM satellite')
# 
#   # SM - set paths
#   SM_sat_filename <- paste0('sat-SM.y.',year,'.nc')
#   SM_sat_data_path <- paste0(root_data,SM_sat_ext, SM_sat_filename)
#   SM_sat_mean_path <- paste0(root_data,SM_sat_ext, 'sat-SM.99-13.monMean.nc')
#   SM_sat_std_path <- paste0(root_data,SM_sat_ext, 'sat-SM.99-13.monSTD.nc')
# 
#   # SM - get stacks
#   stack_SM_sat_data <- stack(SM_sat_data_path, varname = 'sm')
#   stack_SM_sat_mean_data <- stack(SM_sat_mean_path, varname = 'sm')
#   stack_SM_sat_std_data <- stack(SM_sat_std_path, varname = 'sm')
# 
#   # SM - get df
#   df_SM_sat_data <- stack_to_df(stack_SM_sat_data, 'SM_sat')
#   df_SM_sat_mean <- stack_to_df(stack_SM_sat_mean_data, 'SM_sat_mean', stat_type = 'mean')
#   df_SM_sat_std <- stack_to_df(stack_SM_sat_std_data, 'SM_sat_std', stat_type = 'std')
# 
#   # Join dfs
#   df_SM_satellite <-  left_join(df_SM_sat_data,df_SM_sat_mean)
#   df_SM_satellite <-  left_join(df_SM_satellite, df_SM_sat_std)
#   dim(df_SM_satellite) ; head(df_SM_satellite)
#   rm(df_SM_sat_data, df_SM_sat_mean, df_SM_sat_std)
#   rm(stack_SM_sat_data, stack_SM_sat_mean, stack_SM_sat_std)
# 
#   ####### SM modelled   ######
#   print('SM modelled')
# 
#   # SM - set paths
#   SM_mod_filename <- paste0('mod-SM.y.',year,'.nc')
#   SM_mod_data_path <- paste0(root_data,SM_mod_ext, SM_mod_filename)
#   SM_mod_mean_path <- paste0(root_data,SM_mod_ext, 'mod-SM.99-13.monMean.nc')
#   SM_mod_std_path <- paste0(root_data,SM_mod_ext, 'mod-SM.99-13.monSTD.nc')
# 
#   # SM - get stacks
#   stack_SM_mod_data <- stack(SM_mod_data_path, varname = 'sm')
#   stack_SM_mod_mean_data <- stack(SM_mod_mean_path, varname = 'sm')
#   stack_SM_mod_std_data <- stack(SM_mod_std_path, varname = 'sm')
# 
#   # SM - get df
#   df_SM_mod_data <- stack_to_df(stack_SM_mod_data, 'SM_mod')
#   df_SM_mod_mean <- stack_to_df(stack_SM_mod_mean_data, 'SM_mod_mean', stat_type = 'mean')
#   df_SM_mod_std <- stack_to_df(stack_SM_mod_std_data, 'SM_mod_std', stat_type = 'std')
# 
#   # Join dfs
#   df_SM_modelled <-  left_join(df_SM_mod_data,df_SM_mod_mean)
#   df_SM_modelled <-  left_join(df_SM_modelled, df_SM_mod_std)
#   dim(df_SM_modelled) ; head(df_SM_modelled) ; summary(df_SM_modelled)
#   rm(df_SM_mod_data, df_SM_mod_mean, df_SM_mod_std)
#   rm(stack_SM_mod_data, stack_SM_mod_mean, stack_SM_mod_std)
# 
#   ####### other data    ######
#   print('KG dataset')
# 
#   #Add in KG data
#   KG_filename <- 'koppen-Geiger_1440x720_5zones.remap.nc'
#   KG_data_path <- paste0(root_data,other_ext, KG_filename)
# 
#   stack_KG_data <- stack(KG_data_path, varname = 'climzone')
#   df_KG_data <- as.data.frame(stack_KG_data, xy = T, na.rm = F, long = T) # don't remove NAs to ensure all datapoints are coverd
#   df_KG_data[3] <-NULL ; names(df_KG_data)[3] <- "KG" ;
#   df_KG_data$KG[df_KG_data$KG=="1"] <- "EQUATORIAL"
#   df_KG_data$KG[df_KG_data$KG=="2"] <- "ARID"
#   df_KG_data$KG[df_KG_data$KG=="3"] <- "TEMPERATE"
#   df_KG_data$KG[df_KG_data$KG=="4"] <- "CONTINENTAL"
#   df_KG_data$KG[df_KG_data$KG=="5"] <- "ICE"
#   df_KG_data$KG  <- factor(df_KG_data$KG, levels = c("EQUATORIAL","ARID","TEMPERATE","CONTINENTAL", "ICE"))
#   df_KG_data$x <- round(df_KG_data$x, digits = 3) ; df_KG_data$y <- round(df_KG_data$y, digits = 3)
# 
#   # get the area of each raster cell
#   stack_area <- area(stack_KG_data)
#   stack_area <- round(stack_area, digits = 3)
#   df_area <- as.data.frame(stack_area, xy = T, na.rm = F, long = T)
#   df_area[3] <-NULL ; names(df_area)[3] <- "area" ;
#   df_area$x <- round(df_area$x, digits = 3) ; df_area$y <- round(df_area$y, digits = 3)
#   # join other dfs
#   df_other <- left_join(df_KG_data, df_area) ; summary(df_other)
# 
# 
#   ####### combine data  ######
#   print('combine data')
#   #summary(df_LAI_satellite);
#   #summary(df_LAI_modelled)
#   #summary(df_SM_satellite)
#   #summary(df_SM_modelled)
#   #summary(df_other)
# 
#   # Save each year (at the end make a combined year file)
#   df_sat_mod <- left_join(df_LAI_satellite, df_LAI_modelled)
#   df_sat_mod <- df_sat_mod %>% left_join(df_SM_satellite) %>%
#                             left_join(df_SM_modelled) %>%
#                             left_join(df_other)
# 
#   # some of the KGs are negative due to the sea probably so we filter on these
#   # many of the satellite SM measurements are NA - could probably filter on these also
#   #df_sat_mod <- df_sat_mod %>% filter(!is.na(KG)) %>%
#   #                             filter(!is.na(SM_sat))
# 
#   # Add a region category - use same as SIF work
#   df_sat_mod <- df_sat_mod %>% mutate( Region = ifelse( (x<(-24.) & y>(0.) ), 'NAmerica/Carrib',
#                                        ifelse( (x<(-24.) ), 'SAmerica',
#                                                 ifelse( (x<(60.) & y>(24.) ), 'Europe/ME',
#                                                         ifelse( (x<60.), 'SubSahara',
#                                                                 ifelse( (y>6.), 'Asia',
#                                                                         ifelse( (y<=6), 'Oceania', NA )
#                                                                         )))))  )
#   df_sat_mod$Region  <- factor(df_sat_mod$Region, levels = c("NAmerica/Carrib","SAmerica","Europe/ME","SubSahara", "Asia","Oceania"))
# 
#   # save dataframe for year
#   summary(df_sat_mod)
#   save( df_sat_mod, file=paste0(output_path, script_info, '.y.',year,'.RData') )
# 
# 
# } # end of year loop
# 
# 
# # Create df for all years combined as rows
# 
# df_sat_mod_allY <- NULL
# 
# for(year in start_year:end_year){
#   load(file=paste0(output_path, script_info, '.y.',year,'.RData') ) # df_sat_mod
#   # Add rounding to all numeric columns
#   df_sat_mod <- df_sat_mod %>% mutate_if(is.numeric, round, 3)
#   #add as rows to allY df
#   df_sat_mod_allY <- bind_rows(df_sat_mod_allY, df_sat_mod)
# }
# 
# # head(dat.df.all)
# # summary(dat.df.all)
# print(dim(df_sat_mod_allY))
# print(paste0('saving all year rdata file to : ', output_path))
# save( df_sat_mod_allY, file=paste0(output_path, script_info, '.y.all','.RData') )
# 
# 
# ######     FINALISE                           #####
# # timing
# end_time <- Sys.time()
# print(end_time - start_time)

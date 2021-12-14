# original name : createDF_albedo.R
#
# ########################################################
# Title         : 
# Description   : This code opens the .nc files and exports the data to dataframes with 0.1 or 0.25 res
#                 the dataframes will cover the full time period 
# Inputs	      : provide input dir and base_file_name e.g. /home/mark/Documents/work_jrc/work_d1/work_COPERNICUSII/data/ET_ERA5sl_025_*_.nc
#                 
# Outputs	      : dataframes
#                 /mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/COPERNICUSII/data/albedo/dataframe
#                 
# Options	      : 
# Date          : 26/10/20
# Version       : 1.1
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

######     GLOBAL VARS                        #####
script_name <-'createDF_generic.R'               # name of the script
var <- 'albedo_NASA_01'             # 025 and 01 # ET_GLEAM SM_ERA5l SM_sat SM_ERA5l_01_2009.nc LAI_theia_01_1998.nc
root_data   <- '/eos/jeodpp/data/projects/COPERNICUSII/data/NASA_albedo/data/6_monmean_01/nc_for_df/'
output_path <- '/eos/jeodpp/data/projects/COPERNICUSII/data/NASA_albedo/data/dataframe/'
#script_info <-'albedo_GEE'                 # 
start_year <- 2001 ; end_year <- 2017 #18      # just hardcode year range for now
mm_list <- c(31,28,31,30,31,30,31,31,30,31,30,31)  # list of days in months
# pre-programmed scale-factors: these sf make the satellite and model netcdf values cross comparable (eg. conversion to same units)
sf <- list( ET_GLEAM = 1, ET_ERA5l = -1000,  SM_sat = 1, SM_ERA5l = 1, SM_ERA5l = 1, LAI_theia = 1, LAI_ERA5sl = 1, albedo_NASA = 1, albedo_GEE = 100000, albedo_ERA5l = 1, LST_GEE = 100, SKT_ERA5l =1) # 
#var_sf_1 <- 1000     # this is the sf according to the MODIS info: https://www.umb.edu/spectralmass/terra_aqua_modis/v006/mcd43a3_albedo_product
#var_off_1 <- 100  # this is the scale factor I added to the GEE script - divide by this


######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%
library(ncdf4) # ncvar_get etc

library(raster) # package for raster manipulation
library(lattice)    # enables levelplots
library(rasterVis)  # enables levelplots
library(lubridate)   # enables date manipulation
library(stringr)

require(ggplot2)
require(magrittr)
require(here)
require(tidyr)

#####     SET I/O PATHS                      #####
# base paths
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)}
#if(! dir.exists(paste0(output_path,'scripts/'))) {dir.create(paste0(output_path,'scripts/'),recursive=T)}

# find date:
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ") ; full_time <- full_date[[1]][2]
full_date <- full_date[[1]][1]
var_name <- paste0( str_split(var, '_' )[[1]][1],'_', str_split(var, '_' )[[1]][2]) # e.g. ET_ERA5l
var_res  <- paste0( str_split(var, '_' )[[1]][3]) #,'_', str_split(var, '_' )[[1]][2]) # e.g. ET_ERA5l

###################################################
######     SET FUNCTIONS                      #####
###################################################

date_to_col <- function(df, inc_year=TRUE, inc_month=TRUE){
  # function takes a dataframe with Z column a date and extracts the month and year component, inserting them as columns
  if(inc_year)df <- df %>% mutate(year = lubridate::year(Z))
  if(inc_month){ df <- df %>% mutate( month = lubridate::month(Z) ) }
  df$Z <- NULL 
  return(df)
}


###################################################
######     MAIN                               #####
###################################################

# albedo_list <- c('bsa_nir', 'bsa_shortwave', 'wsa_nir', 'wsa_shortwave')
albedo_list <- c('bsa_nir', 'bsa_vis', 'wsa_nir', 'wsa_vis')

#loop over the different albedos
for (jj in 1:length(albedo_list) ){
albedo_type <- albedo_list[jj]
######     CREATE DATAFRAME BY YEAR AND TYPE#####
for(year_i in start_year:end_year){
  print(paste0('year is: ', year_i))
  #nc_file <- list.files(path=root_data, pattern=paste0(var, "_", year_i, ".nc"), full.names=FALSE, recursive=FALSE) #paste0("alb*", alb_type, "*",year,"*.tif")
  #file <- paste0(root_data, var, "_", year_i, ".nc")

  df_albedos <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c('x','y','year','month'))))
  

  file <- paste0(root_data, 'MCD43GF_', albedo_type, '_', var_res, '_', year_i, '_monmean.nc')
  
  print(file)
  ncin <- nc_open(file)
  df <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c('x','y','year','month'))))
  
  # loop over variables in the netcdf
  for (variable in names(ncin$var) ){
    #skip time_bnds and QA variables so they do not appear in dataframe
    if(variable == 'time_bnds'){print(paste('skip : ',variable)) ; next}
    #if(variable != 'bsa_nir_QAok' | variable != 'bsa_vis_QAok' | variable != 'wsa_nir_QAok' | variable != 'wsa_vis_QAok'){print(paste('skip : ', variable)); next}
    if( ! grepl('QAall', variable, fixed = TRUE  ) ){print(paste('skip : ', variable)); next}
    if(grepl( 'aggregated', variable , fixed = TRUE) ){print(paste('skip :',variable)) ; next}
    #extract variable stack and relevant info
    ncvar <- ncvar_get( ncin, variable )
    var_units <- ncatt_get(ncin, variable ,"units")$value                   # manually set
    print(paste0 (variable, ', units : ', var_units) )
    var_fillvalue <- ncatt_get(ncin, variable ,"_FillValue")$value  # manually set - seems to be 255
    
    # get stack
    r <- stack( file, varname = variable )
    # function takes a dataframe with Z column a date and extracts the month and year component
    df_variable <- as.data.frame(r, xy = T, na.rm = F, long = T)
    # remove NA values - probably not needed
    df_variable$value[df_variable$value == var_fillvalue ] <- NA ;   df_variable <- na.omit(df_variable)
    print(names(df_variable))
    # add month and year
    df_variable <- date_to_col(df_variable)
    print(names(df_variable))
    # apply scale_factors and offset -> Albedo=(file data - add_offset)*scale_factor - this is not necessary to all nc files with correctly labelled scale factors
    #    var_sf <-   ncatt_get(ncin, variable ,"scale_factor")$value
    #    var_offset <-   ncatt_get(ncin, variable ,"add_offset")$value
    # df_variable$value_sf_int <- ( df_variable$value - var_offset ) / (var_sf)
    
    # additional programmed scale_factors for cross comparison in df
    if(sf[[var_name]] != 1 ){
      print('apply sf ')
      df_variable$value_sf <- df_variable$value*sf[[var_name]]
      if(var_name == "ET_ERA5l"){ df_variable$value_sf <- df_variable$value_sf*mm_list[df_variable$month] } # makes CDS monthly evaporation, rather than daily
      colnames(df_variable)[ which(names(df_variable) == 'value_sf')] <- paste0(variable)
      colnames(df_variable)[ which(names(df_variable) == 'value')] <- paste0(variable, '_preSF')
    } else{
      colnames(df_variable)[ which(names(df_variable) == 'value')] <- paste0(variable)
    }
    
    #round xy
    df_variable$x <- round(df_variable$x, digits = 3) ; df_variable$y <- round(df_variable$y, digits = 3)
    # don't round values as may be small numbers
    #df_variable$value <- round(df_variable$value, digits = 3) ; df_variable$value_sf <- round(df_variable$value_sf, digits = 3) ;
    
    # add variable to the df columns
    print(names(df_variable))
    print('join different variables in df')
    df <- full_join(df, df_variable, by = c('x','y','year','month'))
    print(names(df))
  } # end loop over variables

  print(names(df))
  print('join different albedos in df_albedos')
  #df_albedos <- full_join(df_albedos, df, by = c('x','y','year','month'))
  #df <- df_albedos
  
  # save dataframe
  print('save df')
  save( df , file=paste0(output_path, 'df_', var, '_', albedo_type, '_', year_i,'.RData') )
  
} # end year loop

} # end loop over albedos

# df_all <- NULL
# # now create one extra large dataframe
# for(year_i in start_year:end_year){
#   load( file=paste0(output_path, 'df_', var, '_', year_i,'.RData') )
#   df_all <- rbind(df_all, df)
#   
# }
# save( df_all , file=paste0(output_path, 'df_', var, '_allY.RData') )

######     FINALISE                           #####
# timing
end_time <- Sys.time()
print(end_time - start_time)

# file_bsa_nir <- paste0(root_data, 'MCD43GF_bsa_nir_', var_res, '_', year_i, '_monmean.nc')
# file_bsa_shortwave <- paste0(root_data, 'MCD43GF_bsa_shortwave_', var_res, '_', year_i, '_monmean.nc')
# file_wsa_nir <- paste0(root_data, 'MCD43GF_wsa_nir_', var_res, '_', year_i, '_monmean.nc')
# file_wsa_shortwave <- paste0(root_data, 'MCD43GF_wsa_shortwave_', var_res, '_', year_i, '_monmean.nc')
# print(file_bsa_nir)
# print(file_bsa_shortwave)
# print(file_wsa_nir)
# print(file_wsa_shortwave)
# ncin_bn
# ncin_bs <- nc_open(file_bsa_shortwave)
# ncin_wn <- nc_open(file_wsa_nir)
# ncin_ws <- nc_open(file_wsa_shortwave)
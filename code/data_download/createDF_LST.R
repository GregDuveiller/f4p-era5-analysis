# original name : createDF_LST.R
#
# ########################################################
# Title         : 
# Description   : This code opens the albedo .tif files and exports the data to dataframes with 0.1 and 0.25 res
#                 the dataframes will cover the full time period and also have information on the number of clear - LST days
#                 (the data itself is an average of the top 5 warmest days)
#                 Scale factor must be directly applied to albedo (cannot be sourced from .nc as no .nc file)
# Inputs	      : /home/mark/Documents/work_jrc/work_d1/work_COPERNICUSII/data/albedo/LST
#                 
# Outputs	      : dataframes
#                 /mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/COPERNICUSII/data/albedo/dataframe (later moved to COPII_v2)
#                 
# Options	      : 
# Date          : 08/09/20
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
script_name <-'createDF_LST.R'               # name of the script
script_info <-'LST_GEE'                 # 
var_name <- 'LST'
start_year <- 2014 ; end_year <- 2019 #18      # just hardcode year range for now
sf <- 1     # this is the sf according to the LST inf - no scale factor
sf_int <- 100  # this is the scale factor I added to the GEE script before converting data to int - divide by this 


######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%

library(raster) # package for raster manipulation
library(lattice)    # enables levelplots
library(rasterVis)  # enables levelplots
library(lubridate)   # enables date manipulation
library(stringr)
# requred for JEODPP
library("rgdal",lib="~/R") #  personally installed library(gdal) #
#require(ggplot2)
#require(magrittr)
#require(here)
require(tidyr)

#####     SET I/O PATHS                      #####
root_data   <- '/mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/COPERNICUSII/data/GEE_LST/data/GEE/'  #'/home/mark/Documents/work_jrc/work_d1/work_COPERNICUSII/data/LST/GEE/'
output_path <- '/mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/COPERNICUSII/data/GEE_LST/data/dataframe/' # '/home/mark/Documents/work_jrc/work_d1/work_COPERNICUSII/data/LST/dataframe/'
# base paths
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)}
#if(! dir.exists(paste0(output_path,'scripts/'))) {dir.create(paste0(output_path,'scripts/'),recursive=T)}

# find date:
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ") ; full_time <- full_date[[1]][2]
full_date <- full_date[[1]][1]

###################################################
######     SET FUNCTIONS                      #####
###################################################
stack_to_df <- function(stack, year=year_i, month=month_i, apply_sf = FALSE ){
  # function takes a dataframe with Z column a date and extracts the month and year component
  df <- as.data.frame(stack, xy = T, na.rm = T) #, long = T)
  # it seems all NA values are counted as zero - so we should remove values of zero 
  #df$value[df$value == 0] <- NA ;   df <- na.omit(df)
  # apply the scale factors (GEE and MODIS) to the data
  if (apply_sf ){  df[names(df)[3]] <- df[names(df)[3]]/(sf_int*sf ) }
  df[names(df)[3]] <- round(df[names(df)[3]], digits = 3) ; #df$value_sf <- round(df$value_sf, digits = 3) ;
  df$x <- round(df$x, digits = 3) ; df$y <- round(df$y, digits = 3) 
  # add month and year cols
  #df$layer <- NULL # remove col
  df <- df %>% mutate(year = year_i )
  df <- df %>% mutate( month = month_i )

  return(df)
}



###################################################
######     MAIN                               #####
###################################################

######     CREATE DATAFRAME BY YEAR AND TYPE#####
for(year_i in start_year:end_year){  
print(paste0('year is: ', year_i))
  # loop over the files in that year - # one is the monthly temperatures, the other is the number of temperature entries per month
  file <- list.files(path=root_data, pattern=paste0("lst_scl5_", year_i, "_1-12_x100int16.tif"), full.names=FALSE, recursive=FALSE) 
  file_entries <- list.files(path=root_data, pattern=paste0("lstEntries_scl5_", year_i, "_1-12.tif"), full.names=FALSE, recursive=FALSE) 
  print(file); print(file_entries)
    # extract rasters for the months in each dataset
  r <- stack( paste0(root_data,file) )
  r_entries <- stack( paste0(root_data,file_entries) )
  #df_brick <- data.frame()
    #df_i_005 <- data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL, c('x','y','year','month', 'entries', 'value')))) #, 'value_sf'
    df_i_01 <- data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL, c('x','y','year','month', 'LST_count', 'LST_max5_mean')))) #, 'value_sf'
    df_i_025 <- data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL, c('x','y','year','month', 'LST_count', 'LST_max5_mean')))) #, 'value_sf'
      
    for( i in 1:12){
      month_i <- i
      print(month_i)
      stack_i <- r[[names(r)[i]]]
      stack_i_entries <- r_entries[[names(r_entries)[i]]]
      # aggregate to 0.25 and 0.1 stack
      #res(stack_i)
      # zero temperatures must convert to NA to ensure the aggregation does not artificially lower the temperatures (e.g. at coastlines)
      values(stack_i)[values(stack_i) == 0] = NA
      stack_i_01 <- aggregate(stack_i, fact=2, na.rm=TRUE)
      stack_i_025 <- aggregate(stack_i, fact=5, na.rm=TRUE)
      # with the nEntries stacks we do not need to remove zero entries as we want the total sum of entries that went into a aggregated pixel,
      # divided by the number of pixels aggregated. I think this means temperatures aggregated from some NA temperature pixels will have the 
      # correct temperature, but will have lowered mean # entries, and can therefore be trusted less (which is OK - we don't want to remove 0s
      # as they are physically possible and we would bias the count - saying that all points have some data - basically then we take the count as 
      # a confidence measure on the aggregated points)
      # it may downgrade the temperatures at coasts though...
      stack_i_01_entries <- aggregate(stack_i_entries, fact=2, na.rm=TRUE)
      stack_i_025_entries <- aggregate(stack_i_entries, fact=5, na.rm=TRUE)
      #rm(stack_i)
      
      # get the dataframe versions of the stacks. The df_entries will be much larger as it contains the zero-valued points (mostly sea)
      # we can therefore do a left_join onto df, as we only need num_entries for valid LST observations
      df_temp <- stack_to_df(stack_i_01, month = month_i, year = year_i, apply_sf = TRUE)
      df_entries <- stack_to_df(stack_i_01_entries, month = month_i, year = year_i, apply_sf = FALSE )
      names(df_temp)[3] <- 'LST_max5_mean' ; names(df_entries)[3] <- 'LST_count'
      df_temp <- df_temp %>% left_join(df_entries)
      print(names(df_i_01))
      print(names(df_temp))
      df_i_01 <- rbind(df_i_01, df_temp)
      
      rm(df_temp); rm(df_entries)
      # test
      #df_005 <- stack_to_df(stack_i, month = month_i, year = year_i )
      #df_005_entries <- stack_to_df(stack_i_005, month = month_i, year = year_i )
      
      df_temp <- stack_to_df(stack_i_025, month = month_i, year = year_i, apply_sf = TRUE)
      df_entries <- stack_to_df(stack_i_025_entries, month = month_i, year = year_i, apply_sf = FALSE )
      names(df_temp)[3] <- 'LST_max5_mean' ; names(df_entries)[3] <- 'LST_count'
      df_temp <- df_temp %>% left_join(df_entries)
      print(names(df_i_025))
      print(names(df_temp))
      df_i_025 <- rbind(df_i_025, df_temp)
      
    }
  # combine all the months into a df of a given albedo type over a given year
  
  # rename column
  
  # round before saving
  df_i_01$x <- round(df_i_01$x, digits = 3) ; df_i_01$y <- round(df_i_01$y, digits = 3)
  df_i_025$x <- round(df_i_025$x, digits = 3) ; df_i_025$y <- round(df_i_025$y, digits = 3) 
  
  df <- df_i_01  
save( df , file=paste0(output_path, 'df_', script_info, '_01_' ,  year_i,'.RData') )
df <- df_i_025  
save( df, file=paste0(output_path, 'df_', script_info, '_025_', year_i,'.RData') )

} # end year loop
  
  
######     FINALISE                           #####
# timing
end_time <- Sys.time()
print(end_time - start_time)

  

  
###################################################
######     PSUEDO CODE                        #####
###################################################

# For a given year
  # run over the files in a given year
    # extract the stacks for the tif and add into a dataframe of all results (taking the month)
  # get the dataframe for KG etc
  #
  

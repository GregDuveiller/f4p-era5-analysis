# original name : createDF_SKT_JEODPP.R
#
# ########################################################
# Title         : 
# Description   : This code opens the ERA5 skin temperature files. For each pixel it selects the hottest 5 days of each month 
#                 and then takes an average and exports the data .nc files
# Inputs	      : /eos/jeodpp/data/base/Meteo/GLOBAL/ECMWF/Reanalysis/ERA5-Land/VER1-0/Data/NetCDF/skin_temperature/
#                 it uses an input directory and user inputs the year of interest to run over all the files
# Outputs	      : netcdf of each month for each year showing the different months and the top5 average
#                 /mnt/cidstorage/cidportal/data/cid-bulk22/Shared/projectData/COPERNICUSII/data/CDS_data/SKT_ERA5_land/data/nc_for_df/
#                 
# Options	      : 
# Date          : 16/09/20
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

script_name <-'createDF_SKT_JEODPP.R'               # name of the script
script_info <-'skt_ERA5_top5monthlyAvg_finalCDO'                 # 
proc <- 'skt'
#sf <- 1     # this is the sf according to the LST inf - no scale factor
#sf_int <- 100  # this is the scale factor I added to the GEE script before converting data to int - divide by this 
hour_satellite <- 14 # this acts as a double check on the hour of passage
file_year <- 2019 # insert year to loop over


######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%

library(raster) # package for raster manipulation
library(lattice)    # enables levelplots
library(rasterVis)  # enables levelplots
library(lubridate)   # enables date manipulation
library(stringr)
library(ncdf4)  # open manipulate netcdf files

#require(ggplot2)
#require(magrittr)
#require(here)
require(tidyr)

###################################################
######     FUNCTIONS                          #####
###################################################

############### COLLATE ARGUMENTS ######################
# arg = commandArgs(trailingOnly=TRUE)
# # test if there is at least one argument: if not, return an error
# if (length(arg)==0 || length(arg)>1 ) {
#   #arg <- 'cds_era5_land_skin_temperature_2015.nc' # for eos
#   arg <- 'ST_2019_mixmonthsdaystimes_spain.nc' # default - remove eos
#   print(paste0('no argument supplied, using default:', arg))
#   #stop("One argument must be supplied (input file).nc", call.=FALSE)
# } else {#if (length(args)==1) {
#   # default output file
#   #args[2] = "out.txt"
#   print(paste0("There is/are ", length(arg), " input files:"))
#   print(arg)
# }
#current_file <- args[1]


###################################################
######     INPUT/OUTPUT                       #####
###################################################

#root_data   <- '/home/mark/Documents/work_jrc/work_d1/work_COPERNICUSII/data/CDS_skin_temperature/scripts/'
# on eos - from source
# root_data <- '/eos/jeodpp/data/base/Meteo/GLOBAL/ECMWF/Reanalysis/ERA5-Land/VER1-0/Data/NetCDF/skin_temperature/'7
# on eos - from reduced files:
root_data <- '/mnt/cidstorage/cidportal/data/cid-bulk22/Shared/projectData/COPERNICUSII/data/CDS_data/SKT_ERA5_land/data/nc_separated_month/'
#output_path <- '/home/mark/Documents/work_jrc/work_d1/work_COPERNICUSII/data/CDS_skin_temperature/scripts/'
#eos from source
output_path <- '/mnt/cidstorage/cidportal/data/cid-bulk22/Shared/projectData/COPERNICUSII/data/CDS_data/SKT_ERA5_land/data/nc_for_df/'

# create output directory
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)}


###################################################
######     MAIN                               #####
###################################################

################################ Open the file in question #########################
# loop over the files
files <- list.files(path=root_data, pattern=paste0("cds_era5_land_skin_temperature_", file_year,"_t1400_mon*"), full.names=FALSE, recursive=FALSE) 

for (input_proc_filename in files) {
#input_proc_filename  <- arg
  
# get month
# find date:
mon_i <- input_proc_filename
mon_i <- strsplit(mon_i, "_")[[1]][8] ; mon_i <- strsplit(mon_i, ".nc")[1] ; mon_i <- substr(mon_i, 4, 6)

  

print(paste0('input_proc_filename: ',input_proc_filename))
#input_proc_file <- paste0(root_data_proc,input_proc_ext,input_proc_filename) #GPP
input_proc_file <- paste0(root_data, input_proc_filename) 
print(paste0('input path : ', input_proc_file))
proc_object <- nc_open(input_proc_file)
proc_object_size <- (object.size(proc_object))
print('proc_object size') ; print(proc_object_size, units = "auto")

## get attributes
nc_atts <- ncatt_get(proc_object,0)
## get dimensions
proc_units <- ncatt_get(proc_object,proc,'units')
proc_lat <- ncvar_get(proc_object,"latitude") ;  proc_lon <- ncvar_get(proc_object,"longitude")
proc_time <- ncvar_get(proc_object,"time")
len_lons <-length(proc_lon); len_lats <-length(proc_lat); len_tims <-length(proc_time)
print('length of lons, lats, and times in input file: '); print(len_lons) ; print(len_lats) ; print(len_tims) 


############# extract the date and time of each slice of the file #############
## reformat the time units to create proc_time_dates vector of actual dates:
proc_time_units <- ncatt_get(proc_object,"time","units")
## convert time -- split the time units string into fields
tustr <- strsplit(proc_time_units$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-") ; days_or_hours <- tustr[[1]][1]
# extract origin date of .nc
tmonth <- as.integer(unlist(tdstr)[2]) ; tday <- as.integer(unlist(tdstr)[3]) ; tyear <- as.integer(unlist(tdstr)[1])
# chron calculates days difference (must convert to days if units are hours difference) - if not days or hours, break
if(days_or_hours == 'hours'){   proc_time <- proc_time/24 }else if(days_or_hours != 'days'){  stop("Incorrect date format", call.=FALSE)}
proc_time_dates <- chron(proc_time,origin=c(tmonth, tday, tyear))
#print('dates in file are:') ; proc_time_dates
print('test the list of times to run over: ')
for(tims in 1:20 ){
  print(chron::hours(proc_time_dates[tims]) )
}

############# extract starting/ending index of each month from the proc_times_dates  #############
# # we have a huge array of times that we want to loop over (as an index) but we need to extract the indices relevant to that month (mon_i)
# # and relevant to satellite bypass time (14)
# # Ideally we want to save the start and end indices of the month (to start and end the counting) and we want to remove 14 as we go through
# print('get start index')
# index_starts <- c( NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, length(proc_time_dates) + 1)
# index_months <- c( 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
# times_array <- c( NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
# # loop over the indices and extract the month start dates where available
# for(i in 1:length(proc_time_dates)){
#   # get the month
#   mon_name <- months(proc_time_dates[i])
#   mon_j <-  match(mon_name, index_months)
#   #print(i) ; print(mon_i[1]) 
#   if( is.na(index_starts[mon_j]) ){ index_starts[mon_j] <- i ; times_array[mon_j] <- proc_time_dates[i]}
# }
# print('index_starts for looping over each month (start, end+1)')
# print(index_starts)
# print('times_array for months')
# print(chron(times_array,origin=c(tmonth, tday, tyear)) )




#################### Main work ##########################

  ########### get and edit the proc variable to fill with NAs ############
  print('open proc_object')
  proc_array  <- ncvar_get(proc_object, proc)
  proc_array_size <- (object.size(proc_array))
  print('proc_array size')
  print(proc_array_size, units = "auto")
  #head(proc_array); dim(proc_array); proc_array[1:10,1:10,1:2]
  
  print('fillvalues')
  proc_fillvalue  <- ncatt_get(proc_object, proc,'_FillValue')
  proc_array[proc_array==proc_fillvalue$value] <- NA
  proc_array[proc_array==1e+20] <- NA
  
  
  ################# initialise output netcdf ###############
  print('create dimensions')
  londim <- ncdim_def("longitude","degrees_east",as.double(proc_lon)) 
  latdim <- ncdim_def("latitude","degrees_north",as.double(proc_lat)) 
  
  ########## create output array of top values ########
  # output array has regular dimensions, and one time dimension for each month (can later join them)
  output_array  <- array(data = NA, dim = c(len_lons, len_lats, 1))

  ########## loop over pixels ##############
  print('looping over proc entries')
  for(lons in 1:len_lons) {
    if(lons %% 50==0) {print(lons)}
    #if(lons %% 100!=0) {next}
    for(lats in 1:len_lats) {
      #if(lats %% 100!=0) {next}
      #if(lons==1 && lats == 1) {print(lons); print(lats)}
        # create an empty array of the temperatures in that month
        mon_temps <- rep(NA,31) ; day_count <- 1     
        #mon_entry_dates <- rep(NA,31) 
        #for(tims in index_starts[mon_i]:(index_starts[mon_i+1] -1) ){ # loop over only time_array within month (superseeded by month loop)
        for(tims in 1:(dim(proc_array)[3]) ){ # loop over all time_array
        #for(tims in seq(15,dim(proc_array)[3],24) ){ # for eos - loop over only values at 1400
        # for(tims in seq(4,dim(proc_array)[3],6) ){ # for local
          # skip times != 1400 satellite flyby
          #if(lons %% 10==0 && lats %% 50 ==0 &&)print(chron::hours(proc_time_dates[tims]))
          if(chron::hours(proc_time_dates[tims]) != hour_satellite) { print('Error: cycled over time != 14:00'); print(chron::hours(proc_time_dates[tims])) ; next}
          # obtain the skt at this lon/lat/time
          proc_val <- proc_array[lons,lats,tims]
          ##if(lons ==1 && lats == 1) {print(lons) ; print(lats) ; print(tims) ; print(proc_time_dates[tims]) #} print(proc_val)
          #add the temperature to the monthly collection at the index denoting number of days in month
          mon_temps[day_count] <- proc_val
          #mon_entry_dates[day_count] <- proc_time_dates[tims]
          #increment the number of days we have for each month (should equal the days in the month if no missing days)
          day_count <- day_count + 1 # count the number of days we place in each month        
        }
        #if(lons==2000 && lats == 2000) {
        #print('time taken are: '); print(mon_entry_dates)#}
        # select the top 5 temperatures
        top5_avg <- mean(tail(sort(mon_temps),5), na.rm = TRUE)
        output_array[lons,lats,1] <- top5_avg
#        if(lons %% 300==0 && lats %% 300 ==0 ) {print('month temperatures are: '); print(mon_temps) ; print('top5avg') ; print(top5_avg) }
      
    } # end loop lats
  } # end loop lons
  print('completed loop over lat/lon within month')
  output_name <- paste0(script_info, '_', file_year, '_', mon_i, '.nc' )
  output_file      <- paste0(output_path,output_name) 
  

############# FILL THE OUTPUT NC   ###############

# take the time as the first entry in the month
timestamp_array <- proc_time[1]
print(paste0('timestamp array : ', timestamp_array))
print(chron(timestamp_array,origin=c(tmonth, tday, tyear)) )
if(days_or_hours == 'hours'){   timestamp_array <- timestamp_array*24 }else if(days_or_hours != 'days'){  stop("Incorrect date format", call.=FALSE)}
print('timestamp_array after change to hours')
print(chron(timestamp_array,origin=c(tmonth, tday, tyear)) )
timdim <- ncdim_def("time",proc_time_units$value,as.double(timestamp_array))
fillvalue <- 1e20 #proc_fillvalue # negative values causing problems
def_proc  <- ncvar_def(paste0(proc,"_top5avg"),'Skin temperature monthly average of top 5',list(londim,latdim,timdim),fillvalue,'Skin temperature monthly average of top 5',prec="single")

# create netCDF file
print(paste0('create netcdf file: ', output_file))
nc_output <- nc_create(output_file,list(def_proc),force_v4=TRUE)
# put variables from arrays

print('insert variables into netcdf')
ncvar_put(nc_output, def_proc, output_array)
# put additional attributes into dimension and data variables
print('insert attributes into netcdf')
ncatt_put(nc_output,"longitude","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(nc_output,"latitude","axis","Y")
ncatt_put(nc_output,"time","axis","T")
ncatt_put(nc_output, 0, 'title', paste0(proc," average of top 5 monthly values at ", hour_satellite))
ncatt_put(nc_output, 0, 'date_created', date());
#add the attributes from the input file
ncatt_put(nc_output, 0, 'summary', nc_atts$summary); ncatt_put(nc_output, 0, 'keywords', nc_atts$keywords)
ncatt_put(nc_output, 0, 'references', nc_atts$references); ncatt_put(nc_output, 0, 'spatial_resolution', nc_atts$spatial_resolution); 
ncatt_put(nc_output, 0, 'cdm_data_type', nc_atts$cdm_data_type); ncatt_put(nc_output, 0, 'cdm_data_type', nc_atts$cdm_data_type);
ncatt_put(nc_output, 0, 'history', paste0(date(), ': Mark Pickering ',script_name,'\n', nc_atts$history))
#output_atts <- ncatt_get(nc_output,0)
#output_atts$history
print('close netcdf')
nc_close(nc_output) # close the output file


end_time <- Sys.time()
print(paste0('completed another file_loop: ', (end_time - start_time) ) )
nc_close(proc_object) # close the input file
} # end file loop 
print('finish looping over files')



######     FINALISE                           #####
# timing
end_time <- Sys.time()
print(end_time - start_time)

###################################################
######     PSUEDO CODE                        #####
###################################################

# do we loop over pixels first or time slices first? - maybe do by month?

# open the big file of temperatures.
# create a year slice of the 12 month and latxlon
# Loop over months
  # create empty array for monthly_top5_avg
  # loop over each pixel
    # create a list of 5 NAs
    # loop over the days in that month
      # select only 1400 time slice
      # select the top 5 days in the month and add to list (if greater than existing)
    # average the list for each pixel
  # get the month slice showing the avg(top5) - add to a year slice
# save the year slice (12 months - every pixel 005x005)
# aggregate to 0.1 and 0.25 and save also


# 
# ############# depricated
# month_name <- month_list[mon_i]
# # extract only the dates of proc_time_dates that occur in month mon_i and at time 1400
# date_vec <- c()
# for (j in 1:length(proc_time_dates)){
#   if(chron::hours(proc_time_dates[j]) == hour_satellite && months(proc_time_dates[j]) == month_name){
#     date_vec <- c(date_vec, proc_time_dates[j])
#     #print(proc_time_dates[j])
#   }
# }
# 
# # remove times!=14:00 and months that are not being looped over currently
# # run over the dates in that month and check 
# #chron(
# sapply(y, f_accept_month_hour,  accept_hour = 14, accept_month = 1) 
# 
# #, origin=c(tmonth,tday,tyear))
# for(mon_i in 1:12){
#   print(mon_i)
#   index_start <- index_starts[mon_i] # select the start of this month (calculated previously)
#   month_name <- month_list[mon_i]
#   print(month_name)
#   #loop over the array of dates in the file
#   for (j in index_start:length(proc_time_dates)){
#     print(j)
#     # get the end index of the next month
#     if( months(proc_time_dates[j]) != month_name ){ index_starts[mon_i+1] <- index_start + j}
#   }# loop over proc_time_dates
# } #loop over months

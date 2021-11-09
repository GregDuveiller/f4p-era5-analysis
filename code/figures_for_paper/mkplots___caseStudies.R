# original name : prepare_caseStudies.R
#
# ########################################################
# Title         : 
# Description   : This code aims to make map/hist of a) values of the sim/obs variables during HW event  b) long-term mean values of sim/obs across multi-year
#                                                    c) difference between sim(obs) and long-term mean   d) z-score for the variables between heatwave and LT-mean
#                                                    e) bias (ERA5-obs) during HW                        f) bias (ERA5-obs) average
#                                                    g) z-score of bias (ERA5-obs) during HW 
#                                                    h) scatter of bias                                  i) scatter of z-score of bias
#                 For different input months and x,y coordinate bounds. It should also plot the long term mean etc between dates.
# Inputs	      : come from prepare__caseStudies_file
#                 
# Outputs	      : figures & histograms as above
#                 figures going into final paper are marked as **** FIGURE GOES INTO FINAL PAPER ****
#                 
# Options	      : 
# Date          : 08/11/21
# Version       : 1.3 - this is updated for figures in paper
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
# choose variables/parameters for script - GREG select here the case study you want to plot

# select which case_study to plot
## 2003 European hw
case_study <- 'hw2003' ; print('2003 heatwave')
##  2010 Russia hw
# case_study <- 'hw2010' ; print('2010 heatwave')
# # 2018 Europe hw
# case_study <- 'hw2018' ; print('2018 heatwave')

case_study_dir <- paste0('/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/data/COP_caseStudies/', case_study, '/data/') # replace with git data directory
#case_study_dir <- paste0('/media/mark/HD/Mark/Mark_COPERNICUS/figures/COPERNICUSII_V3/COP_caseStudies/', case_study, '/data/')      # Mark local load data
output_root <- paste0('/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/data/COP_caseStudies/')
#output_root  <- '/media/mark/HD/Mark/Mark_COPERNICUS/figures/COPERNICUSII_V3/COP_caseStudies/' # Mark local output


# shape files from https://www.naturalearthdata.com/downloads/50m-physical-vectors/
shape_path <- '/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/data/shape_files/'
# shape_path <- '/home/mark/Documents/work_jrc/work_d1/work_SIF/data/vector_shapefiles/50m_coastline/'
# coast_shapefile <- paste('/home/mark/Documents/work_jrc/work_d1/work_SIF/data/vector_shapefiles/50m_coastline/ne_50m_coastline.shp')


######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%

library(raster) # package for raster manipulation
library(lattice)    # enables levelplots
library(rasterVis)  # enables levelplots
#library(lubridate)   # enables date manipulation

require(ggplot2)
require(magrittr)
require(here)
require(tidyr)

require(scales)
library(viridis)
library(RColorBrewer)

# for the maps
library(sp)
library(rgdal)
library(sf)         # (Greg maps) for adding maps to ggplot

# library(maptools)
# library(rgeos)
# library(classInt)
library(RColorBrewer)
library(nlme)      # for lmList
require(ggpubr)     # for multiple ggplots on one plot and adding lm to plots with stat_regline_equation

# load basic functions, e.g. maps
# root_script <- '/home/mark/Documents/work_jrc/work_d1/work_SIF_V2_ancova/part_1_landcover_only/200901_v1_anova/scripts/'
# root_script <- '/home/mark/ownCloud/copernicus/scripts/git_mp_fork/f4p-era5-analysis/code/'  # <---- df_cz
# functions_path <- paste0(root_script, 'function___misc.R')
# source(functions_path)


###################################################
######     FUNCTIONS                          #####
###################################################

NiceNumbers=c(0,1,1.5,2,3,4,5,6,7,8,9,10)
roundUpNice <- function(x, nice = NiceNumbers) { #https://rdrr.io/cran/subscreen/src/inst/shiny-app/subscreen/app.R
  if (length(x) != 1) stop("'x' must be of length 1")
  if (x >= 0) 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) *nice)[[1]]]]
  else -1 * (roundDownNice(-x, nice = NiceNumbers))
}

roundDownNice <- function(x, nice = NiceNumbers) {
  if (length(x) != 1) stop("'x' must be of length 1")
  if (x >= 0) 10^floor(log10(x)) * nice[[max(which(x >= 10^floor(log10(x)) * nice))]]
  else -1 * (roundUpNice(-x, nice = NiceNumbers))
}

############### Create maps
# download here: https://www.naturalearthdata.com/downloads/50m-physical-vectors/
# extract shapefiles - workstation links
# shape_path <- '/ESS_Datasets/USERS/Pickering/data/data_climate_LU/VectorMaps/'
# coast_shapefile <- paste(shape_path, "global_50m_coastline/ne_50m_coastline.shp", sep="")
# bb_shapefile <- paste(shape_path, "global_50m_graticules_boundingbox/ne_50m_wgs84_bounding_box.shp", sep="")

# extract shapefiles - laptop links
# shape_path <- '/home/mark/Documents/work_jrc/work_d1/work_SIF/data/vector_shapefiles/50m_coastline/'
# coast_shapefile <- paste('/home/mark/Documents/work_jrc/work_d1/work_SIF/data/vector_shapefiles/50m_coastline/ne_50m_coastline.shp')
# local
coast_shapefile <- paste0(shape_path, "ne_50m_coastline.shp")
bb_shapefile <- paste(shape_path, "ne_50m_wgs84_bounding_box.shp", sep="")
grat30_shapefile <- paste(shape_path, "global_50m_graticules_boundingbox/ne_50m_graticules_30.shp", sep="")
# local 
grat30_shapefile <- paste(shape_path, "ne_50m_graticules_30.shp", sep="")

#convert to lines
layer <- ogrListLayers(coast_shapefile)
ogrInfo(coast_shapefile, layer=layer)
coast_lines <- readOGR(coast_shapefile, layer=layer)# read the shape file
layer <- ogrListLayers(bb_shapefile)
ogrInfo(bb_shapefile, layer=layer)
bb_poly <- readOGR(bb_shapefile, layer=layer)
bb_lines <- as(bb_poly, "SpatialLines")
layer <- ogrListLayers(grat30_shapefile)
ogrInfo(grat30_shapefile, layer=layer)
grat30_lines <- readOGR(grat30_shapefile, layer=layer)

# get CRS
unproj_proj4string <- proj4string(coast_lines)
unproj_proj4string  # this is the unprojected CRS



######################

# input a df and the obs_name/sim_name/obs_name_mean/sim_name_mean etc as well as nice numbers etc
# output a nice bounding range
f_get_min_max_nice <- function(df,obs_name,sim_name,obs_mean_name, sim_mean_name){ 
  # set the min and max values on the histograms, as well as the y_max
  min_scale_r_hist <- min( c(min(df[[obs_name]], na.rm = T), min(df[[sim_name]], na.rm = T), min(df[[obs_mean_name]], na.rm = T), min(df[[sim_mean_name]], na.rm = T) ) ) ;
  max_scale_r_hist <- max( c(max(df[[obs_name]], na.rm = T), max(df[[sim_name]], na.rm = T), max(df[[obs_mean_name]], na.rm = T), max(df[[sim_mean_name]], na.rm = T) ) ) ; 
  print(min_scale_r_hist) ; print(max_scale_r_hist)
  min_scale_r_hist <- roundDownNice(min_scale_r_hist) ; max_scale_r_hist <- roundUpNice(max_scale_r_hist)    # use min of either col
  print(min_scale_r_hist) ; print(max_scale_r_hist)
  # set the min and max values to use as the scales for the maps if not forced
  min_scale_r <- min_scale_r_hist  ; max_scale_r <- max_scale_r_hist ; # max_y <- dim(df)/3  # use same scales as hist
  
  return( c(min_scale_r, max_scale_r, min_scale_r_hist, max_scale_r_hist) )
}



# function to print the obs and sim of a dataframe (with distributions)
# dataframe should be x, y, year, month, obs, sim format
# labels figures by col_name column names [5, 6] 
f_map_xyt <- function(df, col_name) {
  
  # levelplot settings
  cutpts_r <- seq(min_scale_r,max_scale_r, (max_scale_r-min_scale_r)/divisions) 
  plot_name_base <- paste0(case_study, '_', plot_info, '_', col_name)
  

  # set values below min or above max to the min max values
  df <- df %>% mutate( !!as.symbol(col_name) := ifelse( ( !!as.symbol(col_name) > max_scale_r) , max_scale_r,  ifelse( ( !!as.symbol(col_name) < min_scale_r), min_scale_r, !!as.symbol(col_name)  ) ) )# recalibrate out of bounds values to max val for levelplot
  
  # put df in x,y,z format
  dfr_r <- df[,c(1,2)] ; dfr_r[col_name] <- df[col_name]
  dfr_r <- rasterFromXYZ(dfr_r)
  
  # aggregate pixels if required
  if(agg_fact != 1){
    print('aggregate')
    agg_fact <- 4 # 3 factor for no aggregation # 4 for some aggregation
    dfr_r_agg <- aggregate(dfr_r, agg_fact, fun=mean, expand=TRUE, na.rm=TRUE) ## as we have fewer pixels now (unfilled) we should aggregate - making sure to remove NA values
  }
  
  # make levelplot
  plot_name <- paste0(plot_name_base, '_r') ; print(plot_name)
  png(file = paste0(output_path , plot_name,'.png'), width=width, height= height )
  crs(dfr_r) <- unproj_proj4string
  plt <-   levelplot(dfr_r, margin=F, at = cutpts_r,  xlab=list("longitude", cex=cex_size), ylab=list("latitude", cex=cex_size), cuts = length(cutpts_r), pretty = F, #main = col_name, # main = paste0('Temporal correlation, r, between mean ', x_var ,' and ', y_var, ' over the first growing season of the year [2007-2014]'), # , vjust = -.1)
                     par.settings=mapTheme, maxpixels = n_pix,  # at = cutpts_r
                     main = list(legend_title, cex=cex_size),
                     # par.settings = list(layout.heights=list(xlab.key.padding=1)), # try and add padding to plot
                     colorkey = list(  labels=list(cex=cex_size), labels=as.character( cutpts_r), 
                                       space = "top", # vjust = -.5 #  position
                                       height = 0.5 # width = 0.5
                                       #title=list('SIF', cex=40)  # legend title cant change size...
                     ) , # at=as.numeric( factor( c( seq(-1, 1, by=0.2), "NA"))) )  # key size and cut ticks #  tick.number = length(cutpts_r),
                     scales=list(x=list(cex=cex_size),y=list(cex=cex_size)) # change x-y label tick size
  ) 
  #print( plt + latticeExtra::layer(sp.lines(coast_lines, col="black", lwd=0.5))  )
  #dev.off()
  plt_layer <- plt  + latticeExtra::layer(sp.lines(coast_lines, col="black", lwd=0.5))
  print(  plt_layer )
  
  # # add the custom legend 
  # grid.newpage()
  # print(  plt_layer )
  # #lp_grob <- as.grob(plt_layer) #lp_grob <- as.grob(plt) #lat_grob <- as.grob(plt_lat)
  # g_grob <- as.grob(h_dist)
  # #plot(lp_grob) #grid.draw(lp_grob)  #grid.draw(lp_lat)
  # # vp_size = 0.25 ; vp = viewport(x=.20, y=.25, width=vp_size*3/4, height=vp_size) # big maps
  # vp_size = 0.34 ; vp = viewport(x=.25, y=.31, width=vp_size*3/5, height=vp_size) # small maps
  # pushViewport(vp)
  # grid.draw(g_grob)
  
  dev.off()
  
}

# basic histogram
f_hist <- function(df, col_name) {
  # create histogram
  h_dist_obs <- ggplot(df, aes_string(x = sym(col_name) )) + 
    geom_histogram( bins = n_bins , colour = "black", fill='black') + #  # ..density.. breaks = seq(-1, 1, by = 10), aes_string(y =rescaled_hist_density, colour = cat_a)
    #stat_function(fun = dnorm, args = list(mean = mean(model.metrics_int$.resid), sd = sd(model.metrics_int$.resid)))
    theme_classic() +
    scale_y_continuous(limits = c(0, max_y)) +
    scale_x_continuous(limits = c(min_scale_r_hist, max_scale_r_hist), oob = scales::squish) +
    theme(text = element_text(size = text_size)) +          
    labs( y= paste0( 'Frequency'), #title= paste0( gsub('_',' ', x_var), ' Vs ', gsub('_',' ', y_var),' per pixel for 1st GS of year'),
          x= paste0(gsub('_',' ', col_name))       )
}

f_scatter <- function (df, col_name_x, col_name_y, label_x = col_name_x, label_y = col_name_y, x_scale = NA, y_scale = NA, lm_type = 'false' ){
  ### plot an overall scatter (no separation by time)
  
  # set density
  g_scatter <- ggplot(df, aes_string(x = col_name_x, y = col_name_y)) +
    stat_bin2d(bins= c(n_bins,n_bins) , aes(fill = ..ndensity..), na.rm = TRUE) +
    scale_fill_gradientn(colors = r, name = "Relative distribution") + #, limits=c(min_scale_r_hist, max_scale_r_hist), breaks=seq(0,1,0.1)) + #, trans = "log",limits=c(1, 10000), breaks=c(1,3,10,30,100,300,1000,3000,10000) ) + # 
    #geom_text(data=total, aes(x=2, y=-0.8, label=n), colour="black", inherit.aes=FALSE, parse=FALSE) + #stat_n_text() +
    geom_hline(yintercept=0, lwd=1, lty=2, col="grey60" ) + geom_vline(xintercept=0, lwd=1, lty=2, col="grey60" ) + 
    #theme(legend.position="top", legend.key.width = unit(legend_size, "cm")) +
    labs(title= paste0(legend_title), x = label_x, y = label_y ) + # , ' n = ', nrow(df_map)), y= col_name_y, x= col_name_x ) +
    theme_grey(base_size = 12) + 
    theme(
      legend.position = "bottom",
      text = element_text(size=rel(rel_txt_size*3/4)),
      strip.text.x = element_text(size=rel(rel_txt_size*2)),
      strip.text.y = element_text(size=rel(rel_txt_size*2)), 
      legend.text=element_text(size=rel_txt_size*6), legend.title=element_text(size=rel_txt_size*8),
      legend.key.size = unit(legend_size*1, "cm"), legend.key.width = unit(legend_size*1, "cm"),
      axis.title=element_text(size=text_size,face="bold"),
      axis.text=element_text(size=axis_text_size)) #axis.text=element_text(size=14,face="bold"), #xlim(0, lim[1]) + ylim(0,lim[2]) + 
  
  if( !is.na(x_scale) ) { g_scatter <- g_scatter +   scale_x_continuous( limits = x_scale ) } #, limits = q_y_scale) + # , limits = c(-0.04,0.02)
  if( !is.na(y_scale) ) { g_scatter <- g_scatter +   scale_y_continuous( limits = y_scale ) } #, limits = q_y_scale) + # , limits = c(-0.04,0.02)
  
  line_size = 1
  if(lm_type == 'lm'){
    # lm version
    #print('apply linear model')
    #form <- as.formula(paste( col_name_y, paste(col_name_x), sep = " ~ "))
    # print(form)    print(names(df))
    g_scatter <-  g_scatter + 
      stat_smooth(data = df, method = "lm",  
                  formula = y ~ x , size = line_size,  colour = 'grey50', alpha = 0.75, show.legend = TRUE, mapping = aes_string(col_name_x , y = col_name_y ) ) +  #, mapping = aes(weight = SIFPK_mean) ) + # size = 0.1, # alpha = 0.75, 
      stat_regline_equation(formula = y ~ x,
                            aes( label =  paste( ..eq.label.., ..rr.label.., sep = "~~~~") ) #, color = Region
      )
  }

  return(g_scatter)
  
}

###################################################
######       I/O                                ###
###################################################

# output_root  <- '/media/mark/HD/Mark/Mark_COPERNICUS/figures/COPERNICUSII_V3/COP_caseStudies/'
output_path <- paste0(output_root,  case_study, '/figures/', full_date, '/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)}


##################################################
######     LOAD PREVIOUS SAVED DATA           #####
###################################################
# here we join together the dataframes for separate variables (or load single variables to look specifically at only them - avoid this)

# list of variables to run over
var_list <- c('LAI', 'LST',  'E', 'albedo_wsa_vis') # albedo_bsa_nir albedo_bsa_vis albedo_wsa_nir 'SM'

# create a single dataframe of all variables binded 
df <- data.frame()

for (i in 1:length(var_list)){
  variable_i <- var_list[i] ; print(variable_i)
  file_i <- paste0(case_study, '_', variable_i , '_stats.RData')  ;
  load( paste0(case_study_dir, file_i) )
  colnames(df_year)[5:length(df_year)] <- paste0(colnames(df_year)[5:length(df_year)], '.', variable_i)
  if (0 %in% dim(df)){ df <- df_year }
  else{ df <- left_join(x = df, y= df_year, by = c('x','y', 'year', 'month') ) }
  
}

# # alternatively load separate dataframes of single variables and make plots
# variable_i <- 'LAI' ; # can be LST LAI etc...
# df <- data.frame()
# file_i <- paste0(case_study, '_', variable_i , '_stats.RData')  ;
# load( paste0(case_study_dir, file_i) )
# colnames(df_year)[5:length(df_year)] <- paste0(colnames(df_year)[5:length(df_year)], '.', variable_i)


# for each x,y, we have (for each variable) values for heatwave and LT-average of sim, obs and various statistics related to these parameters
summary(df); dim(df) ; names(df)


###################################################
######     UNIVERSAL MAP SETTINGS             #####
###################################################

# extra info - to label plots based on heatwave date and average month
if (case_study == 'hw2003'){ print('2003 European heatwave')
  specific_date <- 'Aug 2003' ;  mean_date <- 'Aug mean'
}
if (case_study == 'hw2010'){ print('2010 Russian heatwave')
  specific_date <- 'July 2010' ;  mean_date <- 'July mean'
}
if (case_study == 'hw2018'){ print('2018 European heatwave')
  specific_date <- 'July 2018' ;  mean_date <- 'July mean'
}


# universal settings - maps
width=1400 ; height= 1200 ; cex_size = 4 ; # cex_size = 2 # big maps; 
n_pix <- 2e8 # n_pix <- 2e7 # maximum number of pixels in plots (2e7 very big but contains all)
fmt = '.png'
text_size = 30; #label
axis_text_size = 30; 
legend_size <- 1.75
# Colours and aggregation
agg_fact <- 1 # no aggregation


# histogram settings
#wid <- 15 ; hei <- 24 # readjusted for paper
wid <- 16 ; hei <- 10 #original


###################################################
######     RUN MAPPING - BASIC PLOTS          ##### # These figures will not go into the paper
###################################################
# Here we create figures showing (for each variable)
# 1) The obs and sim data during the heatwave 
# 2) The average obs and sim data during that month 
# 3) The difference obs (and sim) between the heatwave and average
# 4) optional - add histograms of the data for each of the above

n_bins <- 50 # n_bins for histograms and scatter densities
# colour themes
mapTheme <- rasterTheme(region=rev(brewer.pal(11, "RdYlBu"))) # colours in the scale

for (i in 1:length(var_list)){
  plot_info <- var_list[i] ; print(plot_info)
  # set the names of the plotting variables from the dataframe
  obs_name <- paste0('obs.', plot_info) ; sim_name <- paste0('sim.', plot_info) ; obs_mean_name <- paste0('obs_mean.', plot_info) ; sim_mean_name <- paste0('sim_mean.', plot_info)
  
  
  # hardcode labels and (if required) scales
  min_scale_r_hardcode <- NA ; max_scale_r_hardcode <- NA
  if(plot_info == 'LST') {
    legend_title_obs <- 'Satellite LST [C]' ; legend_title_sim <- 'ERA5 LST [C]'
    NiceNumbers= seq(0, 10, 1) ;  # map scale using use NiceNumbers= seq(0, 10, 0.1) for second sigfig (i.e. 192 > 190)
    divisions <- 12 ; # number of divisions in the scale on the maps
    min_scale_r_hardcode <- 10 ; max_scale_r_hardcode <- 60
  }
  if(plot_info == 'LAI') {
    legend_title_obs <- 'Theia LAI [m2/m2]' ; legend_title_sim <- 'ERA5 LAI [m2/m2]'
    NiceNumbers= seq(0, 10, 0.1) ;  # use second sigfig (i.e. 192 > 190)
    divisions <- 12 ; # number of divisions in the scale on the maps
    min_scale_r_hardcode <- 0 ; max_scale_r_hardcode <- 6
  }
  if(plot_info == 'SM') {
    legend_title_obs <- 'Satellite SM [m3/m3]' ; legend_title_sim <- 'ERA5 SM [m3/m3]'
    NiceNumbers= seq(0, 10, 10) ;  # use second sigfig (i.e. 192 > 190)
    divisions <- 10 ; # number of divisions in the scale on the maps
  }
  if(plot_info == 'E') {
    legend_title_obs <- 'GLEAM Evaporation [mm/mon]' ; legend_title_sim <- 'ERA5 evaporation [mm/mon]'
    NiceNumbers= seq(0, 10, 0.1) ;  # use second sigfig (i.e. 192 > 190)
    divisions <- 8 ; # number of divisions in the scale on the maps
    min_scale_r_hardcode <- 0 ; max_scale_r_hardcode <- 160 # hardcode
  }
  if(plot_info == 'albedo_wsa_vis') {
    legend_title_obs <- 'Satellite vis-WSA albedo' ; legend_title_sim <- 'ERA5 vis-WSA albedo'
    NiceNumbers= seq(0, 10, 0.1) ;  # use second sigfig (i.e. 192 > 190)
    divisions <- 10 ; # number of divisions in the scale on the maps
    min_scale_r_hardcode <- 0 ; max_scale_r_hardcode <- 0.2 # hardcode
  }
  # extract the hist/map scales/legends based on maximum and minimum values
  scale_r <- f_get_min_max_nice(df, obs_name, sim_name, obs_mean_name, sim_mean_name) ; min_scale_r <- scale_r[1] ; max_scale_r <- scale_r[2] ; min_scale_r_hist <- scale_r[3];  max_scale_r_hist <-  scale_r[4]
  if(!is.na(min_scale_r_hardcode)) {min_scale_r <- min_scale_r_hardcode}
  if(!is.na(max_scale_r_hardcode)) {max_scale_r <- max_scale_r_hardcode}
  print(min_scale_r) ; print(max_scale_r)
  
  
  ##### create maps #####
  # observed event
  legend_title <- paste0(legend_title_obs, ', ', specific_date) # legend title
  f_map_xyt(df, obs_name)
  # simulated event
  legend_title <- paste0(legend_title_sim, ', ', specific_date)
  f_map_xyt(df, sim_name)
  # mean monthly obs
  legend_title <- paste0(legend_title_obs, ', ', mean_date)
  f_map_xyt(df, obs_mean_name)
  # mean monthly sim
  legend_title <- paste0(legend_title <- legend_title_sim, ', ', mean_date)
  f_map_xyt(df, sim_mean_name)
  
  
  ##### optional - histogram #####
  max_y <- dim(df)/3  # use same scales as hist
  #obs event
  hist_out <- f_hist(df, obs_name)
  ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', obs_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
  # sim event
  hist_out <- f_hist(df, sim_name)
  ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', sim_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
  # obs mean
  hist_out <- f_hist(df, obs_mean_name)
  ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', obs_mean_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
  # sim mean
  hist_out <- f_hist(df, sim_mean_name)
  ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', sim_mean_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
  
  
}


###################################################
######     RUN MAPPING - diff & z-score val-LTmean ##### One figure will go into the final paper
###################################################
# mapping and histos for:
# 1) The raw differences of obs to long term mean of obs #       ***** LST FIGURE GOES INTO FINAL PAPER *****
# 2) The raw differences of sim to long term mean of sim
# 3) z-score  of obs (sim) to long term mean of obs (sim)
# ranges/legends/titles are hardcoded for each figure, because it's difficult to generalise between heatwaves and 

n_bins <- 50 # n_bins for histograms and scatter densities
# colour themes
mapTheme <- rasterTheme(region=rev(brewer.pal(11, "RdYlBu"))) # colours in the scale
max_y <- dim(df)/3  # use same scales as hist

plot_info <- 'LST' ; 

#       ***** THIS FIGURE GOES INTO FINAL PAPER *****
# observed - long-term mean 
var_name <- 'diff_obsSmean.LST' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', satellite LST [C]')
min_scale_r <- -15 ; max_scale_r <- 15 ; min_scale_r_hist <- -15 ; max_scale_r_hist <- 15
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/3  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'diff_simSmean.LST' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', ERA5 LST [C]')
min_scale_r <- -15 ; max_scale_r <- 15 ; min_scale_r_hist <- -15 ; max_scale_r_hist <- 15
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/3  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'z_score_obs.LST' ;  legend_title <- paste0(specific_date, ', satellite LST z-score')
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)[1]/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'z_score_sim.LST' ;  legend_title <- paste0(specific_date, ', ERA5 LST z-score')
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)


plot_info <- 'LAI' ; 
var_name <- 'diff_obsSmean.LAI' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', Theia LAI [m2/m2]')
min_scale_r <- -1 ; max_scale_r <- 1 ; min_scale_r_hist <- -2 ; max_scale_r_hist <- 2
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'diff_simSmean.LAI' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', ERA5 LAI [m2/m2]')
min_scale_r <- -1 ; max_scale_r <- 1 ; min_scale_r_hist <- -2 ; max_scale_r_hist <- 2
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'z_score_obs.LAI' ;  legend_title <- paste0(specific_date, ', Theia LAI z-score')
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)[1]/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'z_score_sim.LAI' ;  legend_title <- paste0(specific_date, ', ERA5 LAI z-score')
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)


plot_info <- 'E' ; 
var_name <- 'diff_obsSmean.E' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', satellite evaporation [mm/mon]')
min_scale_r <- -50 ; max_scale_r <- 50 ; min_scale_r_hist <- -100 ; max_scale_r_hist <- 100
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'diff_simSmean.E' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', ERA5 evaporation [mm/mon]')
min_scale_r <- -50 ; max_scale_r <- 50 ; min_scale_r_hist <- -100 ; max_scale_r_hist <- 100
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'z_score_obs.E' ;  legend_title <- paste0(specific_date, ', satellite evaporation z-score')
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)[1]/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'z_score_sim.E' ;  legend_title <- paste0(specific_date, ', ERA5 evaporation z-score')
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)


plot_info <- 'albedo_wsa_vis' ; 
var_name <- 'diff_obsSmean.albedo_wsa_vis' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', satellite albedo wsa vis')
min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'diff_simSmean.albedo_wsa_vis' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', ERA5 albedo wsa vis')
min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'z_score_obs.albedo_wsa_vis' ;  legend_title <- paste0(specific_date, ', satellite albedo wsa vis z-score')
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)[1]/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

var_name <- 'z_score_sim.albedo_wsa_vis' ;  legend_title <- paste0(specific_date, ', ERA5 albedo wsa vis z-score')
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

{
# 
# plot_info <- 'SM' ; 
# 
# var_name <- 'diff_obsSmean.SM' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', satellite SM [m3/m3]')
# min_scale_r <- -0.15 ; max_scale_r <- 0.15 ; min_scale_r_hist <- -0.2 ; max_scale_r_hist <- 0.2
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'diff_simSmean.SM' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', ERA5 SM [m3/m3]')
# min_scale_r <- -0.15 ; max_scale_r <- 0.15 ; min_scale_r_hist <- -0.2 ; max_scale_r_hist <- 0.2
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'z_score_obs.SM' ;  legend_title <- paste0(specific_date, ', satellite SM z-score')
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)[1]/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'z_score_sim.SM' ;  legend_title <- paste0(specific_date, ', ERA5 SM z-score')
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

#### OTHER ALBEDO TYPES
# 
# plot_info <- 'albedo_bsa_nir' ; 
# 
# var_name <- 'diff_obsSmean.albedo_bsa_nir' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', satellite albedo bsa nir')
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'diff_simSmean.albedo_bsa_nir' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', ERA5 albedo bsa nir')
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'z_score_obs.albedo_bsa_nir' ;  legend_title <- paste0(specific_date, ', satellite albedo bsa nir z-score')
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)[1]/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'z_score_sim.albedo_bsa_nir' ;  legend_title <- paste0(specific_date, ', ERA5 albedo bsa nir z-score')
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# plot_info <- 'albedo_bsa_vis' ; 
# 
# var_name <- 'diff_obsSmean.albedo_bsa_vis' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', satellite albedo bsa vis')
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'diff_simSmean.albedo_bsa_vis' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', ERA5 albedo bsa vis')
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'z_score_obs.albedo_bsa_vis' ;  legend_title <- paste0(specific_date, ', satellite albedo bsa vis z-score')
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)[1]/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'z_score_sim.albedo_bsa_vis' ;  legend_title <- paste0(specific_date, ', ERA5 albedo bsa vis z-score')
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# 
# plot_info <- 'albedo_wsa_nir' ; 
# 
# var_name <- 'diff_obsSmean.albedo_wsa_nir' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', satellite albedo wsa nir')
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'diff_simSmean.albedo_wsa_nir' ;  legend_title <- paste0(specific_date, ' - ', mean_date, ', ERA5 albedo wsa nir')
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'z_score_obs.albedo_wsa_nir' ;  legend_title <- paste0(specific_date, ', satellite albedo wsa nir z-score')
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)[1]/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# var_name <- 'z_score_sim.albedo_wsa_nir' ;  legend_title <- paste0(specific_date, ', ERA5 albedo wsa nir z-score')
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
}

###################################################
######     RUN MAPPING - diff & s.d. and scaled difference for sim-obs ##### one map goes into paper difference: HW bias - average bias LST, LAI, E, alb
###################################################
# here plot (for each variable):
# 1) bias (difference between sim - obs) for i) heatwave and ii) average conditions
# 2) s.d. (avg variation) in bias 
# 3) difference between bias in heatwave and mean bias #       ***** THIS FIGURE GOES INTO FINAL PAPER *****
# 4) z-score of the bias in heatwave ( figure 3/ figure 2)

# colour themes
mapTheme <- rasterTheme(region=rev(brewer.pal(11, "RdYlBu"))) # colours in the scale
n_bins <- 50 

plot_info <- 'LST' ; 

# first show August or specific date
var_name <- 'diff_simSobs.LST' ;  legend_title <- paste0('LST bias [C]', ', ', specific_date) 
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -13 ; max_scale_r_hist <- 13
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# next show mean month (average August)
var_name <- 'diff_simSobs_mean.LST' ;  legend_title <- paste0('ERA5 LST - sat LST [C]', ', ', mean_date) 
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -13 ; max_scale_r_hist <- 13
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# # next show sd month (sd August)
# var_name <- 'sd_diff_simSobs.LST' ;  legend_title <- paste0('s.d. in ERA5 LST - sat LST [C]', ', ', mean_date) 
# min_scale_r <- 0 ; max_scale_r <- 4 ; min_scale_r_hist <- 0 ; max_scale_r_hist <- 4
# divisions <- 8 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # # next show sd month (sd August)
# var_name <- 'scaled_diff_simSobs.LST' ;  legend_title <- paste0('(ERA5 LST - sat LST)/s.d. ', ', ', specific_date) 
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

#       ***** THIS FIGURE GOES INTO FINAL PAPER *****
# next show (ERA5-sat) Aug03 - avg Aug - so difference in ERA5-sat in this August compared to a normal August
var_name <- 'diff_simSobsSmean.LST' ;  legend_title <- paste0(plot_info, ' difference in bias, ', specific_date, ' - ', mean_date) 
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -7 ; max_scale_r_hist <- 7
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# next show z-score: [(ERA5-sat) Aug03 - avg Aug]/sd in difference_august - so difference in ERA5-sat in this August compared to a normal August, then scaled by average variation
var_name <- 'z_score_simSobs.LST' ;  legend_title <- paste0(plot_info, ' bias z-score, ', specific_date)
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)



plot_info <- 'LAI' ; 

# first show August or specific date
var_name <- 'diff_simSobs.LAI' ;  legend_title <- paste0('ERA5 LAI - Theia  [m2/m2]', ', ', specific_date) 
min_scale_r <- -2.5 ; max_scale_r <- 2.5 ; min_scale_r_hist <- -5 ; max_scale_r_hist <- 5
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# next show mean month (average August)
var_name <- 'diff_simSobs_mean.LAI' ;  legend_title <- paste0('ERA5 LAI - Theia  [m2/m2]', ', ', mean_date) 
min_scale_r <- -2.5 ; max_scale_r <- 2.5 ; min_scale_r_hist <- -5 ; max_scale_r_hist <- 5
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# # next show sd month (sd August)
# var_name <- 'sd_diff_simSobs.LAI' ;  legend_title <- paste0('s.d. in ERA5 LAI - Theia  [m2/m2]', ', ', mean_date) 
# min_scale_r <- 0 ; max_scale_r <- 1 ; min_scale_r_hist <- 0 ; max_scale_r_hist <- 1.5
# divisions <- 8 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
#
# # next august 03 difference scaled by sd month (sd August)
# var_name <- 'scaled_diff_simSobs.LAI' ;  legend_title <- paste0('(ERA5 LAI - Theia)/s.d. ', ', ', specific_date) 
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

#       ***** THIS FIGURE GOES INTO FINAL PAPER *****
# next show (ERA5-sat) Aug03 - avg Aug - so difference in ERA5-sat in this August compared to a normal August
var_name <- 'diff_simSobsSmean.LAI' ;  legend_title <-  paste0(plot_info, ' difference in bias, ', specific_date, ' - ', mean_date)  # paste0('diff (ERA5 LAI - Theia) in ', specific_date, ' - diff in ', mean_date) 
min_scale_r <- -1.5 ; max_scale_r <- 1.5 ; min_scale_r_hist <- -2 ; max_scale_r_hist <- 2
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# next show z-score: [(ERA5-sat) Aug03 - avg Aug]/sd in difference_august - so difference in ERA5-sat in this August compared to a normal August, then scaled by average variation
var_name <- 'z_score_simSobs.LAI' ;  legend_title <-  paste0(plot_info, ' bias z-score, ', specific_date) # paste0('[diff (ERA5 - Theia) ', specific_date, ' - diff ', mean_date, '] / s.d. ', mean_date) 
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

plot_info <- 'E' ; 

# first show August or specific date
var_name <- 'diff_simSobs.E' ;  legend_title <- paste0('ERA5 E - satellite [mm/mon]', ', ', specific_date) 
min_scale_r <- -50 ; max_scale_r <- 50 ; min_scale_r_hist <- -100 ; max_scale_r_hist <- 100
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# next show mean month (average August)
var_name <- 'diff_simSobs_mean.E' ;  legend_title <- paste0('ERA5 E - satellite  [mm/mon]', ', ', mean_date) 
min_scale_r <- -50 ; max_scale_r <- 50 ; min_scale_r_hist <- -100 ; max_scale_r_hist <- 100
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# # next show sd month (sd August)
# var_name <- 'sd_diff_simSobs.E' ;  legend_title <- paste0('s.d. in ERA5 E - satellite  [mm/mon]', ', ', mean_date) 
# min_scale_r <- 0 ; max_scale_r <- 50 ; min_scale_r_hist <- 0 ; max_scale_r_hist <- 100
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# # next august 03 difference scaled by sd month (sd August)
# var_name <- 'scaled_diff_simSobs.E' ;  legend_title <- paste0('(ERA5 E - satellite)/s.d. ', ', ', specific_date) 
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

#       ***** THIS FIGURE GOES INTO FINAL PAPER *****
# next show (ERA5-sat) Aug03 - avg Aug - so difference in ERA5-sat in this August compared to a normal August
var_name <- 'diff_simSobsSmean.E' ;  legend_title <-  paste0(plot_info, ' difference in bias, ', specific_date, ' - ', mean_date)  # paste0('diff (ERA5 E - sat) in ', specific_date, ' - diff in ', mean_date)
min_scale_r <- -50 ; max_scale_r <- 50 ; min_scale_r_hist <- -100 ; max_scale_r_hist <- 100
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# next show z-score: [(ERA5-sat) Aug03 - avg Aug]/sd in difference_august - so difference in ERA5-sat in this August compared to a normal August, then scaled by average variation
var_name <- 'z_score_simSobs.E' ;  legend_title <- paste0(plot_info, ' bias z-score, ', specific_date) # paste0('[diff (ERA5 - sat) ', specific_date, ' - diff ', mean_date, '] / s.d. ', mean_date)
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)



plot_info <- 'albedo_wsa_vis' ; 

# first show August or specific date
var_name <- paste0('diff_simSobs.', plot_info) ;  legend_title <- paste0('ERA5 albedo wsa vis - sat', ', ', specific_date) 
min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# next show mean month (average August)
var_name <- paste0('diff_simSobs_mean.', plot_info) ;  legend_title <- paste0('ERA5 albedo wsa vis - sat', ', ', mean_date) 
min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# # next show sd month (sd August)
# var_name <- paste0('sd_diff_simSobs.', plot_info) ;  legend_title <- paste0('s.d. in ERA5 albedo wsa vis - sat', ', ', mean_date) 
# min_scale_r <- 0 ; max_scale_r <- 0.05 ; min_scale_r_hist <- 0 ; max_scale_r_hist <- 1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # # next august 03 difference scaled by sd month (sd August)
# var_name <- paste0('scaled_diff_simSobs.', plot_info) ;  legend_title <- paste0('(ERA5 wsa vis - sat)/s.d. ', ', ', specific_date) 
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

#       ***** THIS FIGURE GOES INTO FINAL PAPER *****
# next show (ERA5-sat) Aug03 - avg Aug - so difference in ERA5-sat in this August compared to a normal August
var_name <- paste0('diff_simSobsSmean.', plot_info) ;  legend_title <-  paste0('Vis wsa albedo difference in bias, ', specific_date, ' - ', mean_date)  # paste0('diff (ERA5 - sat) in ', specific_date, ' - diff in ', mean_date) 
min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# next show z-score: [(ERA5-sat) Aug03 - avg Aug]/sd in difference_august - so difference in ERA5-sat in this August compared to a normal August, then scaled by average variation
var_name <- paste0('z_score_simSobs.', plot_info) ;  paste0('Vis wsa albedo  bias z-score, ', specific_date) # legend_title <- paste0('[diff (ERA5 - sat) ', specific_date, ' - diff ', mean_date, '] / s.d. ', mean_date) 
min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
divisions <- 10 ; # number of divisions in the scale on the maps
max_y <- dim(df)/4  # use same scales as hist
f_map_xyt(df, var_name)
hist_out <- f_hist(df, var_name)
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)


#### alternative albedo figures
{
# 
# 
# plot_info <- 'SM' ; 
# 
# # first show August or specific date
# var_name <- 'diff_simSobs.SM' ;  legend_title <- paste0('ERA5 SM - satellite  [m2/m2]', ', ', specific_date) 
# min_scale_r <- -0.15 ; max_scale_r <- 0.15 ; min_scale_r_hist <- -0.4 ; max_scale_r_hist <- 0.4
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show mean month (average August)
# var_name <- 'diff_simSobs_mean.SM' ;  legend_title <- paste0('ERA5 SM - satellite  [m2/m2]', ', ', mean_date) 
# min_scale_r <- -0.15 ; max_scale_r <- 0.15 ; min_scale_r_hist <- -0.4 ; max_scale_r_hist <- 0.4
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show sd month (sd August)
# var_name <- 'sd_diff_simSobs.SM' ;  legend_title <- paste0('s.d. in ERA5 SM - satellite  [m2/m2]', ', ', mean_date) 
# min_scale_r <- 0 ; max_scale_r <- 0.06 ; min_scale_r_hist <- 0 ; max_scale_r_hist <- 0.08
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # # next august 03 difference scaled by sd month (sd August)
# # var_name <- 'scaled_diff_simSobs.SM' ;  legend_title <- paste0('(ERA5 SM - satellite)/s.d. ', ', ', specific_date) 
# # min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# # divisions <- 10 ; # number of divisions in the scale on the maps
# # max_y <- dim(df)/4  # use same scales as hist
# # f_map_xyt(df, var_name)
# # hist_out <- f_hist(df, var_name)
# # ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show (ERA5-sat) Aug03 - avg Aug - so difference in ERA5-sat in this August compared to a normal August
# var_name <- 'diff_simSobsSmean.SM' ;  legend_title <-  paste0(plot_info, ' difference in bias, ', specific_date, ' - ', mean_date)  # paste0('diff (ERA5 SM - sat) in ', specific_date, ' - diff in ', mean_date) 
# min_scale_r <- -0.1 ; max_scale_r <- 0.1 ; min_scale_r_hist <- -0.2 ; max_scale_r_hist <- 0.2
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show z-score: [(ERA5-sat) Aug03 - avg Aug]/sd in difference_august - so difference in ERA5-sat in this August compared to a normal August, then scaled by average variation
# var_name <- 'z_score_simSobs.SM' ;  legend_title <- paste0(plot_info, ' bias z-score, ', specific_date) # paste0('[diff (ERA5 - sat) ', specific_date, ' - diff ', mean_date, '] / s.d. ', mean_date) 
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -5 ; max_scale_r_hist <- 5
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)

# plot_info <- 'albedo_bsa_nir' ; 
# 
# # first show August or specific date
# var_name <- paste0('diff_simSobs.', plot_info) ;  legend_title <- paste0('ERA5 albedo bsa nir - sat', ', ', specific_date) 
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show mean month (average August)
# var_name <- paste0('diff_simSobs_mean.', plot_info) ;  legend_title <- paste0('ERA5 albedo bsa nir - sat', ', ', mean_date) 
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show sd month (sd August)
# var_name <- paste0('sd_diff_simSobs.', plot_info) ;  legend_title <- paste0('s.d. in ERA5 albedo bsa nir - sat', ', ', mean_date) 
# min_scale_r <- 0 ; max_scale_r <- 0.05 ; min_scale_r_hist <- 0 ; max_scale_r_hist <- 1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # # next august 03 difference scaled by sd month (sd August)
# # var_name <- paste0('scaled_diff_simSobs.', plot_info) ;  legend_title <- paste0('(ERA5 bsa nir - sat)/s.d. ', ', ', specific_date) 
# # min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# # divisions <- 10 ; # number of divisions in the scale on the maps
# # max_y <- dim(df)/4  # use same scales as hist
# # f_map_xyt(df, var_name)
# # hist_out <- f_hist(df, var_name)
# # ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show (ERA5-sat) Aug03 - avg Aug - so difference in ERA5-sat in this August compared to a normal August
# var_name <- paste0('diff_simSobsSmean.', plot_info) ;  legend_title <-  paste0(plot_info, ' difference in bias, ', specific_date, ' - ', mean_date) 
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show z-score: [(ERA5-sat) Aug03 - avg Aug]/sd in difference_august - so difference in ERA5-sat in this August compared to a normal August, then scaled by average variation
# var_name <- paste0('z_score_simSobs.', plot_info) ;  legend_title <- paste0(plot_info, ' bias z-score, ', specific_date) # paste0('[diff (ERA5 - sat) ', specific_date, ' - diff ', mean_date, '] / s.d. ', mean_date) 
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# 
# 
# plot_info <- 'albedo_bsa_vis' ; 
# 
# # first show August or specific date
# var_name <- paste0('diff_simSobs.', plot_info) ;  legend_title <- paste0('ERA5 albedo bsa vis - sat', ', ', specific_date) 
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show mean month (average August)
# var_name <- paste0('diff_simSobs_mean.', plot_info) ;  legend_title <- paste0('ERA5 albedo bsa vis - sat', ', ', mean_date) 
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show sd month (sd August)
# var_name <- paste0('sd_diff_simSobs.', plot_info) ;  legend_title <- paste0('s.d. in ERA5 albedo bsa vis - sat', ', ', mean_date) 
# min_scale_r <- 0 ; max_scale_r <- 0.05 ; min_scale_r_hist <- 0 ; max_scale_r_hist <- 1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # # next august 03 difference scaled by sd month (sd August)
# # var_name <- paste0('scaled_diff_simSobs.', plot_info) ;  legend_title <- paste0('(ERA5 bsa vis - sat)/s.d. ', ', ', specific_date) 
# # min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# # divisions <- 10 ; # number of divisions in the scale on the maps
# # max_y <- dim(df)/4  # use same scales as hist
# # f_map_xyt(df, var_name)
# # hist_out <- f_hist(df, var_name)
# # ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show (ERA5-sat) Aug03 - avg Aug - so difference in ERA5-sat in this August compared to a normal August
# var_name <- paste0('diff_simSobsSmean.', plot_info) ;  legend_title <-  paste0(plot_info, ' difference in bias, ', specific_date, ' - ', mean_date)  # paste0('diff (ERA5 - sat) in ', specific_date, ' - diff in ', mean_date) 
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show z-score: [(ERA5-sat) Aug03 - avg Aug]/sd in difference_august - so difference in ERA5-sat in this August compared to a normal August, then scaled by average variation
# var_name <- paste0('z_score_simSobs.', plot_info) ;  legend_title <- paste0(plot_info, ' bias z-score, ', specific_date) # paste0('[diff (ERA5 - sat) ', specific_date, ' - diff ', mean_date, '] / s.d. ', mean_date) 
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# 
# 
# 
# plot_info <- 'albedo_wsa_nir' ; 
# 
# # first show August or specific date
# var_name <- paste0('diff_simSobs.', plot_info) ;  legend_title <- paste0('ERA5 albedo wsa nir - sat', ', ', specific_date) 
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show mean month (average August)
# var_name <- paste0('diff_simSobs_mean.', plot_info) ;  legend_title <- paste0('ERA5 albedo wsa nir - sat', ', ', mean_date) 
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show sd month (sd August)
# var_name <- paste0('sd_diff_simSobs.', plot_info) ;  legend_title <- paste0('s.d. in ERA5 albedo wsa nir - sat', ', ', mean_date) 
# min_scale_r <- 0 ; max_scale_r <- 0.05 ; min_scale_r_hist <- 0 ; max_scale_r_hist <- 1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # # next august 03 difference scaled by sd month (sd August)
# # var_name <- paste0('scaled_diff_simSobs.', plot_info) ;  legend_title <- paste0('(ERA5 wsa nir - sat)/s.d. ', ', ', specific_date) 
# # min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# # divisions <- 10 ; # number of divisions in the scale on the maps
# # max_y <- dim(df)/4  # use same scales as hist
# # f_map_xyt(df, var_name)
# # hist_out <- f_hist(df, var_name)
# # ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show (ERA5-sat) Aug03 - avg Aug - so difference in ERA5-sat in this August compared to a normal August
# var_name <- paste0('diff_simSobsSmean.', plot_info) ;  legend_title <-  paste0(plot_info, ' difference in bias, ', specific_date, ' - ', mean_date)  # paste0('diff (ERA5 - sat) in ', specific_date, ' - diff in ', mean_date) 
# min_scale_r <- -0.05 ; max_scale_r <- 0.05 ; min_scale_r_hist <- -0.1 ; max_scale_r_hist <- 0.1
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# # next show z-score: [(ERA5-sat) Aug03 - avg Aug]/sd in difference_august - so difference in ERA5-sat in this August compared to a normal August, then scaled by average variation
# var_name <- paste0('z_score_simSobs.', plot_info) ;  paste0(plot_info, ' bias z-score, ', specific_date) # legend_title <- paste0('[diff (ERA5 - sat) ', specific_date, ' - diff ', mean_date, '] / s.d. ', mean_date) 
# min_scale_r <- -5 ; max_scale_r <- 5 ; min_scale_r_hist <- -10 ; max_scale_r_hist <- 10
# divisions <- 10 ; # number of divisions in the scale on the maps
# max_y <- dim(df)/4  # use same scales as hist
# f_map_xyt(df, var_name)
# hist_out <- f_hist(df, var_name)
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', var_name, '_h.png'),   plot = hist_out , width = wid, height = hei)
# 
# 
}

###################################################
######     SCATTER  - tighten bounding box    #####
###################################################
# In order to show the same maps for different heatwaves we have expanded the bounding box from v1.2
# We can either a) apply a tighter bounding box to the data 

if (case_study == 'hw2003'){ print('2003 European heatwave')
  v_lon_min <- -12 ; v_lon_max <- 24  ; v_lat_min <- 36 ; v_lat_max <- 60 
  df_tight <- df %>%  filter(x >= v_lon_min  & x <= v_lon_max) %>%
                      filter( y >= v_lat_min & y <= v_lat_max) 
}
if (case_study == 'hw2010'){ print('2010 Russian heatwave')
    v_lon_min <- 30   ; v_lon_max <- 58 ; v_lat_min <- 47 ; v_lat_max <- 62
  df_tight <- df %>%  filter(x >= v_lon_min  & x <= v_lon_max) %>%
    filter( y >= v_lat_min & y <= v_lat_max) 
}
if (case_study == 'hw2018'){ print('2018 European heatwave')
  v_lon_min <- -12   ; v_lon_max <- 23 ; v_lat_min <- 47 ; v_lat_max <- 63
  df_tight <- df %>%  filter(x >= v_lon_min  & x <= v_lon_max) %>%
    filter( y >= v_lat_min & y <= v_lat_max) 
}

## or b) apply a cut on the temperature anomaly (e.g. to only look at pixels with a 'obsEvent-LT' mean above a certain value)
#df_tight <- df %>% filter(diff_obsSmean.LST > 0)

## or c) no bounding box 
# df_tight <- df

###################################################
######     SCATTER  - diff & z-score with LAI ##### # Not in final paper
###################################################
# these plots give the difference in (ERA5-obs) in both axes (y-axis: LST, E, alb, SM, ; x-axis LAI) for i) heatwave and ii) average


rf <- colorRampPalette(rev(brewer.pal(11,'Spectral'))) ;  r <- rf(32)
n_bins <- 30
#legend_size <- 1 ; 
rel_txt_size <- 3
#wid <- 25 ; hei <- 25 #original
legend_size <- 1.75
# histogram settings
text_size = 30; #label
axis_text_size = 30; 
# Colours and aggregation
wid <- 10 ; hei <- 12 # make more square

################################ LST ###########

plot_info <- 'scatter_biasEvent' ; col_variable_x <- 'diff_simSobs.LAI' ; col_variable_y <- 'diff_simSobs.LST' ; label_name_x <- 'LAI bias [m2/m2]' ; label_name_y <- 'LST bias [C]'
x_lim <- c(-4, 4) ; y_lim <- c(-10, 10)
legend_title <- paste0('') # #legend_title <- paste0('Difference between ERA5-obs, ',  specific_date)

g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim )
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
       plot = g_scatter , width = wid, height = hei, dpi = 300)

plot_info <- 'scatter_biasAvg' ; col_variable_x <- 'diff_simSobs_mean.LAI' ; col_variable_y <- 'diff_simSobs_mean.LST' ; col_name_x <- 'LAI bias [m2/m2]' ; col_name_y <- 'LST bias [C]'
#legend_title <- paste0('Difference between ERA5-obs, ',  mean_date)
legend_title <- paste0('')

g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = col_name_x, label_y = col_name_y, x_scale = x_lim, y_scale = y_lim )
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
       plot = g_scatter , width = wid, height = hei, dpi = 300)

# # try scatter with linear equation
# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim, lm_type = 'lm' )


################################ E ###########

plot_info <- 'scatter_biasEvent' ; col_variable_x <- 'diff_simSobs.LAI' ; col_variable_y <- 'diff_simSobs.E'
label_name_x <- 'LAI bias [m2/m2]' ; label_name_y <- 'Evaporation bias [m3/m3]'
x_lim <- c(-4, 4) ; y_lim <- c(-50, 50)
legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  specific_date)

# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y)
g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )

ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
       plot = g_scatter , width = wid, height = hei, dpi = 300)


plot_info <- 'scatter_biasAvg' ; col_variable_x <- 'diff_simSobs_mean.LAI' ; col_variable_y <- 'diff_simSobs_mean.E'
label_name_x <- 'LAI bias [m2/m2]' ; label_name_y <- 'Evaporation bias [m3/m3]'
x_lim <- c(-4, 4) ; y_lim <- c(-50, 50)
legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  mean_date)

g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
       plot = g_scatter , width = wid, height = hei, dpi = 300)

################################ albedo ###########
albedo_type <- 'wsa_vis'

plot_info <- 'scatter_biasEvent' ; col_variable_x <- 'diff_simSobs.LAI' ; col_variable_y <- paste0('diff_simSobs.albedo_', albedo_type)
label_name_x <- 'LAI bias [m2/m2]' ; label_name_y <- paste0('albedo ', gsub('_', ' ', albedo_type), '  bias')
x_lim <- c(-4, 4) ; y_lim <- c(-0.1, 0.1)
legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  specific_date)

# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y)
g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )

ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
       plot = g_scatter , width = wid, height = hei, dpi = 300)


plot_info <- 'scatter_biasAvg' ; col_variable_x <- 'diff_simSobs_mean.LAI' ; col_variable_y <- paste0('diff_simSobs_mean.albedo_', albedo_type)
label_name_x <- 'LAI bias [m2/m2]' ; label_name_y <- paste0('albedo ', gsub('_', ' ', albedo_type), ' bias')
x_lim <- c(-4, 4) ; y_lim <- c(-0.1, 0.1)
legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  mean_date)

g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
       plot = g_scatter , width = wid, height = hei, dpi = 300)

# SM
{
# ################################ SM ###########
# 
# plot_info <- 'scatter_biasEvent' ; col_variable_x <- 'diff_simSobs.LAI' ; col_variable_y <- 'diff_simSobs.SM'
# label_name_x <- 'LAI bias [m2/m2]' ; label_name_y <- 'SM bias [m3/m3]'
# x_lim <- c(-4, 4) ; y_lim <- c(-0.2, 0.2)
# legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  specific_date)
# 
# # g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y)
# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )
# 
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
#        plot = g_scatter , width = wid, height = hei, dpi = 300)
# 
# 
# plot_info <- 'scatter_biasAvg' ; col_variable_x <- 'diff_simSobs_mean.LAI' ; col_variable_y <- 'diff_simSobs_mean.SM'
# label_name_x <- 'LAI bias [m2/m2]' ; label_name_y <- 'SM bias [m3/m3]'
# x_lim <- c(-4, 4) ; y_lim <- c(-0.2, 0.2)
# legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  mean_date)
# 
# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
#        plot = g_scatter , width = wid, height = hei, dpi = 300)
}

########################################
################ Now the z-score  ###### # ***** THIS FIGURE GOES INTO FINAL PAPER *****
########################################
# These create scatter of z-score bias anomaly and LAI bias anomaly (i.e. how large the heatwave ERA5-sat bias is compared to average year, scaled by the average deviation)
# ***** THIS FIGURE GOES INTO FINAL PAPER *****


rf <- colorRampPalette(rev(brewer.pal(11,'Spectral'))) ;  r <- rf(32)
n_bins <- 30
#legend_size <- 1 ; 
rel_txt_size <- 3
#wid <- 25 ; hei <- 25 #original
legend_size <- 1.75
# histogram settings
text_size = 30; #label
axis_text_size = 30; 
# Colours and aggregation
wid <- 10 ; hei <- 12 # make more square

# LST # ***** THIS FIGURE GOES INTO FINAL PAPER *****
plot_info <- 'scatter_biasZscore' ; col_variable_x <- 'z_score_simSobs.LAI' ; col_variable_y <- 'z_score_simSobs.LST'
label_name_x <- 'LAI bias anomaly [z-score]' ; label_name_y <- paste0('LST bias anomaly [z-score]')
x_lim <- c(-4, 4) ; y_lim <- c(-4, 4)
legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  specific_date)
# legend_title <- paste0('z-score between ERA5-obs, ',  specific_date)

# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y)
g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
       plot = g_scatter , width = wid, height = hei, dpi = 300)

# # try scatter with linear equation
# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim, lm_type = 'lm' )

# Evaporation # ***** THIS FIGURE GOES INTO FINAL PAPER *****
plot_info <- 'scatter_biasZscore' ; col_variable_x <- 'z_score_simSobs.LAI' ; col_variable_y <- 'z_score_simSobs.E'
label_name_x <- 'LAI bias anomaly [z-score]' ; label_name_y <- paste0('evaporation bias anomaly [z-score]')
x_lim <- c(-4, 4) ; y_lim <- c(-4, 4)
legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  specific_date)
# legend_title <- paste0('z-score between ERA5-obs, ',  specific_date)

g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
       plot = g_scatter , width = wid, height = hei, dpi = 300)

# # try scatter with linear equation
# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim, lm_type = 'lm' )

# albedo ***** THIS FIGURE GOES INTO FINAL PAPER *****
albedo_type <- 'wsa_vis'
plot_info <- 'scatter_biasZscore' ; col_variable_x <- 'z_score_simSobs.LAI' ; col_variable_y <- paste0('z_score_simSobs.albedo_', albedo_type)
label_name_x <- 'LAI bias anomaly [z-score]' ; label_name_y <- paste0('Vis wsa albedo bias anomaly [z-score]')
x_lim <- c(-4, 4) ; y_lim <- c(-4, 4)
legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  specific_date)
# legend_title <- paste0('z-score between ERA5-obs, ',  specific_date)

g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )
ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
       plot = g_scatter , width = wid, height = hei, dpi = 300)

# # try scatter with linear equation
# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim, lm_type = 'lm' )

# SM
{
# plot_info <- 'scatter_biasZscore' ; col_variable_x <- 'z_score_simSobs.LAI' ; col_variable_y <- 'z_score_simSobs.SM'
# label_name_x <- 'LAI bias anomaly [z-score]' ; label_name_y <- paste0('SM bias anomaly [z-score]')
# x_lim <- c(-4, 4) ; y_lim <- c(-4, 4)
# legend_title <- '' # legend_title <- paste0('Difference between ERA5-obs, ',  specific_date)
# # legend_title <- paste0('z-score between ERA5-obs, ',  specific_date)
# 
# g_scatter <- f_scatter(df_tight,  col_name_x = col_variable_x, col_name_y = col_variable_y, label_x = label_name_x, label_y = label_name_y, x_scale = x_lim, y_scale = y_lim  )
# ggsave(filename = paste0(output_path, case_study, '_', plot_info, '_', col_variable_x, '-', col_variable_y, '_s.png'), 
#        plot = g_scatter , width = wid, height = hei, dpi = 300)
}

##### now also for all the data across all years (by month and all months)

###################################################
######     FINALISE                           #####
###################################################
# timing
end_time <- Sys.time()
print(end_time - start_time)



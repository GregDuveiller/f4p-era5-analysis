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
start_time <- Sys.time(); print(start_time)
# find date:
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ") ; full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]

######     GLOBAL VARS                        #####

# update to rproject or here() - place the local data directory here:
dir_path <- paste0('/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/data/') # replace with git data directory


######     SET LIBRARIES                      #####
library(chron) # useful for converting time values
library(dplyr) # use %>%
library(raster) # package for raster manipulation
library(lattice)    # enables levelplots
library(gridExtra)  # grouping levelplots into grids
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
library(RColorBrewer)

###################################################
######       I/O                                ###
###################################################

# output path set within case_study_directory for now - can set to figures directly when script is stabalised
output_path <- paste0(dir_path, 'COP_caseStudies/'  , 'figures/', full_date, '/') # temporary
#output_path <- paste0(dir_path, 'figures_for_paper/', 'figures/', full_date, '/') # final output path
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)}

# load dataframe
load(paste0(dir_path, 'COP_caseStudies/', 'data/', 'hwAll_varAll_stats.RData'))


###################################################
######     FUNCTIONS                          #####
###################################################

############### Load map shapefiles
# download here: https://www.naturalearthdata.com/downloads/50m-physical-vectors/

# local
coast_shapefile <- paste0(dir_path, 'shape_files/', "ne_50m_coastline.shp")
bb_shapefile <- paste(dir_path, 'shape_files/', "ne_50m_wgs84_bounding_box.shp", sep="")
grat30_shapefile <- paste(dir_path, 'shape_files/', "global_50m_graticules_boundingbox/ne_50m_graticules_30.shp", sep="")
# local 
grat30_shapefile <- paste(dir_path, 'shape_files/', "ne_50m_graticules_30.shp", sep="")
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

######################

# function to print the columns of a dataframe 
# dataframe should be x, y, ... col_names, etc format
f_map_xyt <- function(df, col_name, index=i) {
  
  # levelplot settings
  cutpts_r <- seq(min_scale_r,max_scale_r, (max_scale_r-min_scale_r)/divisions) 
  # set values below min or above max to the min max values - i.e. oob
  df <- df %>% mutate( !!as.symbol(col_name) := ifelse( ( !!as.symbol(col_name) > max_scale_r) , max_scale_r,  ifelse( ( !!as.symbol(col_name) < min_scale_r), min_scale_r, !!as.symbol(col_name)  ) ) )# recalibrate out of bounds values to max val for levelplot
  
  # put df in x,y,z format for the levelplot
  dfr_r <- df[,c(1,2)] ; dfr_r[col_name] <- df[col_name]
  dfr_r <- rasterFromXYZ(dfr_r)
  
  # aggregate pixels if required - this visually aggregates pixels together in the raster. If factor = 1 then no change
  if(agg_fact != 1){ print('aggregate') ;
    dfr_r_agg <- aggregate(dfr_r, agg_fact, fun=mean, expand=TRUE, na.rm=TRUE) ## as we have fewer pixels now (unfilled) we should aggregate - making sure to remove NA values
  }
  
  if(add_colorkey){
  colorkey_list = list(  labels=list(cex=cex_size), labels=as.character( cutpts_r), 
                         space = "top", # vjust = -.5 #  position
                         height = map_legend_size # width = 0.5
                         #title=list('SIF', cex=40)  # legend title cant change size...
  )  # at=as.numeric( factor( c( seq(-1, 1, by=0.2), "NA"))) )  # key size and cut ticks #  tick.number = length(cutpts_r),
  }else{ colorkey_list <- F}
  
  # make levelplot
  crs(dfr_r) <- unproj_proj4string # add in the projection
  plt <-   levelplot(dfr_r, margin=F, at = cutpts_r,   cuts = length(cutpts_r), pretty = F, xlab=x_lab_name, ylab=list(y_lab_name,cex=cex_size), #  xlab=list("longitude", cex=cex_size), ylab=list("latitude", cex=cex_size) # set x-y labels (e.g. lon/lat)
                     par.settings=mapTheme, maxpixels = n_pix,  # at = cutpts_r
                     main = list(legend_title, cex=cex_size), # set title and text size
                     colorkey=colorkey_list, # optional colour key - see above
                     #scales=list(x=list(cex=cex_size),y=list(cex=cex_size)) # add x-y lon/lat label and ticks and tick size
                     scales=list(x=list(draw=FALSE), y=list( draw=FALSE),tck = c(0,0) ) #  no x-y label or tick marks
  ) 
  # add in bounding box and coastlines
  plt_layer <- plt  + latticeExtra::layer(sp.lines(coast_lines, col="black", lwd=0.5))

  # set the margin between the figures - hacked in- needs tweaking and formatting
  if(index==1){cex_top = 4 ; cex_bottom = -4}
  if(index==2){cex_top = 2.5 ; cex_bottom = 2.5} # -0.15
  if(index==3){cex_top = -4 ; cex_bottom = 7}
  plt_layer$par.settings$layout.heights[
    c( 'bottom.padding' # 'key.sub.padding', # 'axis.xlab.padding', # 'key.axis.padding', # 'main.key.padding',
       )] <- cex_bottom
  plt_layer$par.settings$layout.heights[
    c( 'top.padding')] <- cex_top
  plt_layer$par.settings$layout.widths[
    c( 'left.padding', 'right.padding'
       )] <- -0.1
  plt_layer$aspect.fill <- TRUE
  
  return(plt_layer)
}

# leave scatter script in, in case we want to add the scatter plots - unneccesary for current figure maps
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
######     UNIVERSAL MAP SETTINGS             #####
###################################################

# list of dataframes to evaluate 
df_hw = data.frame( row.names=c("hw2003","hw2010","hw2018") )

# universal settings - maps
width=2800 ; height= 1600 ; cex_size = 3 ; # cex_size = 2 # big maps; 
n_pix <- 2e8 # n_pix <- 2e7 # maximum number of pixels in plots (2e7 very big but contains all)
fmt = '.png'
text_size = 30; #label
axis_text_size = 30; 
# Colours and aggregation
agg_fact <- 1 # no aggregation
map_legend_size <- 0.75

# # histogram/scatter settings - if we chose to add
# #wid <- 15 ; hei <- 24 # readjusted for paper
# wid <- 16 ; hei <- 10 #original
# n_bins <- 50
# legend_size <- 1.75


###################################################
######     LOOP OVER FIGURES FOR INCLUSION    ##### 
###################################################
# loop over heatwaves and produce plots of the different variables

for( i in 1:dim(df_hw)[1] ){ # i <- 1
  print(i) ; case_study <- row.names(df_hw)[i] ; print( case_study  )  
  
  # filter to specific case study
  df_case_study <- df_all %>% filter(hw == case_study)
  if(i==1){add_colorkey = T } else{ add_colorkey = F} # only x-axis labels on first
  x_lab_name <- NULL # ;  y_cex_tck_size <- cex_size # choose whether to add an x/y label (e.g. Lon/Lat) - can also chose to add tickmarks
  
  mapTheme <- rasterTheme(region=rev(brewer.pal(11, "RdYlBu"))) # set colourscale for all/some maps
  
  # HW temperature anomaly: create the levelplot
  df <- df_case_study %>% filter(variable == 'LST')
  if(i==1){legend_title <- paste0( 'LST anomaly [C]') } else{legend_title <- ''} # label of legend
  y_lab_name <- substr(case_study,3,7) # ; y_cex_tck_size <- 0 # add facet label for heatwave year
  
  var_name <- 'diff_obsSmean' ;  
  min_scale_r <- -15 ; max_scale_r <- 15  # scale range
  divisions <- 10 ; # number of divisions in the scale on the map
  m_LST_anom <- f_map_xyt(df, var_name, index=i) # create map
  
  # other plots in the row - add/remove y-axis and ticks
  y_lab_name <- NULL # NULL # ; y_cex_tck_size <- 0

  # LST: create the levelplot for hw-mean bias difference
  df <- df_case_study %>% filter(variable == 'LST')
  var_name <- 'diff_simSobsSmean' ;  
  if(i==1){legend_title <- paste0( 'LST bias shift [C]') }else{legend_title <- ''}
  min_scale_r <- -5 ; max_scale_r <- 5 
  divisions <- 10 ; # number of divisions in the scale on the maps
  m_LST_biasDiff <- f_map_xyt(df, var_name, index=i)
  
  # LAI: create the levelplot for hw-mean bias difference
  df <- df_case_study %>% filter(variable == 'LAI')
  var_name <- 'diff_simSobsSmean' ;  
  if(i==1){legend_title <- paste0( 'LAI bias shift [m2/m2]') }else{legend_title <- ''}
  min_scale_r <- -1.5 ; max_scale_r <- 1.5 
  divisions <- 12 ; # number of divisions in the scale on the maps
  m_LAI_biasDiff <- f_map_xyt(df, var_name, index=i)
  
  # E: create the levelplot for hw-mean bias difference
  df <- df_case_study %>% filter(variable == 'E')
  var_name <- 'diff_simSobsSmean' ;  
  if(i==1){legend_title <- paste0( 'Evaporation bias shift [mm]') }else{legend_title <- ''}
  min_scale_r <- -50 ; max_scale_r <- 50 
  divisions <- 10 ; # number of divisions in the scale on the maps
  m_E_biasDiff <- f_map_xyt(df, var_name, index=i)
  
  # albedo_wsa_vis: create the levelplot for hw-mean bias difference
  if(case_study!='hw2018'){
  df <- df_case_study %>% filter(variable == 'albedo_wsa_vis')
  var_name <- 'diff_simSobsSmean' ;  
  if(i==1){legend_title <- paste0( 'Albedo bias shift') }else{legend_title <- ''}
  min_scale_r <- -0.05 ; max_scale_r <- 0.05 
  divisions <- 10 ; # number of divisions in the scale on the maps
  m_albedo_biasDiff <- f_map_xyt(df, var_name, index=i)
  }
  
  if(i==1) {m_LST_anom_2003 <- m_LST_anom ; m_LST_biasDiff_2003 <- m_LST_biasDiff; m_LAI_biasDiff_2003 <- m_LAI_biasDiff ; m_E_biasDiff_2003 <- m_E_biasDiff ; m_albedo_biasDiff_2003 <- m_albedo_biasDiff } 
  if(i==2) {m_LST_anom_2010 <- m_LST_anom ; m_LST_biasDiff_2010 <- m_LST_biasDiff; m_LAI_biasDiff_2010 <- m_LAI_biasDiff ; m_E_biasDiff_2010 <- m_E_biasDiff ; m_albedo_biasDiff_2010 <- m_albedo_biasDiff }  #list_hw_maps_2010 <- c(m_LST_anom, m_LST_biasDiff, m_LAI_biasDiff, m_E_biasDiff, m_albedo_biasDiff)  
  if(i==3) {m_LST_anom_2018 <- m_LST_anom ; m_LST_biasDiff_2018 <- m_LST_biasDiff; m_LAI_biasDiff_2018 <- m_LAI_biasDiff ; m_E_biasDiff_2018 <- m_E_biasDiff ; m_albedo_biasDiff_2018 <- m_albedo_biasDiff }  # list_hw_maps_2018 <- c(m_LST_anom, m_LST_biasDiff, m_LAI_biasDiff, m_E_biasDiff )  
}

# produce pdf or png
#pdf(file = paste0(output_path ,  'test53_allHW.pdf')) #, width=width, height= height )
png(file = paste0(output_path ,  'fig_allHW_allVar.png'), width = width, height = height) #, width=width, height= height )
grid.arrange(m_LST_anom_2003 , m_LST_biasDiff_2003 ,m_LAI_biasDiff_2003 , m_E_biasDiff_2003, m_albedo_biasDiff_2003,
             m_LST_anom_2010 , m_LST_biasDiff_2010 ,m_LAI_biasDiff_2010 , m_E_biasDiff_2010, m_albedo_biasDiff_2010,
             m_LST_anom_2018 , m_LST_biasDiff_2018 ,m_LAI_biasDiff_2018 , m_E_biasDiff_2018, 
             nrow=3, ncol=5  ) # bottom = 'Longitude', left = 'Latitude' # padding = unit(5, "cm")
dev.off()
  


###################################################
######     FINALISE                           #####
###################################################
# timing
end_time <- Sys.time()
print(end_time - start_time)


  
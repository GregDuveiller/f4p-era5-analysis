#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_heatwaves.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot figures illustrating heatwaves
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #


#### Initialization ####

require(ggplot2)
require(scales)
require(viridis)
require(tidyr)
require(dplyr)
require(RColorBrewer)
require(patchwork)

# require(rasterVis)
# require(raster)
# require(gridExtra)

#### Load the data ####

load('data/figures_for_paper/hwAll_varAll_stats.RData')   # <---- "df_all" 
load('data/figures_for_paper/hwAll_gislayers.RData')   # <---- ... 



#### Test with ggplot and sf ####

# some graphic param...
lgd <- theme(legend.position = 'top',
             legend.key.width = unit(1.2, units = 'cm'),
             panel.background = element_rect(fill = 'grey50'),
             panel.grid = element_blank(),
             axis.title = element_blank(),
             strip.text = element_text(size = 12)) 
gds <- guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5, 
                                    frame.colour = 'black', ticks.colour = 'black'))




xlims <- c(-12, 58); ylims <- c(36, 71)

hw_labeller <- labeller(
  hw = c('hw2003' = 'Aug. 2003', 'hw2010' = 'Jul. 2010', 'hw2018' = 'Jul. 2018'))



# LST anomaly
gGEN <- ggplot(df_all %>% filter(variable == 'LST')) + 
  geom_raster(aes(x = x, y = y, fill = diff_obsSmean)) +
  geom_sf(data = ocean_europe, fill = 'grey40', size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hw~., labeller = hw_labeller) +
  scale_fill_gradientn('LST anomaly [K]', 
                       colours = rev(brewer.pal(11, "RdYlBu")), 
                       limits = c(-15, 15), 
                       oob = squish) +
  gds + lgd


# LST shift
gLST <- ggplot(df_all %>% filter(variable == 'LST')) + 
  geom_raster(aes(x = x, y = y, fill = diff_simSobsSmean)) +
  geom_sf(data = ocean_europe, fill = 'grey40', size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hw~., labeller = hw_labeller) + 
  scale_fill_gradientn('LST bias shift [K]', 
                       colours = rev(brewer.pal(11, "RdBu")), 
                       limits = c(-4.5, 4.5), 
                       oob = squish) +
  gds + lgd


# LAI shift
gLAI <- ggplot(df_all %>% filter(variable == 'LAI')) + 
  geom_raster(aes(x = x, y = y, fill = diff_simSobsSmean)) +
  geom_sf(data = ocean_europe, fill = 'grey40', size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hw~., labeller = hw_labeller) + 
  scale_fill_gradientn('LAI bias shift [m2/m2]', 
                       colours = rev(brewer.pal(11, "BrBG")), 
                       limits = c(-1.5, 1.5),
                       oob = squish) +
  gds + lgd


# E shift
gEVA <- ggplot(df_all %>% filter(variable == 'E')) + 
  geom_raster(aes(x = x, y = y, fill = diff_simSobsSmean)) +
  geom_sf(data = ocean_europe, fill = 'grey40', size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hw~., labeller = hw_labeller) + 
  scale_fill_gradientn('Evaporation bias shift [mm]', 
                       colours = rev(brewer.pal(11, "PRGn")), 
                       limits = c(-50, 50), 
                       oob = squish) +
  gds + lgd

# Albedo shift
gALB <- ggplot(df_all %>% filter(variable == 'albedo_wsa_vis_MCD43C3')) + 
  geom_raster(aes(x = x, y = y, fill = diff_simSobsSmean)) +
  geom_sf(data = ocean_europe, fill = 'grey40', size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hw~., labeller = hw_labeller) + 
  scale_fill_gradientn('Albedo bias shift [.]', 
                       colours = rev(brewer.pal(11, "PuOr")), 
                       limits = c(-0.05, 0.05), oob = squish) +
  gds + lgd

# assemble together

g <- gGEN + gLST + gLAI + gEVA + gALB + plot_layout(ncol = 5)


#### Export the figure ####


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'heatwaves'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g,
       path = fig.path, width = 16, height = 8)




# 
# 
# #### Make the plot ####
# 
# # dataframe should be x, y, ... col_names, etc format
# f_map_xyt <- function(df, col_name, index=i) {
#   
#   # levelplot settings
#   cutpts_r <- seq(min_scale_r,max_scale_r, (max_scale_r-min_scale_r)/divisions) 
#   # set values below min or above max to the min max values - i.e. oob
#   df <- df %>% mutate( !!as.symbol(col_name) := ifelse( ( !!as.symbol(col_name) > max_scale_r) , max_scale_r,  ifelse( ( !!as.symbol(col_name) < min_scale_r), min_scale_r, !!as.symbol(col_name)  ) ) )# recalibrate out of bounds values to max val for levelplot
#   
#   # put df in x,y,z format for the levelplot
#   dfr_r <- df[,c(1,2)] ; dfr_r[col_name] <- df[col_name]
#   dfr_r <- rasterFromXYZ(dfr_r)
#   
#   # aggregate pixels if required - this visually aggregates pixels together in the raster. If factor = 1 then no change
#   if(agg_fact != 1){ print('aggregate') ;
#     dfr_r_agg <- aggregate(dfr_r, agg_fact, fun=mean, expand=TRUE, na.rm=TRUE) ## as we have fewer pixels now (unfilled) we should aggregate - making sure to remove NA values
#   }
#   
#   if(add_colorkey){
#     colorkey_list = list(  labels=list(cex=cex_size), labels=as.character( cutpts_r), 
#                            space = "top", # vjust = -.5 #  position
#                            height = map_legend_size # width = 0.5
#                            #title=list('SIF', cex=40)  # legend title cant change size...
#     )  # at=as.numeric( factor( c( seq(-1, 1, by=0.2), "NA"))) )  # key size and cut ticks #  tick.number = length(cutpts_r),
#   }else{ colorkey_list <- F}
#   
#   # make levelplot
#   crs(dfr_r) <- unproj_proj4string # add in the projection
#   plt <-   levelplot(dfr_r, margin=F, at = cutpts_r,   cuts = length(cutpts_r), pretty = F, xlab=x_lab_name, ylab=list(y_lab_name,cex=cex_size), #  xlab=list("longitude", cex=cex_size), ylab=list("latitude", cex=cex_size) # set x-y labels (e.g. lon/lat)
#                      par.settings=mapTheme, maxpixels = n_pix,  # at = cutpts_r
#                      main = list(legend_title, cex=cex_size), # set title and text size
#                      colorkey=colorkey_list, # optional colour key - see above
#                      #scales=list(x=list(cex=cex_size),y=list(cex=cex_size)) # add x-y lon/lat label and ticks and tick size
#                      scales=list(x=list(draw=FALSE), y=list( draw=FALSE),tck = c(0,0) ) #  no x-y label or tick marks
#   ) 
#   # add in bounding box and coastlines
#   plt_layer <- plt  + latticeExtra::layer(sp.lines(coast_lines, col="black", lwd=0.5))
#   
#   # set the margin between the figures - hacked in- needs tweaking and formatting
#   if(index==1){cex_top = 4 ; cex_bottom = -4}
#   if(index==2){cex_top = 2.5 ; cex_bottom = 2.5} # -0.15
#   if(index==3){cex_top = -4 ; cex_bottom = 7}
#   plt_layer$par.settings$layout.heights[
#     c( 'bottom.padding' # 'key.sub.padding', # 'axis.xlab.padding', # 'key.axis.padding', # 'main.key.padding',
#     )] <- cex_bottom
#   plt_layer$par.settings$layout.heights[
#     c( 'top.padding')] <- cex_top
#   plt_layer$par.settings$layout.widths[
#     c( 'left.padding', 'right.padding'
#     )] <- -0.1
#   plt_layer$aspect.fill <- TRUE
#   
#   return(plt_layer)
# }
# 
# 
# 
# # list of dataframes to evaluate 
# df_hw = data.frame( row.names=c("hw2003","hw2010","hw2018") )
# 
# # universal settings - maps
# cex_size = 3 ; # cex_size = 2 # big maps; 
# n_pix <- 2e8 # n_pix <- 2e7 # maximum number of pixels in plots (2e7 very big but contains all)
# 
# text_size = 30; #label
# axis_text_size = 30; 
# # Colours and aggregation
# agg_fact <- 1 # no aggregation
# map_legend_size <- 0.75
# 
# 
# for( i in 1:dim(df_hw)[1] ){ # i <- 1
#   print(i) ; case_study <- row.names(df_hw)[i] ; print( case_study  )  
#   
#   # filter to specific case study
#   df_case_study <- df_all %>% filter(hw == case_study)
#   if(i==1){add_colorkey = T } else{ add_colorkey = F} # only x-axis labels on first
#   x_lab_name <- NULL # ;  y_cex_tck_size <- cex_size # choose whether to add an x/y label (e.g. Lon/Lat) - can also chose to add tickmarks
#   
#   mapTheme <- rasterTheme(region=rev(brewer.pal(11, "RdYlBu"))) # set colourscale for all/some maps
#   
#   # HW temperature anomaly: create the levelplot
#   df <- df_case_study %>% filter(variable == 'LST')
#   if(i==1){legend_title <- paste0( 'LST anomaly [C]') } else{legend_title <- ''} # label of legend
#   y_lab_name <- substr(case_study,3,7) # ; y_cex_tck_size <- 0 # add facet label for heatwave year
#   
#   var_name <- 'diff_obsSmean' ;  
#   min_scale_r <- -15 ; max_scale_r <- 15  # scale range
#   divisions <- 10 ; # number of divisions in the scale on the map
#   m_LST_anom <- f_map_xyt(df, var_name, index=i) # create map
#   
#   # other plots in the row - add/remove y-axis and ticks
#   y_lab_name <- NULL # NULL # ; y_cex_tck_size <- 0
#   
#   # LST: create the levelplot for hw-mean bias difference
#   df <- df_case_study %>% filter(variable == 'LST')
#   var_name <- 'diff_simSobsSmean' ;  
#   if(i==1){legend_title <- paste0( 'LST bias shift [C]') }else{legend_title <- ''}
#   min_scale_r <- -5 ; max_scale_r <- 5 
#   divisions <- 10 ; # number of divisions in the scale on the maps
#   m_LST_biasDiff <- f_map_xyt(df, var_name, index=i)
#   
#   # LAI: create the levelplot for hw-mean bias difference
#   df <- df_case_study %>% filter(variable == 'LAI')
#   var_name <- 'diff_simSobsSmean' ;  
#   if(i==1){legend_title <- paste0( 'LAI bias shift [m2/m2]') }else{legend_title <- ''}
#   min_scale_r <- -1.5 ; max_scale_r <- 1.5 
#   divisions <- 12 ; # number of divisions in the scale on the maps
#   m_LAI_biasDiff <- f_map_xyt(df, var_name, index=i)
#   
#   # E: create the levelplot for hw-mean bias difference
#   df <- df_case_study %>% filter(variable == 'E')
#   var_name <- 'diff_simSobsSmean' ;  
#   if(i==1){legend_title <- paste0( 'Evaporation bias shift [mm]') }else{legend_title <- ''}
#   min_scale_r <- -50 ; max_scale_r <- 50 
#   divisions <- 10 ; # number of divisions in the scale on the maps
#   m_E_biasDiff <- f_map_xyt(df, var_name, index=i)
#   
#   # albedo_wsa_vis: create the levelplot for hw-mean bias difference
#   if(case_study!='hw2018'){
#     df <- df_case_study %>% filter(variable == 'albedo_wsa_vis')
#     var_name <- 'diff_simSobsSmean' ;  
#     if(i==1){legend_title <- paste0( 'Albedo bias shift') }else{legend_title <- ''}
#     min_scale_r <- -0.05 ; max_scale_r <- 0.05 
#     divisions <- 10 ; # number of divisions in the scale on the maps
#     m_albedo_biasDiff <- f_map_xyt(df, var_name, index=i)
#   }
#   
#   if(i==1) {m_LST_anom_2003 <- m_LST_anom ; m_LST_biasDiff_2003 <- m_LST_biasDiff; m_LAI_biasDiff_2003 <- m_LAI_biasDiff ; m_E_biasDiff_2003 <- m_E_biasDiff ; m_albedo_biasDiff_2003 <- m_albedo_biasDiff } 
#   if(i==2) {m_LST_anom_2010 <- m_LST_anom ; m_LST_biasDiff_2010 <- m_LST_biasDiff; m_LAI_biasDiff_2010 <- m_LAI_biasDiff ; m_E_biasDiff_2010 <- m_E_biasDiff ; m_albedo_biasDiff_2010 <- m_albedo_biasDiff }  #list_hw_maps_2010 <- c(m_LST_anom, m_LST_biasDiff, m_LAI_biasDiff, m_E_biasDiff, m_albedo_biasDiff)  
#   if(i==3) {m_LST_anom_2018 <- m_LST_anom ; m_LST_biasDiff_2018 <- m_LST_biasDiff; m_LAI_biasDiff_2018 <- m_LAI_biasDiff ; m_E_biasDiff_2018 <- m_E_biasDiff ; m_albedo_biasDiff_2018 <- m_albedo_biasDiff }  # list_hw_maps_2018 <- c(m_LST_anom, m_LST_biasDiff, m_LAI_biasDiff, m_E_biasDiff )  
# }
# 
# # plotting details, in case not inherited... 
# if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
# if(exists('fig.fmt') != T){ fig.fmt <- 'png'}
# 
# dir.create(path = fig.path, recursive = T, showWarnings = F)
# 
# fig.name <- 'heatwaves'
# 
# # ggsave(filename = paste0(fig.name, '.', fig.format), plot = g_all_smooth,
# #        path = fig.path, width = 9, height = 11)
# 
# 
# png(file = paste0(fig.path ,  fig.name, '.', fig.fmt), width = 2800, height = 1600)
# grid.arrange(m_LST_anom_2003 , m_LST_biasDiff_2003 ,m_LAI_biasDiff_2003 , m_E_biasDiff_2003, m_albedo_biasDiff_2003,
#              m_LST_anom_2010 , m_LST_biasDiff_2010 ,m_LAI_biasDiff_2010 , m_E_biasDiff_2010, m_albedo_biasDiff_2010,
#              m_LST_anom_2018 , m_LST_biasDiff_2018 ,m_LAI_biasDiff_2018 , m_E_biasDiff_2018, 
#              nrow=3, ncol=5  ) # bottom = 'Longitude', left = 'Latitude' # padding = unit(5, "cm")
# dev.off()
# 
# 

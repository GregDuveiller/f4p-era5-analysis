# original name : mkplots___biasWtemp.R
#
# ########################################################
# Title         : 
# Description   : This code aims to produce figures showing LST/LAI bias of CZ quantiles in Europe
# Inputs	      : df_comb___LAI.RData etc produced by Greg initially, then filtered through prepare___biasWtemp.R script
#                 this script takes average LST and LAI in quantiles across European CZ (European data points only)
# Outputs	      : figures of LST/LAI bias for each CZ quantile
# Options	      : 
# Date          : 07/12/21
# Version       : 1.3 - this is updated the paper
# Licence       : N/A
# Authors       : Mark Pickering
# Maintainer    : Mark Pickering
# Notes		      : 
# Example use   : 
# ########################################################

# initialise
rm(list = ls()) 
start_time <- Sys.time() ; print(start_time)
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ") ; full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]


library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pals)
library(patchwork)
library(sf)
library(RColorBrewer)


###################################################
######       I/O                                ###
###################################################

output_root  <- '/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/data/COP_tempAnomaly/figures/'
output_path <- paste0(output_root, full_date,'/')
print(paste0('output_path is : ', output_path ))
if(! dir.exists(output_path)) {dir.create(paste0(output_path),recursive=T)}

# load clim zone locations...  these are the climate zones that are mapped onto the maps (same dimensions as all paper figures)
load('data/inter_data/ancillary_info/df_KG_climatezones.RData', verbose = T) # df_lgd
#load('/home/mark/Documents/work_jrc/work_d1/work_COPERNICUS/work_COPERNICUSII_V3_2021/scripts/KG_class/Map_KG-Global/df_KG_climatezones.RData', verbose = T) # df_lgd
v_cz_in <- c('Dfc', 'Dfb', 'Csa', 'Csb', 'Cfa', 'Cfb') # chose which to consider (or remove if too detailed)
v_lon_min <- -12 ; v_lon_max <- 58  ; v_lat_min <- 36 ; v_lat_max <- 71 # reduce area of interest to Europe for the mapping (data is already Europe only)
df_cz <- df_cz %>% 
  filter(x >= v_lon_min  & x <= v_lon_max) %>%
  filter( y >= v_lat_min & y <= v_lat_max ) 

# load vectors... (for coasts on figures) - Greg change this to europe (I was unsure where you got your vectors from and don't want to use similar but not exact versions - perhaps you can add them)
wmap_df_land <- st_read('data/input_data/vector_layers/ne_50m_land.shp', quiet = T)
wmap_df_ocean <- st_read('data/input_data/vector_layers/ne_50m_ocean.shp', quiet = T)
# wmap_df_land <- st_read('/home/mark/ownCloud/copernicus/data/land_maps/ne_50m_land.shp', quiet = T)
# wmap_df_ocean <- st_read('/home/mark/ownCloud/copernicus/data/land_maps/ne_50m_ocean.shp', quiet = T)

# input dataframe
load('data/COP_tempAnomaly/data/df_LSTLAI_tempAnomalyQuantile_aug.RData')
#load('/home/mark/ownCloud/copernicus/scripts/git_paper_version/f4p-era5-analysis/data/COP_tempAnomaly/data/df_LSTLAI_tempAnomalyQuantile_aug.RData')

###################################################
######     UNIVERSAL MAP SETTINGS             #####
###################################################
# ggplot2 parameters
rel_txt_size <- 3
axis_text_size = 30; 
legend_size <- 1.75

###################################################
######     CREATE FIGURE                      #####
###################################################


g_plot_quantile <- ggplot(df_all, aes_string(x = 'LST_anomaly' , y = 'Bias')) + 
  geom_line( size = 5) +
  geom_hline(yintercept = 0, colour = 'black') +
  geom_vline(xintercept = 0, colour = 'black') +
  facet_grid(y_var~CZ,  scales = 'free_y') +
  scale_y_continuous( 'Bias' ) + # , limits = y_scale
  scale_x_continuous( 'Temperature anomaly [C]' ) + # , limits = x_scale
  #ggtitle(label = paste0(legend_title)) + 
  theme_grey(base_size = 12) + 
  theme( legend.position = 'bottom',
         text = element_text(size=rel(rel_txt_size)),
         strip.text.x = element_text(size=rel(rel_txt_size*2.5)),
         strip.text.y = element_text(size=rel(rel_txt_size*2.5)), 
         legend.text=element_text(size=rel_txt_size*5), legend.title=element_text(size=rel_txt_size*10),
         legend.key.size = unit(legend_size*1.3, "cm"), legend.key.width = unit(legend_size*1.3, "cm"),
         axis.title=element_text(size=axis_text_size,face="bold"),
         axis.text=element_text(size=axis_text_size))  
 #g_plot_quantile

 
 cz_cols <- df_lgd$cz_colours
 names(cz_cols) <- df_lgd$cz_name
 
 # I'm not sure which european map you are usinng, but just change the geom_sf here to europe
 g_maps <- ggplot(df_cz %>% 
                    filter(cz_name %in% v_cz_in) ) + 
   geom_sf(data = wmap_df_land, fill='Grey50',colour='Grey50',size=0) + 
   geom_sf(data = wmap_df_ocean, fill='Grey20',colour='Grey20',size=0) +
   geom_tile(aes(x = x, y = y, fill = cz_name)) + 
   facet_grid(~cz_name) +
   scale_fill_manual('Koppen-Geiger climate zones', values = cz_cols) + 
   scale_y_continuous('Latitude', expand = c(0, 0)) + 
   scale_x_continuous('Longitude', expand = c(0, 0)) + 
   theme(legend.position = 'none',
         text = element_text(size=rel(rel_txt_size*1.5)),
         strip.text.x = element_blank() , #element_text(size=rel(rel_txt_size*2.5)),
         strip.text.y = element_blank()   #element_text(size=rel(rel_txt_size*2.5))
   ) + 
   guides(fill = guide_legend(title.position = 'top', nrow = 4))
 
 g_combo <- g_maps + g_plot_quantile + plot_layout(nrow = 2, heights = c(1, 1)) # , heights = c(1, 4), widths = c(1,1)
 
 ggsave(filename = paste0(output_path, 'tempAnomaly_vs_bias.png'), 
        plot = g_combo , width = 50, height = 25, dpi = 300)




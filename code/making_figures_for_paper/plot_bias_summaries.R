#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_bias_summaries.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot figure showing general overview of the bias
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

#### Load the data ####

load('data/final_data/figures_for_paper/data_for_bias_summary_maps.RData')   

# load land shapefile of the world
land_shapefile <- "data/input_data/world_vectors/ne_50m_land.shp"
land <- sf::st_read(land_shapefile, quiet = TRUE)



#### Make the plots ####

# set-up gray colours for maps
if(exists('gry_land') != T){ gry_land <- 'grey20'}
if(exists('gry_meer') != T){ gry_meer <- 'grey30'}


# some background details
bck_details <- theme(legend.key.height = unit(1.5, 'cm'),
                     panel.background = element_rect(fill = gry_meer),
                     panel.grid = element_line(colour = gry_meer))


season_labeller <- labeller(monthS = c('1' = 'Winter', '7' = 'Summer'))





# LAI maps 
LAI_bias_colpal <- RColorBrewer::brewer.pal(n = 9, name = 'BrBG')

gLAImaps <- ggplot(df_LAI_bias) + 
  geom_sf(data = land, fill = gry_land, colour = NA) +
  geom_tile(aes(x = x, y = y, fill = diff_simSobs)) +  
  scale_fill_gradientn('LAI bias', 
                       colours = LAI_bias_colpal,
                       limits = c(-2,2), oob = squish) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  facet_grid(monthS~., labeller = season_labeller) + 
  coord_sf(expand = F) +
  theme(legend.position = "left") + bck_details 



# LST maps... 
LST_bias_colpal <- rev(RColorBrewer::brewer.pal(n = 9, name = 'RdBu'))

gLSTmaps <- ggplot(df_LST_bias) + 
  geom_sf(data = land, fill = gry_land, colour = NA) +
  geom_tile(aes(x = x, y = y, fill = diff_simSobs)) +  
  scale_fill_gradientn('LST bias', 
                       colours = LST_bias_colpal,
                       limits = c(-20,20), oob = squish) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  facet_grid(monthS~., labeller = season_labeller) + 
  coord_sf(expand = F) +
  theme(legend.position = "right") + bck_details 



#### Export the figure #### 

# assemble the panels...

# organise the layout
g_all <- gLAImaps + gLSTmaps + plot_annotation(
  title = 'Mean interannual bias for leaf area index (LAI) and for land surface temperature (LST)',
  subtitle = 'Biases are defined as ERA5L minus observations',
  caption = '[Each season in this plot is represented by a single month, consisting of either January or July depeding on whether the data is in the Northern or Southern Hemisphere]')
#+ plot_layout(guides = 'collect')


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'bias_summaries'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g_all,
       path = fig.path, width = 12, height = 5)

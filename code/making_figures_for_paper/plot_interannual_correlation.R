#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_interannual_correlation.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot figure showing the interannual correlation of the biases
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

load('data/final_data/figures_for_paper/data_for_corr_summary_maps.RData')   

land_shapefile <- "data/input_data/world_vectors/ne_50m_land.shp"
land <- sf::st_read(land_shapefile, quiet = TRUE)

#### Make the plots ####

# set-up gray colours for maps
if(exists('gry_land') != T){ gry_land <- 'grey50'}
if(exists('gry_meer') != T){ gry_meer <- 'grey30'}


# some background details
bck_details <- theme(legend.key.height = unit(1.5, 'cm'),
                     panel.background = element_rect(fill = gry_meer),
                     panel.grid = element_blank())


season_labeller <- labeller(monthS = c('1' = 'Winter', '7' = 'Summer'))



# Corr maps... 
corr_colpal <- c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#F7F7F7", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695")

gmaps <- ggplot(df_LSTb_LAIb_corr %>% filter(p < 0.05)) + 
  geom_sf(data = land, fill = gry_land, colour = NA) +
  geom_tile(aes(x = x, y = y, fill = r)) +  
  scale_fill_gradientn('Correlation', 
                       colours = corr_colpal, na.value = gry_land,
                       limits = c(-1,1), oob = squish) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  facet_grid(monthS~., labeller = season_labeller) + 
  coord_sf(expand = F) +
  theme(legend.position = "right") + bck_details 

#### Export the figure #### 

# assemble the panels...
g_all <- gmaps +
  plot_annotation( 
    title = 'Inter-annual correlation (p < 0.05) between the biases in LAI and in LST', 
    subtitle = 'Biases are defined as ERA5L minus observations',
    caption = '[Each season in this plot is represented by a single month, consisting of either January or July\n depeding on whether the data is in the Northern or Southern Hemisphere]')


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'corr_summaries'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g_all,
       path = fig.path, width = 6, height = 5)


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

load('data/figures_for_paper/data_for_summary_maps.RData')   # <---- ...  for now, there is a lot in here... needs to be trimmed

df_LAI_bias <- sp_agr_monthS_var1 %>%  # <---- this and the one below should be integraded in the input above... 
  filter(monthS %in% c(1,7)) %>%
  select(x, y, monthS, diff_simSobs)

df_LST_bias <- sp_agr_monthS_var2 %>% 
  filter(monthS %in% c(1,7)) %>%
  select(x, y, monthS, diff_simSobs)


land_shapefile <- "data/input_data/vector_layers/ne_50m_land.shp"
land <- sf::st_read(land_shapefile, quiet = TRUE)



#### Make the plots ####

# some background details
bck_details <- theme(legend.key.height = unit(1.5, 'cm'),
                     panel.background = element_rect(fill = 'grey30'),
                     panel.grid = element_line(colour = 'grey40'))


season_labeller <- labeller(monthS = c('1' = 'Winter', '7' = 'Summer'))


# LAI maps 
LAI_bias_colpal <- RColorBrewer::brewer.pal(n = 9, name = 'BrBG')

gLAImaps <- ggplot(df_LAI_bias) + 
  geom_sf(data = land, fill = 'grey20', colour = NA) +
  geom_tile(aes(x = x, y = y, fill = diff_simSobs)) +  
  scale_fill_gradientn('LAI bias', 
                       colours = LAI_bias_colpal,
                       limits = c(-2,2), oob = squish) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  facet_grid(monthS~., labeller = season_labeller) + 
  coord_sf(expand = F) +
  theme(legend.position = "left") + bck_details +
  ggtitle('Mean interannual bias for LAI', 
          subtitle = '[Represented by a single seasonally-corrected month]')



# LST maps... 
LST_bias_colpal <- RColorBrewer::brewer.pal(n = 9, name = 'RdBu')

gLSTmaps <- ggplot(df_LST_bias) + 
  geom_sf(data = land, fill = 'grey20', colour = NA) +
  geom_tile(aes(x = x, y = y, fill = diff_simSobs)) +  
  scale_fill_gradientn('LST bias', 
                       colours = LST_bias_colpal,
                       limits = c(-20,20), oob = squish) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  facet_grid(monthS~., labeller = season_labeller) + 
  coord_sf(expand = F) +
  theme(legend.position = "right") + bck_details +
  ggtitle('Mean interannual bias for LST', 
          subtitle = '[Represented by a single seasonally-corrected month]')



#### Export the figure #### 

# assemble the panels...

# organise the layout
g_all <- gLAImaps + gLSTmaps #+ plot_layout(guides = 'collect')


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'bias_summaries'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g_all,
       path = fig.path, width = 12, height = 5)

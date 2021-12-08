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

load('data/figures_for_paper/data_for_summary_maps.RData')   # <---- ...  for now, there is a lot in here... needs to be trimmed

df_dum <- data.frame(
  x = sp_agr_monthS_var2$x,
  y = sp_agr_monthS_var2$y,
  monthS = sp_agr_monthS_var2$monthS,
  r = sp_agr_monthS_var2$agre_bVARvsbLAI$r)

df_LSTb_LAIb_corr <- df_dum %>% 
  filter(monthS %in% c(1,7)) %>%
  select(x, y, monthS, r)

#### Make the plots ####

# some background details
bck_details <- theme(legend.key.height = unit(1.5, 'cm'),
                     panel.background = element_rect(fill = 'grey30'),
                     panel.grid = element_line(colour = 'grey40'))


season_labeller <- labeller(monthS = c('1' = 'Winter', '7' = 'Summer'))



# Corr maps... 
corr_colpal <- c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#F7F7F7", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695")

gmaps <- ggplot(df_LSTb_LAIb_corr) + 
  geom_sf(data = land, fill = 'grey20', colour = NA) +
  geom_tile(aes(x = x, y = y, fill = r)) +  
  scale_fill_gradientn('Correlation', 
                       colours = corr_colpal,
                       limits = c(-1,1), oob = squish) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  facet_grid(monthS~., labeller = season_labeller) + 
  coord_sf(expand = F) +
  theme(legend.position = "right") + bck_details +
  ggtitle('Inter-annual correlation between the biases in LAI and those in LST', 
          subtitle = '[Represented by a single seasonally-corrected month]')



#### Export the figure #### 

# assemble the panels...
g_all <- gmaps

# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'corr_summaries'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g_all,
       path = fig.path, width = 6, height = 5)


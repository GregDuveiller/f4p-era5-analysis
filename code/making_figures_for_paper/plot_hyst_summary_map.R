#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_hyst_summary_map.R ####
# ---------------------------------------------------------------------------- #
# Purpose: process data to make figures of hysteresis plots
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #



#### Initialization ####

require(ggplot2)
require(scales)
require(viridis)
require(tidyr)
require(RColorBrewer)
require(patchwork)



#### Load the data ####

load('data/final_data/figures_for_paper/hysteresis_map_ready4fig.RData')   # <----- df_hyst_map


#### Make the plots ####

# set-up gray colours for maps
if(exists('gry_land') != T){ gry_land <- 'grey50'}
if(exists('gry_meer') != T){ gry_meer <- 'grey30'}



col_pal_BD <- c('#4B90FE', '#9BB4FA',  '#C9D2F6', '#EBECF0', '#E7DFC9', '#CEBB7B', '#AA9419' )

lgd <- theme(legend.position = 'right',
             legend.key.height = unit(1.2, units = 'cm'),
             panel.background = element_rect(fill = gry_land),
             panel.grid = element_blank()) 
gds <- guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5, 
                                    frame.colour = 'black', ticks.colour = 'black'))


lgd_map <-   theme(legend.position = 'none',
                   panel.background = element_rect(fill = gry_meer),
                   panel.grid = element_blank(),
                   axis.title = element_blank())

gmapBiasI <- ggplot(df_hyst_map) + 
  geom_tile(aes(x = x, y = y, fill = biasI)) +
  scale_fill_gradientn(colours = col_pal_BD, na.value = gry_land) + 
  coord_cartesian(expand = F, ylim = c(-54, 86)) +
  lgd_map

gcspBiasI <- ggplot(df_hyst_map) +
  geom_tile(aes(x = t2.clim, y = sm.clim, fill = biasI)) +
  scale_fill_gradientn('BD', colours = col_pal_BD) +    
  scale_x_continuous('Mean annual temperature') +
  scale_y_continuous('Mean annual soil moisture') +
  coord_cartesian(expand = F) +
  ggtitle('Which is the dominating bias?', subtitle = '(green for LAI, red for LST)') +
  lgd + gds


gmapHystI <- ggplot(df_hyst_map) + 
  geom_tile(aes(x = x, y = y, fill = hystI)) +
  scale_fill_viridis_c(option = 'F', na.value = gry_land) +
  coord_cartesian(expand = F, ylim = c(-54, 86)) +
  lgd_map

gcspHystI <- ggplot(df_hyst_map) +
  geom_tile(aes(x = t2.clim, y = sm.clim, fill = hystI)) +
  scale_fill_viridis_c('HI', option = 'F') +
  scale_x_continuous('Mean annual temperature') +
  scale_y_continuous('Mean annual soil moisture') +
  coord_cartesian(expand = F) +
  ggtitle('Is hysteresis relatively important?', subtitle = '(Light mean YES, dark means NO)') +
  lgd + gds



#### Export the figure ####

# assemble the figure

gAll <- gcspHystI +  gmapHystI +gcspBiasI +  gmapBiasI + 
  plot_layout(ncol = 2, widths = c(1,3))



# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'hystereris_maps'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = gAll,
       path = fig.path, width = 12, height = 8)

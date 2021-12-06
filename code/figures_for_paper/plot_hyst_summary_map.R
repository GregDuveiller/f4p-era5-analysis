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

load('data/figures_for_paper/hysteresis_map_ready4fig.RData')   # <----- df_hyst_map


#### Make the data ####

gmapBiasI <- ggplot(df_hyst_map) + 
  geom_tile(aes(x = x, y = y, fill = biasI)) +
  scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn')) + 
  coord_cartesian(expand = F, ylim = c(-54, 86)) +
  theme(legend.position = 'right',
        legend.key.height = unit(1.4, units = 'cm'),
        axis.title = element_blank())

gcspBiasI <- ggplot(df_hyst_map) +
  geom_tile(aes(x = t2.clim, y = sm.clim, fill = biasI)) +
  scale_fill_gradientn(colours = brewer.pal(9, 'RdYlGn')) + 
  coord_cartesian(expand = F) +
  theme(legend.position = 'none')


gmapHystI <- ggplot(df_hyst_map) + 
  geom_tile(aes(x = x, y = y, fill = hystI)) +
  scale_fill_viridis_c(option = 'F') +
  coord_cartesian(expand = F) +
  coord_cartesian(expand = F, ylim = c(-54, 86)) +
  theme(legend.position = 'right',
        legend.key.height = unit(1.4, units = 'cm'),
        axis.title = element_blank())

gcspHystI <- ggplot(df_hyst_map) +
  geom_tile(aes(x = t2.clim, y = sm.clim, fill = hystI)) +
  scale_fill_viridis_c(option = 'F') +
  coord_cartesian(expand = F) +
  theme(legend.position = 'none')



#### Export the figure ####

# assemble the figure

gAll <- gcspBiasI +  gmapBiasI + gcspHystI +  gmapHystI + 
  plot_layout(ncol = 2, widths = c(1,3))



# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'hystereris_maps'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = gAll,
       path = fig.path, width = 12, height = 8)

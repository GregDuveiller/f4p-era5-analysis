#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_heatwave_timeseries.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot time series of spatially averaged heatwave effects 
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

load('data/final_data/figures_for_paper/hw_ts.RData')   # <---- ... 


#### Make the plots ####


lgd <- theme(legend.position = 'bottom',
             #legend.key.width = unit(1.2, units = 'cm'),
             #panel.grid = element_blank(),
             strip.text = element_text(size = 12)) 
# gds <- guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5, 
#                                     frame.colour = 'black', ticks.colour = 'black'))

hw_labeller <- labeller(
  hwname = c('HW03' = 'HW03', 'HW10' = 'HW10', 'HW18a' = 'HW18a', 'HW18b' = 'HW18b'))


gEVA <- ggplot(ts_E, aes(x = month)) +
  geom_vline(aes(xintercept = hwmonth), size = 14, colour = 'grey80') +
  geom_line(aes(y = E, colour = type, linetype = source)) +
  facet_grid(hwname~., labeller = hw_labeller) +
  #scale_colour_manual(values = c('LAI_clim'='grey20','LAI_year'= 'cornflowerblue')) +
  scale_colour_manual(values = c('E_year'= 'cornflowerblue','E_clim'='grey20')) +
  scale_y_continuous('E [mm]') + 
  scale_x_continuous('', expand = c(0,0), 
                     breaks = c(3,6,9,12),
                     labels = c('3'='Mar', '6'='Jun', '9'='Sep', '12'='Dec')) +
  #theme_bw() + 
  lgd


gLAI <- ggplot(ts_LAI, aes(x = month)) +
  geom_vline(aes(xintercept = hwmonth), size = 14, colour = 'grey80') +
  geom_line(aes(y = LAI, colour = type, linetype = source)) +
  facet_grid(hwname~., labeller = hw_labeller) +
  #scale_colour_manual(values = c('LAI_clim'='grey20','LAI_year'= 'cornflowerblue')) +
  scale_colour_manual(values = c('LAI_year'= 'cornflowerblue','LAI_clim'='grey20')) +
  scale_y_continuous('LAI [m2/m2]') + 
  scale_x_continuous('', expand = c(0,0), 
                     breaks = c(3,6,9,12),
                     labels = c('3'='Mar', '6'='Jun', '9'='Sep', '12'='Dec')) +
  #theme_bw() + 
  lgd


gLST <- ggplot(ts_LST, aes(x = month)) +
  geom_vline(aes(xintercept = hwmonth), size = 14, colour = 'grey80') +
  geom_line(aes(y = LST, colour = type, linetype = source)) +
  facet_grid(hwname~., labeller = hw_labeller) +
  #scale_colour_manual(values = c('LAI_clim'='grey20','LAI_year'= 'cornflowerblue')) +
  scale_colour_manual(values = c('LST_year'= 'cornflowerblue','LST_clim'='grey20')) +
  scale_y_continuous('LST [K]') + 
  scale_x_continuous('', expand = c(0,0), 
                     breaks = c(3,6,9,12),
                     labels = c('3'='Mar', '6'='Jun', '9'='Sep', '12'='Dec')) +
  #theme_bw() + 
  lgd


gALB <- ggplot(ts_Albedo, aes(x = month)) +
  geom_vline(aes(xintercept = hwmonth), size = 14, colour = 'grey80') +
  geom_line(aes(y = Albedo, colour = type, linetype = source)) +
  facet_grid(hwname~., labeller = hw_labeller) +
  #scale_colour_manual(values = c('LAI_clim'='grey20','LAI_year'= 'cornflowerblue')) +
  scale_colour_manual(values = c('Albedo_year'= 'cornflowerblue','Albedo_clim'='grey20')) +
  scale_y_continuous('Albedo [.]') + 
  scale_x_continuous('', expand = c(0,0), 
                     breaks = c(3,6,9,12),
                     labels = c('3'='Mar', '6'='Jun', '9'='Sep', '12'='Dec')) +
  #theme_bw() +  
  lgd

#### Export the figure ####

# assemble the figure
g <- gLAI + gLST + gEVA +  plot_layout(ncol = 3)

# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'heatwave_timeseries'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g,
       path = fig.path, width = 14, height = 8)




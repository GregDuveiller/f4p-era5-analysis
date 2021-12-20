#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_heatwave_locations.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot figures introducing the location of heatwaves
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

#### Load the data ####

load('data/final_data/figures_for_paper/hw_maps.RData')   # <---- "df_all" 
load('data/final_data/figures_for_paper/hw_gislayers.RData')   # <---- ... 


#### Make the plots and sf ####

# set-up gray colours for maps
if(exists('gry_land') != T){ gry_land <- 'grey50'}
if(exists('gry_meer') != T){ gry_meer <- 'grey30'}


# some graphic param...
lgd <- theme(legend.position = 'bottom',
             legend.key.width = unit(1.8, units = 'cm'),
             panel.background = element_rect(fill = gry_land),
             panel.grid = element_blank(),
             axis.title = element_blank(),
             strip.text = element_text(size = 12)) 
gds <- guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5, 
                                    frame.colour = 'black', ticks.colour = 'black'))


# col_palettes ...

col_pal_df <- data.frame(LST = rev(brewer.pal(11, "RdBu")),
  stringsAsFactors = F)



xlims <- c(-12, 58); ylims <- c(36, 71)


hw_lbls <- c('2003' = 'Western Europe (Aug. 2003)', 
             '2010' = 'Western Russia (Jul. 2010)', 
             '2018' = 'Central/Northern Europe (Jul. 2018)')

hw_labeller <- labeller(hwyear = hw_lbls)



# LST anomaly
gGEN <- ggplot(df_all %>% filter(variable == 'LST')) + 
  geom_raster(aes(x = x, y = y, fill = diff_obsSmean)) +
  geom_sf(data = ocean_europe, fill = gry_meer, size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  geom_sf_text(data = hw_polygons, aes(label = hwname), 
               fontface = 'bold', colour = 'black', size = 3.5, nudge_y = -2.0) +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(.~hwyear, labeller = hw_labeller) +
  scale_fill_gradientn('LST anomaly [K]', 
                       colours = rev(brewer.pal(11, "RdYlBu")), 
                       limits = c(-15, 15), 
                       oob = squish) +
  gds + lgd


#### Export the figure ####


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'heatwave_intro'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = gGEN,
       path = fig.path, width = 12, height = 5)




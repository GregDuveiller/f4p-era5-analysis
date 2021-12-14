#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_bias_against_temp_anomalies.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot figure showing the biases for different temp anomalies
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


#### Load and prepare the data ####

load('data/final_data/figures_for_paper/df_LSTLAI_tempAnomalyQuantile_aug.RData')
load('data/final_data/figures_for_paper/hwAll_gislayers.RData')
load('data/inter_data/ancillary_info/df_KG_climatezones.RData') # df_lgd & df_cz   # <---- should probably be in "final_data"

# set-up climate zones to show
v_cz_in <- c('Dfc', 'Dfb', 'Cfb', 'Csa')

# set-up geographical boundaries
v_lon_min <- -12
v_lon_max <- 58 
v_lat_min <- 36
v_lat_max <- 71 

# prepare dataset for maps
df_cz_sub <- df_cz %>% 
  filter(x >= v_lon_min & x <= v_lon_max) %>%
  filter(y >= v_lat_min & y <= v_lat_max) %>% 
  filter(cz_name %in% v_cz_in) %>%
  mutate(cz_name = factor(cz_name, levels = v_cz_in)) %>%
  tibble()

# prepare quantiles
df_qtls <- df_all %>% 
  filter(cz_name %in% v_cz_in) %>%
  mutate(cz_name = factor(cz_name, levels = v_cz_in)) %>%
  mutate(bias_rescaled = ifelse(y_var == 'LAI', bias, bias/10)) %>% # <----  Rescaling LST to deciKelvin
  tibble() 


#### Make the plots ####

# set-up gray colours for maps
if(exists('gry_land') != T){ gry_land <- 'grey50'}
if(exists('gry_meer') != T){ gry_meer <- 'grey30'}

# Set-up some colour parameters
gry1 <- 'grey60'  # <-- the axes
gry2 <- 'grey20'  # <-- the borders of the shapes/circles

# Colours for the legend
cz_cols <- df_lgd$cz_colours
names(cz_cols) <- df_lgd$cz_name

# get the labels of facets a bit more sophisticated
var_labeller = labeller(y_var = c('LAI' = 'Bias in LAI [m2/m2]', 'LST' = 'Bias in LST [K]'))

# plot for the maps
g_maps <- ggplot(df_cz_sub) + 
  geom_sf(data = land_europe, fill = gry_land, colour = gry_land, size = 0) + 
  geom_tile(aes(x = x, y = y, fill = cz_name)) + 
  geom_sf(data = ocean_europe, fill = gry_meer, colour = gry_meer, size = 0) +
  facet_grid(.~cz_name) +
  coord_sf(ylim = c(v_lat_min, v_lat_max)) + 
  scale_fill_manual('Koppen-Geiger climate zones', values = cz_cols) + 
  scale_y_continuous('', expand = c(0, 0)) + 
  scale_x_continuous('', expand = c(0, 0)) + 
  theme(legend.position = 'none') +
  ggtitle('How the biases change for different thermal anomalies',
          subtitle = "Detailed for selected Koppen-Geiger climate zones across Europe")

# plot for the quantiles
g_plot_quantile <- ggplot(df_qtls) + 
  geom_hline(yintercept = 0, colour = gry1) +
  geom_vline(xintercept = 0, colour = gry1) +
  geom_line(aes(x = LST_anomaly_value, y = bias, colour = y_var), 
            size = 1, show.legend = F) +
  geom_point(aes(x = LST_anomaly_value, y = bias, colour = y_var,
                 fill = LST_anomaly_quantile), 
             size = 3, shape = 21, show.legend = F) +
  facet_grid(y_var~cz_name,  scales = 'free_y', labeller = var_labeller) +
  scale_y_continuous( 'Bias (ERA - obs)' ) + # , limits = y_scale
  scale_x_continuous( 'Land surface temperature anomaly [K]' ) + # , limits = x_scale
  scale_colour_manual('', values = c('LAI' = gry2, 'LST' = gry2)) + # , limits = x_scale
  scale_fill_gradientn('Quantile', colours = rev(RColorBrewer::brewer.pal(10, 'RdBu'))) + # , limits = y_scale
  theme(legend.position = 'bottom')



#### Export the figure #### 

# assemble the panels...
g_all <- g_maps + g_plot_quantile + plot_layout(nrow = 2, heights = c(1, 2)) # , heights = c(1, 4), widths = c(1,1)
# g_all <- g_maps + g_plot_quantile + plot_layout(ncol = 2, widths = c(1, 2)) # , heights = c(1, 4), widths = c(1,1)

# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'bias_vs_anomaly'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g_all,
       path = fig.path, width = 10, height = 8)
 

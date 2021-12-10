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

#### Load the data ####

load('data/figures_for_paper/df_LSTLAI_tempAnomalyQuantile_aug.RData')
load('data/figures_for_paper/hwAll_gislayers.RData')
load('data/inter_data/ancillary_info/df_KG_climatezones.RData') # df_lgd


v_cz_in <- c('Dfc', 'Dfb', 'Cfb', 'Csa')
# v_cz_in <- c('Dfc', 'Dfb', 'Csa', 'Csb', 'Cfa', 'Cfb') # chose which to consider (or remove if too detailed)
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
load('data/figures_for_paper/df_LSTLAI_tempAnomalyQuantile_aug.RData')



#### Make the plots ####

gry1 <- 'grey60'  # <-- the axes
gry2 <- 'grey30'  # <-- 
gry3 <- 'grey45'  # <-- 



# ggplot2 parameters
rel_txt_size <- 3
axis_text_size = 30; 
legend_size <- 1.75


cz_cols <- df_lgd$cz_colours
names(cz_cols) <- df_lgd$cz_name

g_maps <- ggplot(df_cz %>% filter(cz_name %in% v_cz_in) ) + 
  #geom_sf(data = land_europe, fill = 'Grey50', colour ='Grey50', size=0) + 
  geom_tile(aes(x = x, y = y, fill = cz_name)) + 
  geom_sf(data = ocean_europe, fill = 'Grey20', colour = 'Grey20', size=0) +
  facet_grid(.~cz_name) +
  scale_fill_manual('Koppen-Geiger climate zones', values = cz_cols) + 
  scale_y_continuous('Latitude', expand = c(0, 0)) + 
  scale_x_continuous('Longitude', expand = c(0, 0)) + 
  theme(legend.position = 'none',
        #text = element_text(size=rel(rel_txt_size*1.5)),
        # strip.text.x = element_blank() , #element_text(size=rel(rel_txt_size*2.5)),
        # strip.text.y = element_blank()   #element_text(size=rel(rel_txt_size*2.5))
  ) + 
  guides(fill = guide_legend(title.position = 'top', nrow = 4))


df_qtls <- df_all %>% 
  filter(CZ %in% v_cz_in) %>% 
  mutate(Bias_rescaled = ifelse(y_var == 'LAI', Bias, Bias/10)) # Rescaling LST to deciKelvin

g_plot_quantile <- ggplot(df_qtls) + 
  geom_hline(yintercept = 0, colour = gry1) +
  geom_vline(xintercept = 0, colour = gry1) +
  geom_line(aes(x = LST_anomaly, y = Bias_rescaled, colour = y_var), 
            size = 1, show.legend = F) +
  geom_point(aes(x = LST_anomaly, y = Bias_rescaled, colour = y_var,
                 fill = LST_anomaly_quantile), 
             size = 3, shape = 21, show.legend = F) +
  facet_grid(y_var~CZ,  scales = 'free_y') +
  scale_y_continuous( 'Bias in LAI [m2/m2] or in LST [dK]' ) + # , limits = y_scale
  scale_x_continuous( 'Temperature anomaly [C]' ) + # , limits = x_scale
  scale_colour_manual('', values = c('LAI' = 'grey10', 'LST' = 'grey30')) + # , limits = x_scale
  scale_fill_gradientn('Quantile', colours = rev(RColorBrewer::brewer.pal(10, 'RdBu'))) + # , limits = y_scale
  #ggtitle(label = paste0(legend_title)) + 
  theme_grey(base_size = 12) + 
  theme(legend.position = 'bottom')



# g_plot_quantile <- ggplot(df_qtls) + 
#   geom_hline(yintercept = 0, colour = gry1) +
#   geom_vline(xintercept = 0, colour = gry1) +
#   geom_line(aes(x = LST_anomaly, y = Bias_rescaled, colour = y_var), size = 1) +
#   facet_grid(.~CZ,  scales = 'free_y') +
#   scale_y_continuous( 'Bias' ) + # , limits = y_scale
#   scale_x_continuous( 'Temperature anomaly [C]' ) + # , limits = x_scale
#   #ggtitle(label = paste0(legend_title)) + 
#   theme_grey(base_size = 12) + 
#   theme(legend.position = 'bottom')
# 




#### Export the figure #### 

# assemble the panels...
g_all <- g_maps + g_plot_quantile + plot_layout(ncol = 2, widths = c(1, 2)) # , heights = c(1, 4), widths = c(1,1)

g_all <- g_maps + g_plot_quantile + plot_layout(nrow = 2, heights = c(1, 2)) # , heights = c(1, 4), widths = c(1,1)

# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'bias_vs_anomaly'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g_all,
       path = fig.path, width = 8, height = 9)
 
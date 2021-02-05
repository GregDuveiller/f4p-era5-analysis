# design of a delta-delta plot for multiple variables
# ------------
# G. Duveiller

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pals)
library(patchwork)
library(sf)

out.path <- 'results/plots4comparison/'
dir.create(out.path, recursive = T, showWarnings = F)

# load vectors... (for coasts on figures)
wmap_df_land <- st_read('data/world_vectors/ne_50m_land.shp', quiet = T)
wmap_df_ocean <- st_read('data/world_vectors/ne_50m_ocean.shp', quiet = T)


load('results/vardf4comparison/df_LAI_comb.RData', verbose = T)
load('results/vardf4comparison/df_LST_comb.RData', verbose = T)
load('results/vardf4comparison/df_ET_comb.RData', verbose = T)
load('results/vardf4comparison/df_SM_comb.RData', verbose = T)

by_vctr <- c("year", "monthS", "cz_name", "time")

df_all_comb <- tibble(left_join(by = by_vctr, x = df_LAI_comb, y = df_LST_comb))

# set-up colour palette
require(RColorBrewer)
cols_lst <- brewer.rdbu(9)
cols_mon <- c(viridis(n = 6), magma(n = 6)[6:1])

require(scales)


plot_per_clim <- function(sel_cz,  df_lgd, fmt = 'png'){
  
  cz_cols <- df_lgd$cz_colours
  names(cz_cols) <- df_lgd$cz_name
  
  data <- df_all_comb %>% filter(cz_name %in% sel_cz)
  
  g_plot1 <- ggplot(data) +
    geom_point(aes(x = LAI_dif_mu, y = LST_dif_mu, fill = monthS),
               colour = 'grey35', shape = 21) +
    geom_hline(yintercept = 0, colour = 'grey20') +
    geom_vline(xintercept = 0, colour = 'grey20') +
    scale_fill_gradientn('Month', colours = cols_mon) +
    scale_y_continuous('Bias in LST (ERA - obs)') +
    scale_x_continuous('Bias in LAI (ERA - obs)') +
    theme(legend.position = 'top',
          legend.key.width = unit(1.5, units = 'cm')) + 
    guides(fill = guide_colourbar(title.position = 'top', title.hjust = 0.5))
  
  
  g_plot2 <- ggplot(data) +
    geom_point(aes(x = LAI_dif_mu, y = LST_obs_mu - 273.15, fill = LST_dif_mu),
               colour = 'grey35', shape = 21) +
    geom_vline(xintercept = 0, colour = 'grey20') +
    scale_fill_gradientn('Bias in LST (ERA - obs)', colours = cols_lst, 
                         limits = c(-1,1) * max(abs(range(data$LST_dif_mu, na.rm = T)))) +
    scale_y_continuous('Mean LST (obs) [K]') +
    scale_x_continuous('Bias in LAI (ERA - obs)') +
    theme(legend.position = 'top',
          legend.key.width = unit(1.5, units = 'cm')) + 
    guides(fill = guide_colourbar(title.position = 'top', title.hjust = 0.5))
  
  g_maps <- ggplot(df_cz %>% 
                     filter(cz_name %in% sel_cz) %>% 
                     mutate(cz_grp = paste('Distribution of', sel_cz))) + 
    geom_sf(data = wmap_df_land, fill='Grey50',colour='Grey50',size=0) + 
    geom_sf(data = wmap_df_ocean, fill='Grey20',colour='Grey20',size=0) +
    geom_tile(aes(x = x, y = y, fill = cz_name)) + 
    scale_fill_manual('Koppen-Geiger climate zones', values = cz_cols) + 
    scale_y_continuous('Latitude', expand = c(0, 0)) + 
    scale_x_continuous('Longitude', expand = c(0, 0)) + 
    ggtitle(label = paste0('Heat bias due to LAI bias over climate ', sel_cz)) + 
    theme(legend.position = 'none') + 
    guides(fill = guide_legend(title.position = 'top', nrow = 4))
  
  g_combo <- g_maps / (g_plot1 + g_plot2) + plot_layout(nrow = 2, heights = c(2/5, 3/5 ))
  
  ggsave(filename = paste0('LAIbias_perCZ_', sel_cz, '.', fmt), 
         path = out.path, plot = g_combo, width = 8, 
         height = 8)
}

plot_per_clim('Af',  df_lgd, 'png')
plot_per_clim('Aw',  df_lgd, 'png')
plot_per_clim('Cfa', df_lgd, 'png')
plot_per_clim('Cfb', df_lgd, 'png')
plot_per_clim('Csa', df_lgd, 'png')
plot_per_clim('Cwa', df_lgd, 'png')

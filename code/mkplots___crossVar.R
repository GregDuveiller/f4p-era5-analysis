#!/usr/local/bin/Rscript
################################################################################
# Purpose: make diagnostic plot of how LAI biases relate to other variables
# Project: f4p-era5-analysis
# Authors: G.Duveiller
################################################################################



library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pals)
library(patchwork)
library(sf)

fig.format <- 'png'
out.path <- 'results/multi_delta_bias'
dir.create(out.path, showWarnings = F, recursive = T)


# load vectors... (for coasts on figures)
wmap_df_land <- st_read('data/input_data/world_vectors/ne_50m_land.shp', quiet = T)
wmap_df_ocean <- st_read('data/input_data/world_vectors/ne_50m_ocean.shp', quiet = T)


load('data/inter_data/df_single_var_agreement/df_single_var_agr_LAI.RData')
df_LAI_comb <- temp_agr_det 
load('data/inter_data/df_single_var_agreement/df_single_var_agr_LST.RData')
df_LST_comb <- temp_agr_det 
load('data/inter_data/df_single_var_agreement/df_single_var_agr_SM.RData')
df_SM_comb <- temp_agr_det 
load('data/inter_data/df_single_var_agreement/df_single_var_agr_albedo_wsa_vis.RData')
df_albedo_comb <- temp_agr_det 




by_vctr <- c("time", "cz_name")

df_all_comb <- bind_rows(
  left_join(by = by_vctr, suffix = c("_LAI", "_LST"),
            x = df_LAI_comb,
            y = df_LST_comb) %>%
    select(by_vctr, dif_mu_LAI, dif_sd_LAI, dif_mu_LST, dif_sd_LST) %>%   # 
              rename(y_mu = dif_mu_LST, y_sd = dif_sd_LST) %>%
              mutate(y_mu_std = y_mu/sd(y_mu, na.rm = T),
                     y_sd_std = y_sd/sd(y_mu, na.rm = T),
                     y_var = 'LST'),
  left_join(by = by_vctr, suffix = c("_LAI", "_SM"),
            x = df_LAI_comb,
            y = df_SM_comb) %>%
    select(by_vctr, dif_mu_LAI, dif_sd_LAI, dif_mu_SM, dif_sd_SM) %>%   # 
    rename(y_mu = dif_mu_SM, y_sd = dif_sd_SM) %>%
    mutate(y_mu_std = y_mu/sd(y_mu, na.rm = T),
           y_sd_std = y_sd/sd(y_mu, na.rm = T),
           y_var = 'SM'),
  left_join(by = by_vctr, suffix = c("_LAI", "_albedo"),
            x = df_LAI_comb,
            y = df_albedo_comb) %>%
    select(by_vctr, dif_mu_LAI, dif_sd_LAI, dif_mu_albedo, dif_sd_albedo) %>%   # 
    rename(y_mu = dif_mu_albedo, y_sd = dif_sd_albedo) %>%
    mutate(y_mu_std = y_mu/sd(y_mu, na.rm = T),
           y_sd_std = y_sd/sd(y_mu, na.rm = T),
           y_var = 'albedo')) %>%
  mutate(monthS = as.numeric(format(time, '%m')))


# set-up colour palette
cols <- c(viridis(n = 6), magma(n = 6)[6:1])

# cols7 <- viridis(n = 7)
# cols <- c(cols7, cols7[6:2])

# load clim zone locations... 
load('results/ancillary_info/df_KG_climatezones.RData', verbose = T) # df_lgd


plot_per_clim <- function(cz_grp, cz_lbl, df_lgd, fmt = 'png'){
  
  sel_cz <- df_lgd$cz_name[grep(pattern = cz_grp, x = df_lgd$cz_name)]
  cz_cols <- df_lgd$cz_colours
  names(cz_cols) <- df_lgd$cz_name
  
  g_plots <- ggplot(df_all_comb %>% filter(cz_name %in% sel_cz)) +
    geom_point(aes(x = dif_mu_LAI, y = y_mu_std, fill = monthS),
               colour = 'grey35', shape = 21) +
    geom_hline(yintercept = 0, colour = 'grey20') +
    geom_vline(xintercept = 0, colour = 'grey20') +
    facet_grid(cz_name~y_var) +
    scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
    scale_y_continuous('Standardized bias in Y_var (ERA - obs)') +
    scale_x_continuous('Bias in LAI (ERA - obs)') +
    ggtitle(label = paste0('Patterns for selected variables in ', cz_lbl)) + 
    theme(legend.position = 'left',
          legend.key.height = unit(3, units = 'cm'))
  
  g_maps <- ggplot(df_cz %>% 
                     filter(cz_name %in% sel_cz) %>% 
                     mutate(cz_grp = paste('Distribution of', cz_lbl))) + 
    geom_sf(data = wmap_df_land, fill='Grey50',colour='Grey50',size=0) + 
    geom_sf(data = wmap_df_ocean, fill='Grey20',colour='Grey20',size=0) +
    geom_tile(aes(x = x, y = y, fill = cz_name)) + 
    facet_grid(cz_name~cz_grp) +
    scale_fill_manual('Koppen-Geiger climate zones', values = cz_cols) + 
    scale_y_continuous('Latitude', expand = c(0, 0)) + 
    scale_x_continuous('Longitude', expand = c(0, 0)) + 
    theme(legend.position = 'none') + 
    guides(fill = guide_legend(title.position = 'top', nrow = 4))
  
  g_combo <- g_plots + g_maps + plot_layout(ncol = 2, widths = c(3, 2))
  
  ggsave(filename = paste0('czd2plot__', cz_grp, '.', fmt), 
         path = out.path, plot = g_combo, width = 12, 
         height = length(sel_cz) * 2 + 1)
}

plot_per_clim('A', 'tropical climates', df_lgd, 'png')
plot_per_clim('B', 'dry climates', df_lgd, 'png')
plot_per_clim('Cs', 'temperate mediterranean climates', df_lgd, 'png')
plot_per_clim('Cw', 'dry-winter temperate climates', df_lgd, 'png')
plot_per_clim('Cf', 'no-dry-season temperate climates', df_lgd, 'png')
plot_per_clim('Ds', 'dry-summer continental climates', df_lgd, 'png')
plot_per_clim('Dw', 'dry-winter continental climates', df_lgd, 'png')
plot_per_clim('Df', 'no-dry-season continental climates', df_lgd, 'png')


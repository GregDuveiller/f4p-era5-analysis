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

df_all_comb <- bind_rows(
  left_join(by = by_vctr, 
            x = df_LAI_comb,
            y = df_LST_comb %>%
              select(by_vctr, LST_dif_mu, LST_dif_sd) %>%
              rename(y_mu = LST_dif_mu, y_sd = LST_dif_sd) %>%
              mutate(y_mu_std = y_mu/sd(y_mu, na.rm = T),
                     y_sd_std = y_sd/sd(y_mu, na.rm = T),
                     y_var = 'LST')),
  left_join(by = by_vctr, 
            x = df_LAI_comb,
            y = df_ET_comb %>%
              select(by_vctr, ET_dif_mu, ET_dif_sd) %>%
              rename(y_mu = ET_dif_mu, y_sd = ET_dif_sd) %>%
              mutate(y_mu_std = y_mu/sd(y_mu, na.rm = T),
                     y_sd_std = y_sd/sd(y_mu, na.rm = T),
                     y_var = 'ET')),
  left_join(by = by_vctr, 
            x = df_LAI_comb,
            y = df_SM_comb %>%
              select(by_vctr, SM_dif_mu, SM_dif_sd) %>%
              rename(y_mu = SM_dif_mu, y_sd = SM_dif_sd) %>%
              mutate(y_mu_std = y_mu/sd(y_mu, na.rm = T),
                     y_sd_std = y_sd/sd(y_mu, na.rm = T),
                     y_var = 'SM'))) %>%
  select(-LAI_obs_mu, -LAI_obs_sd, -LAI_rea_mu, -LAI_rea_sd) %>%
  filter(!is.na(y_var))


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
    geom_point(aes(x = LAI_dif_mu, y = y_mu_std, fill = monthS),
               colour = 'grey35', shape = 21) +
    geom_hline(yintercept = 0, colour = 'grey20') +
    geom_vline(xintercept = 0, colour = 'grey20') +
    facet_grid(cz_name~y_var) +
    scale_fill_gradientn('Month', colours = cols) +
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



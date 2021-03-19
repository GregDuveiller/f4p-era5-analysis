#!/usr/local/bin/Rscript
################################################################################
# Purpose: some plots on Africa 
# Project: f4p-era5-analysis (but not strictly speakeing)
# Authors: G.Duveiller
################################################################################



require(dplyr)
require(ggplot2)
require(scales)
require(sf)


fpath <- 'results/plots4Africa'
dir.create(fpath)

## define bounding box

bbox <- c(ymin = -35, ymax = 40, xmin = -18, xmax = 53)

wmap_df_land <- st_read('data/input_data/world_vectors/ne_50m_land.shp', quiet = T)
wmap_df_ocean <- st_read('data/input_data/world_vectors/ne_50m_ocean.shp', quiet = T)


## Plot on the Delta_T for Africa -----

dpath <- '../lulcc-bph-chgclim/data/inter_data/df_s4t'
df_s4t <- data.frame(NULL)
iMonth <- 7
for(iTrans in c('CaGtoEFO', 'CaGtoDFO')){
  load(paste0(dpath, '/df_s4t_LST_Day_CMG_',iTrans,'_2003to2019_', month.abb[iMonth],'.RData'))
  df_s4t <- bind_rows(df_s4t,
                      df_s4t_month %>% 
                        mutate(transition = iTrans) %>%
                        filter(lon >= bbox['xmin'], lon <= bbox['xmax'],
                               lat >= bbox['ymin'], lat <= bbox['ymax']))
}
df_s4t_all <- df_s4t %>% 
  group_by(lat, lon) %>%
  summarize(dT.lcc = weighted.mean(dT.lcc, w = 1/dT.lcc.u, na.rm = T))


metric_colpal <- rev(c("#081a2b", "#14406b", "#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B", "#700f1b", "#2d060b"))  # RColorBrewer::brewer.pal(n = 9, name = 'RdBu')
metric_lims_s <- c(-1,1) * max(abs(quantile(df_s4t_month$dT.lcc, probs = c(0.025, 0.975), na.rm = T)))  # <--- need to adjust!
metric_label <- 'D_LST'

g_map_1 <- ggplot(df_s4t_all) + 
  geom_sf(data = wmap_df_land, fill='Grey50',colour='Grey50',size=0) + 
  geom_tile(aes(x = lon, y = lat, fill = dT.lcc)) +  
  geom_sf(data = wmap_df_ocean, fill='Grey20',colour='Grey20',size=0) +
  scale_fill_gradientn(metric_label, colours = metric_colpal, 
                       limits = metric_lims_s, oob = squish) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  coord_sf(expand = F, 
           xlim = bbox[c('xmin', 'xmax')],
           ylim = bbox[c('ymin', 'ymax')]) +
  theme(legend.position = "right",
        legend.key.height = unit(1.8, 'cm'),
        panel.background = element_rect(fill = 'grey30'),
        panel.grid = element_line(colour = 'grey40')) +
  ggtitle(label = 'Change in daytime surface temperature following potential afforestation',
          subtitle = paste('Mean value for the month of', month.name[iMonth], 'based on all values covering 2003 until 2019'))

ggsave(filename = 'Africa_delta_LST_afforestation.png',plot = g_map_1, path = fpath, 
       width = 10, height = 10)




## map of errors between ERA LST and satellite LST ---- 

dpath <- '../f4p-era5-analysis/data/inter_data/df_single_var_agreement'
dfile <- paste0(dpath, '/df_single_var_agr_LST.RData')

load(dfile, verbose = T)

metric_colpal <- rev(c("#081a2b", "#14406b", "#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B", "#700f1b", "#2d060b"))  # RColorBrewer::brewer.pal(n = 9, name = 'RdBu')
metric_lims_s <- c(-1,1) * max(abs(quantile(sp_agr$agre$bias, probs = c(0.025, 0.975), na.rm = T)))  # <--- need to adjust!
metric_label <- 'Bias'
  
sp_agr_sub <- sp_agr %>% 
  filter(x >= bbox['xmin'], x <= bbox['xmax'],
         y >= bbox['ymin'], y <= bbox['ymax'])

g_map_2 <- ggplot(sp_agr_sub) + 
  geom_tile(aes(x = x, y = y, fill = agre$bias)) +  
  geom_sf(data = wmap_df_ocean, fill='Grey20',colour='Grey20',size=0) +
  scale_fill_gradientn(metric_label, colours = metric_colpal, 
                       limits = metric_lims_s, oob = squish) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  coord_sf(expand = F, 
                  xlim = bbox[c('xmin', 'xmax')],
                  ylim = bbox[c('ymin', 'ymax')]) +
  theme(legend.position = "right",
        legend.key.height = unit(1.8, 'cm'),
        panel.background = element_rect(fill = 'grey30'),
        panel.grid = element_line(colour = 'grey40')) +
  ggtitle(label = 'Mean bias in skin/surface temperature of hottest days in the month',
          subtitle = 'Based on all monthly values covering 2003 until 2019. Bias consists of reanalysis (ERA5) minus satellite retrievals (MYD11)')

ggsave(filename = 'Africa_ERA5_bias_LST.png',plot = g_map_2, path = fpath, 
       width = 10, height = 10)


# 
# ## Error in LST vs error in LAI ----
# 
# load('data/inter_data/df_single_var_agreement/df_single_var_agr_LAI.RData')
# df_LAI_comb <- temp_agr_det 
# load('data/inter_data/df_single_var_agreement/df_single_var_agr_LST.RData')
# df_LST_comb <- temp_agr_det 
# 
# 
# by_vctr <- c("time", "cz_name")
# 
# df_all_comb <- left_join(by = by_vctr, suffix = c("_LAI", "_LST"),
#             x = df_LAI_comb,
#             y = df_LST_comb) %>%
#     select(by_vctr, obs_mu_LAI, dif_mu_LST, dif_sd_LST) 
# 

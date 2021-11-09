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


fpath <- 'results/plots4SouthAmerica'
dir.create(fpath)

## define bounding box

bbox <- c(ymin = -40, ymax = 10, xmin = -80, xmax = -40)

wmap_df_land <- st_read('data/input_data/world_vectors/ne_50m_land.shp', quiet = T)
wmap_df_ocean <- st_read('data/input_data/world_vectors/ne_50m_ocean.shp', quiet = T)





## map of errors between ERA LST and satellite LST ---- 

dpath <- '../f4p-era5-analysis/data/inter_data/df_single_var_agreement'
dfile <- paste0(dpath, '/df_single_var_agr_LST.RData')

load(dfile, verbose = T)

metric_colpal <- viridisLite::viridis(n = 9, option = "A")
metric_lims_s <- c(0,1) 
metric_label <- 'L'

sp_agr_sub <- sp_agr %>% 
  filter(x >= bbox['xmin'], x <= bbox['xmax'],
         y >= bbox['ymin'], y <= bbox['ymax'])

g_map_2 <- ggplot(sp_agr_sub) + 
  geom_tile(aes(x = x, y = y, fill = agre$L)) +  
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
  ggtitle(label = 'Agreementin skin/surface temperature of hottest days in the month',
          subtitle = 'Based on all monthly values covering 2003 until 2019. Comparison between reanalysis (ERA5) and satellite retrievals (MYD11)')

ggsave(filename = 'SouthAmerica_ERA5_IoAg_LST.png',plot = g_map_2, path = fpath, 
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

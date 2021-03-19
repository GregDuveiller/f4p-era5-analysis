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

## define bounding box

bbox <- c(ymin = -35, ymax = 40, xmin = -18, xmax = 53)


## map of errors between ERA LST and satellite LST

dpath <- '../f4p-era5-analysis/data/inter_data/df_single_var_agreement'
dfile <- paste0(dpath, '/df_single_var_agr_LST.RData')

load(dfile, verbose = T)

metric_colpal <- rev(c("#081a2b", "#14406b", "#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B", "#700f1b", "#2d060b"))  # RColorBrewer::brewer.pal(n = 9, name = 'RdBu')
metric_lims_s <- c(-1,1) * max(abs(quantile(sp_agr$agre$bias, probs = c(0.025, 0.975), na.rm = T)))  # <--- need to adjust!
metric_label <- 'Bias'
  
sp_agr_sub <- sp_agr %>% filter(x >= bbox['xmin'], x <= bbox['xmax'],
                                y >= bbox['ymin'], y <= bbox['ymax'])

g_map <- ggplot(sp_agr_sub, aes(x = x, y = y)) + 
  geom_tile(aes(fill = agre$bias)) +  
  scale_fill_gradientn(metric_label, colours = metric_colpal, 
                       limits = metric_lims_s, oob = squish) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  coord_cartesian(expand = F, 
                  xlim = bbox[c('xmin', 'xmax')],
                  ylim = bbox[c('ymin', 'ymax')]) +
  theme(legend.position = "right",
        legend.key.height = unit(1.8, 'cm'),
        panel.background = element_rect(fill = 'grey30'),
        panel.grid = element_line(colour = 'grey40')) +
  ggtitle(label = 'Mean bias in skin/surface temperature of hottest days in the month',
          subtitle = 'Period from 2003 until 2019. Bias consists of reanalysis (ERA5) minus satellite retrievals (MYD11)')


## Plot on the Delta_T for Africa






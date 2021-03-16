#!/usr/local/bin/Rscript
################################################################################
# Purpose: make diagnostic plot of agreement of single variables
# Project: f4p-era5-analysis
# Authors: G.Duveiller
################################################################################


library(ggplot2)
library(patchwork)
library(scales)
library(dplyr)


varname <- 'albedo_wsa_vis' # 'LST'
metric <- 'IoAs'

fig.format <- 'png'
fig.path <- 'results/single_var_agreement'
dir.create(fig.path, showWarnings = F, recursive = T)

if(metric == 'IoAg'){metric_lbl <- 'agre$L'; metric_lims <- c(0,1); metric_longname <- 'Index of agreement (Duveiller et al. 2016)'}
if(metric == 'IoAs'){metric_lbl <- 'agre$L.unsys'; metric_lims <- c(0,1); metric_longname <- 'Unsystematic index of agreement (Duveiller et al. 2016)'}



# varname <- 'albedo_wsa_vis'

load( paste0('data/inter_data/df_single_var_agreement/df_single_var_agr_', varname,'.RData'))
# 'agr', 'freq', 'sp_agr', 'temp_agr_gen', 'temp_agr_det'

colnames(agr) <- paste0("agre$", colnames(agr))

heatmap_cols <- c("#FFFFFF", viridis::inferno(9, direction = -1))

g_b2d <- ggplot(freq) +
  geom_tile(aes(x = obs_val, y = sim_val, fill = freq)) + 
  geom_abline(colour = 'grey50') + 
  scale_fill_gradientn(colours = heatmap_cols, 
                       limits = c(0,100000), oob = squish, guide = NULL) + 
  scale_x_continuous('Observations derived from satellite') + 
  scale_y_continuous('Values derived from ERA5') +
  coord_equal(expand = F) +
  ggtitle(paste('Overall metric value across dataset:',
                signif(agr[,which(colnames(agr) == metric_lbl)], digits = 3)))
  
g_map <- ggplot(sp_agr, aes(x = x, y = y)) + 
  geom_raster(aes_string(fill = metric_lbl)) +  
  scale_fill_viridis_c(option = 'A', limits = metric_lims) +
  scale_x_continuous('Longitude') + 
  scale_y_continuous('Latitude') +
  coord_cartesian(expand = F) +
  theme(legend.position = "right",
        legend.key.height = unit(1.8, 'cm'),
        panel.background = element_rect(fill = 'grey30'),
        panel.grid = element_line(colour = 'grey40')) +
  ggtitle('Temporal agreement represented in space')

g_tmp <- ggplot(temp_agr_gen %>% filter(cz_major_zone != 'O'),
                aes(x = time, colour = cz_major_zone)) + 
  geom_line(aes_string(y = metric_lbl)) +
  scale_colour_manual('Climate zone: ', 
                      values = c('A'="#960000", 'B'="#FFCC00", 'C'="#00AA00", 'D'="#6E28B4", 'E'="#6496FF")) +
  scale_y_continuous(metric_longname, limits = metric_lims) +
  scale_x_date('', expand = c(0,0)) +
  theme(legend.position = c(0.5, 0.1), legend.direction = "horizontal") + 
  ggtitle('Spatial agreement within climate zones across time')


g_all <- (g_b2d + g_map) / g_tmp +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(title = paste('Agreement analysis for', varname, 'based on', metric_longname)) 

fig.name <- paste0('genplot_', varname, '_', metric, '.', fig.format)


ggsave(filename = fig.name, plot = g_all, path = fig.path, width = 16, height = 9)



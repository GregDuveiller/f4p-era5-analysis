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


varname <- 'albedo_wsa_vis'  # 'LST'
metric <- 'IoAu'

fig.format <- 'png'
fig.path <- 'results/single_var_agreement'
dir.create(fig.path, showWarnings = F, recursive = T)

if(metric == 'IoAg'){
  metric_code <- 'agre$L' 
  metric_label <- 'L'
  metric_colpal <- viridisLite::viridis(n = 9, option = "A")
  metric_lims <- c(0,1) 
  metric_longname <- 'index of agreement (Duveiller et al. 2016)'}
if(metric == 'IoAu'){
  metric_code <- 'agre$L.unsys'
  metric_label <- 'L_u'
  metric_colpal <- viridisLite::viridis(n = 9, option = "A")
  metric_lims <- c(0,1) 
  metric_longname <- 'unsystematic index of agreement (Duveiller et al. 2016)'}
if(metric == 'Fsys'){
  metric_code <- 'agre$f.sys'
  metric_label <- 'Fsys'
  metric_colpal <- viridisLite::viridis(n = 9, option = "D")
  metric_lims <- c(0,1) 
  metric_longname <- 'fraction of systematic agreement (Duveiller et al. 2016)'}
if(metric == 'Corr'){
  metric_code <- 'agre$r'
  metric_label <- 'rho'
  metric_colpal <- rev(c("#081a2b", "#14406b", "#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B", "#700f1b", "#2d060b"))  # RColorBrewer::brewer.pal(n = 9, name = 'RdBu')
  metric_lims <- c(-1,1) 
  metric_longname <- 'Pearson correlation coefficient'}


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
                signif(agr[,which(colnames(agr) == metric_code)], digits = 3)))
  
g_map <- ggplot(sp_agr, aes(x = x, y = y)) + 
  geom_tile(aes_string(fill = metric_code)) +  
  scale_fill_gradientn(metric_label, colours = metric_colpal, limits = metric_lims) +
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
  geom_line(aes_string(y = metric_code)) +
  scale_colour_manual('Koppen-Geiger general climate zones: ', 
                      values = c('A'="#960000", 'B'="#FFCC00", 'C'="#00AA00", 'D'="#6E28B4", 'E'="#6496FF"),
                      labels = c('A'="Tropical", 'B'="Arid", 'C'="Temperate", 'D'="Continental", 'E'="Polar")) +
  scale_y_continuous(metric_label, limits = metric_lims) +
  scale_x_date('', expand = c(0,0)) +
  theme(legend.position = 'bottom',
        legend.box.spacing = unit(-0.6,"cm"), #legend.box.margin = margin(0, 6, 1, 6), 
        legend.direction = "horizontal") +
  ggtitle('Time series of the spatial agreement within climate zones')


g_all <- (g_b2d + g_map) / g_tmp +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(title = paste('Agreement analysis for', varname, 'based on', metric_longname)) 

fig.name <- paste0('genplot_', varname, '_', metric, '.', fig.format)


ggsave(filename = fig.name, plot = g_all, path = fig.path, width = 16, height = 9)



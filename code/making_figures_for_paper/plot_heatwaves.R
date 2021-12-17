#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_heatwaves.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot figures illustrating heatwaves
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

load('data/final_data/figures_for_paper/hw_maps.RData')   # <---- "df_all" 
load('data/final_data/figures_for_paper/hw_gislayers.RData')   # <---- ... 
load('data/final_data/figures_for_paper/hw_stats.RData')   # <---- "df_barsHW_all" 



#### Make the plots and sf ####

# set-up gray colours for maps
if(exists('gry_land') != T){ gry_land <- 'grey50'}
if(exists('gry_meer') != T){ gry_meer <- 'grey30'}


# some graphic param...
lgd <- theme(legend.position = 'top',
             legend.key.width = unit(1.2, units = 'cm'),
             panel.background = element_rect(fill = gry_land),
             panel.grid = element_blank(),
             axis.title = element_blank(),
             strip.text = element_text(size = 12)) 
gds <- guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5, 
                                    frame.colour = 'black', ticks.colour = 'black'))


# col_palettes ...

col_pal_df <- data.frame(
  LST = rev(brewer.pal(11, "RdBu")),
  LAI = rev(brewer.pal(11, "BrBG")),
  E = rev(brewer.pal(11, "PRGn")),
  Albedo = rev(brewer.pal(11, "PuOr")),
  SM = rev(brewer.pal(11, "PiYG")),
  stringsAsFactors = F)



xlims <- c(-12, 58); ylims <- c(36, 71)

hw_lbls <- c('HW03' = 'HW03 (Aug. 2003)', 
             'HW10' = 'HW10 (Jul. 2010)', 
             'HW18' = 'HW18 (Jul. 2018)')

hw_lbls <- c('2003' = 'HW03 (Aug. 2003)', 
             '2010' = 'HW10 (Jul. 2010)', 
             '2018' = 'HW18 (Jul. 2018)')

hw_labeller <- labeller(hwyear = hw_lbls)



# LST anomaly
gGEN <- ggplot(df_all %>% filter(variable == 'LST')) + 
  geom_raster(aes(x = x, y = y, fill = diff_obsSmean)) +
  geom_sf(data = ocean_europe, fill = gry_meer, size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hwyear~., labeller = hw_labeller) +
  scale_fill_gradientn('LST anomaly [K]', 
                       colours = rev(brewer.pal(11, "RdYlBu")), 
                       limits = c(-15, 15), 
                       oob = squish) +
  gds + lgd


# LST shift
varname <- 'LST'
gLST <- ggplot(df_all %>% filter(variable == varname)) + 
  geom_raster(aes(x = x, y = y, fill = diff_simSobsSmean)) +
  geom_sf(data = ocean_europe, fill = gry_meer, size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hwyear~., labeller = hw_labeller) + 
  scale_fill_gradientn('LST bias shift [K]', 
                       colours = col_pal_df[,varname], 
                       limits = c(-4.5, 4.5), 
                       oob = squish) +
  gds + lgd


# LAI shift
varname <- 'LAI'
gLAI <- ggplot(df_all %>% filter(variable == varname)) + 
  geom_raster(aes(x = x, y = y, fill = diff_simSobsSmean)) +
  geom_sf(data = ocean_europe, fill = gry_meer, size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hwyear~., labeller = hw_labeller) + 
  scale_fill_gradientn('LAI bias shift [m2/m2]', 
                       colours = col_pal_df[,varname], 
                       limits = c(-1.5, 1.5),
                       oob = squish) +
  gds + lgd


# E shift
varname <- 'E'
gEVA <- ggplot(df_all %>% filter(variable == varname)) + 
  geom_raster(aes(x = x, y = y, fill = diff_simSobsSmean)) +
  geom_sf(data = ocean_europe, fill = gry_meer, size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hwyear~., labeller = hw_labeller) + 
  scale_fill_gradientn('Evaporation bias shift [mm]', 
                       colours = col_pal_df[,varname], 
                       limits = c(-50, 50), 
                       oob = squish) +
  gds + lgd

# Albedo shift
varname <- 'Albedo'
gALB <- ggplot(df_all %>% filter(variable == varname)) + 
  geom_raster(aes(x = x, y = y, fill = diff_simSobsSmean)) +
  geom_sf(data = ocean_europe, fill = gry_meer, size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hwyear~., labeller = hw_labeller) + 
  scale_fill_gradientn('Albedo bias shift [.]', 
                       colours = col_pal_df[,varname], 
                       limits = c(-0.05, 0.05), oob = squish) +
  gds + lgd


# SM shift
varname <- 'SM'
gSM <- ggplot(df_all %>% filter(variable == varname)) + 
  geom_raster(aes(x = x, y = y, fill = diff_simSobsSmean)) +
  geom_sf(data = ocean_europe, fill = gry_meer, size = .2, colour = 'grey10') +
  geom_sf(data = hw_polygons, fill = NA, size = 0.8, colour = 'black') +
  coord_sf(expand = F, xlim = xlims, ylim = ylims) +
  facet_grid(hwyear~., labeller = hw_labeller) + 
  scale_fill_gradientn('SM bias shift [.]', 
                       colours = col_pal_df[,varname], 
                       limits = c(-0.15, 0.15), oob = squish) +
  gds + lgd

# bar plots below... 

df_bars <- df_barsHW_all %>% 
  filter(metric %in% c('bias_hw_mu', 'bias_cl_mu')) %>%
  left_join(by = c('variable', 'hwname', 'hwyear'), 
            df_barsHW_all %>%
              filter(metric == 'bias_shift_mu') %>%
              mutate(shift_up = value > 0) %>%
              select(-metric, -value)) %>%
  mutate(colorCodedBias = paste(metric, shift_up, sep = '_'))

mk_gbar_plot <- function(varname){
  
  col_pal <- col_pal_df[,varname]
  
  cols <- c('bias_hw_mu_TRUE' = col_pal[10], 'bias_hw_mu_FALSE' = col_pal[2],
            'bias_cl_mu_TRUE' = col_pal[7], 'bias_cl_mu_FALSE' = col_pal[5])
  
  gbars <- ggplot(df_bars %>% filter(variable == varname)) + 
    geom_col(aes(x = hwname, fill = colorCodedBias, y = value), 
             colour = 'grey20', position = 'dodge') + 
    geom_hline(yintercept = 0, colour = 'grey20') +
    scale_x_discrete('') + 
    scale_y_continuous(paste0(varname, ' bias (ERA - obs)')) +
    scale_fill_manual('', values = cols ) +
    theme_classic() + 
    theme(legend.position = 'none',
          axis.title.x = element_blank())
}

gbarLST <- mk_gbar_plot('LST')
gbarLAI <- mk_gbar_plot('LAI')
gbarEVA <- mk_gbar_plot('E')
gbarALB <- mk_gbar_plot('Albedo')
gbarSM <- mk_gbar_plot('SM')




gText <- ggplot() + 
  ### We could play around to pur an arrow ... but this would need a box
  # geom_segment(aes(x = 1.05, y = 1, xend = 1.02, yend = 1),
  #              arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", 
           x = 1, y = 1,
           size = 3, lineheight = 1, 
           vjust = "inward", hjust = "inward",
           label = "These bars on the left represent\nthe mean spatial biases for each\nvariable and heatwave. The lighter\nbar represents the climatological\nbias while the darker one represent\nthe one for the year of the heatwave") + 
  theme_void()




# assemble together

g <- gGEN + gLAI + gLST + gALB + gEVA + gText + gbarLAI + gbarLST + gbarALB + gbarEVA +
  plot_layout(ncol = 5, heights = c(5,1))


#### Export the figure ####


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'heatwaves'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g,
       path = fig.path, width = 16, height = 9)





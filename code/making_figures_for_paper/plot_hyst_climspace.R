#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_hyst_climspace.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot figure of hysteresis for a climate space
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #


#### Initialization ####

require(ggplot2)
require(scales)
require(viridis)
require(tidyr)



#### Load the data ####

load('data/final_data/figures_for_paper/hysteresis_data_ready4fig.RData')   # <---- "df_r_all" "df_p_all" "df_s_all"



# some graphic param...
lgd <- theme(legend.position = 'left',
             legend.key.height = unit(3, units = 'cm'),
             panel.grid = element_blank(),
             strip.text = element_text(size = 6))
# set-up colour palette
cols <- c(viridis(n = 6), magma(n = 6)[6:1])

## prepare the labeller function for the bins...

# t2m bins...
t2.clim.bin.values <- as.numeric(levels(df_s_all$t2.clim.bin))
t2.binwidth <- unique(diff(t2.clim.bin.values))/2
t2.clim.bin.labels <- paste(t2.clim.bin.values - t2.binwidth, '< T2m <', 
                            t2.clim.bin.values + t2.binwidth)
names(t2.clim.bin.labels) <- levels(df_s_all$t2.clim.bin)

# # reverse t2m bins order
# df_s_all$t2.clim.bin <- factor(df_s_all$t2.clim.bin, levels = rev(levels(df_s_all$t2.clim.bin)))



# sm bins...
sm.clim.bin.values <- as.numeric(levels(df_s_all$sm.clim.bin))
sm.binwidth <- unique(round(diff(sm.clim.bin.values), digits = 6))/2
sm.clim.bin.labels <- paste(sm.clim.bin.values - sm.binwidth, '< SM <', 
                            sm.clim.bin.values + sm.binwidth)
names(sm.clim.bin.labels) <- levels(df_s_all$sm.clim.bin)

# reverse sm bins order
df_s_all$sm.clim.bin <- factor(df_s_all$sm.clim.bin, levels = rev(levels(df_s_all$sm.clim.bin)))


# labeller...
climbin_labeller <- labeller(
  t2.clim.bin = t2.clim.bin.labels, 
  sm.clim.bin = sm.clim.bin.labels)


#### Make the plot ####

g_all_smooth <- ggplot(df_s_all) +
  geom_hline(yintercept = 0, colour = 'grey50') +
  geom_vline(xintercept = 0, colour = 'grey50') +
  geom_point(aes(x = x, y = y, colour = t),
             shape = 20, size = 0.5) +
  facet_grid(sm.clim.bin~t2.clim.bin, labeller = climbin_labeller) +
  scale_colour_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  scale_y_continuous('Bias in LST (ERA - obs)') +
  scale_x_continuous('Bias in LAI (ERA - obs)', breaks = c(-1, 0, 1)) +
  coord_cartesian(xlim = c(-1, 1.5), ylim = c(-20, 15)) + 
  ggtitle(label = paste0('Hysteresis patterns of biases between observational sources and variables in ERA5'),
          subtitle = 'Analysized in a climate space defined by bins of mean annual 2m temperature (horizontally) \nand mean soil moisture (vertically)') + 
  theme(legend.position = 'right',
        legend.key.height = unit(3, units = 'cm'),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        panel.border = element_rect(colour = 'white', fill = NA),
        strip.text = element_text(size = 8, angle = 90),
        strip.text.y = element_text(size = 8, angle = 0))





#### Export the figure ####


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'figs/'}
# if(exists('fig.fmt') != T){ fig.fmt <- 'png'}
fig.fmt <- "pdf"

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'hystereris_climspace'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g_all_smooth,
       path = fig.path, width = 9, height = 11)
 
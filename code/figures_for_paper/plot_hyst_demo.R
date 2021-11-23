#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_hyst_demo.R ####
# Purpose: plot figure of hysteresis demo
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #

#### Initialization ####

require(ggplot2)
require(scales)
require(viridis)
#require(RColorBrewer)


#### Load the data ####

load('data/figures_for_paper/hysteresis_data_ready4fig.RData')

# select which bin to plot

t2.bin.val <- "26"
sm.bin.val <- "0.24"

df_r <- df_r_all %>%
  filter(t2.clim.bin == t2.bin.val) %>%  
  filter(sm.clim.bin == sm.bin.val)    

df_s <- df_s_all %>%
  filter(t2.clim.bin == t2.bin.val) %>%  
  filter(sm.clim.bin == sm.bin.val)   


# calculate monthly averages
df_m <- df_r %>%
  group_by(monthS) %>%
  summarize(y_mu = mean(y),
            x_mu = mean(x),
            y_sd = sd(y),
            x_sd = sd(x))



#### Make the plot ####

ebarheight <- 0.05
ebarwidth <- 0.02

# some graphic param...
lgd <- theme(legend.position = 'left',
             legend.key.height = unit(3, units = 'cm'),
             panel.grid = element_blank(),
             strip.text = element_text(size = 6))
# set-up colour palette
cols <- c(viridis(n = 6), magma(n = 6)[6:1])


gry1 <- 'grey60'  # <-- the axes
gry2 <- 'grey30'  # <-- the edges of the points
gry3 <- 'grey45'  # <-- the smoothed path

fig.format <- 'png'

xrange <- range(df_all$x)
yrange <- range(df_all$y)


# overall plot of the situation
gxy <- ggplot(df_r) + 
  geom_hline(yintercept = 0, colour = gry1) +
  geom_vline(xintercept = 0, colour = gry1) +
  geom_point(aes(x = x, y = y, fill = monthS),
             colour = gry2, stroke = 0.4, shape = 21, size = 2) +
  geom_path(data = df_s, aes(x = x, y = y), colour = gry3, size = 2) +
  geom_errorbar(data = df_m, aes(x = x_mu, ymin = y_mu - y_sd, ymax = y_mu + y_sd),
                colour = gry2,  width = ebarwidth) +
  geom_errorbarh(data = df_m, aes(y = y_mu, xmin = x_mu - x_sd, xmax = x_mu + x_sd),
                 colour = gry2,  height = ebarheight) +
  geom_point(data = df_m, aes(x = x_mu, y = y_mu, fill = monthS),
             colour = gry2, stroke = 0.3, shape = 21, size = 4) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
  scale_x_continuous('Bias in LAI (ERA - obs)') +
  #coord_cartesian(xlim = xrange, ylim = yrange) +
  ggtitle(label = paste0('Hysteresis pattern between bias in LAI and bias in ', varname), 
          subtitle = paste0('Centre of T2 bin: ', t2.bin.val, '   ',
                            'Centre of SM bin: ', sm.bin.val)) + 
  lgd





#### Export the figure ####


# plotting details, in case not inherited... 
if(exists(fig.path) != T){ fig.path <- 'paper/figures/'}
if(exists(fig.fmt) != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'hystereris_demo'


 
# require(patchwork)
# 
#g <- gxy + gx / gy
ggsave(filename = paste0(fig.name, '.', fig.format),
       path = fig.path, width = 8, height = 7)









  
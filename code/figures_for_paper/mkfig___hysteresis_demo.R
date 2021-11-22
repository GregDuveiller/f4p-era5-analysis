#!/usr/local/bin/Rscript
################################################################################
# Purpose: plot figure of hysteresis demo
# Project: f4p-era5-analysis
# Authors: G.Duveiller
################################################################################

require(ggplot2)


#### __initialization__ ####

# plotting details, in case not inherited... 
if(exists(fig.path) != T){ fig.path <- 'paper/figures/'}
if(exists(fig.fmt) != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'hystereris_demo'
  

#### load the data ####









  
)
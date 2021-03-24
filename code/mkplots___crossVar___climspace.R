#!/usr/local/bin/Rscript
################################################################################
# Purpose: make general climspace plot of how LAI biases relate to other vars
# Project: f4p-era5-analysis
# Authors: G.Duveiller
################################################################################



library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)


fig.format <- 'png'
out.path <- 'results/multi_delta_bias'
dir.create(out.path, showWarnings = F, recursive = T)






load('data/inter_data/df_single_var_agreement/df_single_var_agr_tmp_LAI_cleaner.RData')
df_LAI_comb <- temp_agr_con 
load('data/inter_data/df_single_var_agreement/df_single_var_agr_tmp_LST_cleaner.RData')
df_LST_comb <- temp_agr_con 
load('data/inter_data/df_single_var_agreement/df_single_var_agr_tmp_SM_cleaner.RData')
df_SM_comb <- temp_agr_con 
load('data/inter_data/df_single_var_agreement/df_single_var_agr_tmp_albedo_wsa_vis_cleaner.RData')
df_albedo_comb <- temp_agr_con 



by_vctr <- c("time", "t2.clim.bin", "sm.clim.bin")

df_all_comb <- bind_rows(
  left_join(by = by_vctr, suffix = c("_LAI", "_LST"),
            x = df_LAI_comb,
            y = df_LST_comb) %>%
    select(by_vctr, dif_mu_LAI, dif_sd_LAI, dif_mu_LST, dif_sd_LST) %>%   # 
    rename(y_mu = dif_mu_LST, y_sd = dif_sd_LST) %>%
    mutate(y_mu_std = y_mu/sd(y_mu, na.rm = T),
           y_sd_std = y_sd/sd(y_mu, na.rm = T),
           y_var = 'LST'),
  left_join(by = by_vctr, suffix = c("_LAI", "_SM"),
            x = df_LAI_comb,
            y = df_SM_comb) %>%
    select(by_vctr, dif_mu_LAI, dif_sd_LAI, dif_mu_SM, dif_sd_SM) %>%   # 
    rename(y_mu = dif_mu_SM, y_sd = dif_sd_SM) %>%
    mutate(y_mu_std = y_mu/sd(y_mu, na.rm = T),
           y_sd_std = y_sd/sd(y_mu, na.rm = T),
           y_var = 'SM'),
  left_join(by = by_vctr, suffix = c("_LAI", "_albedo"),
            x = df_LAI_comb,
            y = df_albedo_comb) %>%
    select(by_vctr, dif_mu_LAI, dif_sd_LAI, dif_mu_albedo, dif_sd_albedo) %>%   # 
    rename(y_mu = dif_mu_albedo, y_sd = dif_sd_albedo) %>%
    mutate(y_mu_std = y_mu/sd(y_mu, na.rm = T),
           y_sd_std = y_sd/sd(y_mu, na.rm = T),
           y_var = 'albedo')) %>%
  mutate(monthS = as.numeric(format(time, '%m')))


# set-up colour palette
cols <- c(viridis(n = 6), magma(n = 6)[6:1])



## prepare the labeller function for the bins...

# t2m bins...
t2.clim.bin.values <- as.numeric(levels(df_all_comb$t2.clim.bin))
t2.binwidth <- unique(diff(t2.clim.bin.values))/2
t2.clim.bin.labels <- paste(t2.clim.bin.values - t2.binwidth, '< T2m <', 
                            t2.clim.bin.values + t2.binwidth)
names(t2.clim.bin.labels) <- levels(df_all_comb$t2.clim.bin)

# sm bins...
sm.clim.bin.values <- as.numeric(levels(df_all_comb$sm.clim.bin))
sm.binwidth <- unique(round(diff(sm.clim.bin.values), digits = 6))/2
sm.clim.bin.labels <- paste(sm.clim.bin.values - sm.binwidth, '< SM <', 
                            sm.clim.bin.values + sm.binwidth)
names(sm.clim.bin.labels) <- levels(df_all_comb$sm.clim.bin)

# labeller...
climbin_labeller <- labeller(
  t2.clim.bin = t2.clim.bin.labels, sm.clim.bin = sm.clim.bin.labels)





varname <- 'SM'
varname_list <- c('SM', 'LST', 'albedo')

var_range <- list('SM' = NULL,
                  'albedo' = c(-0.05,0.05),
                  'LST' = NULL)

mk_plot_allBins <- function(varname){
  g_plots <- ggplot(df_all_comb %>% 
                      filter(y_var == varname) %>%
                      filter(!is.na(t2.clim.bin) & t2.clim.bin != 34) %>%
                      filter(!is.na(sm.clim.bin) & sm.clim.bin != 0)) +
    geom_hline(yintercept = 0, colour = 'grey50') +
    geom_vline(xintercept = 0, colour = 'grey50') +
    geom_point(aes(x = dif_mu_LAI, y = y_mu, fill = monthS),
               colour = 'grey35', shape = 21) +
    facet_grid(t2.clim.bin~sm.clim.bin, labeller = climbin_labeller) +
    scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
    scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
    scale_x_continuous('Bias in LAI (ERA - obs)') +
    coord_cartesian(ylim = var_range[[varname]]) +
    ggtitle(label = paste0('Patterns for selected variable: ', varname),
            subtitle = 'Analysized per bins in a climate space of mean annual 2m Temperature vs. Soil Moisture') + 
    theme(legend.position = 'left',
          legend.key.height = unit(3, units = 'cm'),
          panel.grid = element_blank(),
          strip.text = element_text(size = 6))
  
  ggsave(filename = paste0('climspace_plot__', varname, '.', fig.format), 
         path = out.path, plot = g_plots, width = 16, height = 9)
  
}

mk_plot_allBins('SM')
mk_plot_allBins('LST')
mk_plot_allBins('albedo')





# zoom plots for selected areas 




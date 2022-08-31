#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_hyst_demo.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot figure of hysteresis demo
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #


#### Initialization ####

require(ggplot2)
require(dplyr)
require(scales)
require(viridis)
require(patchwork)


#### Load the data ####

load('data/final_data/figures_for_paper/hysteresis_data_ready4fig.RData')
load('data/final_data/figures_for_paper/df_climspace_bin.RData')
load('data/final_data/figures_for_paper/hw_gislayers.RData')

# define different cases to showcase
case_list <- list(
  A = list(t2.bin.val = "26", sm.bin.val = "0.24",
           case_longname = "24 < T2M <= 28 | 0.22 < SM <= 0.26 ",
           map_pos = c(0.025, 0.025, 0.6, 0.4)),
  B = list(t2.bin.val =  "2", sm.bin.val = "0.32",
           case_longname = "0 < T2M <= 4 | 0.30 < SM <=0.34",
           map_pos = c(0.025, 0.6, 0.6, 0.975))
  # B = list(t2.bin.val =  "6", sm.bin.val = "0.44",
  #          case_longname = "4 < T2M <= 8 | 0.42 < SM <=0.46",
  #          map_pos = c(0.025, 0.6, 0.6, 0.975))
  )


# filter out
df_r <- bind_rows(
  df_r_all %>% 
    filter(t2.clim.bin == case_list$A$t2.bin.val & 
             sm.clim.bin == case_list$A$sm.bin.val) %>%
    mutate(case = 'A'),
  df_r_all %>% 
    filter(t2.clim.bin == case_list$B$t2.bin.val & 
             sm.clim.bin == case_list$B$sm.bin.val) %>%
    mutate(case = 'B'))

df_s <- bind_rows(
  df_s_all %>% 
    filter(t2.clim.bin == case_list$A$t2.bin.val & 
             sm.clim.bin == case_list$A$sm.bin.val) %>%
    mutate(case = 'A'),
  df_s_all %>% 
    filter(t2.clim.bin == case_list$B$t2.bin.val & 
             sm.clim.bin == case_list$B$sm.bin.val) %>%
    mutate(case = 'B'))

# calculate monthly averages
df_m <- df_r %>%
  group_by(monthS, case) %>%
  summarise(y_mu = mean(y),
            x_mu = mean(x),
            y_sd = sd(y),
            x_sd = sd(x)) 


#### Make the plot ####

plot.case <- function(caseID, varname){
  
  if(varname == 'LST'){ ebarheight <- 0.2}
  if(varname == 'Albedo'){ ebarheight <- 0.0005 }
  
  ebarwidth <- 0.02
  
  # some graphic param...
  lgd <- theme(legend.position = 'left',
               legend.key.height = unit(2.5, units = 'cm'),
               panel.grid = element_blank(),
               strip.text = element_text(size = 6))
  
  
  
  # set-up colour palette
  cols <- c(viridis(n = 6), magma(n = 6)[6:1])
  
  gry1 <- 'grey60'  # <-- the axes
  gry2 <- 'grey30'  # <-- the edges of the points
  gry3 <- 'grey45'  # <-- the smoothed path
  
  # overall plot of the situation
  g <- ggplot(df_r %>% filter(case == caseID)) + 
    geom_hline(yintercept = 0, colour = gry1) +
    geom_vline(xintercept = 0, colour = gry1) +
    geom_path(data = df_s %>% filter(case == caseID), 
              aes(x = x, y = y, colour = t), size = 2, show.legend = F) +
    geom_point(aes(x = x, y = y, fill = monthS),
               colour = gry2, stroke = 0.4, shape = 21, size = 2) +
    geom_errorbar(data = df_m %>% filter(case == caseID), 
                  aes(x = x_mu, ymin = y_mu - y_sd, ymax = y_mu + y_sd),
                  colour = gry2,  width = ebarwidth) +
    geom_errorbarh(data = df_m %>% filter(case == caseID),
                   aes(y = y_mu, xmin = x_mu - x_sd, xmax = x_mu + x_sd),
                   colour = gry2,  height = ebarheight) +
    geom_point(data = df_m %>% filter(case == caseID),
               aes(x = x_mu, y = y_mu, fill = monthS),
               colour = gry2, stroke = 0.3, shape = 21, size = 4) +
    scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
    scale_colour_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
    scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
    scale_x_continuous('Bias in LAI (ERA - obs)') +
    #  ggtitle(label = paste0('Hysteresis pattern between bias in LAI and bias in ', varname)) + 
    lgd
  
  g_map <- ggplot(df_climspace_bin %>% 
                    filter(t2.clim.bin == case_list[[caseID]]$t2.bin.val & 
                             sm.clim.bin == case_list[[caseID]]$sm.bin.val)) + 
    geom_sf(data = land, fill = gry1, colour = NA) +
    geom_tile(aes(x = x, y = y), fill = 'black') + 
    coord_sf(expand = F) +
    # ggtitle(label = paste0('For climate zone:\n ', case_list[[caseID]]$case_longname)) + 
    ggtitle(label = 'For climate zone:', subtitle = case_list[[caseID]]$case_longname) + 
    theme(plot.title = element_text(hjust = 0.5, size = 10),
          plot.subtitle = element_text(hjust = 0.5, size = 8), 
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.border = element_rect(colour = gry2, fill = NA)) 
  
  pos <- case_list[[caseID]]$map_pos
  g + inset_element(g_map, pos[1],  pos[2],  pos[3],  pos[4], ignore_tag = T)
  
}

g1 <- plot.case(caseID = 'A', varname = 'LST')
g2 <- plot.case(caseID = 'B', varname = 'LST')


#### Export the figure ####


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'hysteresis_demo'

g <- g1 + g2 + plot_layout(guides = "collect")  + 
  plot_annotation(
    title = 'Examples of hysteresis patterns between biases in LAI and biases in LST for different climates',
    subtitle = '[LAI obs from GEOV2/AVHRR, LST obs from on MYD11A1, model data from ERA5-Land]') & 
  theme(legend.position = 'left') 

ggsave(filename = paste0(fig.name, '.', fig.fmt),
       path = fig.path, width = 12, height = 6)

# g <- g1 / g2 + plot_layout(guides = "collect") & theme(legend.position = 'left')
# ggsave(filename = paste0(fig.name, '.', fig.fmt),
#        path = fig.path, width = 8, height = 14)








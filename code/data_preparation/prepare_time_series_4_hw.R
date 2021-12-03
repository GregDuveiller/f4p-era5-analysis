library(dplyr)
library(tidyr)
library(sf)

load('data/figures_for_paper/hwAll_gislayers.RData')   # <---- hw_polygons



# input files are the 'df_comb___var.RData' produced by Greg's script
input_dir <- 'data/inter_data/df_comb_obs_vs_sim/'

# list of variables to run over
var_list <- c('LAI', 'LST',  'E', 'albedo_wsa_vis') # albedo_bsa_nir albedo_bsa_vis albedo_wsa_nir 'SM',


output_path <- 'data/figures_for_paper/'
print(paste0('output_path is : ', output_path ))
if(!dir.exists(output_path)) {dir.create(paste0(output_path), recursive = T)}


df_hw <- data.frame(row.names = c("hw2003", "hw2010", "hw2018"),
                    year = c(2003, 2010, 2018),
                    month = c(8, 7, 7))



get_hw_spatial_average <- function(hwname, varname){
  
  load(paste0(input_dir, 'df_comb___', varname, '.RData'))  # <--- df_comb
  
  hw_bbox <- hw_polygons %>% filter(hw == hwname) %>% st_bbox()
  
  df_subsetHW <- df_comb %>%
    filter(x >= hw_bbox$xmin & x <= hw_bbox$xmax) %>%
    filter(y >= hw_bbox$ymin & y <= hw_bbox$ymax)
  
  rm(df_comb)
  
  df_subset_ts <- df_subsetHW  %>%
    group_by(year, month) %>%
    summarise(obs = mean(obs, na.rm = T), sim = mean(sim, na.rm = T)) %>%
    mutate(time = as.Date(paste(year, month, '15', sep = '-'), '%Y-%M-%d'),
           varname = varname,
           hwname = hwname)
  
  return(df_subset_ts)
}

df_subset_1 <- get_hw_spatial_average(hwname = 'hw2003', varname = 'LAI')
df_subset_2 <- get_hw_spatial_average(hwname = 'hw2010', varname = 'LAI')
df_subset_3 <- get_hw_spatial_average(hwname = 'hw2018', varname = 'LAI')

df_subset_ts <- bind_rows(df_subset_1, df_subset_2, df_subset_3)

df_subset_ts <- bind_rows(
  get_hw_spatial_average(hwname = 'hw2003', varname = 'LST'),
  get_hw_spatial_average(hwname = 'hw2010', varname = 'LST'),
  get_hw_spatial_average(hwname = 'hw2018', varname = 'LST')
)


df_subset_ts <- bind_rows(
  get_hw_spatial_average(hwname = 'hw2003', varname = 'E'),
  get_hw_spatial_average(hwname = 'hw2010', varname = 'E'),
  get_hw_spatial_average(hwname = 'hw2018', varname = 'E')
)



hw_clim_ts <- df_subset_ts %>% 
  group_by(month, hwname) %>%
  summarise(clim_obs = mean(obs, na.rm = T),
            clim_sim = mean(sim, na.rm = T))



ts_all <- df_subset_ts %>% 
  filter(hwname == paste0('hw', year)) %>%
  left_join(hw_clim_ts, by = c('month', 'hwname')) %>%
  rename(year_of_hw = year) %>%
  left_join(data.frame(year_of_hw = c(2003, 2010, 2018), 
                       month_of_hw = c(8, 7, 7)), by = "year_of_hw")



ggplot(ts_all, aes(x = month)) +
  geom_vline(aes(xintercept = month_of_hw), size = 2) +
  geom_line(aes(y = clim_obs)) +
  geom_line(aes(y = clim_sim), linetype = 'dashed') +
  geom_line(aes(y = obs, colour = factor(year_of_hw)), show.legend = F) +
  geom_line(aes(y = sim, colour = factor(year_of_hw)), show.legend = F, linetype = 'dashed') +
  facet_grid(year_of_hw~.) +
  scale_x_continuous(expand = c(0,0)) 





start_year <- 2003; end_year <- 2018

dd_clim <- df_subsetHW %>% 
  filter(year >= start_year & year <= end_year ) %>%
  group_by(x, y, month) %>% 
  summarise(obs_clim = mean(obs, na.rm = T),
            sim_clim = mean(sim, na.rm = T))

dd_hw <- df_subsetHW %>% filter(year == 2003, month == 8)

ggplot(dd_clim %>% filter(month == 8)) + 
  geom_raster(aes(x = x,  y = y,fill = sim_clim - obs_clim)) + 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(7, 'RdBu'),
                          limits = c(-3, 3)) + 
  facet_wrap( ~ month) + 
  ggtitle('Climatological bias') + 
  theme(legend.position = 'bottom')

ggplot(dd_hw) + 
  geom_raster(aes(x = x, y = y, fill = sim - obs)) + 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(7, 'RdBu'),
                       limits = c(-3, 3)) + 
  facet_wrap( ~ month) + ggtitle('HW2003 bias') + 
  theme(legend.position = 'bottom')

ggplot(dd_hw %>% left_join(dd_clim, by = c('month', 'x', 'y')) %>% filter(month == 8)) + 
  geom_raster(aes(x = x, y = y, fill = (sim - sim_clim) - (obs - obs_clim))) + 
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(7, 'BrBG')),
                       limits = c(-1.5, 1.5)) + 
  facet_wrap(~month) + 
  ggtitle('difference in bias') + 
  theme(legend.position = 'bottom')



dum <- dd_hw %>% 
  left_join(dd_clim, by = c('month', 'x', 'y')) %>% 
  filter(month == 8) %>% 
  mutate(bias = (sim - sim_clim) - (obs - obs_clim))

#}

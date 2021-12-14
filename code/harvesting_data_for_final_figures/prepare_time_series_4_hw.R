library(dplyr)
library(tidyr)
library(sf)

load('data/final_data/figures_for_paper/hwAll_gislayers.RData')   # <---- hw_polygons



# input files are the 'df_comb___var.RData' produced by Greg's script
input_dir <- 'data/inter_data/df_comb_obs_vs_sim/'

# list of variables to run over
var_list <- c('LAI', 'LST',  'E', 'albedo_wsa_vis') # albedo_bsa_nir albedo_bsa_vis albedo_wsa_nir 'SM',


output_path <- 'data/final_data/figures_for_paper/'
print(paste0('output_path is : ', output_path ))
if(!dir.exists(output_path)) {dir.create(paste0(output_path), recursive = T)}


df_hw <- data.frame(row.names = c("hw2003", "hw2010", "hw2018"),
                    year = c(2003, 2010, 2018),
                    month = c(8, 7, 7))


varname <- 'LAI'
load(paste0(input_dir, 'df_comb___', varname, '.RData'))  # <--- df_comb

get_hw_spatial_average <- function(hwname, df_comb){
  
  hw_bbox <- hw_polygons %>% filter(hw == hwname) %>% st_bbox()
  
  df_subsetHW <- df_comb %>%
    filter(x >= hw_bbox$xmin & x <= hw_bbox$xmax) %>%
    filter(y >= hw_bbox$ymin & y <= hw_bbox$ymax)

  df_subset_ts <- df_subsetHW  %>%
    group_by(year, month) %>%
    summarise(obs = mean(obs, na.rm = T), sim = mean(sim, na.rm = T)) %>%
    mutate(time = as.Date(paste(year, month, '15', sep = '-'), '%Y-%M-%d'),
           hwname = hwname)
  
  return(df_subset_ts)
}

df_subset_ts <- bind_rows(
  get_hw_spatial_average(hwname = 'hw2003', df_comb),
  get_hw_spatial_average(hwname = 'hw2010', df_comb),
  get_hw_spatial_average(hwname = 'hw2018', df_comb)
)

rm(df_comb)


start_year <- 2003; end_year <- 2018

hw_clim_ts <- df_subset_ts %>% 
  filter(year >= start_year & year <= end_year ) %>%
  group_by(month, hwname) %>%
  summarise(obs = mean(obs, na.rm = T),
            sim = mean(sim, na.rm = T)) %>%
  pivot_longer(cols = c('obs', 'sim'), 
               names_to = 'source', values_to = paste0(varname,'_clim'))


ts_all <- df_subset_ts %>% 
  filter(hwname == paste0('hw', year)) %>%
  pivot_longer(cols = c('obs', 'sim'), 
               names_to = 'source', values_to = paste0(varname,'_year')) %>% 
  left_join(hw_clim_ts, by = c('month', 'hwname', 'source')) %>%
  rename(year_of_hw = year) %>%
  left_join(data.frame(year_of_hw = c(2003, 2010, 2018), 
                       month_of_hw = c(8, 7, 7)), by = "year_of_hw") %>%
  pivot_longer(cols = c(paste0(varname,'_year'), paste0(varname,'_clim')),
               names_to = 'type', values_to = varname)





lgd <- theme(legend.position = 'bottom',
             #legend.key.width = unit(1.2, units = 'cm'),
             #panel.grid = element_blank(),
             strip.text = element_text(size = 12)) 
# gds <- guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5, 
#                                     frame.colour = 'black', ticks.colour = 'black'))

hw_labeller <- labeller(
  hwname = c('hw2003' = 'Aug. 2003', 'hw2010' = 'Jul. 2010', 'hw2018' = 'Jul. 2018'))


g <- ggplot(ts_all, aes(x = month)) +
  geom_vline(aes(xintercept = month_of_hw), size = 14, colour = 'grey80') +
  geom_line(aes(y = LAI, colour = type, linetype = source)) +
  facet_grid(hwname~., labeller = hw_labeller) +
  #scale_colour_manual(values = c('LAI_clim'='grey20','LAI_year'= 'cornflowerblue')) +
  scale_colour_manual(values = c('LAI_year'= 'cornflowerblue','LAI_clim'='grey20')) +
  scale_y_continuous('LAI [m2/m2]') + 
  scale_x_continuous('', expand = c(0,0), 
                     breaks = c(3,6,9,12),
                     labels = c('3'='Mar', '6'='Jun', '9'='Sep', '12'='Dec')) +
  #theme_bw() + 
  lgd



#### Export the figure ####


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'LAI_timeseries'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g,
       path = fig.path, width = 10, height = 8)




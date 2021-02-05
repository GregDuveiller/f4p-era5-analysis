### READ data from df files

library(here)
library(dplyr)
library(ggplot2)
library(tidyr)


load('data/r_data_frames/df_LAI_theia_025_2010.RData', verbose = T)
df_LAIrs <- df


load('data/r_data_frames/df_LAI_ERA5sl_025_2010.RData', verbose = T)
df_LAIra <- df

df_LAI <- bind_rows(
  df_LAIrs %>% 
    #filter(`LAI-RMSE` < 0.5, `LAI-NOBS` > 5) %>%
    select(x, y, year, month, LAI) %>%
    mutate(Source = 'observations'), 
  df_LAIra %>% 
    mutate(LAI = cvh * lai_hv + cvl * lai_lv) %>%
    select(x, y, year, month, LAI) %>%
    mutate(Source = 'reanalysis'))

ggplot(df_LAI %>% filter(month == 6)) +
  geom_tile(aes(x = x, y = y, fill = LAI)) +
  facet_wrap(~Source, nc = 1) +
  scale_fill_viridis_c(option = 'A')


ggplot(df_LAI %>% 
         pivot_wider(names_from = Source, 
                     names_prefix = 'LAI_',
                     values_from = LAI) %>%
         filter(month == 8)) +
  geom_density_2d_filled(aes(x = LAI_observations, y = LAI_reanalysis),
                         bins = 50, na.rm = T, show.legend = F)




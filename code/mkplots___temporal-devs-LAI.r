library(ggplot2)
library(dplyr)
library(tidyr)



LAI_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'theia_025', full.names = T)


load('results/ancillary_info/df_KG_climatezones.RData') #  'df_cz' and 'df_lgd'

load('data/r_data_frames/df_LAI_ERA5sl_025_2010.RData', verbose = T)
df_LAIra <- df %>% 
  mutate(LAI = cvh * lai_hv + cvl * lai_lv) %>%
  select(x, y, month, LAI) %>%
  rename(LAI_reanalysis = LAI)

df_all <- data.frame()

for(ifile in LAI_files){
  
  load(ifile); df_LAIrs <- df
  
  df_LAI <- df_LAIrs %>% 
    #filter(`LAI-RMSE` < 0.5, `LAI-NOBS` > 5) %>%
    select(x, y, year, month, LAI) %>%
    rename(LAI_observations = LAI) %>%
    left_join(df_LAIra, by = c("x", "y", "month")) %>% 
    mutate(LAI_difference = LAI_reanalysis - LAI_observations) %>%
    left_join(df_cz,  by = c("x", "y")) %>%
    group_by(year, month, cz_name) %>%
    summarise(LAI_obs_mu = mean(LAI_observations, na.rm = T),
              LAI_obs_sd = sd(LAI_observations, na.rm = T),
              LAI_rea_mu = mean(LAI_reanalysis, na.rm = T),
              LAI_rea_sd = sd(LAI_reanalysis, na.rm = T),
              LAI_dif_mu = mean(LAI_difference, na.rm = T),
              LAI_dif_sd = sd(LAI_difference, na.rm = T))
  
  df_all <- bind_rows(df_all, df_LAI) 
}

df_all <- df_all %>% 
  mutate(time = as.Date(x = paste(year, month, '15', sep = '-')))


library(ggplot2)

cz_cols <- df_lgd$cz_colours
names(cz_cols) <- df_lgd$cz_name

sel_cz <- c('Cfc', 'BSh', 'Dfb', 'Af', 'ET')


ggplot(df_all %>% 
         filter(cz_name %in% sel_cz), 
       aes(x = time, colour = cz_name)) + 
  geom_line(aes(y = LAI_obs_mu), linetype = 1) + 
  geom_line(aes(y = LAI_rea_mu), linetype = 2) + 
  scale_colour_manual(values = cz_cols, guide = 'none') + 
  facet_wrap(~cz_name, nc = 1)  +
  scale_x_date('') +
  coord_cartesian(expand = F) +
  ggtitle(label = 'Average LAI for selected climate zones',
          subtitle = '[LAI reanalysis (ERA5) in dashed line, LAI observations (Theia) in full lines]')




ggplot(df_all %>% 
         filter(cz_name %in% sel_cz), 
       aes(x = time, fill = cz_name)) + 
  geom_ribbon(aes(ymin = LAI_dif_mu - LAI_dif_sd, 
                  ymax = LAI_dif_mu + LAI_dif_sd),
              alpha = 0.2) +
  geom_line(aes(y = LAI_dif_mu, colour = cz_name),
            linetype = 1, size = 1) + 
  geom_hline(yintercept = 0, colour = 'grey30') + 
  facet_wrap(~cz_name, nc = 1)   +
  scale_colour_manual(values = cz_cols, guide = 'none') + 
  scale_fill_manual(values = cz_cols, guide = 'none') +
  scale_y_continuous('Difference in LAI') +
  scale_x_date('') +
  coord_cartesian(expand = F) +
  ggtitle(label = 'Bias in LAI for selected climate zones',
          subtitle = '[LAI_reanalysis (ERA5) - LAI_observations (Theia)]')

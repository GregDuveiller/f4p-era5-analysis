library(ggplot2)
library(dplyr)
library(tidyr)



LST_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'LST_GEE', full.names = T) #"df_i_025"


load('results/ancillary_info/df_KG_climatezones.RData') #  'df_cz' and 'df_lgd'


SKT_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'SKT_ERA5l', full.names = T)





df_LST_comb <- data.frame()

for(ifile in LST_files){
  
  load(ifile); df_LSTrs <- df_i_025
  
  load(gsub(ifile, pattern = 'LST_GEE', replacement = 'SKT_ERA5l')); df_SKTra <- df
  
  df_LST <- df_LSTrs %>% 
    #filter(`LST_count` > 5) %>%
    select(x, y, year, month, LST_max5_mean) %>%
    # need to rotate cold months in Southern Hemisphere
    mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
    left_join(df_SKTra %>%
                # need to rotate cold months in Southern Hemisphere
                mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month)), 
              by = c("x", "y", "month", "monthS", "year")) %>% 
    mutate(LST_difference = skt_top5avg - LST_max5_mean) %>%
    left_join(df_cz,  by = c("x", "y")) %>%
    group_by(year, monthS, cz_name) %>%
    summarise(LST_obs_mu = mean(LST_max5_mean, na.rm = T),
              LST_obs_sd = sd(LST_max5_mean, na.rm = T),
              LST_rea_mu = mean(skt_top5avg, na.rm = T),
              LST_rea_sd = sd(skt_top5avg, na.rm = T),
              LST_dif_mu = mean(LST_difference, na.rm = T),
              LST_dif_sd = sd(LST_difference, na.rm = T))
  
  df_LST_comb <- bind_rows(df_LST_comb, df_LST) 
}

df_LST_comb <- df_LST_comb %>% 
  mutate(monthS = ifelse(monthS == 0, 12, monthS)) %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-')))
# NOTE: This time is indicative, and based on the Northern Hemisphere

library(ggplot2)

cz_cols <- df_lgd$cz_colours
names(cz_cols) <- df_lgd$cz_name

sel_cz <- c('Cfc', 'BSh', 'Dfb', 'Af', 'ET')


ggplot(df_LST_comb %>% 
         filter(cz_name %in% sel_cz), 
       aes(x = time, colour = cz_name)) + 
  geom_line(aes(y = LST_obs_mu), linetype = 1) + 
  geom_line(aes(y = LST_rea_mu), linetype = 2) + 
  scale_colour_manual(values = cz_cols, guide = 'none') + 
  facet_wrap(~cz_name, nc = 1)  +
  scale_x_date('') +
  coord_cartesian(expand = F) +
  ggtitle(label = 'Average LST for selected climate zones',
          subtitle = '[LST reanalysis (ERA5) in dashed line, LST observations (Theia) in full lines]')




ggplot(df_LST_comb %>% 
         filter(cz_name %in% sel_cz), 
       aes(x = time, fill = cz_name)) + 
  geom_ribbon(aes(ymin = LST_dif_mu - LST_dif_sd, 
                  ymax = LST_dif_mu + LST_dif_sd),
              alpha = 0.2) +
  geom_line(aes(y = LST_dif_mu, colour = cz_name),
            linetype = 1, size = 1) + 
  geom_hline(yintercept = 0, colour = 'grey30') + 
  facet_wrap(~cz_name, nc = 1)   +
  scale_colour_manual(values = cz_cols, guide = 'none') + 
  scale_fill_manual(values = cz_cols, guide = 'none') +
  scale_y_continuous('Difference in LST') +
  scale_x_date('') +
  coord_cartesian(expand = F) +
  ggtitle(label = 'Bias in LST for selected climate zones',
          subtitle = '[LST_reanalysis (ERA5) - LST_observations (Theia)]')

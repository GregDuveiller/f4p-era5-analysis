library(ggplot2)
library(dplyr)
library(tidyr)



SAT_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'GLEAM', full.names = T) #"df_i_025"


load('results/ancillary_info/df_KG_climatezones.RData') #  'df_cz' and 'df_lgd'


ERA_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'ET_ERA5l', full.names = T)





df_ET_comb <- data.frame()

for(ifile in SAT_files){
  
  load(ifile); df_SAT <- df
  
  load(gsub(ifile, pattern = 'GLEAM', replacement = 'ERA5l')); df_ERA <- df
  
  df_ET <- df_SAT %>% 
    select(x, y, year, month, Et) %>%
    # need to rotate cold months in Southern Hemisphere
    mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
    mutate(monthS = ifelse(monthS == 0, 12, monthS)) %>% 
    left_join(df_ERA %>%
                # need to rotate cold months in Southern Hemisphere
                mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month),
                       monthS = ifelse(monthS == 0, 12, monthS)), 
              by = c("x", "y", "month", "monthS", "year")) %>% 
    mutate(ET_difference = e - Et) %>%
    left_join(df_cz,  by = c("x", "y")) %>%
    group_by(year, monthS, cz_name) %>%
    summarise(ET_obs_mu = mean(Et, na.rm = T),
              ET_obs_sd = sd(Et, na.rm = T),
              ET_rea_mu = mean(e, na.rm = T),
              ET_rea_sd = sd(e, na.rm = T),
              ET_dif_mu = mean(ET_difference, na.rm = T),
              ET_dif_sd = sd(ET_difference, na.rm = T))
  
  df_ET_comb <- bind_rows(df_ET_comb, df_ET) 
}

df_ET_comb <- df_ET_comb %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-')))
# NOTE: This time is indicative, and based on the Northern Hemisphere

library(ggplot2)

cz_cols <- df_lgd$cz_colours
names(cz_cols) <- df_lgd$cz_name

sel_cz <- c('Cfc', 'BSh', 'Dfb', 'Af', 'ET')


ggplot(df_ET_comb %>% 
         filter(cz_name %in% sel_cz), 
       aes(x = time, colour = cz_name)) + 
  geom_line(aes(y = ET_obs_mu), linetype = 1) + 
  geom_line(aes(y = ET_rea_mu), linetype = 2) + 
  scale_colour_manual(values = cz_cols, guide = 'none') + 
  facet_wrap(~cz_name, nc = 1)  +
  scale_x_date('') +
  coord_cartesian(expand = F) +
  ggtitle(label = 'Average ET for selected climate zones',
          subtitle = '[ET reanalysis (ERA5) in dashed line, ET observations (Theia) in full lines]')




ggplot(df_ET_comb %>% 
         filter(cz_name %in% sel_cz), 
       aes(x = time, fill = cz_name)) + 
  geom_ribbon(aes(ymin = ET_dif_mu - ET_dif_sd, 
                  ymax = ET_dif_mu + ET_dif_sd),
              alpha = 0.2) +
  geom_line(aes(y = ET_dif_mu, colour = cz_name),
            linetype = 1, size = 1) + 
  geom_hline(yintercept = 0, colour = 'grey30') + 
  facet_wrap(~cz_name, nc = 1)   +
  scale_colour_manual(values = cz_cols, guide = 'none') + 
  scale_fill_manual(values = cz_cols, guide = 'none') +
  scale_y_continuous('Difference in ET') +
  scale_x_date('') +
  coord_cartesian(expand = F) +
  ggtitle(label = 'Bias in ET for selected climate zones',
          subtitle = '[ET_reanalysis (ERA5) - ET_observations (Theia)]')

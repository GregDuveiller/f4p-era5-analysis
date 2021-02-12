library(ggplot2)
library(dplyr)
library(tidyr)



SAT_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'SM_sat', full.names = T) #"df_i_025"


load('results/ancillary_info/df_KG_climatezones.RData') #  'df_cz' and 'df_lgd'


ERA_files <- list.files(path = 'data/r_data_frames', 
                        pattern = 'SM_ERA5l', full.names = T)





df_SM_comb <- data.frame()

for(ifile in SAT_files){
  
  load(ifile); df_SMrs <- df
  
  load(gsub(ifile, pattern = 'SM_sat', replacement = 'SM_ERA5l')); df_SMra <- df
  
  df_SM <- df_SMrs %>% 
    #filter(`nobs` > 5) %>%
    select(x, y, year, month, sm) %>%
    # need to rotate cold months in Southern Hemisphere
    mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
    mutate(monthS = ifelse(monthS == 0, 12, monthS)) %>% 
    left_join(df_SMra %>%
                # need to rotate cold months in Southern Hemisphere
                mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month),
                       monthS = ifelse(monthS == 0, 12, monthS)), 
              by = c("x", "y", "month", "monthS", "year")) %>% 
    mutate(SM_difference = swvl1 - sm) %>%
    left_join(df_cz,  by = c("x", "y")) %>%
    group_by(year, monthS, cz_name) %>%
    summarise(SM_obs_mu = mean(sm, na.rm = T),
              SM_obs_sd = sd(sm, na.rm = T),
              SM_rea_mu = mean(swvl1, na.rm = T),
              SM_rea_sd = sd(swvl1, na.rm = T),
              SM_dif_mu = mean(SM_difference, na.rm = T),
              SM_dif_sd = sd(SM_difference, na.rm = T))
  
  df_SM_comb <- bind_rows(df_SM_comb, df_SM) 
}

df_SM_comb <- df_SM_comb %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-')))
# NOTE: This time is indicative, and based on the Northern Hemisphere

library(ggplot2)

cz_cols <- df_lgd$cz_colours
names(cz_cols) <- df_lgd$cz_name

sel_cz <- c('Cfc', 'BSh', 'Dfb', 'Af', 'ET')


ggplot(df_SM_comb %>% 
         filter(cz_name %in% sel_cz), 
       aes(x = time, colour = cz_name)) + 
  geom_line(aes(y = SM_obs_mu), linetype = 1) + 
  geom_line(aes(y = SM_rea_mu), linetype = 2) + 
  scale_colour_manual(values = cz_cols, guide = 'none') + 
  facet_wrap(~cz_name, nc = 1)  +
  scale_x_date('') +
  coord_cartesian(expand = F) +
  ggtitle(label = 'Average SM for selected climate zones',
          subtitle = '[SM reanalysis (ERA5) in dashed line, SM observations (Theia) in full lines]')




ggplot(df_SM_comb %>% 
         filter(cz_name %in% sel_cz), 
       aes(x = time, fill = cz_name)) + 
  geom_ribbon(aes(ymin = SM_dif_mu - SM_dif_sd, 
                  ymax = SM_dif_mu + SM_dif_sd),
              alpha = 0.2) +
  geom_line(aes(y = SM_dif_mu, colour = cz_name),
            linetype = 1, size = 1) + 
  geom_hline(yintercept = 0, colour = 'grey30') + 
  facet_wrap(~cz_name, nc = 1)   +
  scale_colour_manual(values = cz_cols, guide = 'none') + 
  scale_fill_manual(values = cz_cols, guide = 'none') +
  scale_y_continuous('Difference in LST') +
  scale_x_date('') +
  coord_cartesian(expand = F) +
  ggtitle(label = 'Bias in SM for selected climate zones',
          subtitle = '[SM_reanalysis (ERA5) - SM_observations (Theia)]')

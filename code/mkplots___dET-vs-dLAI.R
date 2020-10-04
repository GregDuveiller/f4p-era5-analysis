require(ggplot2)
require(dplyr)
require(pals)


source('~/work/workspace/f4p-era5-analysis/code/mkplots___temporal-devs-LAI.r')
source('~/work/workspace/f4p-era5-analysis/code/mkplots___temporal-devs-ET.r')


df_all_comb <- left_join(df_LAI_comb,  df_ET_comb,
                         by = c("year", "monthS", "cz_name", "time"))


cz_cols <- df_lgd$cz_colours
names(cz_cols) <- df_lgd$cz_name

sel_cz <- c('Cfc', 'BSh', 'Dfb', 'Af', 'ET')
sel_cz <- c('Aw', 'Cfa', 'Cwb', 'Dfb', 'Am', 'Cwc')

# general plot for all climate zones
ggplot(df_all_comb) + # %>% filter(cz_name %in% sel_cz)) +
  geom_point(aes(x = LAI_dif_mu, y = ET_dif_mu, fill = cz_name),
             colour = 'grey35', shape = 21) +
  geom_hline(yintercept = 0, colour = 'grey20') +
  geom_vline(xintercept = 0, colour = 'grey20') +
  facet_wrap(~cz_name, nc = 4) +
  scale_fill_manual(values = cz_cols, guide = 'none') +
  scale_y_continuous('Bias is ET (ERA - obs)') +
  scale_x_continuous('Bias is LAI (ERA - obs)')

ggplot(df_all_comb) + # %>% filter(cz_name %in% sel_cz)) +
  geom_point(aes(x = LAI_dif_mu, y = ET_dif_mu, fill = monthS),
             colour = 'grey35', shape = 21) +
  geom_hline(yintercept = 0, colour = 'grey20') +
  geom_vline(xintercept = 0, colour = 'grey20') +
  facet_wrap(~cz_name, nc = 4) +
  scale_fill_gradientn('Season', colours = kovesi.cyclic_mygbm_30_95_c78(12)) +
  scale_y_continuous('Bias is ET (ERA - obs)') +
  scale_x_continuous('Bias is LAI (ERA - obs)')

# plot for single climate zone with seasonality
ggplot(df_all_comb %>% filter(cz_name %in% sel_cz)) +
  geom_point(aes(x = LAI_dif_mu, y = ET_dif_mu, fill = monthS),
             colour = 'grey35', shape = 21) +
  geom_hline(yintercept = 0, colour = 'grey20') +
  geom_vline(xintercept = 0, colour = 'grey20') +
  facet_wrap(~cz_name, nc = 2) +
  scale_fill_gradientn(colours = kovesi.cyclic_mygbm_30_95_c78(12)) +
  scale_y_continuous('Bias is ET (ERA - obs)') +
  scale_x_continuous('Bias is LAI (ERA - obs)')

# plot for single climate zone with seasonality
ggplot(df_all_comb %>% filter(cz_name %in% 'Aw')) +
  geom_point(aes(x = LAI_dif_mu, y = ET_dif_mu, fill = monthS),
             colour = 'grey35', shape = 21) +
  geom_hline(yintercept = 0, colour = 'grey20') +
  geom_vline(xintercept = 0, colour = 'grey20') +
  facet_wrap(~cz_name, nc = 3) +
  scale_fill_gradientn(colours = kovesi.cyclic_mygbm_30_95_c78(12)) +
  scale_y_continuous('Bias is ET (ERA - obs)') +
  scale_x_continuous('Bias is LAI (ERA - obs)')

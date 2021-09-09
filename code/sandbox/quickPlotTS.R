require(tidyr)
require(dplyr)
require(ggplot2)
require(patchwork)


## prepare data source
load('data/inter_data/df_single_var_agreement/df_single_var_agr_tmp_LAI_cleaner.RData')
df_LAI_comb <- temp_agr_con 

load('data/inter_data/df_single_var_agreement/df_single_var_agr_tmp_LST_cleaner.RData')
df_LST_comb <- temp_agr_con ; varname <- 'LST'

by_vctr <- c("time", "t2.clim.bin", "sm.clim.bin")

df_all <- inner_join(by = by_vctr, suffix = c("_LAI", "_LST"),
                    x = df_LAI_comb, y = df_LST_comb) %>%
  select(by_vctr, dif_mu_LAI, dif_mu_LST, N_LAI, N_LST, 
         obs_mu_LAI, sim_mu_LAI, obs_mu_LST, sim_mu_LST) %>%   # 
  mutate(year = as.numeric(format(time,'%Y'))) %>%
  mutate(monthS = as.numeric(format(time, '%m')))
  

## Sorting out labels of the bins
# T2 bins 
t2.clim.bin.values <- as.numeric(levels(df_all$t2.clim.bin))
t2.binwidth <- unique(diff(t2.clim.bin.values))/2
t2.clim.bin.labels <- paste(t2.clim.bin.values - t2.binwidth, '< T2m <', 
                            t2.clim.bin.values + t2.binwidth)
names(t2.clim.bin.labels) <- sprintf('T2bin%02d', 1:length(t2.clim.bin.labels))
names(t2.clim.bin.values) <- sprintf('T2bin%02d', 1:length(t2.clim.bin.labels))

# SM bins 
sm.clim.bin.values <- as.numeric(levels(df_all$sm.clim.bin))
sm.binwidth <- unique(round(diff(sm.clim.bin.values), digits = 6))/2
sm.clim.bin.labels <- paste(sm.clim.bin.values - sm.binwidth, '< SM <', 
                            sm.clim.bin.values + sm.binwidth)
names(sm.clim.bin.labels) <- sprintf('SMbin%02d', 1:length(sm.clim.bin.labels))
names(sm.clim.bin.values) <- sprintf('SMbin%02d', 1:length(sm.clim.bin.labels))



# define bin selection
t2.bin.centre <- 6
sm.bin.centre <- 0.4

t2.bin.id <- names(which(t2.clim.bin.values == t2.bin.centre))
sm.bin.id <- names(which(sm.clim.bin.values == sm.bin.centre))

df4ts <- df_all %>%
  filter(t2.clim.bin == factor(t2.bin.centre, levels(df_all$t2.clim.bin))) %>%  
  filter(sm.clim.bin == factor(sm.bin.centre, levels(df_all$sm.clim.bin)))

df4hyst <-  df_all %>%
  select(by_vctr, dif_mu_LAI, dif_mu_LST, N_LAI, N_LST) %>%   # 
  rename(y = dif_mu_LST, x = dif_mu_LAI, ny = N_LST, nx = N_LAI) %>%
  filter(!is.na(y) & !is.na(x)) %>% 
  filter(t2.clim.bin == factor(t2.bin.centre, levels(df_all$t2.clim.bin))) %>%  
  filter(sm.clim.bin == factor(sm.bin.centre, levels(df_all$sm.clim.bin))) %>%
  mutate(year = as.numeric(format(time,'%Y'))) %>%
  mutate(monthS = as.numeric(format(time, '%m')))   

out <- fit_hyst(df4hyst)






# for graphs... 
fig.format <- 'png'
fig.path <- 'results/czbinDiagnostics_LAIvsLST'
dir.create(fig.path)


# some graphic param...
lgd <- theme(legend.position = 'right',
             legend.key.height = unit(3, units = 'cm'),
             panel.grid = element_blank(),
             strip.text = element_text(size = 6))
# set-up colour palette
cols <- c(viridis(n = 6), magma(n = 6)[6:1])


gry1 <- 'grey60'  # <-- the axes
gry2 <- 'grey30'  # <-- the edges of the points
gry3 <- 'grey45'  # <-- the smoothed path


ebarwidth <- 0.02
ebarheight <- 0.2

gLAI <- ggplot(df4ts) +
  geom_line(aes(x = time, y = sim_mu_LAI), colour = 'grey25') + 
  geom_line(aes(x = time, y = obs_mu_LAI), colour = 'olivedrab') +
  scale_y_continuous('LAI') +
  scale_x_date('', expand = c(0, 0)) +
  ggtitle(paste('LAI', 'for climate space:', t2.clim.bin.labels[t2.bin.id], 'and', sm.clim.bin.labels[sm.bin.id]))

gLST <- ggplot(df4ts) +
  geom_line(aes(x = time, y = sim_mu_LST), colour = 'grey25') + 
  geom_line(aes(x = time, y = obs_mu_LST), colour = 'orangered') +
  scale_y_continuous('LST') +
  scale_x_date('', expand = c(0, 0)) +
  ggtitle('Mean LST of the max LST days')

gCross <- ggplot(df4hyst) +
  geom_hline(yintercept = 0, colour = gry1) +
  geom_vline(xintercept = 0, colour = gry1) +
  geom_point(aes(x = x, y = y, fill = monthS),
             colour = gry2, stroke = 0.3, shape = 21, size = 2) +
  geom_path(data = out$df_s, aes(x = x, y = y), colour = gry3, size = 2) +
  geom_errorbar(data = out$df_m, aes(x = x_mu, ymin = y_mu - y_sd, ymax = y_mu + y_sd),
                colour = gry2,  width = ebarwidth) +
  geom_errorbarh(data = out$df_m, aes(y = y_mu, xmin = x_mu - x_sd, xmax = x_mu + x_sd),
                 colour = gry2,  height = ebarheight) +
  geom_point(data = out$df_m, aes(x = x_mu, y = y_mu, fill = monthS),
             colour = gry2, stroke = 0.3, shape = 21, size = 4) +
  scale_y_continuous(paste('Bias in LST', '(ERA - obs)')) +
  scale_x_continuous('Bias in LAI (ERA - obs)') +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) + 
  ggtitle('Hysteresis patterns between LAI and LST biases') +
  lgd



g <- (gLAI / gLST) | gCross

ggsave(filename = paste0('csbinDiagnostic_', 'LST', 'vsLAI_', 
                         t2.bin.id, '_', sm.bin.id, '.', fig.format),
       path = fig.path, width = 16, height = 9)

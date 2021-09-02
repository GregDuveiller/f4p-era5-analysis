
require(ggplot2)
require(dplyr)


# some graphic param...
lgd <- theme(legend.position = 'left',
             legend.key.height = unit(3, units = 'cm'),
             panel.grid = element_blank(),
             strip.text = element_text(size = 6))




# load/prepare dataframe with data...
df_all <- df_all_comb %>% 
  rename(x_mu = dif_mu_LAI, x_sd = dif_sd_LAI) %>%  # rename for simplicity
  select(-y_mu_std, -y_sd_std) %>%  # not sure where these come from exactly
  filter(y_var == 'LST' -> varname) %>%
  filter(!is.na(y_mu) & !is.na(x_mu)) %>%
  mutate(year = as.numeric(format(time,'%Y'))) 

# filter here for a section of the climspace
df <- df_all %>%
  filter(t2.clim.bin == 10) %>%
  filter(sm.clim.bin == 0.32)



# calculate monthly averages
df_m <- df %>%
  group_by(monthS) %>%
  summarize(y_sd = sd(y_mu),
            x_sd = sd(x_mu),
            y_mu = mean(y_mu),
            x_mu = mean(x_mu))


# fit the hysteresis curve decomposing x and y 
# (we do it on the raw data instead of the means, but resuylt is identical)
y <- rep(df$y_mu, times = 3)
x <- rep(df$x_mu, times = 3)
t <- c(df$monthS - 12, df$monthS, df$monthS + 12)
per <- 12

yfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
xfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))

# yfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
# xfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))

df_s <- data.frame(t = seq(0,12,0.1))
df_s$y_mu <- predict.lm(yfit, df_s)
df_s$x_mu <- predict.lm(xfit, df_s)
df_s$monthS <- df_s$t





# overall plot of the situation
ggplot(df) + 
  geom_hline(yintercept = 0, colour = 'grey60') +
  geom_vline(xintercept = 0, colour = 'grey60') +
  geom_point(aes(x = x_mu, y = y_mu, fill = monthS),
             colour = 'grey30', stroke = 0.4, shape = 21, size = 2) +
  geom_path(data = df_s, aes(x = x_mu, y = y_mu), colour = 'grey45', size = 2) +
  geom_errorbar(data = df_m, aes(x = x_mu, ymin = y_mu - y_sd, ymax = y_mu + y_sd),
                colour = 'grey30',  width = 0.02) +
  geom_errorbarh(data = df_m, aes(y = y_mu, xmin = x_mu - x_sd, xmax = x_mu + x_sd),
                 colour = 'grey30',  height = 0.2) +
  geom_point(data = df_m, aes(x = x_mu, y = y_mu, fill = monthS),
             colour = 'grey45', stroke = 0.3, shape = 21, size = 4) +
  # geom_point(aes(x = dif_mu_LAI, y = y_mu, fill = monthS),
  #            colour = 'grey45', stroke = 0.1, shape = 21, size = 2) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
  scale_x_continuous('Bias in LAI (ERA - obs)') +
  coord_cartesian(ylim = var_range[[varname]]) +
  ggtitle(label = paste0('Patterns for selected variable: ', varname)) + 
  lgd


# make plot of decomposed sequences

ggplot(df, aes(x = monthS, y = x_mu, fill = monthS)) + 
  geom_hline(yintercept = 0, colour = 'grey30') +
  geom_line(data = df_s, colour = 'grey45', size = 2) +
  geom_point(colour = 'grey70', stroke = 0.3, shape = 21, size = 2) +
  geom_point(data = df_m, colour = 'grey45', stroke = 0.3, shape = 21, size = 4) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  ggtitle(label = paste0('Temporal patterns for a given axis: ', 'x_mu')) + lgd

ggplot(df, aes(x = monthS, y = y_mu, fill = monthS)) + 
  geom_hline(yintercept = 0, colour = 'grey30') +
  geom_line(data = df_s, colour = 'grey45', size = 2) +
  geom_point(colour = 'grey70', stroke = 0.3, shape = 21, size = 2) +
  geom_point(data = df_m, colour = 'grey45', stroke = 0.3, shape = 21, size = 4) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  ggtitle(label = paste0('Temporal patterns for a given axis: ', 'y_mu')) + lgd




require(ggplot2)
require(dplyr)
require(viridis)

# some graphic param...
lgd <- theme(legend.position = 'left',
             legend.key.height = unit(3, units = 'cm'),
             panel.grid = element_blank(),
             strip.text = element_text(size = 6))
# set-up colour palette
cols <- c(viridis(n = 6), magma(n = 6)[6:1])

# load/prepare dataframe with data...
load('data/inter_data/df_single_var_agreement/df_single_var_agr_tmp_LAI_cleaner.RData')
df_LAI_comb <- temp_agr_con 
load('data/inter_data/df_single_var_agreement/df_single_var_agr_tmp_LST_cleaner.RData')
df_LST_comb <- temp_agr_con ; varname <- 'LST'

by_vctr <- c("time", "t2.clim.bin", "sm.clim.bin")

df_all <- left_join(by = by_vctr, suffix = c("_LAI", "_LST"),
            x = df_LAI_comb, y = df_LST_comb) %>%
  select(by_vctr, dif_mu_LAI, dif_mu_LST) %>%   # 
  rename(y = dif_mu_LST, x = dif_mu_LAI) %>%
  filter(!is.na(y) & !is.na(x)) %>%
  mutate(year = as.numeric(format(time,'%Y'))) %>%
  mutate(monthS = as.numeric(format(time, '%m')))

# filter here for a section of the climspace
df <- df_all %>%
  filter(t2.clim.bin == -2) %>%  # <-  need to change these accordingly ///
  filter(sm.clim.bin == 0.52)    # <-  need to change these accordingly /// 

# calculate monthly averages
df_m <- df %>%
  group_by(monthS) %>%
  summarize(y_mu = mean(y),
            x_mu = mean(x),
            y_sd = sd(y),
            x_sd = sd(x))

# fit the hysteresis curve decomposing x and y 
# (we do it on the raw data instead of the means, but resuylt is identical)
y <- rep(df$y, times = 3)
x <- rep(df$x, times = 3)
t <- c(df$monthS - 12, df$monthS, df$monthS + 12)
per <- 12
  
# second harmonic fits
tyfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
txfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))

lmfit <- lm(y ~ x)

# more complicated fits... 
tyfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
txfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))

df_s <- data.frame(t = seq(0,12,0.1))
df_s$y <- predict.lm(tyfit, df_s)
df_s$x <- predict.lm(txfit, df_s)
df_s$monthS <- df_s$t


#### get metrics 
df_p <- data.frame(
  yIntercept = lmfit$coefficients[1],
  slope = lmfit$coefficients[2],
  area = 0.5 * sum(diff(df_s$y, lag = 2) * df_s$x[2:(length(df_s$x)-1)]),
  loopDir = sign(area)
)






#### plotting ####

gry1 <- 'grey60'  # <-- the axes
gry2 <- 'grey30'  # <-- the edges of the points
gry3 <- 'grey45'  # <-- the smoothed path

# overall plot of the situation
ggplot(df) + 
  geom_hline(yintercept = 0, colour = gry1) +
  geom_vline(xintercept = 0, colour = gry1) +
  geom_point(aes(x = x, y = y, fill = monthS),
             colour = gry2, stroke = 0.4, shape = 21, size = 2) +
  geom_abline(slope = lmfit$coefficients[2], intercept = lmfit$coefficients[1],
              colour = gry3, linetype = 'dashed') +
  geom_path(data = df_s, aes(x = x, y = y), colour = gry3, size = 2) +
  geom_errorbar(data = df_m, aes(x = x_mu, ymin = y_mu - y_sd, ymax = y_mu + y_sd),
                colour = gry2,  width = 0.02) +
  geom_errorbarh(data = df_m, aes(y = y_mu, xmin = x_mu - x_sd, xmax = x_mu + x_sd),
                 colour = gry2,  height = 0.2) +
  geom_point(data = df_m, aes(x = x_mu, y = y_mu, fill = monthS),
             colour = gry2, stroke = 0.3, shape = 21, size = 4) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
  scale_x_continuous('Bias in LAI (ERA - obs)') +
  ggtitle(label = paste0('Hysteresis pattern between bias in LAI and bias in ', varname),
          subtitle = paste0('Slope = ', format(df_p$slope, digits = 4), ' | ',
                            'Area = ', format(df_p$area, digits = 4))) + 
  lgd


# make plot of decomposed sequences

ggplot(df, aes(x = monthS, fill = monthS)) + 
  geom_hline(yintercept = 0, colour = gry1) +
  geom_line(data = df_s, aes(y = x), 
            colour = gry3, size = 2) +
  geom_point(aes(y = x), 
             colour = gry2, stroke = 0.3, shape = 21, size = 2) +
  geom_errorbar(data = df_m, aes(ymin = x_mu - x_sd, ymax = x_mu + x_sd),
                colour = gry2,  width = 0.2) +
  geom_point(data = df_m, aes(y = x_mu), 
             colour = gry2, stroke = 0.5, shape = 21, size = 4) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  ggtitle(label = paste0('Smoothed temporal pattern for the LAI bias axis')) + 
  lgd

ggplot(df, aes(x = monthS, fill = monthS)) + 
  geom_hline(yintercept = 0, colour = gry1) +
  geom_line(data = df_s, aes(y = y), 
            colour = gry3, size = 2) +
  geom_point(aes(y = y), 
             colour = gry2, stroke = 0.3, shape = 21, size = 2) +
  geom_errorbar(data = df_m, aes(ymin = y_mu - y_sd, ymax = y_mu + y_sd),
                colour = gry2,  width = 0.2) +
  geom_point(data = df_m, aes(y = y_mu), 
             colour = gry2, stroke = 0.5, shape = 21, size = 4) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  ggtitle(label = paste0('Smoothed temporal pattern for the ', varname, ' bias axis')) + 
  lgd

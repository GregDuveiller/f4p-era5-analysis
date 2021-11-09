

require(dplyr)
require(tidyr)


load("~/work/workspace/f4p-era5-analysis/data/inter_data/df_comb_obs_vs_sim/df_comb___LAI.RData")
df_LAI <- df_comb %>% mutate(delta_LAI = sim - obs) %>% select(-obs, -sim)
load("~/work/workspace/f4p-era5-analysis/data/inter_data/df_comb_obs_vs_sim/df_comb___LST.RData")
df_LST <- df_comb %>% mutate(delta_LST = sim - obs) %>% select(-obs, -sim)
rm(df_comb)
df_all <- inner_join(df_LAI, df_LST, by = c('x', 'y', 'year', 'month')) 


df_all <- df_all %>% 
  mutate(monthS = ifelse(sign(y) < 0, (month + 6) %% 12, month))  %>%
  mutate(monthS = ifelse(monthS == 0, 12, monthS))  %>% 
  mutate(time = as.Date(x = paste(year, monthS, '15', sep = '-')))

df_all <- df_all %>%
  rename(lat = y, lon = x)

df <- df_all %>% filter(lon == 15.125, lat == 48.375)


## PLOT BREAK !! 

require(viridis)
# some graphic param...
lgd <- theme(legend.position = 'left',
             legend.key.height = unit(3, units = 'cm'),
             panel.grid = element_blank(),
             strip.text = element_text(size = 6))
# set-up colour palette
cols <- c(viridis(n = 6), magma(n = 6)[6:1])

ggplot(df) + 
  geom_point(aes(x = delta_LAI, y = delta_LST, fill = monthS), shape = 21) +
  geom_path(data = df_s, aes(x = x, y = y)) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  scale_y_continuous(paste('Bias in', 'LST', '(ERA - obs)')) +
  scale_x_continuous('Bias in LAI (ERA - obs)') +
  lgd



fit.hyst <- function(df){
  
  

y <- rep(df$delta_LST, times = 3)
x <- rep(df$delta_LAI, times = 3)
t <- c(df$monthS - 12, df$monthS, df$monthS + 12)
per <- 12

lmfit <- lm(y ~ x)

# # more complicated fits...
# tyfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t)+sin(8*pi/per*t)+cos(8*pi/per*t))
# txfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t)+sin(8*pi/per*t)+cos(8*pi/per*t)) 
# third order harmonics ? ...
tyfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
txfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
# # second harmonic fits
# tyfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
# txfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))

df_s <- data.frame(t = seq(0.5,12.5,0.1))
df_s$y <- predict.lm(tyfit, df_s)
df_s$x <- predict.lm(txfit, df_s)
df_s$t2.clim.bin <- unique(df$t2.clim.bin)
df_s$sm.clim.bin <- unique(df$sm.clim.bin)


#### get metrics and make an output dataframe

beta_y <- tyfit$coefficients; names(beta_y) <- paste0('b', 'y', 0:(length(tyfit$coefficients)-1))
beta_x <- txfit$coefficients; names(beta_x) <- paste0('b', 'x', 0:(length(txfit$coefficients)-1))

df_p <- data.frame(
  lat = df$lat[1],
  lon = df$lon[1],
  slope = lmfit$coefficients[2],
  y_int = lmfit$coefficients[1],
  areaL = 0.5 * sum(diff(df_s$y, lag = 2) * df_s$x[2:(length(df_s$x)-1)]),
  t(beta_y), t(beta_x),
  xrange_s = abs(diff(range(df_s$x))),
  yrange_s = abs(diff(range(df_s$y)))
)
#return(list(df_p = df_p, df_s = df_s))
return(df_p)

}

out <- fit.hyst(df)

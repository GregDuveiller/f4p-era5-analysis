

## Follow here after "mkplots___crossVar___climspace.R"


varname <- 'LST'


df <- df_all_comb %>% 
  filter(y_var == varname) %>%
  filter(t2.clim.bin == 14) %>%
  filter(sm.clim.bin == 0.32) %>% 
  filter(!is.na(y_mu) & !is.na(dif_mu_LAI)) 

df <- df_all_comb %>%
  filter(y_var == varname) %>%
  filter(t2.clim.bin == 22) %>%
  filter(sm.clim.bin == 0.32) %>%
  filter(!is.na(y_mu) & !is.na(dif_mu_LAI))

df <- df_all_comb %>%
  filter(y_var == varname) %>%
  filter(t2.clim.bin == 10) %>%
  filter(sm.clim.bin == 0.40) %>%
  filter(!is.na(y_mu) & !is.na(dif_mu_LAI))




df <- df %>% 
  mutate(year = as.numeric(format(time,'%Y')))



lgd <- theme(legend.position = 'left',
               legend.key.height = unit(3, units = 'cm'),
               panel.grid = element_blank(),
               strip.text = element_text(size = 6))


ggplot(df_dum) + 
  geom_hline(yintercept = 0, colour = 'grey60') +
  geom_vline(xintercept = 0, colour = 'grey60') +
  geom_point(aes(x = dif_mu_LAI, y = y_mu, fill = monthS),
             colour = 'grey45', stroke = 0.1, shape = 21, size = 2) +
  facet_grid(t2.clim.bin~sm.clim.bin, labeller = climbin_labeller) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
  scale_x_continuous('Bias in LAI (ERA - obs)') +
  coord_cartesian(ylim = var_range[[varname]]) +
  ggtitle(label = paste0('Patterns for selected variable: ', varname),
          subtitle = 'Analysized per bins in a climate space of mean annual 2m Temperature vs. Soil Moisture') + 
  lgd




cols_yrs <- viridis(n = length(unique(df$year)))


t <- seq_len(nrow(df_dum))
df <- df_dum %>%
  mutate(xs = smooth.spline(t, dif_mu_LAI)$y,
         ys = smooth.spline(t, y_mu)$y) %>%
  arrange(monthS)

ggplot(df) + 
  geom_hline(yintercept = 0, colour = 'grey30') +
  geom_line(aes(x = monthS, y = dif_mu_LAI, group = year, colour = year),
            size = 0.4) +
  geom_point(aes(x = monthS, y = dif_mu_LAI, fill = year),
             colour = 'grey70', stroke = 0.3, shape = 21, size = 2) +
  scale_colour_gradientn('', colours = cols_yrs) +
  scale_fill_gradientn('', colours = cols_yrs) 


ggplot(df) + 
  geom_hline(yintercept = 0, colour = 'grey30') +
  geom_line(aes(x = monthS, y = y_mu, group = year),
            colour = 'grey70', size = 0.4) +
  geom_point(aes(x = monthS, y = y_mu, fill = monthS),
             colour = 'grey70', stroke = 0.1, shape = 21, size = 2) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  lgd




ggplot(df_dum) +
  geom_line(aes(x = time, y = dif_mu_LAI), colour = 'grey45') +
  geom_line(aes(x = time, y = y_mu), colour = 'red') +
  scale_fill_gradientn('', colours = cols) +
  ggtitle(label = paste0('Patterns for selected variable: ', varname),
          subtitle = 'Analysized per bins in a climate space of mean annual 2m Temperature vs. Soil Moisture') + 
  theme(legend.position = 'left',
        legend.key.height = unit(3, units = 'cm'),
        panel.grid = element_blank(),
        strip.text = element_text(size = 6))







df_dum_m <- df_dum %>%
  group_by(monthS) %>%
  summarize(mean_y = mean(y_mu),
            mean_x = mean(dif_mu_LAI),
            sd_y = sd(y_mu),
            sd_x = sd(dif_mu_LAI))

 


ggplot(df_dum_m) + 
  geom_hline(yintercept = 0, colour = 'grey60') +
  geom_vline(xintercept = 0, colour = 'grey60') +
  geom_errorbar(aes(x = mean_x, ymin = mean_y - sd_y, ymax = mean_y + sd_y),
                colour = 'grey45',  width = 0.02) +
  geom_errorbarh(aes(y = mean_y, xmin = mean_x - sd_x, xmax = mean_x + sd_x),
                colour = 'grey45',  height = 0.2) +
  geom_point(aes(x = mean_x, y = mean_y, fill = monthS),
             colour = 'grey45', stroke = 0.3, shape = 21, size = 2) +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
  scale_x_continuous('Bias in LAI (ERA - obs)') +
  coord_cartesian(ylim = var_range[[varname]]) +
  ggtitle(label = paste0('Patterns for selected variable: ', varname),
          subtitle = 'Analysized per bins in a climate space of mean annual 2m Temperature vs. Soil Moisture') + 
  theme(legend.position = 'left',
        legend.key.height = unit(3, units = 'cm'),
        panel.grid = element_blank(),
        strip.text = element_text(size = 6))





# set.seed(12345)
# up <- seq(0,1,length.out=100)^3
# down <- sqrt(seq(1,0,length.out=100))
# x <- c(seq(0, 1, length.out=length(up)),
#        seq(1, 0, length.out=length(down)))
# 
# data <- data.frame(x=x, y=c(up,down),
#                    measuredx=x + rnorm(length(x))*0.01,
#                    measuredy=c(up,down) + rnorm(length(up)+length(down))*0.03)
# 
# 
# t <- seq_len(nrow(data) + 1)
# xs <- smooth.spline(t, c(data$measuredx, data$measuredx[1]))$y
# ys <- smooth.spline(t, c(data$measuredy, data$measuredy[1]))$y
# with(data, plot(measuredx, measuredy))
# lines(xs, ys)



t <- seq_len(nrow(df_dum) + 1)

df <- df_dum %>% arrange(x = dif_mu_LAI, t = monthS)

xs <- smooth.spline(t, c(df$dif_mu_LAI, df$dif_mu_LAI[1]))$y
ys <- smooth.spline(t, c(df$y_mu, df$y_mu[1]))$y
with(df, plot(dif_mu_LAI, y_mu))
lines(xs, ys)

df_dum$xs <- xs[1:nrow(df_dum)]
df_dum$ys <- ys[1:nrow(df_dum)]

ggplot(df_dum) + 
  geom_hline(yintercept = 0, colour = 'grey60') +
  geom_vline(xintercept = 0, colour = 'grey60') +
  geom_point(aes(x = dif_mu_LAI, y = y_mu, fill = monthS),
             colour = 'grey45', stroke = 0.1, shape = 21, size = 2) +
  geom_line(aes(x = xs, y = ys), colour = 'grey45') +
  scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
  scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
  scale_x_continuous('Bias in LAI (ERA - obs)') +
  coord_cartesian(ylim = var_range[[varname]]) +
  ggtitle(label = paste0('Patterns for selected variable: ', varname),
          subtitle = 'Analysized per bins in a climate space of mean annual 2m Temperature vs. Soil Moisture') + 
  theme(legend.position = 'left',
        legend.key.height = unit(3, units = 'cm'),
        panel.grid = element_blank(),
        strip.text = element_text(size = 6))



t <- seq_len(nrow(df_dum_m) + 1)
xs <- smooth.spline(t, c(df_dum_m$mean_x, df_dum_m$mean_x[1]))$y
ys <- smooth.spline(t, c(df_dum_m$mean_y, df_dum_m$mean_y[1]))$y
with(df_dum_m, plot(mean_x, mean_y))
lines(xs, ys)






library(hysteresis)

model <- fel(df_dum_m$mean_x, df_dum_m$mean_y, method="harmonic2", period=12, times="equal")
plot(model,main="2-step Simple Harmonic Regression Ellipse Example")


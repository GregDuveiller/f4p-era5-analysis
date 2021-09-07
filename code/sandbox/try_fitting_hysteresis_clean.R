
require(ggplot2)
require(dplyr)
require(viridis)
require(tidyr)


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



# t2.bin <- 0.26
# sm.bin <- 0.2

fig.format <- 'png'
fig.path <- 'results/hysteresis'
dir.create(fig.path)

# define function to make plots per 2d bin
fit.hyst.plot <- function(t2.bin.num, sm.bin.num, plotting = NULL){
  
  # filter here for a section of the climspace
  df <- df_all %>%
    filter(t2.clim.bin == levels(df_all$t2.clim.bin)[t2.bin.num]) %>%  
    filter(sm.clim.bin == levels(df_all$sm.clim.bin)[sm.bin.num])    
  
  if(dim(df)[1] == 0){
    # print('No data for this bin')
    return(df_p = NULL)}
  
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
  tyfit2 <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
  txfit2 <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
  
  lmfit <- lm(y ~ x)
  
  # more complicated fits... 
  tyfit3 <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
  txfit3 <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
  
  df_s <- data.frame(t = seq(0.5,12.5,0.1))
  df_s$y3 <- predict.lm(tyfit3, df_s)
  df_s$x3 <- predict.lm(txfit3, df_s)
  df_s$y2 <- predict.lm(tyfit2, df_s)
  df_s$x2 <- predict.lm(txfit2, df_s)
  df_s$monthS <- df_s$t
  
  
  #### get metrics and make an output dataframe
  
  beta_y <- tyfit3$coefficients; names(beta_y) <- paste0('b', 'y', 0:6)
  beta_x <- txfit3$coefficients; names(beta_x) <- paste0('b', 'x', 0:6)
  
  df_p <- data.frame(
    t2.clim.bin = levels(df_all$t2.clim.bin)[t2.bin.num],
    sm.clim.bin = levels(df_all$sm.clim.bin)[sm.bin.num],
    slope = lmfit$coefficients[2],
    y_int = lmfit$coefficients[1],
    areaL = 0.5 * sum(diff(df_s$y3, lag = 2) * df_s$x3[2:(length(df_s$x3)-1)]),
    t(beta_y), t(beta_x),
    xrange_s = abs(diff(range(df_s$x3))),
    yrange_s = abs(diff(range(df_s$y3)))
  )
  
  
  
  #### plotting ####
  
  if(!is.null(plotting)){
    
    # some graphic param...
    lgd <- theme(legend.position = 'left',
                 legend.key.height = unit(3, units = 'cm'),
                 panel.grid = element_blank(),
                 strip.text = element_text(size = 6))
    # set-up colour palette
    cols <- c(viridis(n = 6), magma(n = 6)[6:1])
    
    
    gry1 <- 'grey60'  # <-- the axes
    gry2 <- 'grey30'  # <-- the edges of the points
    gry3 <- 'grey45'  # <-- the smoothed path
    
    fig.format <- 'png'
    
    xrange <- range(df_all$x)
    yrange <- range(df_all$y)
    
    
    # overall plot of the situation
    gxy <- ggplot(df) + 
      geom_hline(yintercept = 0, colour = gry1) +
      geom_vline(xintercept = 0, colour = gry1) +
      geom_point(aes(x = x, y = y, fill = monthS),
                 colour = gry2, stroke = 0.4, shape = 21, size = 2) +
      geom_abline(slope = lmfit$coefficients[2], intercept = lmfit$coefficients[1],
                  colour = gry3, linetype = 'dashed') +
      geom_path(data = df_s, aes(x = x2, y = y2), colour = gry1, size = 1.5) +
      geom_path(data = df_s, aes(x = x3, y = y3), colour = gry3, size = 2) +
      geom_errorbar(data = df_m, aes(x = x_mu, ymin = y_mu - y_sd, ymax = y_mu + y_sd),
                    colour = gry2,  width = 0.02) +
      geom_errorbarh(data = df_m, aes(y = y_mu, xmin = x_mu - x_sd, xmax = x_mu + x_sd),
                     colour = gry2,  height = 0.2) +
      geom_point(data = df_m, aes(x = x_mu, y = y_mu, fill = monthS),
                 colour = gry2, stroke = 0.3, shape = 21, size = 4) +
      scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb) +
      scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
      scale_x_continuous('Bias in LAI (ERA - obs)') +
      coord_cartesian(xlim = xrange, ylim = yrange) +
      ggtitle(label = paste0('Hysteresis pattern between bias in LAI and bias in ', varname), 
              subtitle = paste0('Centre of T2 bin: ', levels(df_all$t2.clim.bin)[t2.bin.num], '   ',
                                'Centre of SM bin: ', levels(df_all$sm.clim.bin)[sm.bin.num])) + 
      # subtitle = paste0('Slope = ', format(df_p$slope, digits = 4), ' | ',
      #                   'Area = ', format(df_p$area, digits = 4))) + 
      lgd
    
    
    # make plot of decomposed sequences
    
    gx <- ggplot(df, aes(x = monthS, fill = monthS)) + 
      geom_hline(yintercept = 0, colour = gry1) +
      geom_line(data = df_s, aes(y = x2), colour = gry1, size = 1.5) +
      geom_line(data = df_s, aes(y = x3), colour = gry3, size = 2) +
      geom_point(aes(y = x), 
                 colour = gry2, stroke = 0.3, shape = 21, size = 2) +
      geom_errorbar(data = df_m, aes(ymin = x_mu - x_sd, ymax = x_mu + x_sd),
                    colour = gry2,  width = 0.2) +
      geom_point(data = df_m, aes(y = x_mu), 
                 colour = gry2, stroke = 0.5, shape = 21, size = 4) +
      scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb, guide = "none") +
      scale_y_continuous(paste('Bias in', 'LAI', '(ERA - obs)')) +
      scale_x_continuous('Month of the year (seasonally synched to NH)', breaks = 1:12, 
                         labels = month.abb, expand = c(0, 0), limits = c(0.5,12.5)) +
      #ggtitle(label = paste0('Smoothed temporal pattern for the LAI bias axis')) + 
      lgd
    
    gy <- ggplot(df, aes(x = monthS, fill = monthS)) + 
      geom_hline(yintercept = 0, colour = gry1) +
      geom_line(data = df_s, aes(y = y2), colour = gry1, size = 1.5) +
      geom_line(data = df_s, aes(y = y3), colour = gry3, size = 2) +
      geom_point(aes(y = y), 
                 colour = gry2, stroke = 0.3, shape = 21, size = 2) +
      geom_errorbar(data = df_m, aes(ymin = y_mu - y_sd, ymax = y_mu + y_sd),
                    colour = gry2,  width = 0.2) +
      geom_point(data = df_m, aes(y = y_mu), 
                 colour = gry2, stroke = 0.5, shape = 21, size = 4) +
      scale_fill_gradientn('', colours = cols, breaks = 1:12, labels = month.abb, guide = "none") +
      scale_y_continuous(paste('Bias in', varname, '(ERA - obs)')) +
      scale_x_continuous('Month of the year (seasonally synched to NH)', breaks = 1:12, 
                         labels = month.abb, expand = c(0, 0), limits = c(0.5,12.5)) +
      #ggtitle(label = paste0('Smoothed temporal pattern for the ', varname, ' bias axis')) + 
      lgd
    
    require(patchwork)
    
    g <- gxy + gx / gy
    ggsave(filename = paste0('hyst_LSTvsLAI_', sprintf('SMbin%02d',sm.bin.num), '_', 
                             sprintf('TMbin%02d',t2.bin.num),
                             '.', fig.format),
           path = fig.path, width = 16, height = 9)
  }
  return(df_p)
}


# loop thru it all
df_p_all <- data.frame(NULL)

for(i in 1:length(levels(df_all$t2.clim.bin))){
  for(j in 1:length(levels(df_all$sm.clim.bin))){
    df_p <- fit.hyst.plot(i, j)
    df_p_all <- bind_rows(df_p_all, fit.hyst.plot(i, j))
  }
}

metricOrder <- c(paste0('b', 'y', 0:6), paste0('b', 'x', 0:6), 
                 'areaL', 'slope', 'y_int', 'xrange_s', 'yrange_s')

# post-process the parameter df 
df <- df_p_all %>%
  mutate(t2.clim.bin = factor(t2.clim.bin, levels = levels(df_all$t2.clim.bin)),
         sm.clim.bin = factor(sm.clim.bin, levels = levels(df_all$sm.clim.bin))) %>%
  pivot_longer(cols = 3:21) 

ggplot(df) + 
  geom_raster(aes(x = t2.clim.bin, y = sm.clim.bin, fill = value)) +
  scale_fill_viridis(option = "A") +
  facet_wrap(~factor(name, levels = metricOrder), ncol = 7) 


dat <- df_p_all[,3:21]

mu <- apply(dat, 2, mean)
sd <- apply(dat, 2, sd)

mu_mat <- matrix(mu, nrow = dim(dat)[1], ncol = dim(dat)[2], byrow = T)
sd_mat <- matrix(sd, nrow = dim(dat)[1], ncol = dim(dat)[2], byrow = T)

dat_std <- (dat - mu_mat)/sd_mat
  
df_p_all_std <- df_p_all
df_p_all_std[,3:21] <- dat_std

df_std <- df_p_all_std %>%
  mutate(t2.clim.bin = factor(t2.clim.bin, levels = levels(df_all$t2.clim.bin)),
         sm.clim.bin = factor(sm.clim.bin, levels = levels(df_all$sm.clim.bin))) %>%
  pivot_longer(cols = 3:21) 




ggplot(df_std) + 
  geom_raster(aes(x = t2.clim.bin, y = sm.clim.bin, fill = value)) +
  scale_fill_viridis(option = "A", limits = c(-3,3)) +
  facet_wrap(~factor(name, levels = metricOrder), ncol = 7) 


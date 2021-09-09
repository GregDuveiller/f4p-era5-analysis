# # filter here for a section of the climspace
# df <- df_all %>%
#   filter(t2.clim.bin == levels(df_all$t2.clim.bin)[t2.bin.num]) %>%  
#   filter(sm.clim.bin == levels(df_all$sm.clim.bin)[sm.bin.num])    

fit_hyst <- function(df){
  
  if(dim(df)[1] == 0){
    print('No data to process...')
    return(list(df_p = NULL, df_s = NULL, df_m = NULL))}
  
  # calculate monthly averages
  df_m <- df %>%
    group_by(monthS) %>%
    summarize(y_mu = mean(y),
              x_mu = mean(x),
              y_sd = sd(y),
              x_sd = sd(x))
  
  # fit the hysteresis curve decomposing x and y 
  # (we do it on the raw data instead of the means, but result is identical)
  
  per <- length(unique(df$monthS))
  y <- rep(df$y, times = 3)
  x <- rep(df$x, times = 3)
  
  #t <- c(df$monthS - 12, df$monthS, df$monthS + 12)
  
  t <- c(df$monthS - per, df$monthS, df$monthS + per)

  lmfit <- lm(y ~ x)
  
  tyfit <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
  txfit <- lm(x ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t)+sin(6*pi/per*t)+cos(6*pi/per*t))
  
  df_s <- data.frame(t = seq(0.5,12.5,0.1))
  df_s$y <- predict.lm(tyfit, df_s)
  df_s$x <- predict.lm(txfit, df_s)
  # df_s$t2.clim.bin <- unique(df$t2.clim.bin)
  # df_s$sm.clim.bin <- unique(df$sm.clim.bin)
  
  
  #### get metrics and make an output dataframe
  
  beta_y <- tyfit$coefficients; names(beta_y) <- paste0('b', 'y', 0:(length(tyfit$coefficients)-1))
  beta_x <- txfit$coefficients; names(beta_x) <- paste0('b', 'x', 0:(length(txfit$coefficients)-1))
  
  df_p <- data.frame(
    # t2.clim.bin = levels(df_all$t2.clim.bin)[t2.bin.num],
    # sm.clim.bin = levels(df_all$sm.clim.bin)[sm.bin.num],
    slope = lmfit$coefficients[2],
    y_int = lmfit$coefficients[1],
    lmRMS = sqrt(mean((lmfit$residuals)^2)),
    areaL = 0.5 * sum(diff(df_s$y, lag = 2) * df_s$x[2:(length(df_s$x)-1)]),
    t(beta_y), t(beta_x),
    xrange_s = abs(diff(range(df_s$x))),
    yrange_s = abs(diff(range(df_s$y)))
  )
  
  return(list(df_p = df_p, df_s = df_s, df_m = df_m))
}
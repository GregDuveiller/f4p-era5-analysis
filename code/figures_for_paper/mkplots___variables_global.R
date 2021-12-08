# original name : prepare_caseStudies.R
#
# ########################################################
# Title         : 
# Description   : This code produces global maps of ERA5-satellite bias in LAI, a chosen variable (default LST). 
#                 It produces one figure made up of four maps and two timeseries, showing:
#                         a) maps of the average January and July temporal bias in LAI and b) LST
#                         c) time series  of the average spatial bias across different climate zones for LAI and LST
#                 One figure showing the correlation between bias in LAI and bias in (LST default) showing:
#                         a) maps of the temporal correlation (Jan July)
#                         c) time series of the spatial correlation for different climate zones
# Inputs	      : come from prepare__variables_global.R
#                 
# Outputs	      : figures  as above
#                 
# Options	      : 
# Date          : 03/12/21
# Version       : 1.1
# Licence       : N/A
# Authors       : Mark Pickering
# Maintainer    : Mark Pickering
# Notes		      : Based on Greg's agreement metrics
#                 
# ########################################################

###################################################
######     INITIALISE                         #####
###################################################

rm(list = ls()) 
start_time <- Sys.time() ; print(start_time)
# find date:
full_date <- as.character(Sys.time()) ; full_date <- strsplit(full_date, " ")
full_time <- full_date[[1]][2] ; full_date <- full_date[[1]][1]


library(ggplot2)
library(patchwork)
library(scales)
library(dplyr)

# chose variable names. LAI should be first and comparison variable second
varname_1 <- 'LAI' # 'albedo_wsa_vis' # 'E' # 'SM'  # 'LST' #'albedo_wsa_nir' albedo_wsa_vis albedo_bsa_nir albedo_bsa_vis #LAI
varname_2 <- 'LST'
base_dir <- 'data/inter_data/df_single_var_summaries/'

###################################################
######     I/O                                #####
###################################################

# output path
fig.format <- 'png'
fig.path <- paste0( base_dir, 'figures/',full_date,'/')
dir.create(fig.path, showWarnings = F, recursive = T)


# load the agreement metrics from prepare___variables.R
dpath <- base_dir
load( paste0(dpath, 'df_single_var_agr__sp_agr_overall__', varname_1,'.RData'))  ; sp_agr_overall_var1 <- sp_agr_overall       # temporal agreement metrics for each pixel across all months/years
load( paste0(dpath, 'df_single_var_agr__sp_agr_monthS__', varname_1,'.RData'))   ; sp_agr_monthS_var1  <- sp_agr_monthS         # temporal agreement metrics for each month (i.e. temporal agreement of a given month across years)
load( paste0(dpath, 'df_single_var_agr__temp_agr_gen__', varname_1,'.RData'))    ; temp_agr_gen_var1   <- temp_agr_gen          # spatial agreeement at a given moneth (for each climatezone)
# load( paste0(dpath, 'df_single_var_agr__agr__', varname_1,'.RData'))             ; agr_var1            <- agr                 # overall agreement metrics, all pixels, all times
# load( paste0(dpath, 'df_single_var_agr__freq__', varname_1,'.RData'))            ; freq_var1           <- freq                # binned histogram of frequency of values (all space and time)

load( paste0(dpath, 'df_single_var_agr__sp_agr_overall__', varname_2,'.RData'))  ; sp_agr_overall_var2 <- sp_agr_overall
load( paste0(dpath, 'df_single_var_agr__sp_agr_monthS__', varname_2,'.RData'))   ; sp_agr_monthS_var2  <- sp_agr_monthS
load( paste0(dpath, 'df_single_var_agr__temp_agr_gen__', varname_2,'.RData'))    ; temp_agr_gen_var2   <- temp_agr_gen
# load( paste0(dpath, 'df_single_var_agr__agr__', varname_2,'.RData'))             ; agr_var2            <- agr
# load( paste0(dpath, 'df_single_var_agr__freq__', varname_2,'.RData'))            ; freq_var2           <- freq
# 'agr', 'freq', 'sp_agr', 'temp_agr_gen', 'temp_agr_det'

# set start and end of timeseries data
start_date <- '2003-01-15' ; end_date <- '2018-12-31'
temp_agr_gen_var1 <- temp_agr_gen_var1 %>% filter(cz_major_zone != 'O')
temp_agr_gen_var1 <- temp_agr_gen_var1 %>% filter(time > as.Date(x = start_date ) & time < as.Date(x = end_date ) )
temp_agr_gen_var2 <- temp_agr_gen_var2 %>% filter(cz_major_zone != 'O')
temp_agr_gen_var2 <- temp_agr_gen_var2 %>% filter(time > as.Date(x = start_date ) & time < as.Date(x = end_date ) )
# colnames(agr_var1) <- paste0("agre$", colnames(agr_var1))   # <--- these seem not used
# colnames(agr_var2) <- paste0("agre$", colnames(agr_var2))   # <--- these seem not used

###################################################
######     FUNCTIONS                          #####
###################################################

# initialise data to fectch from agreement dataframes
f_set_metrics <- function(metric, sp_agr_overall_i = sp_agr_overall, temp_agr_gen_i = temp_agr_gen){
  
  if(metric == 'Bias'){
    metric_code_agr <- 'agre$bias'
    metric_code_tmp <- 'dif_mu' 
    metric_code <- 'diff_simSobs' 
    metric_label <- 'bias'
    metric_colpal <- rev(c("#081a2b", "#14406b", "#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B", "#700f1b", "#2d060b"))  # RColorBrewer::brewer.pal(n = 9, name = 'RdBu')
    metric_lims_s <- c(-1,1) * max(abs(quantile(sp_agr_overall_i$diff_simSobs, probs = c(0.025, 0.975), na.rm = T)))  
    metric_lims_t <- c(-1,1) * max(abs(quantile(temp_agr_gen_i$dif_mu, probs = c(0, 1), na.rm = T)))  
    metric_longname <- 'bias'}
  if(metric == 'Corr_biasLAIvsVAR'){ # this extracts the correlation between bias_VAR and bias_LAI
    metric_code_agr <- 'r'  #  average bias correlation
    metric_code_tmp <- 'agre_bVARvsbLAI$r'  # this is the spatial correlation (in time) between bias_VAR and bias_LAI (temp_agr_gen)
    metric_code <- 'agre_bVARvsbLAI$r'      # this is the temporal correlation (mapped) between bias_VAR and bias_LAI (sp_agr)
    metric_label <- 'correlation' #'rho'
    # metric_colpal <- rev(c("#081a2b", "#14406b", "#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B", "#700f1b", "#2d060b"))  # Previous colours # RColorBrewer::brewer.pal(n = 9, name = 'RdBu') 
    metric_colpal <- c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#F7F7F7", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695")  #same as RdYlBlu but with white in to remove negative eye bias
    metric_lims_s <- c(-1,1) #* max(abs(quantile(sp_agr_overall_i$bias_r, probs = c(0.025, 0.975), na.rm = T)))  
    metric_lims_t <- c(-0.6,0.6) # c(-0.6,0.6) #* max(abs(quantile(temp_agr_gen_i$bias_r, probs = c(0, 1), na.rm = T)))  
    metric_longname <- paste0('correlation between ', varname_2, ' bias and LAI bias')}
  
  metrics <- list(metric_code_agr, metric_code_tmp, metric_code, metric_label, metric_longname, metric_lims_s, metric_lims_t,  metric_colpal )
  names(metrics) <- c('metric_code_agr', 'metric_code_tmp', 'metric_code', 'metric_label', 'metric_longname', 'metric_lims_s', 'metric_lims_t',  'metric_colpal' )
  return(metrics)
}

# this figure organises the different timeseries and maps of bias. It places the variables of var1 and var 2 next to each other
f_bias_2var <- function(metric,  months = c(1,7), set_lims = NA, sp_agr_monthS_input_var1 = sp_agr_monthS_var1, sp_agr_monthS_input_var2 = sp_agr_monthS_var2, temp_agr_gen_input_var1 = temp_agr_gen_var1, temp_agr_gen_input_var2 = temp_agr_gen_var2 ) {

  # create the maps of temporal agreement
  g_map_1 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname_1, month_i = months[1], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input_var1 )
  g_map_2 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname_1, month_i = months[2], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input_var1)
  g_map_3 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname_2, month_i = months[1], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input_var2)
  g_map_4 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname_2, month_i = months[2], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input_var2)
  # create the timeseries of spatial agreement
  g_tmp_1 <- f_maketemp(metric, var_name = varname_1, temp_agr_gen_in = temp_agr_gen_input_var1, legend_pos = "none")
  g_tmp_2 <- f_maketemp(metric, var_name = varname_2, temp_agr_gen_in = temp_agr_gen_input_var2, inc_title = F)
  
  # organise the layout
  g_all <- ( g_map_1 + g_map_3) / ( g_map_2 + g_map_4) / g_tmp_1 / g_tmp_2 +
    plot_layout(heights = c(3, 3, 1, 1)) +
    plot_annotation(title = paste0('Temporal ', tolower(metric), ' represented in space')) # ', metrics$metric_longname, '
  
  # save
  fig.name <- paste0('genplot_', varname_1, '-',varname_2, '_', metric, '.', fig.format)
  print(paste0('save:', fig.path, fig.name))
  ggsave(filename = fig.name, plot = g_all, path = fig.path, width = 16, height = 9)
}

# this figure organises the different timeseries and maps of bias. It shows one variable
f_bias_1var <- function(metric,  months = c(1,7), set_lims = NA, sp_agr_monthS_input = sp_agr_monthS_var2, temp_agr_gen_input = temp_agr_gen_var2, varname = varname_2 ) {
  metrics <- f_set_metrics(metric, temp_agr_gen_i = temp_agr_gen_input)
  metric_longname <- metrics$metric_longname
    
  # create the maps of temporal agreement
  g_map_1 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname, month_i = months[1], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input )
  g_map_2 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname, month_i = months[2], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input)
  # create the timeseries of spatial agreement
  g_tmp_1 <- f_maketemp(metric, var_name = varname, temp_agr_gen_in = temp_agr_gen_input, legend_pos = "none")

  # organise the layout
  g_all <- ( g_map_1 ) / ( g_map_2 ) / g_tmp_1  +
    plot_layout(heights = c(3, 3, 1 )) +
    plot_annotation(title = paste0('Temporal ', metric_longname, ' represented in space')) # ', metrics$metric_longname, '
  
  # save
  fig.name <- paste0('genplot_', varname, '_', metric, '.', fig.format)
  print(paste0('save:', fig.path, fig.name))
  ggsave(filename = fig.name, plot = g_all, path = fig.path, width = 16, height = 9)
}

# this creates a timeseries of a given metric and temporal agreement dataframe
f_maketemp <- function(metric, var_name, temp_agr_gen_in = temp_agr_gen, legend_pos = "bottom", inc_title = T){
  metrics <- f_set_metrics(metric, temp_agr_gen_i = temp_agr_gen_in)
  metric_code_agr <- metrics$metric_code_agr ; metric_code_tmp <- metrics$metric_code_tmp ; metric_code <- metrics$metric_code ; metric_label <- metrics$metric_label ; metric_longname <- metrics$metric_longname
  metric_lims_s <- metrics$metric_lims_s ;  metric_lims_t <- metrics$metric_lims_t ; metric_colpal <- metrics$metric_colpal
  
  g_tmp <- ggplot(temp_agr_gen_in,
                  aes(x = time, colour = cz_major_zone)) + 
    geom_line(aes_string(y = metric_code_tmp )) +
    scale_colour_manual('Koppen-Geiger general climate zones: ', 
                        values = c('A'="#960000", 'B'="#FFCC00", 'C'="#00AA00", 'D'="#6E28B4"), #, 'E'="#6496FF"
                        labels = c('A'="Tropical", 'B'="Arid", 'C'="Temperate", 'D'="Continental")) + #, 'E'="Polar"
    scale_y_continuous(paste0( var_name, ' ', metric_label), limits = metric_lims_t) +
    scale_x_date('', expand = c(0,0), date_breaks = "2 year") +
    theme(legend.position = legend_pos,
          legend.box.spacing = unit(-0.6,"cm"), #legend.box.margin = margin(0, 6, 1, 6), 
          legend.direction = "horizontal") 
    
  if(inc_title) {g_tmp <- g_tmp + ggtitle( paste0('Time series of the spatial ', metric_longname, ' within climate zones') )}
  
  
  return(g_tmp)
  
}

# This produces temporal agreement in space by month_i (ie across years, but within same month)
f_spMapOfTempMetric_byMonth <- function(metric, var_name, month_i = 1, new_lims = NA, sp_agr_monthS_in = sp_agr_monthS){
  metrics <- f_set_metrics(metric, sp_agr_overall_i = sp_agr_monthS_in)
  if( !is.na(new_lims[1] ) ) { metrics$metric_lims_s <- new_lims }
  print(paste0('month_i : ', month_i))
  sp_agr_month_i <- sp_agr_monthS_in %>% filter(monthS == month_i  ) 
  g_map <- f_makemap(metrics, var_name, sp_agr_in = sp_agr_month_i)
  #fig.name <- paste0('g_diff_simSobs_', varname, 'vsLAI_map_temp',metric,'_month', month_i, '.png') ; ggsave(filename = fig.name, plot = g_map, path = fig.path, width = 16, height = 9)
  return(g_map)
}

f_makemap <- function(metrics, var_name, sp_agr_in = sp_agr, new_lims = NA, is_VARvsLAI = F){
  # if is_VARvsLAI = T # same as normal but instead we map the metric with x= biasINLAI and y= biasINVAR (normally x = obs, y = sim)
  metric_code_agr <- metrics$metric_code_agr ; metric_code_tmp <- metrics$metric_code_tmp ; metric_code <- metrics$metric_code ; metric_label <- metrics$metric_label ; metric_longname <- metrics$metric_longname
  metric_lims_s <- metrics$metric_lims_s ;  metric_lims_t <- metrics$metric_lims_t ; metric_colpal <- metrics$metric_colpal
  if( !is.na(new_lims[1] ) ) { metrics$metric_lims_s <- new_lims }
  #print(metric_colpal)
  
  g_map <- ggplot(sp_agr_in, aes(x = x, y = y)) + 
    geom_tile(aes_string(fill = metric_code)) +  
    scale_fill_gradientn( paste0( var_name, ' ', metric_label) , colours = metric_colpal, # previously remove label = 
                         limits = metric_lims_s, oob = squish) +
    scale_x_continuous('Longitude') + 
    scale_y_continuous('Latitude') +
    coord_cartesian(expand = F, 
                    xlim = c(-179.875, 179.875), 
                    ylim = c(-89.875, 83.875)) +
    theme(legend.position = "right",
          legend.key.height = unit(leg_height, 'cm'),
          panel.background = element_rect(fill = 'grey30'),
          panel.grid = element_line(colour = 'grey40')) #+
  # ggtitle( paste0('Temporal ', metric_longname, ' represented in space') )
  
  return(g_map)
}


###################################################
######     RUN                                #####
###################################################

# generic variables
max_y_biasLAIVAR <- 200000    # scatter range for bias VAR vs bias LAI standalone figure
leg_height <- 1.2             # legend height cm

### the overall bias between ERA5 and satellite         ###
# This is figure 1
metric <- 'Bias'
# plot 4 seasonal months bias average:
f_bias_2var(metric, months = c(1,7), set_lims = NA, sp_agr_monthS_input_var1 = sp_agr_monthS_var1, sp_agr_monthS_input_var2 = sp_agr_monthS_var2, temp_agr_gen_input_var1 = temp_agr_gen_var1, temp_agr_gen_input_var2 = temp_agr_gen_var2  ) # LAI: make the image of bias by month 

metric <- 'Corr_biasLAIvsVAR' # show the correlation between dLAI and dVAR
# look at the correlation between biases across different years (same month - selected months)
f_bias_1var(metric, months = c(1,7), set_lims = NA, varname = varname_2, sp_agr_monthS_input = sp_agr_monthS_var2, temp_agr_gen_input = temp_agr_gen_var2 ) # LAI: make the image of bias by month 


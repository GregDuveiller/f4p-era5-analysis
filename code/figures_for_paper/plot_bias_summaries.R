#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### plot_bias_summaries.R ####
# ---------------------------------------------------------------------------- #
# Purpose: plot figure showing general overview of the bias
# Project: f4p-era5-analysis
# Authors: G.Duveiller, M.Pickering
# ---------------------------------------------------------------------------- #


#### Initialization ####

require(ggplot2)
require(scales)
require(viridis)
require(tidyr)
require(dplyr)
require(RColorBrewer)
require(patchwork)

#### Load the data ####

load('data/figures_for_paper/data_for_summary_maps.RData')   # <---- ... 


#### setup a functions ####

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

# This produces temporal agreement in space by month_i (ie across years, but within same month)
f_spMapOfTempMetric_byMonth <- function(metric, var_name, month_i = 1, new_lims = NA, sp_agr_monthS_in = sp_agr_monthS){
  
  metrics <- f_set_metrics(metric, sp_agr_overall_i = sp_agr_monthS_in)
  
  if( !is.na(new_lims[1] ) ) { metrics$metric_lims_s <- new_lims }
  
  sp_agr_month_i <- sp_agr_monthS_in %>% 
    filter(monthS == month_i) 
  
  g_map <- f_makemap(metrics, var_name, sp_agr_in = sp_agr_month_i)
  
  return(g_map)
}

f_maketemp <- function(metric, var_name, temp_agr_gen_in = temp_agr_gen, legend_pos = "bottom", inc_title = T){
  
  metrics <- f_set_metrics(metric, temp_agr_gen_i = temp_agr_gen_in)
  
  metric_code_agr <- metrics$metric_code_agr 
  metric_code_tmp <- metrics$metric_code_tmp  
  metric_code <- metrics$metric_code 
  metric_label <- metrics$metric_label
  metric_longname <- metrics$metric_longname
  metric_lims_s <- metrics$metric_lims_s
  metric_lims_t <- metrics$metric_lims_t
  metric_colpal <- metrics$metric_colpal
  
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

f_makemap <- function(metrics, var_name, sp_agr_in = sp_agr, new_lims = NA, is_VARvsLAI = F){
  # if is_VARvsLAI = T # same as normal but instead we map the metric with x= biasINLAI and y= biasINVAR (normally x = obs, y = sim)
  metric_code_agr <- metrics$metric_code_agr ; 
  metric_code_tmp <- metrics$metric_code_tmp ; 
  metric_code <- metrics$metric_code ; 
  metric_label <- metrics$metric_label ; 
  metric_longname <- metrics$metric_longname
  metric_lims_s <- metrics$metric_lims_s ;  
  metric_lims_t <- metrics$metric_lims_t ; 
  
  metric_colpal <- metrics$metric_colpal
  
  if( !is.na(new_lims[1] ) ) { metrics$metric_lims_s <- new_lims }
  #print(metric_colpal)
  
  
  
  g_map <- ggplot(sp_agr_in, aes(x = x, y = y)) + 
    geom_tile(aes_string(fill = metric_code)) +  
    scale_fill_gradientn(paste0( var_name, ' ', metric_label) , 
                         colours = metric_colpal,
                         limits = metric_lims_s, oob = squish) +
    scale_x_continuous('Longitude') + 
    scale_y_continuous('Latitude') +
    coord_cartesian(expand = F, 
                    #xlim = c(-179.875, 179.875), 
                    ylim = c(-54.875, 75.875)) +
    theme(legend.position = "left",
          legend.key.height = unit(lgd_height, 'cm'),
          panel.background = element_rect(fill = 'grey30'),
          panel.grid = element_line(colour = 'grey40')) #+
  # ggtitle( paste0('Temporal ', metric_longname, ' represented in space') )
  
  return(g_map)
}


#### Make the plots ####

# ...
metric <- 'Bias'
lgd_height <- 1  
max_y_biasLAIVAR <- 200000

sp_agr_monthS_input_var1 <- sp_agr_monthS_var1
sp_agr_monthS_input_var2 <- sp_agr_monthS_var2
temp_agr_gen_input_var1 <- temp_agr_gen_var1
temp_agr_gen_input_var2 <- temp_agr_gen_var2

set_lims <- NA
months <- c(1,7)

LST_bias_colpal <- RColorBrewer::brewer.pal(n = 9, name = 'RdBu')
LAI_bias_colpal <- RColorBrewer::brewer.pal(n = 9, name = 'BrBG')



# create the maps of temporal agreement
g_map_1 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname_1, month_i = months[1], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input_var1)
g_map_2 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname_1, month_i = months[2], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input_var1)
g_map_3 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname_2, month_i = months[1], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input_var2)
g_map_4 <- f_spMapOfTempMetric_byMonth(metric, var_name = varname_2, month_i = months[2], new_lims = set_lims, sp_agr_monthS_in = sp_agr_monthS_input_var2)
# create the timeseries of spatial agreement
g_tmp_1 <- f_maketemp(metric, var_name = varname_1, temp_agr_gen_in = temp_agr_gen_input_var1, legend_pos = "none")
g_tmp_2 <- f_maketemp(metric, var_name = varname_2, temp_agr_gen_in = temp_agr_gen_input_var2, inc_title = F)





#### Export the figure #### 

# assemble the panels...

# organise the layout

gLAI <- g_map_1 + g_map_2 + plot_layout(guides = 'collect')
gLST <- g_map_3 + g_map_4 + plot_layout(guides = 'collect')

g_all <- gLAI / g_tmp_1 / gLST / g_tmp_2 + plot_layout(heights = c(3, 1, 3, 1))

#g_all <- ( g_map_1 + g_map_3) / ( g_map_2 + g_map_4) / g_tmp_1 / g_tmp_2 +
g_all <- g_tmp_1 / ( g_map_1 + g_map_2) / ( g_map_3 + g_map_4) /  g_tmp_2 +
  plot_layout(heights = c(1, 3, 3, 1), guides = 'collect') +
  plot_annotation(title = paste0('Temporal ', tolower(metric), ' represented in space')) # ', metrics$metric_longname, '


# plotting details, in case not inherited... 
if(exists('fig.path') != T){ fig.path <- 'paper/figures/'}
if(exists('fig.fmt') != T){ fig.fmt <- 'png'}

dir.create(path = fig.path, recursive = T, showWarnings = F)

fig.name <- 'bias_summaries'

ggsave(filename = paste0(fig.name, '.', fig.fmt), plot = g_all,
       path = fig.path, width = 14, height = 9)


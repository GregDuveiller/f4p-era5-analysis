


load('data/figures_for_paper/hwAll_gislayers.RData')   # <---- hw_polygons



# input files are the 'df_comb___var.RData' produced by Greg's script
input_dir <- 'data/inter_data/df_comb_obs_vs_sim/'

# list of variables to run over
var_list <- c('LAI', 'LST',  'E', 'albedo_wsa_vis') # albedo_bsa_nir albedo_bsa_vis albedo_wsa_nir 'SM',


output_path <- 'data/figures_for_paper/'
print(paste0('output_path is : ', output_path ))
if(!dir.exists(output_path)) {dir.create(paste0(output_path), recursive = T)}


df_hw <- data.frame(row.names = c("hw2003", "hw2010", "hw2018"),
                    year = c(2003, 2010, 2018),
                    month = c(8, 7, 7))


i <- 1

# for (i in 1:length(var_list)){
variable_i <- var_list[i] ; print(variable_i)
input_file <- paste0('df_comb___',variable_i,'.RData')
load(paste0(input_dir, input_file))

hw_polygons



ts <- df_comb %>%
  filter(x == -125.125, y == 72.375) %>%  #  <---- need to replace this... 
  mutate(time = as.Date(paste(year, month, '15', sep = '-'), '%Y-%M-%d'))


ts_all <- ts %>% 
  group_by(month) %>%
  summarise(clim_obs = mean(obs),
            clim_sim = mean(sim)) %>%
  left_join(ts %>% filter(year %in% c(2003, 2010, 2018)), by = "month") %>%
  rename(year_of_hw = year) %>%
  left_join(data.frame(year_of_hw = c(2003, 2010, 2018), 
                       month_of_hw = c(7, 8, 8)), by = "year_of_hw")





ggplot(ts_all, aes(x = month)) +
  geom_vline(aes(xintercept = month_of_hw), size = 2) +
  geom_line(aes(y = clim_obs)) +
  geom_line(aes(y = clim_sim), linetype = 'dashed') +
  geom_line(aes(y = obs, colour = factor(year_of_hw)), show.legend = F) +
  facet_grid(year_of_hw~.) +
  scale_x_continuous(expand = c(0,0))



#}

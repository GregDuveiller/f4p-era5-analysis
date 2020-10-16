### Script to make dataframe at 0.25 dd of climate zones


require(raster)
require(dplyr)

data_path <- '/Users/greg/work/data/external_datasets/climate_zones/Map_KG-Global/'

file.symlink(to = 'data/climate_zones', from = data_path)
r_HR <- raster('data/climate_zones/KG_1986-2010.grd')

# define function to calc the mode
getmode <- function(v, na.rm) {
  if(na.rm == T){ v <- v[!is.na(v)]}
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# aggregate using the mode (note, we could have used 'resmapling' function too)
r_LR <- aggregate(r_HR, fact = 3, fun = getmode, na.rm = T)


# Set identifier
cz_IDs <- seq(1,32)
# Legend is always drawn in alphabetic order
cz_lbls <- c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk', 'BWh', 'BWk', 'Cfa', 'Cfb','Cfc', 'Csa', 'Csb', 'Csc', 'Cwa','Cwb', 'Cwc', 'Dfa', 'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc', 'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET', 'Ocean')
# Color palette for climate classification
cz_cols <- c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", "#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64", "#007800", "#005000", "#003200", "#96FF00", "#00D700", "#00AA00", "#BEBE00", "#8C8C00", "#5A5A00", "#550055", "#820082", "#C800C8", "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6", "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF", "#6496FF", "#64FFFF", "#F5FFFF")
# Pack it all in legend dataframe
df_lgd <- data.frame(cz_ID = cz_IDs, cz_name = factor(cz_lbls), cz_colours = cz_cols)


# get the final data.frame for the climate zones
df_cz <- as.data.frame(r_LR, xy = T, long = T) %>% 
  rename(cz_ID = value) %>% select(-layer) %>%
  left_join(df_lgd, by = "cz_ID")

dir.create(path = 'results/ancillary_info', recursive = T, showWarnings = F)
save('df_cz', 'df_lgd', file = 'results/ancillary_info/df_KG_climatezones.RData')

# make check-plot 
require(ggplot2)

names(cz_cols) <- cz_lbls
gmap <- ggplot(df_cz) + 
  geom_tile(aes(x = x, y = y, fill = cz_name)) + 
  scale_fill_manual('Koppen-Geiger climate zones', values = cz_cols) + 
  scale_y_continuous('Latitude', expand = c(0, 0)) + 
  scale_x_continuous('Longitude', expand = c(0, 0)) + 
  theme(legend.position = 'bottom') + 
  guides(fill = guide_legend(title.position = 'top', nrow = 4))

ggsave(filename = 'results/ancillary_info/KG_climatezones_map.png', 
       plot = gmap, width = 9, height = 6)
       
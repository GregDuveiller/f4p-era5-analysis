#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prep_gislayers_for_maps.R ####
# ---------------------------------------------------------------------------- #
# Purpose: prepare selected gis (vector) layers for maps over Europe
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #

#### Initialization ####

require(sf)
require(purrr)
require(dplyr)

#### Load map shapefiles ####

# download here: https://www.naturalearthdata.com/downloads/50m-physical-vectors/

coast_shapefile <- "data/input_data/vector_layers/ne_50m_coastline.shp"
coast <- sf::st_read(coast_shapefile, quiet = TRUE)

ocean_shapefile <- "data/input_data/vector_layers/ne_50m_ocean.shp"
ocean <- sf::st_read(ocean_shapefile, quiet = TRUE)


#### Crop the target area (Europe) ####

# define a bounding box
xmin <- -12; xmax <- 58; ymin <- 26; ymax <- 71
bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)

# specify that spherical geometry should NOT be used
sf_use_s2(FALSE)

# crop vectors with the bounding boxes
coast_europe <- sf::st_crop(coast, y = bbox) 
ocean_europe <- sf::st_crop(ocean, y = bbox) 


#### Define the Heatwave zones ####

df_hw2003 <- data.frame(lon = c(-5, 10), lat = c(43, 52))
df_hw2010 <- data.frame(lon = c(35, 57), lat = c(48, 58))
df_hw2018 <- data.frame(lon = c(-2, 16), lat = c(48, 60))

# arrange all in a list of lists 
# (there might be a better way to do this)
coords <- list(
  list(cbind(
    df_hw2003$lon[c(1,2,2,1,1)], 
    df_hw2003$lat[c(1,1,2,2,1)])),
  list(cbind(
    df_hw2010$lon[c(1,2,2,1,1)], 
    df_hw2010$lat[c(1,1,2,2,1)])),
  list(cbind(
    df_hw2018$lon[c(1,2,2,1,1)], 
    df_hw2018$lat[c(1,1,2,2,1)])))
  
polys <- purrr::map(coords, st_polygon) %>% st_as_sfc(crs = st_crs(coast))
labels <- data.frame(hw = c("hw2003", "hw2010", "hw2018"))

hw_polygons <- st_sf(cbind(labels, polys))


#### Export the data ####

save('ocean_europe', 'coast_europe', 'hw_polygons',
     file = 'data/figures_for_paper/hwAll_gislayers.RData')







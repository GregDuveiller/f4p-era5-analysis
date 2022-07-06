#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prepare_gislayers_for_maps.R ####
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

coast_shapefile <- "data/input_data/world_vectors/ne_50m_coastline.shp"
coast <- sf::st_read(coast_shapefile, quiet = TRUE)

ocean_shapefile <- "data/input_data/world_vectors/ne_50m_ocean.shp"
ocean <- sf::st_read(ocean_shapefile, quiet = TRUE)

land_shapefile <- "data/input_data/world_vectors/ne_50m_land.shp"
land <- sf::st_read(land_shapefile, quiet = TRUE)

#### Crop the target area (Europe) ####

# define a bounding box
xmin <- -12; xmax <- 58; ymin <- 26; ymax <- 71
bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)

# specify that spherical geometry should NOT be used
sf_use_s2(FALSE)

# crop vectors with the bounding boxes
coast_europe <- sf::st_crop(coast, y = bbox) 
ocean_europe <- sf::st_crop(ocean, y = bbox) 
land_europe <- sf::st_crop(land, y = bbox) 


#### Define the Heatwave zones ####

df_hw2003 <- data.frame(lon = c(-5, 10), lat = c(43, 52))
df_hw2010 <- data.frame(lon = c(35, 57), lat = c(48, 58))
df_hw2018 <- data.frame(lon = c(-4, 16), lat = c(48, 63))

df_hw2003 <- data.frame(lon = c( 0,  8), lat = c(44, 50))
df_hw2010 <- data.frame(lon = c(39, 57), lat = c(50, 58))
df_hw2018a <- data.frame(lon = c( 2, 16), lat = c(49, 57))   # <---- Germany
df_hw2018b <- data.frame(lon = c(23, 33), lat = c(61, 69))   # <---- Finland
# df_hw2018 <- data.frame(lon = c( 8, 17), lat = c(56, 64))   # <---- South Sweden

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
    df_hw2018a$lon[c(1,2,2,1,1)], 
    df_hw2018a$lat[c(1,1,2,2,1)])),
  list(cbind(
    df_hw2018b$lon[c(1,2,2,1,1)], 
    df_hw2018b$lat[c(1,1,2,2,1)])))
  
polys <- purrr::map(coords, st_polygon) %>% st_as_sfc(crs = st_crs(coast))
labels <- data.frame(hwname = c("HW03", "HW10", "HW18a", "HW18b"),
                     hwyear = c(2003, 2010, 2018, 2018))                
                    # hwyear = c('HW03', 'HW10', 'HW18', 'HW18'))

hw_polygons <- st_sf(cbind(labels, polys))


#### Export the data ####

save('ocean_europe', 'coast_europe', 'land_europe', 'hw_polygons', 'land', 'ocean','coast',
     file = 'data/final_data/figures_for_paper/hw_gislayers.RData')







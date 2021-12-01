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

#### Load map shapefiles ####

# download here: https://www.naturalearthdata.com/downloads/50m-physical-vectors/
  
coast_shapefile <- "data/input_data/vector_layers/ne_50m_coastline.shp"
coast <- sf::st_read(coast_shapefile, quiet = TRUE)

ocean_shapefile <- "data/input_data/vector_layers/ne_50m_ocean.shp"
ocean <- sf::st_read(ocean_shapefile, quiet = TRUE)


#### Arrange the data ####

# define a bounding box
xmin <- -12; xmax <- 58; ymin <- 26; ymax <- 71
bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)

# specify that spherical geometry should NOT be used
sf_use_s2(FALSE)

# crop vectors with the bounding boxes
coast_europe <- sf::st_crop(coast, y = bbox) 
ocean_europe <- sf::st_crop(ocean, y = bbox) 

#### Export the data ####

save('ocean_europe', 'coast_europe', file = 'data/figures_for_paper/hwAll_gislayers.RData')







# ptlist_xy_plat <- st_sfc(st_multipoint(x = as.matrix(df_all[,c('x','y')]), dim = 'XY'), crs = 4326)
# ptlist_xy_laea <- st_transform(ptlist_xy_plat, crs = laes_prj)
# 
# new_coord <- st_coordinates(ptlist_xy_laea) 
# df_all$x_laea <- new_coord[,'X']
# df_all$y_laea <- new_coord[,'Y']

# #convert to lines
# layer <- ogrListLayers(coast_shapefile)
# ogrInfo(coast_shapefile, layer = layer)
# coast_lines <- readOGR(coast_shapefile, layer = layer)# read the shape file
# layer <- ogrListLayers(bb_shapefile)
# ogrInfo(bb_shapefile, layer=layer)
# bb_poly <- readOGR(bb_shapefile, layer=layer)
# bb_lines <- as(bb_poly, "SpatialLines")
# layer <- ogrListLayers(grat30_shapefile)
# ogrInfo(grat30_shapefile, layer=layer)
# grat30_lines <- readOGR(grat30_shapefile, layer=layer)
# # get CRS
# unproj_proj4string <- proj4string(coast_lines)
# 
# 
# 
# #### Export the data ####
# 
# save('coast_lines', 'grat30_lines', 'bb_lines', 'unproj_proj4string', file = 'data/figures_for_paper/hwAll_gislayers.RData')


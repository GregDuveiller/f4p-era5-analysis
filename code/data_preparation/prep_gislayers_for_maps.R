#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prep_gislayers_for_maps.R ####
# ---------------------------------------------------------------------------- #
# Purpose: prepare selected gis (vector) layers for maps over Europe
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #

#### Initialization ####

require(rgdal)
require(sf)

#### Load map shapefiles ####

# download here: https://www.naturalearthdata.com/downloads/50m-physical-vectors/
  
coast_shapefile <- "data/input_data/vector_layers/ne_50m_coastline.shp"
coast <- sf::st_read(coast_shapefile, quiet = TRUE)


#### Arrange the data ####

xmin <- -12; xmax <- 58; ymin <- 26; ymax <- 71
bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)

coastlines_europe <- sf::st_crop(coast, y = bbox) 
plot(coastlines_europe)


#### Export the data ####

save('coastlines_europe', file = 'data/figures_for_paper/hwAll_gislayers.RData')






# Trying to get a cropped ocean vector 
land_shapefile <- "data/input_data/vector_layers/ne_50m_land.shp"
land <- sf::st_read(land_shapefile, quiet = TRUE)

ocean_shapefile <- "data/input_data/vector_layers/ne_50m_ocean.shp"
ocean <- sf::st_read(ocean_shapefile, quiet = TRUE)

xmin <- -12; xmax <- 58; ymin <- 26; ymax <- 71
bbox <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)

ocean_europe <- sf::st_crop(ocean, y = bbox) 
plot(ocean)
plot(ocean_europe)

plot(st_crop(ocean, st_bbox(bbox)))


require(raster)
ocean_europe <- sf::st_intersection(ocean, st_set_crs(
  st_as_sf(as(raster::extent(-12, 58, 36, 71), "SpatialPolygons")), st_crs(ocean))) 
plot(ocean_europe)


# bbox <- matrix(c(xmin,ymin,xmax,ymin,xmax,ymax,xmin,ymax,xmin,ymin), ncol = 2, byrow = TRUE)
# bbox_sf <- st_sfc(st_polygon(x = list(bbox))) %>% st_set_crs(st_crs(ocean))
 





world <- sf::st_read(coast_shapefile, quiet = TRUE)
world <- sf::st_read(land_shapefile, quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world, st_set_crs(
  st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world))) %>%
  st_transform(laes_prj)




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


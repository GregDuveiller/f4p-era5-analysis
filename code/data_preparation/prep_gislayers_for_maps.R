#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### prep_gislayers_for_maps.R ####
# ---------------------------------------------------------------------------- #
# Purpose: prepare selected gis (vector) layers for maps over Europe
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #

#### Initialization ####

require(dplyr)
require(tidyr)


#### Load map shapefiles ####

# download here: https://www.naturalearthdata.com/downloads/50m-physical-vectors/
  
# local
coast_shapefile <- "data/input_data/vector_layers/ne_50m_coastline.shp"
bb_shapefile <- "data/input_data/vector_layers/ne_50m_wgs84_bounding_box.shp"
grat30_shapefile <- "data/input_data/vector_layers/ne_50m_graticules_30.shp"

#convert to lines
layer <- ogrListLayers(coast_shapefile)
ogrInfo(coast_shapefile, layer = layer)
coast_lines <- readOGR(coast_shapefile, layer = layer)# read the shape file
layer <- ogrListLayers(bb_shapefile)
ogrInfo(bb_shapefile, layer=layer)
bb_poly <- readOGR(bb_shapefile, layer=layer)
bb_lines <- as(bb_poly, "SpatialLines")
layer <- ogrListLayers(grat30_shapefile)
ogrInfo(grat30_shapefile, layer=layer)
grat30_lines <- readOGR(grat30_shapefile, layer=layer)
# get CRS
unproj_proj4string <- proj4string(coast_lines)



#### Export the data ####

save('coast_lines', 'grat30_lines', 'bb_lines', file = 'data/figures_for_paper/hwAll_gislayers.RData')


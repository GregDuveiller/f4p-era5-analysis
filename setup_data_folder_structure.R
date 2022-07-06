#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### setup_data_folder_structure.R ####
# ---------------------------------------------------------------------------- #
# Purpose: setup data structure and symlinks in the project folder
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #


#### Location of necessary input files ####
# [this will be specific to a given local set-up]
# [this could be automatized, or separated into another user-specific file]

# NATURAL EARTH VECTOR FILES #
dpath_vectors <- '/Users/gduveiller/data/external_datasets/natural_earth_data/'

# Preprocessed R dataframes with ERA5 data
dpath_r_dataframes <- '/Users/gduveiller/data/internal_datasets/f4p-era5-analysis/r_data_frames/'

# Koppen-Geiger climate zones
dpath_climate_zones <- '/Users/gduveiller/data/external_datasets/climate_zones/Map_KG-Global/'



#### Input data directory setup ####

# folder for input data, which can be downloaded from elsewhere
dir.create('data/input_data', recursive = T)

# setup symlink for shapefile vector data 
file.symlink(to = 'data/input_data/world_vectors', from = dpath_vectors)
# setup symlink to pre-processed R dataframes of ERA5 and satellite data 
file.symlink(to = 'data/input_data/r_data_frames', from = dpath_r_dataframes)
# setup link to KG climate zones...
file.symlink(to = 'data/input_data/climate_zones', from = dpath_climate_zones)


#### Intermediate data directory setup ####

# folder for "intermediate" data, which can be regenerated from code if needed
dir.create('data/inter_data', recursive = T)

# prepared ancillary info, in this case 2 types of climate zones
dir.create('data/inter_data/ancillary_info', recursive = T)
# this one will contain R dataframes where obs and sims (ERA5) are combined
dir.create('data/inter_data/df_comb_obs_vs_sim', recursive = T)
# this one will contain R dataframeshigher order summaries of the previous ones
dir.create('data/inter_data/df_single_var_summaries', recursive = T)


#### Final data directory setup ####

# folder for final data, which is worth saving in a permanent curated repo...
dir.create('data/final_data', recursive = T)

# here we put the essential data needed to reproduce the figures in the final paper
dir.create('data/final_data/figures_for_paper', recursive = T)




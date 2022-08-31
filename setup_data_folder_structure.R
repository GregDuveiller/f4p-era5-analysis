#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### setup_data_folder_structure.R ####
# ---------------------------------------------------------------------------- #
# Purpose: setup data structure/symlinks in the project folder
# Project: f4p-era5-analysis
# Authors: G.Duveiller & M.Pickering
# ---------------------------------------------------------------------------- #


#### Input data directory setup ####
# setup input directories and move input data to these directories

# folder for input data, which can be downloaded from elsewhere
dir.create('data/input_data', recursive = T)
# create directories and move input data (see readme) to these directories
dir.create('data/input_data/world_vectors', recursive = T)
dir.create('data/input_data/r_data_frames', recursive = T)
dir.create('data/input_data/climate_zones', recursive = T)

# ## alternative - create links to input files
# # PATH TO DIRECTORY CONTAINING NATURAL EARTH VECTOR FILES (WORLD VECTORS) #
# dpath_vectors <- ''
# # PATH TO DIRECTORY CONTAINING ZENODO ERA5 AND INDEPENDENT DATASETS #
# dpath_r_dataframes <- ''
# # PATH TO DIRECTORY CONTAINING KOPPEN-GEIGER CLIMATE ZONES #
# dpath_climate_zones <- ''

# # setup symlink for shapefile vector data
# file.symlink(to = 'data/input_data/world_vectors', from = dpath_vectors)
# # setup symlink to pre-processed R dataframes of ERA5 and satellite data
# file.symlink(to = 'data/input_data/r_data_frames', from = dpath_r_dataframes)
# # setup link to KG climate zones...
# file.symlink(to = 'data/input_data/climate_zones', from = dpath_climate_zones)


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




#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### setup_data_folder_structure.R ####
# ---------------------------------------------------------------------------- #
# Purpose: setup data structure and symlinks in the project folder
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #



#### setup directory structure for the data ####

# folder for input data, which can be downloaded from elsewhere
dir.create('data/input_data', recursive = T)
# folder for "intermeadiate" data, which can be regenerated from code if needed
dir.create('data/inter_data', recursive = T)
# folder for final data, which is worth saving in a permanent curated repo...
dir.create('data/final_data', recursive = T)


#### setup symlinks for input data ####
data_path <- '/Users/gduveiller/work/data/external_datasets/natural_earth_data/'
file.symlink(to = 'data/input_data/vector_layers', from = data_path)


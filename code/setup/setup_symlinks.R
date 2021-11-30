#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------- #
# #### setup_symlinks.R ####
# ---------------------------------------------------------------------------- #
# Purpose: setup symlinks to get data in the necessary data in  project folder
# Project: f4p-era5-analysis
# Authors: G.Duveiller
# ---------------------------------------------------------------------------- #


data_path <- '/Users/gduveiller/work/data/external_datasets/natural_earth_data/'
file.symlink(to = 'data/input_data/vector_layers', from = data_path)

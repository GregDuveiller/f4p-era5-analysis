# set-up symlinks to data

# clim data
data_path <- '/Users/greg/work/data/external_datasets/climate_zones/Map_KG-Global/'
file.symlink(to = 'data/input_data/climate_zones', from = data_path)

# vctr data
data_path <- '/Volumes/home/work/data/external_datasets/WorldVector/'
file.symlink(to = 'data/input_data/world_vectors', from = data_path)




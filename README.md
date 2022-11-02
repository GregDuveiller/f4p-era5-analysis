# f4p-era5-analysis

Repository containing the code related to analysing the fitness-for-purpose (F4P) of the ERA5 climate reanalysis to study land-climate interactions. 

All necessary scripts are stored in the `code` folder and relate to data that is structured within a `data` folder. Within this `data` folder there should be one named `input_data` containing source data available from other sources.

The code also uses an ancillary repository to calculate metrics of agreement. This repository is available here:
`https://github.com/GregDuveiller/agreement-index`


## Input data
- download the following datasets 
- `climate_zones` : available at `http://koeppen-geiger.vu-wien.ac.at/present.htm` 
- `climate_space` : `df_climspace_t2xsm.RData` available Zenodo repository with DOI `10.5281/zenodo.6976942`
- `r_data_frames` : all other `.RData` available Zenodo repository with DOI `10.5281/zenodo.6976942`
- `world_vectors` : `ne_50m_land.shp`, `ne_50m_ocean.shp` and `ne_50m_coastline.shp` available at https://www.naturalearthdata.com/downloads/50m-physical-vectors/

## Setup folder structure and input data
- Run `setup_data_folder_structure.R` 
- Add the input data to the corresponding data/input_data/ directories or create symlinks to the directories containing the data

## Combine ERA5 and independent datasets
- For each variable X of interest run `harvest__CombDFs__X.R` files in: `code/combine_sim_with_obs/`

## Prepare climate zones for use in figures
- Run: `code/harvesting_data_for_final_figures/prepare_climzones.R`

## To produce figures of the specific heatwaves
- Create hw_maps.RData containing the bias shifts of different variables during the heatwaves considered.
  Run: `code/harvesting_data_for_final_figures/prepare_data_for_heatwave_maps.R`
- Create hw_gislayers.RData containing information regarding the mapping areas of the heatwaves.
  Run: `code/harvesting_data_for_final_figures/prepare_gislayers_for_maps.R`
- Produce Figure 1: Delimitations of the areas considered for the various heatwave events considered in this study.
  Run : `code/making_figures_for_paper/plot_heatwave_locations.R`
- Create hw_stats.RData for the statistics on the heatwaves
  Run : `code/harvesting_data_for_final_figures/prepare_heatwave_summary_stats.R`
- Produce Figure 8: Maps of bias shifts for different variables when comparing ERA5L to what is considered here as observations.
  Run : `code/making_figures_for_paper/plot_heatwaves.R`

## To produce the figures of bias
- Create data_for_corr_summary_maps.RData and data_for_bias_summary_maps.RData containing global bias and correlation between LAI and LST
  Run: `prepare_data_for_bias_and_cor_summaries.R`
- Produce Figure 2: Overview of the mean biases in LAI and LST between ERA5L and observations (ERA - obs) over the climatological period from 2003 to 2018.
  Run: `code/making_figures_for_paper/plot_bias_summaries.R`
- Produce Figure 6: Inter-annual correlation between the biases in LAI and the biases in LST based on all months of July and January over the period 2003-2018.
  Run: `code/making_figures_for_paper/plot_interannual_correlation.R`


## To produce the binned climate and hysteresis figures
- Create df_climspace_bin.RData, df_LAI_per_clim_bin.RData and df_LST_per_clim_bin.RData containing the data in bins of climate space
  Run: `code/harvesting_data_for_final_figures/prepare_data_per_clim_bin.R`
- Create hysteresis plotting dataframes
  Run: `code/harvesting_data_for_final_figures/prepare_hysteresis_per_bin.R`
  `code/harvesting_data_for_final_figures/prepare_hysteresis_dimensions.R`
- Produce Figure 3: Diagnostic plots illustrating the hysteretic behaviour between the biases in LAI and the biases in LST for two different climate zones
  Run: `code/making_figures_for_paper/plot_hyst_demo.R`
- Produce Figure 4: Hysteretic behaviour between the biases in LAI and the biases in LST for a range of climate zones
  Run: `code/making_figures_for_paper/plot_hyst_climspace.R`
#!/usr/bin/env python
import cdsapi

c = cdsapi.Client()

c.retrieve(
    'reanalysis-era5-single-levels-monthly-means',
    {
        'format': 'netcdf',
        'variable': [
            'high_vegetation_cover', 'leaf_area_index_high_vegetation', 'leaf_area_index_low_vegetation',
            'low_vegetation_cover', 'type_of_high_vegetation', 'type_of_low_vegetation'
        ],
        'product_type': 'monthly_averaged_reanalysis',
        'year': '2010',
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12'
        ],
        'time': '00:00'
    },
    '/ESS_EarthObs/REANALYSIS/ERA5/ERA5_LAI/LAI_025_fixed_monthlyAvgd/ERA5_LAI_fixed_monthlyAvgd.nc')

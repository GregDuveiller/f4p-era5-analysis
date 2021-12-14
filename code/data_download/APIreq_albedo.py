import cdsapi

c = cdsapi.Client()

c.retrieve(
    'reanalysis-era5-single-levels-monthly-means',
    {
        'format': 'netcdf',
        'product_type': 'monthly_averaged_reanalysis',
        'variable': [
            'near_ir_albedo_for_diffuse_radiation', 'near_ir_albedo_for_direct_radiation', 'uv_visible_albedo_for_diffuse_radiation',
            'uv_visible_albedo_for_direct_radiation',
        ],
        'year': [
            '1998', '1999', '2000',
            '2001', '2002', '2003',
            '2004', '2005', '2006',
            '2007', '2008', '2009',
            '2010', '2011', '2012',
            '2013', '2014', '2015',
            '2016', '2017', '2018',
            '2019', '2020',
        ],
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
        ],
        'time': '00:00',
    },
    '/cid/data/cid-bulk22/Shared/projectData/COPERNICUSII/data/CDS_data/ERA5sl_albedo/data/raw_download/ERA5sl_albedo.nc')

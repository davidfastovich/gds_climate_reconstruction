from scipy import interpolate
import pandas as pd
import xarray as xr
import numpy as np
import pyinterp
import pyinterp.backends.xarray

###############################
# READ IN MODELS AND TAKE MEANS
###############################

# Read in CRU data
tmin = xr.open_dataset('cru/cru_ts4.05.1901.2020.tmn.dat.nc')
tmax = xr.open_dataset('cru/cru_ts4.05.1901.2020.tmx.dat.nc')
tave = xr.open_dataset('cru/cru_ts4.05.1901.2020.tmp.dat.nc')
prec = xr.open_dataset('cru/cru_ts4.05.1901.2020.pre.dat.nc')

# Before taking annual averages precipitation needs to be summed across the 
# year
prec = prec.pre.groupby('time.year').sum('time')

# Take average from 1961 to 1990 as in Whitmore et al., 2005
tmin = tmin.tmn.sel(time=slice('1961-01-16', '1990-12-16')).mean('time')
tmax = tmax.tmx.sel(time=slice('1961-01-16', '1990-12-16')).mean('time')
tave = tave.tmp.sel(time=slice('1961-01-16', '1990-12-16')).mean('time')
prec = prec.sel(year=slice(1961, 1990)).mean('year')

##########################################
# READ IN COORDINATES FROM SURFACE SAMPLES
##########################################

coords = pd.read_excel('pollen_counts/GDS_RR_locations.xlsx')

# Random column that needs to be deleted - likely copy/paste issue
coords = coords.drop('Unnamed: 3', 1)

#######################
# PERFORM INTERPOLATION
#######################

# TMIN
interpolator = pyinterp.backends.xarray.Grid2D(tmin)
coords['tmin'] = interpolator.bivariate(coords=dict(lon=coords['LONDD'].to_list(), lat=coords['LATDD'].to_list()))

# TMAX
interpolator = pyinterp.backends.xarray.Grid2D(tmax)
coords['tmax'] = interpolator.bivariate(coords=dict(lon=coords['LONDD'].to_list(), lat=coords['LATDD'].to_list()))

# TAVE
interpolator = pyinterp.backends.xarray.Grid2D(tave)
coords['tave'] = interpolator.bivariate(coords=dict(lon=coords['LONDD'].to_list(), lat=coords['LATDD'].to_list()))

# PREC
interpolator = pyinterp.backends.xarray.Grid2D(prec)
coords['annp'] = interpolator.bivariate(coords=dict(lon=coords['LONDD'].to_list(), lat=coords['LATDD'].to_list()))

#######################################
# COMBINE WITH WHITMORE CLIMATE DATASET
#######################################

# Read in Whitmore dataset to match formatting
whitmore_climate = pd.read_excel('pollen_counts/whitmoreetal2005_v1-8.xlsx', sheet_name='CLIMATE+BIOCLIMATE')

# Only need the new samples but in the Whitmore format
whitmore_pollen_expanded = pd.read_csv('results/whitmore_surface_expanded.csv')
new_surface = whitmore_pollen_expanded[whitmore_pollen_expanded['ID2'] > 4837]

# Splitting new samples by location - this became necessary to track Excel input issues
new_surface_rr = new_surface[new_surface['ID2'] > 4868]
new_surface_dismal = new_surface[new_surface['ID2'] < 4869]

# Modify `coords` to match whitmore_cliamte and bind the two
coords = coords.drop([143, 145, 146, 147]) # Dont have surface counts
coords['SITENAME_SPLIT'] = coords['SITENAME'].str.split('_').apply(lambda x: x[1])

# Splitting coords by site to make it easier to manipulate strings
coords_rr = coords[coords['SITENAME'].str.contains('RR')]
coords_dismal = coords[coords['SITENAME'].str.contains('GDS')]

# Add additional column with text that allows for exact text as in Whitmore surface expanded
coords_rr['SITENAME'] = 'Roanoke Surface Treetag ' + coords_rr['SITENAME_SPLIT']
coords_dismal['SITENAME'] = 'Dismal Swamp Surface ' + coords_dismal['SITENAME_SPLIT']

# Since we have a key column now - join coords with their respective counts
# Only 1 site without coords - RR285
counted_with_coords_dismal = new_surface_dismal.join(coords_dismal.set_index('SITENAME'), on='SITENAME', how='left', lsuffix='pollen')
counted_with_coords_rr = new_surface_rr.join(coords_rr.set_index('SITENAME'), on='SITENAME', how='left', lsuffix='pollen')
counted_with_coords = pd.concat([counted_with_coords_dismal, counted_with_coords_rr])

# Concat new data onto whitmore climate and save as csv - SAME NUMBER OF ROWS
# AS IN WHITMORE POLLEN!
whitmore_climate_expanded = pd.concat([whitmore_climate, counted_with_coords[['ID2', 'SITENAME', 'LONDD', 'LATDD', 'tmin', 'tmax', 'tave', 'annp']]])
whitmore_climate_expanded.to_csv('results/whitmore_climate_expanded.csv')
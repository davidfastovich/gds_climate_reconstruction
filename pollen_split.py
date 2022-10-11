import pandas as pd
import geopandas as gpd

# Import shapefiles
pic_east = gpd.read_file('splits/piceEAST.shp')
pin_ne = gpd.read_file('splits/pinuNE.shp')
pin_se = gpd.read_file('splits/pinuSE.shp')

# Importing modern pollen dataset
whitmore_surface_expanded = pd.read_csv('results/whitmore_surface_expanded.csv')

# Convert whitemore_surface_expanded to gdf
whitmore_surface_expanded_gdf = gpd.GeoDataFrame(whitmore_surface_expanded, geometry=gpd.points_from_xy(whitmore_surface_expanded.LONDD, whitmore_surface_expanded.LATDD))

# Perform intersection
whitmore_surface_expanded_gdf_east = whitmore_surface_expanded_gdf.overlay(pic_east, how="intersection")
whitmore_surface_expanded_gdf_ne = whitmore_surface_expanded_gdf.overlay(pin_ne, how="intersection")
whitmore_surface_expanded_gdf_se = whitmore_surface_expanded_gdf.overlay(pin_se, how="intersection")

# Convert back to pandas data frame
whitmore_surface_expanded_east = pd.DataFrame(whitmore_surface_expanded_gdf_east.drop(columns=['geometry', 'AREA', 'PERIMETER', 'ABIELASI_', 'ABIELASI_I', 'CODE']))
whitmore_surface_expanded_ne = pd.DataFrame(whitmore_surface_expanded_gdf_ne.drop(columns=['geometry', 'AREA', 'PERIMETER', 'ABIELASI_', 'ABIELASI_I', 'CODE']))
whitmore_surface_expanded_se = pd.DataFrame(whitmore_surface_expanded_gdf_se.drop(columns=['geometry', 'AREA', 'PERIMETER', 'ABIELASI_', 'ABIELASI_I', 'CODE']))

# Save as CSV
whitmore_surface_expanded_east.to_csv('results/whitmore_surface_expanded_east.csv', index_label=False)
whitmore_surface_expanded_ne.to_csv('results/whitmore_surface_expanded_ne.csv', index_label=False)
whitmore_surface_expanded_se.to_csv('results/whitmore_surface_expanded_se.csv', index_label=False)

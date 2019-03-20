# !/usr/bin/env ipython3
""" Filtering data faster than R can """

__appname__ = 'data_filter.py'
__author__ = 'Jake Curry (j.curry18@imperial.ac.uk)'
__version__ = '0.0.1'
__license__ = "License for this code/program"


# Imports
import pandas as pd
import os
from osgeo import ogr

# getting WD
os.getcwd()
# read in the R processed spatial data as a pandas dataframe
NHM_AMPH = pd.read_csv('../Data/SA_AMPH_NHM_DF.csv')

IUCN_Shape = r"../Data/AMPHIBIANS.shp"
driver = ogr.GetDriverByName('ESRI Shapefile')
dataSource = driver.Open(IUCN_Shape, 0)




IUCN = pd.read_csv('../Data/IUCN_Spatial.csv', error_bad_lines=False)

NHM_AMPH.head()
# filtering data so that only species on both dataframes are present 


# saving the new files to csv files for analysis in R

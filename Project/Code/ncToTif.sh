#!/bin/bash
## ncToTif.sh
## Authour: Jake Curry
## Email: jake.curry96@gmail.com
## Date: 18/08/2020
## Code for processing nc file to tif 
## This can and should really be generalised, but I just ran this by hand for each of the options
## The only one which we really needed was the lccs_class

gdalwarp -of Gtiff -co COMPRESS=LZW -co TILED=YES -ot Byte -te -180.0000000 -90.0000000 180.0000000 90.0000000 -tr 0.002777777777778 0.002777777777778\
-t_srs EPSG:4326 NETCDF:C3S-LC-L4-LCCS-Map-300m-P1Y-2018-v2.1.1.nc:lccs_class lccs_class.tif

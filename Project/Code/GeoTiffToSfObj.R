#### GeoTiffToSfObj.R
## creation date: 17/08/2020
## authour: J Curry
## email: jake.curry96@gmail.com 

####
# Reworking/ adding code to the project repo for addressing reviewer comments
# Added code to convert newly clipped GeoTiff Area Of Habitation (AOH)
# files for each species to sf objects (polygons). These are to be run through
# the previous submissions scripts instead of the range maps.

# See https://r-spatial.github.io/stars/articles/stars5.html for help on how to.
####

#### installs COMMENT OUT AFTER FRIST RUN ######
#install.packages("stars") ## for reading in .tif files, as sf does not natively
                          ## support that.
###############################################

######## Notes ###############################

## Don't use system.file, it does something totally screwy and won't actually 
## capture the file path. Just pass the file path to an R object as a string.

#############################################

#imports
library(rgdal)
library(sf)
library(ggplot2)
library(stars)
library(stringr)
###### Sandbox #######
#GDALinfo("../Data/gigantea_aoh.tif") ## Check that the file exists and is a GeoTif

#strFileName = "../Data/gigantea_aoh.tif" 
# = read_stars(strFileName) ## reads the geotif in as a raster

#sfTiff = st_as_sf(tif) ## This makes it an sf object (a data.frame to be precise)
                       ## which contains several polygons (which are correct 
                       ## for the data). So its as simple as that really.

#sfTiff ## checking that it is the object I assume it to be. 

## plot(st_geometry(sfTiff)) ## Basic plotting to see if it matches tif.
                             ## Don't plot it as it 

##### Functions ################

#### Main loop fun #############
GeoTiffToPolygon <- function(directory) 
{
  ##get the full path as a string
  fileList = list.files(path = directory, pattern = "*.tif", full.names = TRUE)
  for (file in fileList) 
  {
    ##print(file)
    specificName = str_extract(file, regex("\\/{1}\\w+\\."))
    specificName = gsub("/", "", specificName)
    specificName = gsub("\\.", "\\.shp", specificName)
    print(specificName)
    ##get the species name currently being processed for dynamic file naming
    ##specificFileName = file 
    ##pass to read_stars
    rasterFile = read_stars(file)
    ##pass to st_as_sf
    polygonFile = st_as_sf(rasterFile)
    ##print(class(polygonFile))
    ##save the new object
    st_write(polygonFile, paste0("../Data", "/", specificName))
  }
}

##### Calls to fun/MAIN #######
GeoTiffToPolygon("../Data/") ##pass directory as string with trailing slash

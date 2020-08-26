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
devtools::install_github("JCur96/sfe", force = TRUE)
library(sfe)
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

# library(raster)
# str_name <- "../Data/temminckii_aoh2.tif"
# imported_raster = raster(str_name)
# st_tem = st_as_sf(imported_raster)
# imported_raster
# polyImp = rasterToPolygons(imported_raster)
# 
# 
# library(rgdal)
###allSpPoly = readOGR(dsn = "F:\\tmpShp\\All_spp_shp.shp")
allSpPoly = st_read(dsn = "E:\\tmpShp\\allMinusTemn.shp")

crassi = st_read(dsn ="E:\\tmpShp\\shpFiles\\crassicaudata_aoh.shp")
## rename (to binomial) column 1 and populate with species name conforming to 
## expected input
### essentially this 
names(crassi)[1] <- 'binomial'
crassi$binomial <- 'crassicaudata' ##or something spp name like that 
crasssi <- resolveIUCNGeom(crassi)
crassi <- 
  crassi %>%
  st_as_sf() %>%
  st_transform(4326)

## for file in dir
### read in the file, get the name
#### calculate overlaps against the NHM data FOR THAT SPP based on name
##### save that data then start on the next file (because memory footprint)
RunAOHAnalysis <- function(NHMDataDir, AOHDataDir) 
  {
  # messy but quick process for testing, run process NHM data from MRes Proj
  NHM_Pangolins <- read.csv(NHMDataDir, header=T)
  NHM_Pangolins <- prepNHMData(NHM_Pangolins, 6)
  NHM_Pangolins <- NHM_Pangolins %>% filter(Decade != is.na(Decade))
  NHM_Pangolins <- NHM_Pangolins %>% select(-c(NOTES))
  NHM_Pangolins <- fixTypeNames(NHM_Pangolins)
  NHM_Pangolins$Extent..m. <- (NHM_Pangolins$Extent..m. /1000)
  NHM_Pangolins <- NHM_Pangolins %>% rename(Extent_km = Extent..m.)
  NHM_Pangolins <- addError(NHM_Pangolins)
  myvars <- c('binomial', 'geometry')
  NHM_Pangolins <- NHM_Pangolins[myvars]
  # create a df for overlap data to go to 
  # as we will discard the loaded geometry after each species otherwise memory
  # will run out
  overlapDf = data.frame(binomial=as.character(), Percent_overlap=double(), binomial_overlap=as.integer()) # or something like that
  AOHFileList = list.files(path = AOHDataDir, pattern = "*.shp", full.names = TRUE)
  for (file in AOHFileList) 
    {
    #get the file name ie the species name
    sppName = str_extract(file, regex("\\\\w+\\_"))
    sppName = gsub("/", "", sppName)
    sppName = gsub("_", "", sppName)
    # read the file in!
    sppFile = st_read(dsn = file)
    # pass it to this little pipline
    names(sppFile)[1] <- 'binomial'
    sppFile$binomial <- sppName
    sppFile <- 
      sppFile %>%
      st_as_sf() %>%
      st_transform(4326)
    # calculate the overlaps and append to a df
    overlaps <- calculateOverlaps(NHM_Pangolins, sppFile)
    # calculate binomal overlaps and apppend to df
    overlaps <- binomialOverlap(overlaps)
    st_write(overlaps, here(paste("../Data/overlaps_", sppName, ".csv", sep = "")))
  }
}



RunAOHAnalysis("../Data/NHMPangolinsCompatability.csv", "..\\Data\\shpFiles")

fileList = list.files(path = "..\\Data\\shpFiles", pattern = "*.shp", full.names = TRUE)
for (file in fileList) {
  print(file)
}

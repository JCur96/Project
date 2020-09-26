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
devtools::install_github("JCur96/sfe", ref = "ReSubmitPaper", force = TRUE)
## support that.
###############################################

######## Notes ###############################

## Don't use system.file, it does something totally screwy and won't actually 
## capture the file path. Just pass the file path to an R object as a string.

#############################################

#imports
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
# allSpPoly = st_read(dsn = "E:\\tmpShp\\allMinusTemn.shp")
# 
# crassi = st_read(dsn ="E:\\tmpShp\\shpFiles\\crassicaudata_aoh.shp")
# ## rename (to binomial) column 1 and populate with species name conforming to 
# ## expected input
# ### essentially this 
# names(crassi)[1] <- 'binomial'
# crassi$binomial <- 'crassicaudata' ##or something spp name like that 
# crasssi <- resolveIUCNGeom(crassi)
# crassi <- 
#   crassi %>%
#   st_as_sf() %>%
#  st_transform(4326)

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

## quick summary of funs/code needed to process NHM data, simialr to how it 
## was done for the paper
ReadInAndProcessNHM <- function(NHMDataDir) 
{
  NHM_Pangolins <- read.csv(NHMDataDir, header=T)
  NHM_Pangolins <- invisible(prepNHMData(NHM_Pangolins, 6))
  NHM_Pangolins <- NHM_Pangolins %>% filter(Decade != is.na(Decade))
  NHM_Pangolins <- NHM_Pangolins %>% select(-c(NOTES))
  NHM_Pangolins <- fixTypeNames(NHM_Pangolins)
  NHM_Pangolins$Extent..m. <- (NHM_Pangolins$Extent..m. /1000)
  NHM_Pangolins <- NHM_Pangolins %>% rename(Extent_km = Extent..m.)
  NHM_Pangolins <- addError(NHM_Pangolins)
  myvars <- c('binomial', 'geometry')
  NHM_Pangolins <- NHM_Pangolins[myvars]
  pangolinDfList <- split(NHM_Pangolins, f = NHM_Pangolins$binomial)
  #print(pangolinDfList$Smutsia_temminckii)
  return(invisible(pangolinDfList))
}

## Updated method for processing large shp files 
## calculates overlaps (percent and binomial)
## generates a CSV file for each species
## might add functionality to merge that CSV into one soon
## for file in dir
### read in the file, get the name
#### calculate overlaps against the NHM data FOR THAT SPP based on name
##### save that data then start on the next file (because memory footprint)
RunAOHAnalysis <- function(NHMDataDir, AOHDataDir) 
{
  NHMPangolinList <- ReadInAndProcessNHM(NHMDataDir)
  # create a df for overlap data to go to 
  # as we will discard the loaded geometry after each species otherwise memory
  # will run out
  # overlapDf = data.frame(binomial=as.character(), Percent_overlap=double(), binomial_overlap=as.integer()) # or something like that
  AOHFileList = list.files(path = AOHDataDir, pattern = "*.shp", full.names = TRUE)
  for (file in AOHFileList) 
  {
    #get the file name ie the species name
    #print(file)
    # sppName = str_extract(file, regex("\/\\w+\\_"))
    # print(sppName)
    sppName = str_extract(file, regex("\\/\\w+\\_"))
    #print(sppName)
    sppName = gsub("/", "", sppName)
    sppName = gsub("\\_$", "", sppName)
    #print(sppName)
    #sapply(NHMPangolinList, function(x) unique(x$binomial))
    # read the file in!
    sppFile = st_read(dsn = file)
    # pass it to this little pipline
    names(sppFile)[1] <- 'binomial'
    sppFile$binomial <- sppName
    sppFile <- 
      sppFile %>%
      st_as_sf() %>%
      st_union()## %>%
      ##st_transform(4326)
    #sppFile <- st_union(sppFile)
    sppFile <- st_make_valid(sppFile)
    # calculate the overlaps and append to a df
    for (item in NHMPangolinList) {
      #print(unique(item$binomial))
      Spp <- unique(item$binomial)
      #print(Spp)
      if (unique(item$binomial) == sppName) {
        #print('match')
        #item <- st_transform(item, 4326)
        #sppFile <- st_transform(sppFile, 4326)
        #print(st_crs(item))
        #print("\n")
        #print(st_crs(sppFile))
        #item = st_transform(item, 2163)
        #sppFile = st_transform(sppFile, 2163)
        #print(head(sppFile))
        overlaps <- calculateOverlaps(item, sppFile)
        overlaps <- binomialOverlap(overlaps)
        fullFilePath <- paste("../Data/overlaps_", sppName, ".csv", sep = "")
        if (file.exists(fullFilePath)) { #if you want new output/ fresh rerun must delete all output files
          print('file already exists, this run is likely for just land use!')
          newFileName <- paste("../Data/overlaps_", sppName, "_landUse", ".csv", sep = "")
          st_write(overlaps, newFileName)
        } 
        if (!file.exists(fullFilePath)) {
          st_write(overlaps, paste("../Data/overlaps_", sppName, ".csv", sep = ""))
        }
      }
    }
  }
}

# Make a unified CSV of all overlap info (as later functions require that)
# and this is simpler than rewriting the whole code base
UnifyOverlapCSVs <- function(OverlapCSVDir) {
  OverlapFileList = list.files(path = OverlapCSVDir, pattern = "*overlaps_", full.names = TRUE)
  #print(OverlapFileList)
  #make a data frame here
  overlapDf = data.frame(binomial=as.character(), Percent_overlap=double(), binomial_overlap=as.integer()) 
  for (file in OverlapFileList) {
    csvObj <- read.csv(file, header=T)
    #append/rbind/whatever it is in R to the made DF here
    overlapDf <- rbind(overlapDf, csvObj)
  }
  st_write(overlapDf, "../Data/overlaps.csv")
}


### NEW OVERLAP CODE #########
# Apparently it took me re-writing this (basically just stripping out junk)
# to make this work with the IUCN shp files. 

#############################

# two input function for calculating the percentage overlap
calcOverlaps <- function(df1, df2) {
  #df1 <- lwgeom::lwgeom_make_valid(df1)
  #df2 <- lwgeom::lwgeom_make_valid(df2)
  # adding additional crs transfroms
  # as for some unknown reason it is convinced that CRS do not match at this point
  #print(df2)
  #print("3rd transform")
  #df1 <- st_transform(df1, 2163)
  #df2 <- st_transform(df2, 2163)
  # gives percentage overlap between NHM and IUCN
  overlap <- st_intersection(df1, df2) %>% st_area() / st_area(df1, df2) * 100
  # at this point the output is of class "units" which don't play nice
  overlap <- units::drop_units(overlap)
  # allows for handling of cases of zero overlap
  if (purrr::is_empty(overlap) == T) {
    # as it otherwise returns a list of length zero,
    # which cannot be appended to a df
    overlap <- c(0)
  }
  overlap <- as.list(overlap)
  # returns the result, so can be passed to another fun
  return(overlap)
}

hullOverFun <- function(df1, df2) {
  df1$Percent_overlap <- NA
  #df2 = st_transform(df2, 2163)
  for (row in 1:nrow(df1)) {
    # extract the geometry
    geom <- df1$geometry[row]
    #geom <- st_set_crs(geom, 2163)
    #geom <- st_transform(geom, 2163)
    x <- calcOverlaps(geom, df2)
    df1$Percent_overlap[row] <- x
  }
  return(df1) # return the modified df for use in another fun
}

calculateOverlaps <- function(NHM, AOH) {
  # create an empty list to store results
  output <- c()
  NHM <- st_transform(NHM, 2163)
  AOH <- st_transform(AOH, 2163)
  tmp <- hullOverFun(NHM, AOH)
  # rebuilding the input df with a new col
  output <- rbind(tmp, output)
  output$Percent_overlap <- as.numeric(output$Percent_overlap)
  return(output)
}


##### Calls to fun/MAIN #######
GeoTiffToPolygon("../Data/") ##pass directory as string with trailing slash
#ReadInAndProcessNHM("../Data/NHMPangolinsCompatability.csv")
RunAOHAnalysis("../Data/NHMPangolinsCompatability.csv", "../Data/shpFiles")
UnifyOverlapCSVs("../Data/")
## trying again with dissolved javanica
RunAOHAnalysis("../Data/NHMPangolinsCompatability.csv", "../Data/javanica_dissolved") ## check javanica specific and compare with overlaps all run before

# Old IUCN run to check those overlaps
IUCN <- readOGR("../Data/IUCN_Pholidota", "maps_pholidota")
IUCN <- st_as_sf(IUCN)
myvars <- c('binomial', 'geometry')
IUCN <- IUCN[myvars]
IUCNList <- split(IUCN, f = IUCN$binomial)
for (df in IUCNList) {
  #print(df$binomial)
  name = unique(df$binomial)
  name = gsub(" ", "_", name)
  name = paste(name,"_aoh")
  name = gsub(" ", "", name)
  #print(name)
  st_write(df, paste("../Data/IUCNShpFiles/", name, ".shp", sep = ""))
}
RunAOHAnalysis("../Data/NHMPangolinsCompatability.csv", "../Data/IUCNShpFiles")
UnifyOverlapCSVs("../Data/")
RunAOHAnalysis("../Data/NHMPangolinsCompatability.csv", "../Data/shpFiles")
## FAILED
#RunAOHAnalysis("../Data/NHMPangolinsCompatability.csv", "../Data/temminckii")
#UnifyOverlapCSVs("../Data/")
## attempting temminckii again after Emily unify
RunAOHAnalysis("../Data/NHMPangolinsCompatability.csv", "../Data/temminckii2")
UnifyOverlapCSVs("../Data")


ProcessTemm <- function(TemData) {
  NHM_Pangolins <- read.csv(TemData, header=T)
  NHM_Pangolins <- invisible(prepNHMData(NHM_Pangolins, 6))
  NHM_Pangolins <- NHM_Pangolins %>% filter(Decade != is.na(Decade))
  NHM_Pangolins <- NHM_Pangolins %>% select(-c(NOTES))
  NHM_Pangolins <- fixTypeNames(NHM_Pangolins)
  NHM_Pangolins$Extent..m. <- (NHM_Pangolins$Extent..m. /1000)
  NHM_Pangolins <- NHM_Pangolins %>% rename(Extent_km = Extent..m.)
  NHM_Pangolins <- addError(NHM_Pangolins)
  myvars <- c('binomial', 'geometry')
  NHM_Pangolins <- NHM_Pangolins[myvars]
  pangolinDfList <- split(NHM_Pangolins, f = NHM_Pangolins$binomial)
  return(NHM_Pangolins)
}
temminckii <- ProcessTemm("../Data/temmickiiOnly.csv")
# st_write(temminckii, "../Data/temmickiiProcessed.shp")

NHM_Pangolins <- read.csv("../Data/temmickiiOnly.csv", header=T)
NHM_Pangolins <- invisible(prepNHMData(NHM_Pangolins, 6))
NHM_Pangolins <- NHM_Pangolins %>% filter(Decade != is.na(Decade))
NHM_Pangolins <- NHM_Pangolins %>% select(-c(NOTES))
NHM_Pangolins <- fixTypeNames(NHM_Pangolins)
NHM_Pangolins$Extent..m. <- (NHM_Pangolins$Extent..m. /1000)
NHM_Pangolins <- NHM_Pangolins %>% rename(Extent_km = Extent..m.)
NHM_Pangolins <- addError(NHM_Pangolins)
myvars <- c('binomial', 'geometry')
NHM_Pangolins <- NHM_Pangolins[myvars]
NHM_Pangolins <- st_make_valid(NHM_Pangolins)
class(NHM_Pangolins)
tem1 <- st_read("../Data/tem_div/temminckii_aoh_first")
tem1$binomial = "Smutsia_temminckii"
tem1 <- st_transform(tem1, 2163)
calculateOverlaps(NHM_Pangolins, tem1)
tem2 <- st_read("../Data/tem_div/temminckii_aoh_second")
tem2$binomial = "Smutsia_temminckii"
tem2 <- st_transform(tem2, 2163)
calculateOverlaps(NHM_Pangolins, tem2)
tem4 <- st_read("../Data/tem_div/temminckii_aoh_fourth")
tem4$binomial = "Smutsia_temminckii"
tem4 <- st_transform(tem4, 2163)
calculateOverlaps(NHM_Pangolins, tem4)

RunAOHAnalysis("../Data/NHMPangolinsCompatability.csv", "../Data/tem_Div/All")

RunAOHAnalysis("../Data/NHMPangolinsCompatability.csv", "../Data/tem_Div/temminckii_aoh_fourth")

temAoh <- st_read("../Data/tem_Div/temminckii_aoh_fourth")
geom = list()
for(i in 1:nrow(temAoh)) {
  geom[[i]] = st_union(temAoh[temAoh[i, ], ])
}
geom = do.call(c, geom)
### eg of how to dissolve a polygon, I think
geom = list()
for(i in 1:nrow(pol)) {
  geom[[i]] = st_union(pol[pol[i, ], ])
}
geom = do.call(c, geom)


#####POSSIBLE SOLUTION !!!!!! ######
# treat AOH as have been treating NHM
# Do cumulative percent overlaps
# ie for each NHM specimen, 
# go through each row of the AOH, adding to a list of percent overlaps
# then once all overlaps calculated, add all values in list
# that will be slow but will be the percentage overlap for that specimen
#######################################################################
for (row in NHMPangolins) {
  percentList <- c()
  for (entry in AOHDf) {
    #calculate overlaps for row vs entry
    overlap <- st_intersection(row, entry) %>% st_area() / st_area(row, entry) * 100
    # at this point the output is of class "units" which don't play nice
    overlap <- units::drop_units(overlap)
    # allows for handling of cases of zero overlap
    if (purrr::is_empty(overlap) == T) {
      # as it otherwise returns a list of length zero,
      # which cannot be appended to a df
      overlap <- c(0)
    }
    #append to list
    list.append(percentList, overlap)
  }
  #sum list 
  sumOfPercentList = sum(percentList)
  NHMPangolins$binomial <- sumOfPercentList
}

AOHOverlaps <- function(NHMPangolins, AOHDf) {
  for (row in 1:nrow(NHMPangolins)) {
    percentList <- c()
    for (entry in 1:nrow(AOHDf)) {
      NHMGeom <- NHMPangolins$geometry[row]
      AOHGeom <- AOHDf$geometry[entry]
      #calculate overlaps for row vs entry
      overlap <- st_intersection(NHMGeom, AOHGeom) %>% st_area() / st_area(NHMGeom, AOHGeom) * 100
      # at this point the output is of class "units" which don't play nice
      overlap <- units::drop_units(overlap)
      # allows for handling of cases of zero overlap
      if (purrr::is_empty(overlap) == T) {
        # as it otherwise returns a list of length zero,
        # which cannot be appended to a df
        overlap <- c(0)
      }
      #append to list
      #list.append(percentList, overlap)
      percentList <- rbind(overlap, percentList)
    }
    #sum list 
    sumOfPercentList = sum(percentList)
    NHMPangolins$percentOverlap <- sumOfPercentList #change to percent overlap lol 
    # also make this do for each row but like stop and write out at the end of the row 
    # and start on the next one like destrucivtly to untie memory
    if (entry %% 5 == 0) {
      st_write(NHMPangolins, paste("../Data/Overlaps/TempOverlaps", row, entry, ".csv", sep = "")) # writePangolinData to overlap file in a specific directory, then merge after
    }
  }
  return(NHMPangolins)
}

temAoh <- st_read("../Data/tem_Div/temminckii_aoh_fourth")
temAoh$binomial = "Smutsia_temminckii"
temAoh <- st_transform(temAoh, 2163)
NHM_Pangolins <- read.csv("../Data/temmickiiOnly.csv", header=T)
NHM_Pangolins <- invisible(prepNHMData(NHM_Pangolins, 6))
NHM_Pangolins <- NHM_Pangolins %>% filter(Decade != is.na(Decade))
NHM_Pangolins <- NHM_Pangolins %>% select(-c(NOTES))
NHM_Pangolins <- fixTypeNames(NHM_Pangolins)
NHM_Pangolins$Extent..m. <- (NHM_Pangolins$Extent..m. /1000)
NHM_Pangolins <- NHM_Pangolins %>% rename(Extent_km = Extent..m.)
NHM_Pangolins <- addError(NHM_Pangolins)
myvars <- c('binomial', 'geometry')
NHM_Pangolins <- NHM_Pangolins[myvars]
NHM_Pangolins <- st_make_valid(NHM_Pangolins)
NHM_Pangolins<- st_transform(NHM_Pangolins, 2163)
NHM_Pangolins$percentOverlap <- NA

AOHOverlaps(NHM_Pangolins, temAoh)
UnifyOverlapCSVs("../Data/Overlaps")

#file.rename(list.files(pattern = "*"), paste("Smutsia_temmickii1_aoh"))

# for (file in folder) {
#   #rename file like this 
#   ## Smutsia_temminckii[x]_aoh
#   
# }

##### sandbox ######
# 
# NHM_Pangolins
# st_rasterize(NHM_Pangolins, file = "../Data/temmickiiRaster.tif")
# 
# sfe::plotMaps(NHN_Pangolins, IUCN)
# write.csv(NHM_Pangolins, file ="../Data/temminckiiProcessedCSV.csv")
# st_write(NHM_Pangolins, "../Data/temmicnkiiST.csv")
# st_make_valid(NHM_Pangolins)
# st_write(NHM_Pangolins, "../Data/temmicnkiiST.csv", "sf", append = TRUE)
# 
# sfbbox <- st_bbox(NHM_Pangolins)
# bbox <- unname(sfbbox)
# # puts the coords into the order expected down in ggmap coords
# xlim <- c(bbox[1], bbox[3])
# ylim <- c(bbox[2], bbox[4])
# # make a base map to plot against
# baseMap <- st_as_sf(maps::map('world', plot=F, fill=T))
# # print('This will take some time...')
# IUCN_var <- IUCN[IUCN$binomial == var,]
# NHM_var <- NHM_Pangolins[NHM_Pangolins$binomial == var,]
# p = ggplot2::ggplot(data = baseMap) +
#   ggplot2::geom_sf() +
#   geom_sf(mapping = aes(alpha = 0.5, fill='blue'), data = NHM_Pangolins, show.legend = F) +
#   # adding IUCN maps
#   geom_sf(mapping = aes(alpha = 0.1, fill = "red"), data = IUCN, show.legend = F) +
#   # zooming to correct cords (South America)
#   coord_sf(xlim = xlim, ylim = ylim, expand = T)
# #saving map as png
# png(paste("../Data/", 'temminckii.png', sep=''), width=600, height=500, res=120)
# print(p) # printing to png
# ggplot(p)


# #### trying again for temminckii
# temmFiles = list.files("../Data/tem_div", pattern = "*.shp", recursive = T, full.names = T)
# listOfFour = c("one", "two", "three", "four")
# for (file in temmFiles) {
#   listOfFour[file] = st_read(dsn = file)
# }
# NHMPangolinList <- ReadInAndProcessNHM("../Data/NHMPangolinsCompatability.csv")
# for (section in listOfFour) {
#   #pull out part of list
#   section
#   calculateOverlaps(NHMPangolinList, section)
# }

# NHM_Pangolins <- read.csv("../Data/temmickiiOnly.csv", header=T)
# NHM_Pangolins <- invisible(prepNHMData(NHM_Pangolins, 6))
# NHM_Pangolins <- NHM_Pangolins %>% filter(Decade != is.na(Decade))
# NHM_Pangolins <- NHM_Pangolins %>% select(-c(NOTES))
# NHM_Pangolins <- fixTypeNames(NHM_Pangolins)
# NHM_Pangolins$Extent..m. <- (NHM_Pangolins$Extent..m. /1000)
# NHM_Pangolins <- NHM_Pangolins %>% rename(Extent_km = Extent..m.)
# NHM_Pangolins <- addError(NHM_Pangolins)
# myvars <- c('binomial', 'geometry')
# NHM_Pangolins <- NHM_Pangolins[myvars]
# NHM_Pangolins <- st_make_valid(NHM_Pangolins)
# class(NHM_Pangolins)
# IUCN <- readOGR("../Data/IUCN_Pholidota", "maps_pholidota")
# IUCN <- st_as_sf(IUCN)
# myvars <- c('binomial', 'geometry')
# IUCN <- IUCN[myvars]

#### Minimalist AOH overlap calculation script ####

## libraries ## 
library(rgdal)
library(sf)
library(ggplot2)
library(stars)
library(stringr)

## Functions ##
#############################

# two input function for calculating the percentage overlap
calcOverlaps <- function(df1, df2) {
  df1 <- lwgeom::lwgeom_make_valid(df1)
  df2 <- lwgeom::lwgeom_make_valid(df2)
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
  # rebuilding the input df with a new colc
  output <- rbind(tmp, output)
  output$Percent_overlap <- as.numeric(output$Percent_overlap)
  return(output)
}
ReadInAndProcessNHM <- function(NHMDataDir) #rewrite to work with the NHM download data
{
  NHM_Pangolins <- st_read(NHMDataDir)
  NHM_Pangolins <- st_set_crs(NHM_Pangolins, 4326) ##not sure if this is correct, probs not 2163, probably wgs84
  NHM_Pangolins$binomial <- gsub(' ', '_', NHM_Pangolins$binomial)
  NHM_Pangolins$Extent_km <- as.double(NHM_Pangolins$Extent_km)
  NHM_Pangolins <- st_buffer(NHM_Pangolins, NHM_Pangolins$Extent_km)
  NHM_Pangolins <- st_make_valid(NHM_Pangolins)
  NHM_Pangolins<- st_transform(NHM_Pangolins, 2163)
  NHM_Pangolins$percentOverlap <- NA
  myvars <- c('X_id', 'RegistrationNumber', 'binomial', 'percentOverlap', 'geometry')
  NHM_Pangolins <- NHM_Pangolins[myvars]
  NHM_Pangolins <- st_make_valid(NHM_Pangolins)
  #temFrame <- subset(NHM_Pangolins, binomial == 'Smutsia_temminckii') # dont need these in this fun as this is just for non-tem
  #temFrame <- NHM_Pangolins[NHM_Pangolins$binomial == '*temminckii']
  NHM_Pangolins <- NHM_Pangolins[NHM_Pangolins$binomial != 'Smutsia_temminckii', ]
  ## temp addition, to run from where left off
  ## could be a lot fancier and add as input args, but this is quick 
  NHMDataDir <- NHM_Pangolins[NHM_Pangolins$binomial != 'Manis_crassicaudata', ] ## tmp comment out after run
  NHMDataDir <- NHM_Pangolins[NHM_Pangolins$binomial != 'Manis_javainca', ] ## tmp comment out after run
  ##head(NHM_Pangolins)
  pangolinDfList <- split(NHM_Pangolins, f = NHM_Pangolins$binomial)
  return(invisible(pangolinDfList))
}
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
### main ###
RunAOHAnalysis("../Data/NHM_all_pangolins.csv", "../Data/shpFiles")
## arsehole computer decided to update overnight so halted progress. Thankfully 
## got Manis crassicaudata and Manis javanica and their landuse done, so will
## run again excluding those.

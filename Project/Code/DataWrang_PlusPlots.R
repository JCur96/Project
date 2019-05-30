#!/usr/bin/env Rscript 
# initial.R
# Initial script for working out how this is gonna work
# Going to need to rework out how to do all this using sf

###################dependencies##################
# Build: Ubuntu 18.04 on an unsigned kernel rtl8821ce (need it for wifi to work, go figure)

# If you are on Ubunutu 18.04 you will have to watch 
# for non-zero exits on installs and 
# install via terminal for several packages and dependencies 
# (usually package name followed by the -deb flag)

# install.packages("rgdal") 
# if R won't install it here are some useful guides 

# Add this repository https://launchpad.net/~ubuntugis/+archive/ubuntu/ubuntugis-unstable 
# using the code: sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable && sudo apt-get update
 
# If that doesn't solve it then do the following
# ########WARNING########### 
# DO NOT DO THIS unless you are familiar with linux and 
# the dangers this comes with, see apt-secure(8) manpage and 
# sources.list(5) manpage for details 

# set [trusted=yes] flag. Example:
# deb [trusted=yes] http://ppa.launchpad.net/ubuntugis/ubuntugis-unstable/ubuntu bionic main
# and how to easily do it: 
# sudo gedit /etc/apt/sources.list.d/ubuntugis-ubuntu-ubuntu-ubuntugis-unstable-bionic.list

# repository: 
# sudo add-apt-repository ppa:ubuntugis/ppa

# Next step is to coerce Ubuntu into finding the gdal-config file which 
# R and python3 need to install their versions of gdal. 
# The following helped me work out what went wrong:
# https://stackoverflow.com/questions/12141422/error-gdal-config%20-not-found 

# For me the real fix was:
# apt-file search gdal-config
# sudo apt-get install libgdal1-dev
# sudo apt-get update 
# sudo apt-get upgrade

# this should hopefully get all dependencies into the right 
# places and fill in any missing files such as the config. you're then good 
# to install rgdal via R CMD line 

# install.packages("devtools") # good for the not super stable or secure packages, built via their git repositories
# install.packages("rgdal","tidyverse","sf","ggplot2", "dplyr", "stringr", "ggforce", "maps", "mapdata") 
# won't 
# need all of these as tidyverse should cover most of that, but just so you know what gets used they're there
# git versions of the packages which were problematic to install via install.packages
# devtools::install_github("r-spatial/sf")
# devtools::install_github("dkahle/ggmap")
# install.packages("rgeos") # another one which is a pain to install, 
# leave the PPA's as trusted (you'll need unstable for anything newer than Xenial)
# the missing package was libgeos-c1v5 in my case, so sudo apt-get install libgeos-c1v5 and you should be good to go. 
####################Imports######################
# install.packages("devtools")
# library(devtools)
# devtools::install_github("cran/ggplot2") # need to remove the auth token from env for this to work
# install.packages("lwgeom")
# install.packages("units")
# install.packages("tidyverse")
# install.packages("rgdal")
# install.packages("rgeos")
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("ggforce")
# install.packages("ggmap")
# install.packages("mapdata")
# install.packages("maptools")
# install.packages("maps")
library(units)
library(lwgeom)
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggforce)
library(rgeos)
library(rgdal)
library(maptools)
library(rnaturalearth)
###################Data_Wrangling################
####### NHM collections data #######
# df <- read.csv('../Data/WorkingSouthAmerica.csv', header=T)
# prepNHMData(df) # it works!
NHM_AMPH <- read.csv("../Data/WorkingSouthAmerica.csv", header=T) #reading in the data from csv (have to set your own path)
colnames(NHM_AMPH)[7] <- "binomial" # sets the ColName from UpdatedScientificName to binomial for easier times later on
NHM_AMPH <- NHM_AMPH %>% filter(Longitude != is.na(Longitude) 
                                & Locality != '' & Extent_km < 800 & binomial != '') 
# filtering the data (removing NA's as sf cannot parse data
# with NA's), and improving quality (removing things that were only found to country level) both
# locality null and extent less than 800km, as well as removing blank spaces needed for that
NHM_AMPH$binomial <- gsub(' ', '_', NHM_AMPH$binomial) # removes spaces

# Combines lat/long into one variable (geometry)
# and sets datum to WGS84 (thats the crs 4326 part)
NHM_AMPH <- st_as_sf(NHM_AMPH, coords = c("Longitude", "Latitude"), crs = 4326) 

# saving the spatial frame
write.csv(NHM_AMPH, file = "../Data/SA_AMPH_NHM_DF.csv") 


####### IUCN rangemap data #######
# IUCN data isn't supplied in sf format, so has to be read in the old way
IUCN <- readOGR(dsn = "../Data", layer = "AMPHIBIANS") 
# but is readily transformed to sf format!
IUCN <- st_as_sf(IUCN) 
# making sure its in the same CRS as the other data
IUCN <- st_transform(IUCN, 4326) 
# pulling out the parts we actually need
# myvars <- c("binomial", "SHAPE_Leng", "SHAPE_Area", "geometry") 
myvars <- c("binomial", "TypeStatus", "geometry") 
IUCN$TypeStatus <- "IUCN"
IUCN <- IUCN[myvars]
# removes spaces from spp_genus
IUCN$binomial <- gsub(' ', '_', IUCN$binomial) 

##### this works but takes an age ###
# write.csv(IUCN, file = "../Data/IUCN_Spatial.csv") # saves full data set for use here or elsewhere in its updated form

# needed for filtering the IUCN data
binom_list <- NHM_AMPH$binomial
# pulls out only the species which are in both data sets
IUCN_filtered <- IUCN %>% filter(binomial %in% binom_list) 
NHM_AMPH_filtered <- NHM_AMPH  %>% filter(binomial %in% IUCN_filtered$binomial) 
write.csv(IUCN_filtered, file = "../Data/IUCN_NHM_Spp_Match.csv")

##############Plotting NHM and IUCN ranges##########################
makeMap <- function(df) {
  baseMap <- st_as_sf(map('world', plot=F, fill=T))
  return(baseMap)
}
SAMap <- st_as_sf(map("world", plot=F, fill=T))
# puts all into a km based projection
NHM_AMPH_filtered <- st_transform(NHM_AMPH_filtered, "+proj=utm +zone=42N +datum=WGS84 +units=km") 
# st_buffer used like this computes a circle with radius specified in Extent
buffer <- st_buffer(NHM_AMPH_filtered, NHM_AMPH_filtered$Extent_km) 
# transforms it back to coord based projection
buffer <- st_transform(buffer, 4326) 
filtered_buffer <- buffer[myvars]

# making sensible bounds for a map
sfbbox <- st_bbox(buffer) # getting the bbox but with the corrected datum
sfbbox #shows what this is (a min/max bounds for a map)
bbox <- unname(sfbbox) #pulls the column names out
xlim <- c(bbox[1], bbox[3]) # puts the coords into the order expected down in ggmap coords
ylim <- c(bbox[2], bbox[4])


## binomial names as individual var's
## I dont think this is adding all the points we have to it, only the first one
## need to fix that, maybe a match == T type statement in there
for (var in unique(IUCN_filtered$binomial)) {
  IUCN_var <- IUCN_filtered[IUCN_filtered$binomial == var, ]
  NHM_var <- filtered_buffer[filtered_buffer$binomial == var,]
  p = ggplot(data = SAMap) + # making a map
    geom_sf() + # plotting a world map
    geom_sf(mapping = aes(alpha = 0.5, fill ="blue"), data = NHM_var, show.legend = F) + # adding NHM point-radius
    geom_sf(mapping = aes(alpha = 0.1, fill = "red"), data = IUCN_var, show.legend = F) + # adding IUCN maps
    coord_sf(xlim = xlim, ylim = ylim, expand = T) # zooming to correct cords (South America)
    png(paste("../Output/IUCN_Range_Graphs/Graph_", var, ".png", sep=""), width=600, height=500, res=120) #saving map as png
    print(p) # printing to png
    dev.off() # not sending to screen
}

######working percent overlap functions ######

overlaps <- function(df1, df2) { # two input function for calculating the percentage overlap
  # # get area of each df 
  # # then get the area of the intersection of both dfs 
  # # then (intersection / area df2) * 100
  # # which will give the percentage overlap between new and old areas 
  overlap <- st_intersection(df1, df2) %>% st_area() * 100 / st_area(df2) # gives percentage overlap between NHM and IUCN 
  #overlap <- st_intersection(df1, df2) %>% st_area() * 100 /sqrt(st_area(df1) * st_area(df2)) # this gives area overlapping out of total area shaded
  overlap <- drop_units(overlap) # at this point the output is of class "units" which don't play nice 
  if (is_empty(overlap) ==T) { # allows for handling of cases of zero overlap 
    overlap <- c(0) # as it otherwise returns a list of length zero, which cannot be appended to a df
  }
  overlap <- as.list(overlap) 
  return(overlap) # returns the result, so can be passed to another fun 
}

over_fun <- function(df1, df2) {
  df1[,"Percent_overlap"] <- NA # adds a column of na's
  for (row in 1:nrow(df1)) { # for each row in first df's geometry col
    geom <- df1$geometry[row] # extract the geometry
    x <- overlaps(geom, df2$geometry) # use previous fun to calculate overlaps
    df1$Percent_overlap[row] <- x # and append to the percent overlap col 
  }
  return(df1) # return the modified df for use in another fun 
}

full_overlaps <- function(NHM_df, IUCN_df) {
  output <- c() # create an empty list to store results
  for (var in unique(NHM_df$binomial)) { # find all entries in both dfs which match var
    IUCN_var <- IUCN_df[IUCN_df$binomial == var,] 
    NHM_var <- NHM_df[NHM_df$binomial == var,]
    
    NHM_var <- st_transform(NHM_var, 2163) # ensure planar crs is in use
    IUCN_var <- st_transform(IUCN_var, 2163)
    x <- over_fun(NHM_var, IUCN_var) # then pass to the over_function
    output <- rbind(x, output) # rebuilding the input df with a new col
  }
  output <<- data.frame(output)
  ## below is for thinking about dynamic naming 
  # arg_name <- deparse(substitute(df1)) # Get argument name
  # var_name <- paste("updated", arg_name, sep="_") # Construct the name
  # assign(var_name, df1, env=.GlobalEnv) # Assign values to variable
  # # variable will be created in .GlobalEnv
  return(output)
}


full_overlaps(filtered_buffer, IUCN_filtered) # sooo funny thing, the percent overlap calculation gives the percentage of 
# area total that is overlapped between the two; i.e. it doesn't discriminate between what is overlapping what, but rather the amount of total
# shaded land in which both IUCN and NHM data are 
# i.e. IUCN + NHM shaded = total shaded
# overlaps returns % of total shaded that is both IUCN and NHM
# fixed that! 



##### analysis stuff 
#' Probably want to see about calculating both the 
#' Area of Occupancy AOO
#' and Extent of Occurence EOO
#' Use IUCN guidlines for this
#' EOO is done via summing area of triangles left after an
#' elimination process described on pages 47/48 of 
#' http://cmsdocs.s3.amazonaws.com/RedListGuidelines.pdf
#' AOO is calculated using this equaiton 
#' AOO = no. occupied cells × area of an individual cell
#' given on page 50 of the linked document
#' cells in this case are specified to be 2mx2m grid sqaures
#' which could make things a little interesting
#' 
#' clipping to landmass appears to be possible using
#' something similar to this 
#' https://stackoverflow.com/questions/49266736/clip-spatial-polygon-by-world-map-in-r
#' which amusingly involves messing around with 
#' union and difference again
#' to generalise this could be interesting 
#' this is also of interest 
#' https://gis.stackexchange.com/questions/93096/how-to-perform-a-true-gis-clip-of-polygons-layer-using-a-polygon-layer-in-r
NHM <- NHM_AMPH_filtered
IUCN <- IUCN_filtered

#' look at area of overlap for analysis

# # st_convex_hull(x) # makes a convex hull out of geometry I think
# test <- st_convex_hull(test)
# 
# # The below seems to produce what I expect it to
# # So thats some progress 
# # Can probably use this to compute centroid-centroid distance
# # Or centroid-edge distance (Probably harder as thats for two different geometry collections)
# test <- NHM %>% filter(binomial == 'Batrachyla_leptopus')
# # test <- NHM[5,]
# test_cent <- test$geometry
# test <- st_buffer(test, test$Extent_km) 
# test$Centroid <- NA
# test$Centroid <- st_centroid(test$geometry)
# print(test_cent)
# print(test$Centroid)
# # I think the below works are expected as well
# # the geometry certainly changes 
# # think I should probably map this to see what happens
# plot(test$geometry)
# print(test$geometry)
# test <- st_convex_hull(st_union(test)) # this draws a single straight line between the two centroids
# # so this will possibly be really useful for calculating straight line distance between say centroids
# test <- st_convex_hull(st_combine(test)) # this one doesn't resolve all  boundaries, but makes an actual shape
# plot(test)
# print(test$geometry)
# test
# st_centroid(x) # finds the centroid of a given polygon(s), probably useful for finding those distances


#NHM <- filtered_buffer
#NHM$convex_hull <- NA
# output <- c()
# myvars <- c('binomial', 'convex_hull')
# for (var in unique(NHM$binomial)) { # this isnt working as I'm asking it to populate too many rows 
#   # as it makes one convex hull per spp entry, not per row
#   test <- NHM[NHM$binomial == var,]
#   #print(test)
#   # NHM$convex_hull <- st_convex_hull(st_combine(NHM$geometry[test])) # this isnt subsetting correctly
#   # print(NHM$convex_hull)
#   #hull <- st_convex_hull(st_combine(NHM$geometry[test]))
#   #print(hull)
#   #output <- c(output, hull)
#   #NHM$convex_hull <- hull[test]
#   test$convex_hull <- st_convex_hull(st_combine(test$geometry))
#   #test <- test[myvars]
#   print(test)
#   test <- st_set_crs(test, 4326)
#   output <- rbind(output, test)
#   
#   # NHM <- rbind(test, NHM) 
#   
#   #NHM$convex_hull <- data.frame(hull)
#   # for (row in 1:nrow(test)) {
#   #   NHM$convex_hull <- hull[row]
#   #   #print(test)
#   #   #print(var)
#   #   #print(NHM$convex_hull) # shows that it cats stuff quite a bit 
#   #   #i <- test[test$binomial == row,]
#   #   #NHM$convex_hull <- hull[i]
#   # }
#   # NHM$convex_hull <- hull[test]
#   # test <- NHM[NHM$binomial == var,]
#   # hull <- st_convex_hull(st_combine(test))
#   # test$convex_hull <- hull
#   # 
# }
# # NHM <- output
# 
# #unlockBinding("NHM", environment())
# #bindingIsLocked("NHM", environment())
NHM <- filtered_buffer
makeHulls <- function(df) { # currently I cant see a solution beyond converting the df into a non-sfc object 
  # as sf is the thing causing the problems rn
  # very annoying that somehow it was working just fine yesterday but today it won't behave 
  output <- c() # empty list to rebuild the df from
  df$convex_hull <- NA
  for (var in unique(df$binomial)) { 
    subsetOfDf <- df[df$binomial == var,]
    subsetOfDf$convex_hull <- st_convex_hull(st_combine(subsetOfDf$geometry))
    subsetOfDf <- st_set_crs(subsetOfDf, 4326)
    #print(subsetOfDf)
    #df <<- rbind(df, subsetOfDf)
    output <- rbind(output, subsetOfDf)
    #hull <- st_convex_hull(st_combine(subsetOfDf$geometry))
    #print(hull)
    #df <- do.call(rbind, list(subsetOfDf, df))
    #newDf <<- rbind(subsetOfDf, subsetOfDf)
  }
  # output <<- output
  return(output)
  #newDf <<- data.frame(newDf)
}

NHM <- makeHulls(NHM)
# head(newDf)

# I think what I need to do with the convex hulls is to take all the points for a spp 
# and make it into a convex hull (alpha shape) 
# should be able to do this in a for loop a lot like the 
# graphing I think

# subNHM <- NHM %>% filter(binomial == 'Batrachyla_leptopus')
# subIUCN <- IUCN %>% filter(binomial == 'Batrachyla_leptopus')
# subNHM <- st_transform(subNHM, 4326)
# subIUCN <- st_transform(subIUCN, 4326)
# subNHM <- makeHulls(subNHM)
# hullOver <- st_intersection(subNHM, subIUCN) %>% st_area() * 100 / st_area(subIUCN)
# head(hullOver)
#' I think now I need to make a script to compare % overlap of convex hulls
#' which should be relatively easy I think
#' just reuse ovelaps functions
#' I think the over_fun needs slight rewriting and then it should work fine
hullOverFun <- function(df1, df2) {
  df1[,"Percent_overlap"] <- NA # adds a column of na's
  for (row in 1:nrow(df1)) { # for each row in first df's geometry col
    geom <- df1$convex_hull[row] # extract the geometry
    #geom <- st_set_crs(geom, 2163)
    geom <- st_transform(geom, 2163)
    x <- overlaps(geom, df2$geometry) # use previous fun to calculate overlaps
    df1$Percent_overlap[row] <- x # and append to the percent overlap col 
  }
  return(df1) # return the modified df for use in another fun 
}

hullOverlaps <- function(NHM_df, IUCN_df) {
  output <- c() # create an empty list to store results
  for (var in unique(NHM_df$binomial)) { # find all entries in both dfs which match var
    IUCN_var <- IUCN_df[IUCN_df$binomial == var,] 
    NHM_var <- NHM_df[NHM_df$binomial == var,]
    
    NHM_var <- st_transform(NHM_var, 2163) # ensure planar crs is in use
    IUCN_var <- st_transform(IUCN_var, 2163)
    x <- hullOverFun(NHM_var, IUCN_var) # then pass to the over_function
    output <- rbind(x, output) # rebuilding the input df with a new col
  }
  # output <<- data.frame(output)
  ## below is for thinking about dynamic naming 
  # arg_name <- deparse(substitute(df1)) # Get argument name
  # var_name <- paste("updated", arg_name, sep="_") # Construct the name
  # assign(var_name, df1, env=.GlobalEnv) # Assign values to variable
  # # variable will be created in .GlobalEnv
  return(output)
}

hullOverlaps(NHM, IUCN)


# Still need to work out something for clipping to landmasses 
# I think best suggestion so far is the simplest
# do an st_difference and create an object from that
# might want to make one which modifies all rows convex hull 
# or geometry in for loop so that its just something thats done
landMap <- rnaturalearth::ne_countries(returnclass = 'sf') %>% 
  st_union()
plot(landMap)
plot(st_geometry(NHM$geometry), add = T)
plot(st_geometry(NHM$convex_hull), add = T)


clipHullsToLand <- function(df) {
  # make a world map here maybe?
  landMap <- rnaturalearth::ne_countries(returnclass = 'sf') %>% 
    st_union()
  output <- c()
  for (var in unique(df$binomial)) {
    subsetOfDf <- df[df$binomial == var,]
    clippedHull <- st_intersection(subsetOfDf$convex_hull, landMap)
    # ocean <- st_difference(subsetOfDf$convex_hull, landMap)
    # clippedHull <- st_difference(ocean, subsetOfDf$convex_hull)
    # subsetOfDf$convex_hull <- st_difference(ocean, subsetOfDf$convex_hull)
    # print(clippedHull)
    # print(subsetOfDf$convex_hull)
    if (is_empty(clippedHull)) {
       # do not replace just leave
      # remake the hulls
      #subsetOfDf$convex_hull <- st_convex_hull(st_combine(subsetOfDf$geometry))
      print('Hull is entirely in the ocean')
    } else {
      #subsetOfDf$convex_hull <- st_difference(ocean, subsetOfDf$convex_hull)
      #subsetOfDf$convex_hull <- clippedHull
      #print(subsetOfDf)
      #print(clippedHull)
      #print('theres a difference')
      # print(clippedHull)
      subsetOfDf$convex_hull <- clippedHull
      
    }
    #print(subsetOfDf)
    output <- rbind(output, subsetOfDf)
  }
  return(output)
}

NHM <- clipHullsToLand(NHM)


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
# devtools::install_github('cran/ggplot2')
install.packages("lwgeom")
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
###################Data_Wrangling################
####### NHM collections data #######
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
myvars <- c("binomial", "geometry") 
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
SAMap <- st_as_sf(map("world", plot=F, fill=T))
# puts all into a km based projection
NHM_AMPH_filtered <- st_transform(NHM_AMPH_filtered, "+proj=utm +zone=42N +datum=WGS84 +units=km") 
# st_buffer used like this computes a circle with radius specified in Extent
buffer <- st_buffer(NHM_AMPH_filtered, NHM_AMPH_filtered$Extent_km) 
# transforms it back to coord based projection
buffer <- st_transform(buffer, 4326) 
tokeep <- c("binomial", "geometry")
filtered_buffer <- buffer[tokeep]

# making sensible bounds for a map
sfbbox <- st_bbox(buffer) # getting the bbox but with the corrected datum
sfbbox #shows what this is (a min/max bounds for a map)
bbox <- unname(sfbbox) #pulls the column names out
xlim <- c(bbox[1], bbox[3]) # puts the coords into the order expected down in ggmap coords
ylim <- c(bbox[2], bbox[4])


# binomial names as individual var's
### I dont think this is adding all the points we have to it, only the first one
### need to fix that, maybe a match == T type statement in there
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

# now for overlaps 
# polygonise the buffer 
# then the overlap function may well work
# hopefully then can do a lapply
?st_polygonize
?st_intersection
?st_area
# for the below to work, need a single list of spp and the geometrys from both NHM + IUCN
# buffer is already a polygon, which makes life easier (removes a step)



# maybe pull the single row out of iucn and add it to buffer data is simpler
merged <- c(filtered_buffer, IUCN_filtered) #as it turns out the wya to merge sf objects is to use c
merged <- rbind(IUCN_filtered, filtered_buffer)
isauv <- IUCN_filtered[which(IUCN_filtered$binomial == "Phyllomedusa_sauvagii"),]
sauv <- filtered_buffer[which(filtered_buffer$binomial == "Phyllomedusa_sauvagii"),]
sauv <- rbind(isauv, sauv)
sauv <- st_cast(sauv, "POLYGON")
myvar = c("geometry")
sauv <- sauv[myvar]
sauv <- st_transform(sauv, 2163)
sauv

int <- as_tibble(st_intersection(sauv, isauv))
int$area <- st_area(int$geometry)
int
overlaps <- int %>%
  group_by(binomial) %>%
  summarise(area = sum(area))
int


#square of 2 x 2
pol = st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))
#add two more squares of 2 x 2
b = st_sfc(pol, pol + c(.8, .2), pol + c(4, .8))
b
plot(b)
l <- lapply( sauv, function(x) { # to get these working on the full data set probably just need to set it to df[2,]
  lapply(sauv, function(y) st_intersection( x, y ) %>% st_area() ) 
})
matrix(unlist(l), ncol = length(b), byrow = TRUE)
l2 <- lapply( sauv, function(x) { 
  lapply(sauv, function(y) st_intersection( x, y ) %>% st_area() * 100 /sqrt( st_area(x) * st_area(y) ) ) 
})
matrix(unlist(l2), ncol = length(b), byrow = TRUE)

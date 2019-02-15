#!/usr/bin/env Rscript 
# initial.R
# Initial script for working out how this is gonna work
# Going to need to rework out how to do all this using sf

###################dependencies##################
# Build: Ubuntu 18.04 on an unsigned kernel rtl8821ce (need it for wifi to work, go figure)

# If you are on Ubunutu 18.04 you will have to watch for non-zero exits on installs and 
# install via terminal for several packages and dependencies (usually package name followed by the -deb flag)

# install.packages("rgdal") # gdal is a real pain to get installed if R won't do it for you, so here's a couple of hopefully useful guides 

# Add this repository https://launchpad.net/~ubuntugis/+archive/ubuntu/ubuntugis-unstable it seems to be secure, using the code 
# sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable && sudo apt-get update

# If not you can try the following 
# ########WARNING########### DO NOT DO THIS unless you are familiar with linux and the dangers this comes with, see apt-secure(8) manpage and 
# sources.list(5) manpage for details 

# set [trusted=yes] flag example:
# deb [trusted=yes] http://ppa.launchpad.net/ubuntugis/ubuntugis-unstable/ubuntu bionic main
# and how to easily do it: 
# sudo gedit /etc/apt/sources.list.d/ubuntugis-ubuntu-ubuntu-ubuntugis-unstable-bionic.list

# I stress again that you shouldn't need to do it, but leave it here as the following repository did: 
# sudo add-apt-repository ppa:ubuntugis/ppa

# Next step is to coerce Ubuntu into finding the gdal-config file which R and python3 need to install their versions of gdal. 
# this is particularly useful in finding out what wrong https://stackoverflow.com/questions/12141422/error-gdal-config%20-not-found 

# For me the real fix was:
# apt-file search gdal-config
# sudo apt-get install libgdal1-dev
# sudo apt-get update 
# sudo apt-get upgrade

# this should hopefully get all dependencies into the right places and fill in any missing files such as the config. you're then good 
# to install rgdal via R CMD line 

# install.packages("devtools") # good for the not super stable or secure packages, built via their git repositories
# install.packages("rgdal","tidyverse","sf","ggplot2", "dplyr", "stringr", "ggforce", "maps", "mapdata") #won't 
# need all of these as tidyverse should cover most of that, but just so you know what gets used they're there
# git versions of the packages which were problematic to install via install.packages
#devtools::install_github("r-spatial/sf")
#devtools::install_github("dkahle/ggmap")
# install.packages("rgeos") # another one which is a pain to install, leave the PPA's as trusted (you'll need unstable for anything newer than Xenial)
# the missing package was libgeos-c1v5 in my case, so sudo apt-get install libgeos-c1v5 and you should be good to go. 
####################Imports###################### 
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
library(utils)
library(plyr)
###################Data_Wrangling################
####### NHM collections data #######
# perhaps a loop here to work through all data

df <- read.csv("Data/WorkingSouthAmerica.csv", header=T) #reading in the data from csv (have to set your own path)
colnames(df)[7] <- "binomial" # sets the ColName from UpdatedScientificName to binomial for easier times later on
df <- df %>% filter(Longitude != is.na(Longitude) & Locality != '' & Extent_km < 800 & binomial != '') #filtering the data (removing NA's as sf cannot parse data
# with NA's), and improving quality (removing things that were only found to country level) both 
# locality null and extent less than 800km, as well as removing blank spaces needed for that

raw_binom_list <- df %>% select(binomial)# useful for filtering the big dataset as it requires much less memory than the full df
binom_list <- list()
for(i in raw_binom_list){
  for(j in i){
    j <- gsub(' ', '_', j) # replaces spaces between spp and genus
    binom_list <- append(binom_list, j) # appends to the list 
  }
}

df_sp <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326) # does a neat trick and combines lat/long into one variable (geometry)
# and sets datum to WGS84 (thats the crs 4326 part)
write.csv(df_sp, file = "Data/SA_AMPH_NHM_DF.csv") # saving the spatial frame, possibly to send to python for faster operations


####### IUCN rangemap data #######
IUCN <- readOGR(dsn = "Data", layer = "AMPHIBIANS") # IUCN data isn't supplied in sf format, so has to be read in the old way
IUCN <- st_as_sf(IUCN) # but is readily transformed to sf format! 
IUCN <- st_transform(IUCN, 4326) # making sure its in the same CRS as the other data
#head(IUCN)

IUCN_binom <- IUCN %>% select(binomial) # this is actually probably not any faster than specifying which column on the whole data set
IUCN_binom_ns <- list()
for(x in IUCN_binom$binomial){
  for(y in x){
    y <- gsub(' ', '_', y) # replaces spaces between spp and genus
    IUCN_binom_ns <- append(IUCN_binom_ns, y)
  }
}

IUCN_binom_ns<- data.frame(matrix(unlist(IUCN_binom_ns), ncol=1))
colnames(IUCN_binom_ns)[1] <- "binomial"

head(IUCN_binom_ns)
class(IUCN_binom_ns)

IUCN_test <- IUCN #putting something here to stop having to load the main data set
class(IUCN)

#IUCN_test <- IUCN_test[,-2]
#IUCN_test <- join(IUCN_binom_ns, IUCN_test)

IUCN <- merge(IUCN_test, IUCN_binom_ns, by = "binomial", all.y =T) # replaces the binomal column with a space removed one 

head(IUCN_test$binomial)
class(IUCN_test)
head(IUCN)

write.csv(IUCN, file = "Data/IUCN_Spatial.csv") # saves it for use here or elsewhere in its updated form

IUCN_filtered <- array(NA, dim = c(length(unique(df_sp))))

for(name in IUCN$binomial){
  if(match(IUCN$binomial, binom_list)){
    IUCN_filtered <- append(IUCN_filtered, name)
  }
}
# maybe change the below to a for(unique(i) in binomial) ggplot
#IUCN <- IUCN %>% match(IUCN$binomial %in% df_sp$binomial)
#IUCN <- IUCN %>% which(IUCN$binomial %in% df_sp$binomial) # filtering the IUCN data to those that are found in the NHM data
IUCN <- IUCN %>% filter(binomial == "Pristimantis affinis") # when automating, look at hpc script as splitting into df based on spp will be similar to hpc  


#################Initial NHM visualization#############
# # be aware that right now, if looked at in terms of simple features, this is an XYM sf, as it incorporates a measure (extent radius)
# # equally this is all on an uncorrected datum (but the same one as the base map oddly enough)
# bc_bbox <- make_bbox(lat = Latitude, lon = Longitude, data = df) # computing a bound box using the data from the sheet
# # bound box is just fancy GIS language for the area of map to be made and displayed
# bc_bbox # displays extremes of the bound box
# bc_big <- get_map(location = bc_bbox, source = "google", maptype = "terrain") # gives the bound box to google maps, which converts it to a 
# # google made map centred nicely and made to a reasonable scale. 
# ggmap(bc_big) + #need to filter data to exclude that which is only to country level
#   geom_point(data = df, mapping = aes(x = Longitude, y = Latitude, size = Extent_km, color = ScientificName), alpha = 0.4, show.legend = FALSE) #makes a graphic out 
# # of the google created map using ggplot!
# # the method above has created area circles incorporating error, so should be able to use these to compute overlap 
# # as it stands because alpha is set low overlap of these points is being displayed by fill!



##########################################################THIS BIT WORKS! MAP ISN'T SUPER PRETTY BUT IT WORKS!###############################
# sf method of map making

df_km <- st_transform(df_sp, "+proj=utm +zone=42N +datum=WGS84 +units=km") # puts all into a km based projection 
buffer <- st_buffer(df_km, df_km$Extent_km) # st_buffer used like this computes a circle with radius specified in Extent
buffer <- st_transform(buffer, 4326) # transforms it back to coord based projection
sfbbox <- st_bbox(buffer) # getting the bbox but with the corrected datum
sfbbox #shows what this is (a min/max bounds for a map)
bbox <- unname(sfbbox) #pulls the column names out
xlim <- c(bbox[1], bbox[3]) # puts the coords into the order expected down in ggmap coords
ylim <- c(bbox[2], bbox[4])
SAMap <- st_as_sf(map("world", plot=F, fill=T)) #gets the specified map from maps package circumventing the need for google API
MapPlot <- ggplot(data = SAMap) + #plots it
  geom_sf() +
  geom_sf(mapping = aes(alpha = 0.1, color=buffer$ScientificName), data = buffer, shape = 1, show.legend =F) + 
  coord_sf(xlim = xlim, ylim = ylim, expand =T) # if adding iucn this line needs to be outside of object#zooms the map to encompass the extreme coords only, giving reasonable scale
MapPlot+geom_sf(mapping = aes(alpha = 0.1, color=IUCN$binomial), data = IUCN, show.legend = F)+coord_sf(xlim = xlim, ylim = ylim, expand =T)

# adding the IUCN shape file to the plot 
# data is too large to plot all of at once, so some pruning at this point is going to be needed (i.e. do it by species, one at a time)
# this, like everything else can and should be automated later so you just throw a dataset at it and it all works 
# maybe change the below to a for(unique(i) in binomial) ggplot
# IUCN <- IUCN %>% filter( binomial == "Pristimantis affinis") # when automating, look at hpc script as splitting into df based on spp will be similar to hpc  
# head(IUCN)
# IUCN_Map <- st_as_sf(map("world", plot=F, fill=T)) # making a map to see if this has worked so far 
# Ibbox <- st_bbox(IUCN) # getting the bbox but with the corrected datum
# Ibbox #shows what this is (a min/max bounds for a map)
# bbox <- unname(Ibbox) #pulls the column names out
# xlim <- c(bbox[1], bbox[3]) # puts the coords into the order expected down in ggmap coords
# ylim <- c(bbox[2], bbox[4])
# ggplot(data = IUCN_Map) + # giving it a plot 
#   geom_sf() +
#   geom_sf(mapping = aes(alpha = 0.1, color=IUCN$binomial), data = IUCN, show.legend = F) #+
#   #coord_sf(xlim = xlim, ylim = ylim, expand =T)



###############making error polygons################
#
# 
#
#
# This is where we will need https://r-spatial.github.io/sf/articles/sf1.html the following code will be useful
# geom = st_geometry(nc.web_mercator[sel,])
# buf <- st_buffer(geom, dist = 30000)
# plot(buf, border = 'red')
# plot(geom, add = TRUE)
# plot(st_buffer(geom, -5000), add = TRUE, border = 'blue')
#
#
#
###############adding error to the points###########
#
#
#
# for write up purposes; We have a point (zero dimensional geometry), and we have error around the point. Error can be thought of as a polygon
# (2D sequence of points forming a closed, non-intersecting ring). This works as we draw a cirle around the point at a determined radius, 
# and infer that we have found all the points along the circumference.
#
#
#
###############making range polygons#################
#
#
#
# may do this if I have time, as this could give a nicer overlap picture (percentage overlap and all that)
#
#
#
##############adding IUCN range maps to show overlap###############
#
#
#
# need a working copy of the IUCN range data, ask Natalie or work out how to open the stuff I downloaded
#
#
#
########comments questions and concerns######
#
#
#
# sf is not a package which works natively in R, it requires the gdal framework and drivers, as well as proj4 to be installed,
# R is simply used as an interface for another program really. As such this is unlikely to play nice with a lot of R stuff I think




#########################################messy sandbox#####################################

######plotting######
#
#
# below is very much messing around to see what fits. Almost certianly will need to get a google maps API key (effectively free, $200/month
# credit and 1000 map loads costs $0.50, but still a pain in the arse)
# need to get the base map (get_map) in the correct datum (dont know if theres an easy way to do this)
map <- get_map(location = unname(sfbbox)) # gets a map from the bbox after porviding it in a form that get_map expects
ggmap(map) + 
  geom_sf(data = df_sp, inherit.aes = F) # clearly this doesnt quite work. Datum is off by quite a margin

plot_sf(df_sp, bgMap = map)
install.packages('dismo')
install.packages('XML')
geocode('Latin America') # kinda cool but probably wrong in many ways
library(dismo)
library(XML)
dismo:gmap() # according to a help thread this might solve my issue
gmap("brazil")
# going to need to convert km to decimal degrees to add buffer, which will be instumental in doing nice overlap calcs I think.
# how to do it in python https://stackoverflow.com/questions/18150434/convert-from-kilometers-km-to-decimal-degrees maybe write and call a 
# python script?
#
#
#

  
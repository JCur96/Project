#!/usr/bin/env Rscript 
# functions.R
# script to hold the functions + what they're meant to do
# so that I have at least one working script to demo


correctColName <- function(df) {
  # needs usr input, so probably a 
  # colnames(df) # followed by a 
  # colToChange <- readline(prompt = 'please enter column index to rename to binomial: ')
  # colnames(df)[colToChange] <- 'binomial'
  colnames(df)[7] <- "binomial" # this one might need to be a little more general
}
colnames(NHM_AMPH)[7] <- "binomial" # sets the ColName from UpdatedScientificName to binomial for easier times later on

removeNA <- function(df) {
  df <- df %>% filter(Longitude !=is.na(Longitude)
                      & Locality != '' & Extent_km < 800 & binomial != '')
}
NHM_AMPH <- NHM_AMPH %>% filter(Longitude != is.na(Longitude) 
                                & Locality != '' & Extent_km < 800 & binomial != '') 
# filtering the data (removing NA's as sf cannot parse data
# with NA's), and improving quality (removing things that were only found to country level) both
# locality null and extent less than 800km, as well as removing blank spaces needed for that
prepBinomial <- function(df) {
  df$binomial <- gsub(' ', '_', df$binomial)
}
NHM_AMPH$binomial <- gsub(' ', '_', NHM_AMPH$binomial) # removes spaces

# Combines lat/long into one variable (geometry)
# and sets datum to WGS84 (thats the crs 4326 part)
setCRS <- function(df) {
  df <- st_as_sf(df, coords = c('Longitide', 'Latitude', crs = 4326))
}
NHM_AMPH <- st_as_sf(NHM_AMPH, coords = c("Longitude", "Latitude"), crs = 4326) 

# saving the spatial frame
write.csv(NHM_AMPH, file = "../Data/SA_AMPH_NHM_DF.csv") 


####### IUCN rangemap data #######
# IUCN data isn't supplied in sf format, so has to be read in the old way
inReadOld <- function(df) {
  df <- readOGR(dsn = '../Data', layer = '') # this one will require some thinking
}
IUCN <- readOGR(dsn = "../Data", layer = "AMPHIBIANS") 
# but is readily transformed to sf format!
oldAsNew <- function(df) {
  df <- st_as_sf(df)
  return(df)
}
IUCN <- st_as_sf(IUCN) 
# making sure its in the same CRS as the other data
IUCN <- st_transform(IUCN, 4326) 
# pulling out the parts we actually need
# myvars <- c("binomial", "SHAPE_Leng", "SHAPE_Area", "geometry") 
reduceOldToNeeded <- function(df) {
  myvars <- c('binomial', 'TypeStatus', 'geometry')
  df$TypeStatus <- 'IUCN'
  df <- df[myvars]
  return(df)
}
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

matchBinomial <- function(df1, df2) {
  binomList <- df1$binomial
  df2 <- df2 %>% filter(binomial %in% binomList)
  df1 <- df1 %>% filter(binomial %in% df2)
  return(df1, df2)
}

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
addError <- function(df) {
  myvars <- c("binomial", "TypeStatus", "geometry") 
  df <- st_transform(df, '+proj=utm +zone=42N +datum=WGS84 +units=km')
  df <- st_buffer(df, df$Extent_km)
  df <- st_transform(df, 4326)
  df <- df[myvars]
  return(df)
}
# transforms it back to coord based projection
buffer <- st_transform(buffer, 4326) 
filtered_buffer <- buffer[myvars]

# making sensible bounds for a map
boundMap <- function(df) {
  sfbbox <- st_bbox(df)
  bbox <- unname(sfbbox)
  xlim <- c(bbox[1], bbox[3]) # puts the coords into the order expected down in ggmap coords
  ylim <- c(bbox[2], bbox[4])
  return(xlim, ylim)
}

sfbbox <- st_bbox(buffer) # getting the bbox but with the corrected datum
sfbbox #shows what this is (a min/max bounds for a map)
bbox <- unname(sfbbox) #pulls the column names out
xlim <- c(bbox[1], bbox[3]) # puts the coords into the order expected down in ggmap coords
ylim <- c(bbox[2], bbox[4])


## binomial names as individual var's
## I dont think this is adding all the points we have to it, only the first one
## need to fix that, maybe a match == T type statement in there
plotMaps <- function(df1, df2) { # replace the png(paste) with a prompt eg  path <- readline(prompt = 'enter path: ')
  for (var in unique(df1$binomial)) {
    IUCN_var <- df2[df2$binomial == var,]
    NHM_var <- df1[df1$binomial == var,]
    p = ggplot(data = baseMap) +
      geom_sf() +
      geom_sf(mapping = aes(alpha = 0.5, fill='blue'), data = NHM_var, show.legend = F) +
      geom_sf(mapping = aes(alpha = 0.1, fill = "red"), data = IUCN_var, show.legend = F) + # adding IUCN maps
      coord_sf(xlim = xlim, ylim = ylim, expand = T) # zooming to correct cords (South America)
    # path <- readline(prompt = 'Enter path: ')
    # png(paste(path, var, '.png', sep=''). width=600, height=500, res=120)
    png(paste("../Output/IUCN_Range_Graphs/Graph_", var, ".png", sep=""), width=600, height=500, res=120) #saving map as png
    print(p) # printing to png
    dev.off() # not sending to screen
  }
}

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

#' makes convex hulls for species present and correctly fills in the same hull for the same spp entry
#' eg all P. buckleyi have the same hull as they should
makeHulls <- function(df) {
  for (var in unique(df$binomial)) { 
    subsetOfDf <- df[df$binomial == var,]
    subsetOfDf$convex_hull <- st_convex_hull(st_combine(subusetOfDf$geometry))
    df <- rbind(subsetOfDf, df) 
    
  }
  
}

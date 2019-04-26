# testing all functions out together
### imports ###
library(tidyverse)
library(sf)
library(maps)
library(rgeos)
library(lwgeom)
library(rgdal)


mydata <- read.csv('../Data/WorkingSouthAmerica.csv', header=T)
correctColName(mydata)
removeNA(mydata)
prepBinomial(mydata)
setCRS(mydata)
addError(mydata) # this does the bareminimum step, might want to change that

inReadOld()
oldAsNew(IUCNData)
bareMinimumVars(IUCNData)
prepBinomial(IUCNData)

matchBinomial(mydata, IUCNData)

makeMap(mydata)
boundMap(mydata)
plotMaps(mydata, IUCNData)

full_overlaps(mydata, IUCNData)
### tested working up to here ###


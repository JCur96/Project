#!/usr/bin/env Rscript 
# datastorr.R
# test script for datastorr functionality 
# will use to demo feasibility of datastorr

#####Imports#####
# devtools::install_github("richfitz/datastorr")
devtools::install_github('ropenscilabs/datastorr')

####Data#####
datastorr::autogenerate(repo = 'JCur96/MRes_Data', read = 'read.csv', name = 'mydata') # generates code for adding to package


###Example from the datastorr GitHub####
devtools::install_github("richfitz/datastorr.example")
datastorr.example::mydata_versions()
datastorr.example::mydata_versions(local=FALSE) # remote

#### testing with my data repo 
devtools::install_github("JCur96/Project") # if I'm understanding this correctly
# you must install a package from github, sans data
# and then you can check on what the data is
MRes_Data::mydata_version()

#!usr/bin/R
# Analysis testing file for MRes project
# analysis.R
# I will make and test functions here


# most basic analysis is a simple glmm
# think Natalie advised using mcmcglmm
# need to look that up



# moving on from there its centroid-centroid distances
# which I know nothing about so that could be interesting
# could also look at centroid-edge distances
# again I'll have to search sf and the web for more info


# and finally potentially the most complex method
# is to look at convex hulls and extent of
# occupancy, with clipping of hulls to landmass

# so without further ado, lets get that nicely trimmed data in
df <- MRes::mydata('0.1.2')


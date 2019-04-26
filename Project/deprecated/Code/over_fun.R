over_fun <- function(df1, df2) {
  df1[,"Percent_overlap"] <- NA # adds a column of na's
  for (row in 1:nrow(df1)) { # for each row in first df's geometry col
    geom <- df1$geometry[row] # extract the geometry
    x <- overlaps(geom, df2$geometry) # use previous fun to calculate overlaps
    df1$Percent_overlap[row] <- x # and append to the percent overlap col 
  }
  return(df1) # return the modified df for use in another fun 
}

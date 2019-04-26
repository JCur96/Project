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

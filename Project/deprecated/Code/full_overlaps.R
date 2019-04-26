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

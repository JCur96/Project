setCRS <- function(df) {
  df <- st_as_sf(df, coords = c('Longitude', 'Latitude'), crs = 4326)
  assign('mydata',df,envir=.GlobalEnv)
}



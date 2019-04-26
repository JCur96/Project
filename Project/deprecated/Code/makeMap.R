makeMap <- function(df) {
  baseMap <- st_as_sf(map('world', plot=F, fill=T))
  assign('baseMap',baseMap,envir=.GlobalEnv)
  # return(baseMap)
}


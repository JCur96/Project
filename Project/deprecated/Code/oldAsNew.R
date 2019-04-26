oldAsNew <- function(df) {
  df <- st_as_sf(df)
  df <- st_transform(df, 4326)
  assign('IUCNData',df,envir=.GlobalEnv)
}


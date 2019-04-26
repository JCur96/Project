addError <- function(df) {
  myvars <- c("binomial", "TypeStatus", "geometry") 
  df <- st_transform(df, '+proj=utm +zone=42N +datum=WGS84 +units=km')
  df <- st_buffer(df, df$Extent_km)
  df <- st_transform(df, 4326)
  df <- df[myvars]
  assign('mydata',df,envir=.GlobalEnv)
  # return(df)
}


removeNA <- function(df) {
  df <- df %>% filter(Longitude !=is.na(Longitude)
                      & Locality != '' & Extent_km < 800 & binomial != '')
  assign('mydata',df,envir=.GlobalEnv)
}


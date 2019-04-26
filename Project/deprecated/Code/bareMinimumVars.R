bareMinimumVars <- function(df) {
  myvars <- c('binomial', 'TypeStatus', 'geometry')
  df$TypeStatus <- 'IUCN'
  df <- df[myvars]
  dfName <- readline(prompt = 'please enter df name: ')
  assign(dfName,df,envir=.GlobalEnv)
}


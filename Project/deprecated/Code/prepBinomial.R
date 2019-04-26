prepBinomial <- function(df) {
  df$binomial <- gsub(' ', '_', df$binomial)
  dfName <- readline(prompt = 'please enter your df name: ') 
  assign(dfName,df,envir=.GlobalEnv)
}

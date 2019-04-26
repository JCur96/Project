correctColName <- function(df) {
  # needs usr input, so probably a 
  print(colnames(df)) # followed by a 
  colToChange <- readline(prompt = 'please enter column index to rename to binomial: ')
  colToChange <- as.numeric(colToChange)
  colnames(df)[colToChange] <- 'binomial'
  #argName <- deparse(substitute(df))
  #print(argName)
  #varName <- paste(argName, sep='')
  #print(varName)
  assign('mydata',df,envir=.GlobalEnv) # might want to look at dynamic naming
  
}
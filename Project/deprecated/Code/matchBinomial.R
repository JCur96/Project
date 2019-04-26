matchBinomial <- function(df1, df2) {
  binomList <- df1$binomial
  df2 <- df2 %>% filter(binomial %in% binomList)
  df1 <- df1 %>% filter(binomial %in% df2$binomial)
  assign('mydata',df1,envir=.GlobalEnv)
  assign('IUCNData',df2,envir=.GlobalEnv)
}


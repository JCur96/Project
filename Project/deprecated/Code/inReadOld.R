inReadOld <- function() {
  path <- readline(prompt = 'please enter path to IUCN data: ')
  path <- file.path(path)
  name <- readline(prompt = 'please enter the name of the layer, e.g. AMPHIBIANS: ')
  print('This may take some time')
  df <- readOGR(dsn = path, layer = name)
  assign('IUCNData',df,envir=.GlobalEnv)
}

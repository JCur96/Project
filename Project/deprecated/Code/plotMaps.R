plotMaps <- function(df1, df2) { 
  path <- readline(prompt = 'Enter path: ')
  path <- file.path(path)
  # print('This will take some time...')
  # pb <- txtProgressBar(min = 0, max = length(var), initial = 0,  style = 3) # building a progress bar
  for (var in unique(df1$binomial)) {
    # Sys.sleep(0.1) # for a progress bar
    IUCN_var <- df2[df2$binomial == var,]
    NHM_var <- df1[df1$binomial == var,]
    p = ggplot(data = baseMap) +
      geom_sf() +
      geom_sf(mapping = aes(alpha = 0.5, fill='blue'), data = NHM_var, show.legend = F) +
      geom_sf(mapping = aes(alpha = 0.1, fill = "red"), data = IUCN_var, show.legend = F) + # adding IUCN maps
      coord_sf(xlim = xlim, ylim = ylim, expand = T) # zooming to correct cords (South America)
    png(paste(path, var, '.png', sep=''), width=600, height=500, res=120)
    # png(paste("../Output/IUCN_Range_Graphs/Graph_", var, ".png", sep=""), width=600, height=500, res=120) #saving map as png
    print(p) # printing to png
    # setTxtProgressBar(pb, var) # updating the progress bar
    dev.off() # not sending to screen
  }
}

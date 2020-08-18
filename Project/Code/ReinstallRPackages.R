packages <- installed.packages()[,"Package"]
save(packages, file="Rpackages")


load("Rpackages") 
for (p in setdiff(packages, installed.packages()[,"Package"]))
  install.packages(p)

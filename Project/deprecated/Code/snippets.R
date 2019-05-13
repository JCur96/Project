GeoSpatialEcology::mydata_versions()

GeoSpatialEcology::mydata_versions(local = F)



``` {r echo=FALSE, results="asis"}
pkg <- datastorr:::autogenerate(repo="JCur96/MRes_Data", read="readRDS", name="mydata")
writeLines(c("```r", pkg,  "```"))

a5f537f2bab62c40b766c47adda1adfdfb4a071f





mydata <- read.csv('../deprecated/Data/WorkingSouthAmerica.csv', header=T)
correctColName(mydata)

datastorr::datastorr_auth()
datastorr::setup_github_token()

i <- datastorr::github_release_info("JCur96/GeoSpatialEcology", read.csv)
datastorr::github_release_versions(i, local = F)

GeoSpatialEcology::mydata_release('version 0.0.1, first test data', '')

devtools::install_github("JCur96/MRes_Data", auth_token = "a5f537f2bab62c40b766c47adda1adfdfb4a071f")
MRes_Data::mydata_versions()

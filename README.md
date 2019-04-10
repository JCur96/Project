# Project
MRes project 
Shifting baselines in conservation: developing spatial methods and analysis pipelines in R

###################dependencies##################
Build: Ubuntu 18.04 on an unsigned kernel rtl8821ce (need it for wifi to work, go figure)

If you are on Ubunutu 18.04 you will have to watch 
for non-zero exits on installs and 
install via terminal for several packages and dependencies 
(usually package name followed by the -deb flag)

install.packages("rgdal") 
if R won't install it here are some useful guides 

Add this repository https://launchpad.net/~ubuntugis/+archive/ubuntu/ubuntugis-unstable 
using the code: sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable && sudo apt-get update
 
If that doesn't solve it then do the following
#########WARNING########### 
DO NOT DO THIS unless you are familiar with linux and 
the dangers this comes with, see apt-secure(8) manpage and 
sources.list(5) manpage for details 

set [trusted=yes] flag. Example:
deb [trusted=yes] http://ppa.launchpad.net/ubuntugis/ubuntugis-unstable/ubuntu bionic main
and how to easily do it: 
sudo gedit /etc/apt/sources.list.d/ubuntugis-ubuntu-ubuntu-ubuntugis-unstable-bionic.list

repository: 
sudo add-apt-repository ppa:ubuntugis/ppa

Next step is to coerce Ubuntu into finding the gdal-config file which 
R and python3 need to install their versions of gdal. 
The following helped me work out what went wrong:
https://stackoverflow.com/questions/12141422/error-gdal-config%20-not-found 

For me the real fix was:
apt-file search gdal-config
sudo apt-get install libgdal1-dev
sudo apt-get update 
sudo apt-get upgrade

this should hopefully get all dependencies into the right 
places and fill in any missing files such as the config. you're then good 
to install rgdal via R CMD line 

install.packages("devtools") # good for the not super stable or secure packages, built via their git repositories
install.packages("rgdal","tidyverse","sf","ggplot2", "dplyr", "stringr", "ggforce", "maps", "mapdata") 
won't 
need all of these as tidyverse should cover most of that, but just so you know what gets used they're there
git versions of the packages which were problematic to install via install.packages
devtools::install_github("r-spatial/sf")
devtools::install_github("dkahle/ggmap")
install.packages("rgeos") # another one which is a pain to install, 
leave the PPA's as trusted (you'll need unstable for anything newer than Xenial)
 the missing package was libgeos-c1v5 in my case, so sudo apt-get install libgeos-c1v5 and you should be good to go. 

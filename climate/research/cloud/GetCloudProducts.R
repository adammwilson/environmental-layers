## download cloud products from GEWEX for comparison
setwd("~/acrobates/adamw/projects/cloud/")
library(rasterVis)


## get PATMOS-X 1-deg data
dir1="data/gewex/"

for(y in 2000:2009) system(paste("wget -nc -P ",dir1," http://climserv.ipsl.polytechnique.fr/gewexca/DATA/instruments/PATMOSX/variables/CA/CA_PATMOSX_NOAA_0130PM_",y,".nc.gz",sep=""))
## decompress
lapply(list.files(dir1,pattern="gz",full=T),function(f) system(paste("gzip -dc < ",f," > ",dir1," ",sub(".gz","",basename(f)),sep="")))
## mergetime
system(paste("cdo -mergetime ",list.files(dir1,pattern="nc$",full=T)," ",dir1,"CA_PATMOSX_NOAA.nc",sep=""))


## Get 


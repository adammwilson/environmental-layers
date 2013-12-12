## download cloud products from GEWEX for comparison
setwd("~/acrobates/adamw/projects/cloud/")

## get PATMOS-X data
for(y in 2000:2009) system(paste("wget -nc -P data/gewex http://climserv.ipsl.polytechnique.fr/gewexca/DATA/instruments/PATMOSX/variables/CA/CA_PATMOSX_NOAA_0130PM_",y,".nc.gz",sep=""))

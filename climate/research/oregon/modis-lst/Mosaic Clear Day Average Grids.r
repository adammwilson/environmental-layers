# Mosaics two tiles of daily average clear day coverage for the clear day
# MODIS layer extracted from the MODIS LST files
#
# Reads a directory of .img files and mosaics the h08 and h09 tiles together
#
# Developed by John Donoghue
# Created: 20 October 2010
# Last Updated:
#
# For NCEAS Working Group Environment and Organisms
#

#setwd("/data/project/organisms/R")
#source("Mosaic Clear Day Average Grids.r", echo=TRUE, print.eval=TRUE)

setwd("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL")
library(raster)

for(i in 1:366){
# get the two tiles that comprise each day of the year
myPattern <- paste("Day_", toString(i), "_.*img$", sep="")
print(myPattern)
inFiles <- list.files(pattern=myPattern)
nFiles <- length(inFiles)
print (nFiles)

# should only be two tiles at this point
h08Raster <- raster()
h08Raster <- raster(inFiles[1])
print(sprintf("Reading file: %s", inFiles[1]))
h09Raster <- raster()
h09Raster <- raster(inFiles[2])
print(sprintf("Reading file: %s", inFiles[2]))

newRaster <- merge(h08Raster, h09Raster)

# Check if file exists
outFile <- paste("Day_",toString(i),"_ClearDay_Average",".img",sep="")
if (file.exists(outFile) == FALSE) {
print(sprintf("...Writing file: %s",outFile))
writeRaster(newRaster,filename=outFile,format="HFA",overwrite=TRUE)
}
}

# finished
print("Finished")


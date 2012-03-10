#setwd("/data/project/organisms/R")
#source("ReClass Clear Day Rasters.r", echo=TRUE, print.eval=TRUE)
setwd("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL")
outPath = "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/Scaled/"
library(raster)
inFiles <- list.files(pattern="*.img$")
myFiles <- grep("h08v04",substr(inFiles, 18,23))
nFiles <-  length(myFiles)
inputRaster <- raster()
for (iCtr in 1 : nFiles)
{
message(sprintf("Reading file: %s",inFiles[iCtr]))
inputRaster <- raster(inFiles[iCtr])
calcRaster <- inputRaster * 0.0005
outFile <- substr(inFiles[iCtr],1,36)
outFile <- trim(outFile)
outFile <- paste(outFile, "_Scaled.img", sep="")
outPathFile <- paste(outPath, outFile)
# Check if file exists and write raster if it does not
if (file.exists("ReClass Clear Day Rasters.r") == FALSE) {
message(sprintf("..Recalculating file: %s",outFile))
writeRaster(calcRaster,filename=outPathFile,format="HFA",overwrite=TRUE) 
}
}


# Calculates the daily average clear day coverage for the clear day
# MODIS layer extracted from the MODIS LST files
#
# Reads a directory of .img files and groups them by day to calculate
# a daily aveage. This script also applies the clear day scaling factor
# to the resulting average raster and writes one raster for each day
# of the 1-year 2000 to 2010 period (one file for each 10-year group)
#
# Developed by John Donoghue
# Created: 16 October 2010
# Last Updated:
#
# For NCEAS Working Group Environment and Organisms
#

#setwd("/data/project/organisms/R")
#source("Calc Clear Day Daily Avg.r", echo=TRUE, print.eval=TRUE)

setwd("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL")
inFiles <- list.files(pattern="*.img$")

library(raster)

print("")
print("Processing h08v04 tiles")
print("")

# process h08v04 tiles
# Create day averages for each day in the year
for(i in 1:366){
if (i < 9) {f <- "00"}
if ((i > 9) & (i <99 )) {f <- "0"}
if (i > 99) {f <- ""}
fpart <- paste(toString(f), toString(i), sep="")

# get a list of all files for that day of the year
print("")
print(sprintf("Reading day: %s", toString(fpart)))
myFiles <- grep(fpart,substr(inFiles, 14,16))
nFiles <-  length(myFiles)

# for each file in the list
filestoStack <- list()
for (iCtr in 1 : nFiles)
{
mFile <- myFiles[iCtr]
if (substr(inFiles[mFile],18,23) == "h08v04") {
print(sprintf("..Reading file: %s",inFiles[mFile]))
inputRaster <- raster()
inputRaster <- raster(inFiles[mFile])
filestoStack <- c(filestoStack, inFiles[mFile])
}
}
# now calculate the mean cell values
myStack <- stack(filestoStack)
print(sprintf("....Averaging Daily Values"))

# Check if file exists
outFile <- paste("Day_",toString(i),"_Average_Scaled_h08v04",".img",sep="")
if (file.exists(outFile) == FALSE) {
meanRaster <- mean(myStack, na.rm=TRUE)
scaledRaster <- meanRaster * 0.0005 # apply scaling factor to raster
print(sprintf("......Writing file: %s",outFile))
writeRaster(scaledRaster,filename=outFile,format="HFA",overwrite=TRUE)
}
}

print("")
print("Processing h09v04 tiles")
print("")

# now reloop and process h09v04 tiles
# Create day averages for each day in the year
for(i in 1:366){
if (i < 9) {f <- "00"}
if ((i > 9) & (i <99 )) {f <- "0"}
if (i > 99) {f <- ""}
fpart <- paste(toString(f), toString(i), sep="")

# get a list of all files for that day of the year
print("")
print(sprintf("Reading day: %s", toString(fpart)))
myFiles <- grep(fpart,substr(inFiles, 14,16))
nFiles <-  length(myFiles)

# for each file in the list
filestoStack <- list()
for (iCtr in 1 : nFiles)
{
mFile <- myFiles[iCtr]
if (substr(inFiles[mFile],18,23) == "h09v04") {
print(sprintf("..Reading file: %s",inFiles[mFile]))
inputRaster <- raster()
inputRaster <- raster(inFiles[mFile])
filestoStack <- c(filestoStack, inFiles[mFile])
}
}
# now calculate the mean cell values
myStack <- stack(filestoStack)
print(sprintf("....Averaging Daily Values"))

# Check if file exists
outFile <- paste("Day_",toString(i),"_Average_Scaled_h09v04",".img",sep="")
if (file.exists(outFile) == FALSE) {
meanRaster <- mean(myStack, na.rm=TRUE)
scaledRaster <- meanRaster * 0.0005 # apply scaling factor to raster
print(sprintf("......Writing file: %s",outFile))
writeRaster(scaledRaster,filename=outFile,format="HFA",overwrite=TRUE)
}
}

# finished
print("Finished")


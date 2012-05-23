# Calculates the monthly average clear day coverage for the clear day
# MODIS layer extracted from the MODIS LST files
#
# Reads a directory of .img files and groups them by month to calculate
# a monthly aveage. This script also applies the clear day scaling factor
# to the resulting average raster and writes one raster for each month
# of the 1-year 2000 to 2010 period (one file for each month of the
# 10-year group)
#
# Developed by John Donoghue
# Created: 20 October 2010
# Last Updated:
#
# For NCEAS Working Group Environment and Organisms
#

#setwd("/data/project/organisms/R")
#source("Calc Clear Day Monthly Avg.r", echo=TRUE, print.eval=TRUE)

wkDir <- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate"
setwd(wkDir)

inFiles <- list.files(pattern="*h08v04.005.ClearDay.img$")
nFiles <- length(inFiles)

library(raster)

# build a list of the files for each month
janList <- list()
febList <- list()
marList <- list()
aprList <- list()
mayList <- list()
junList <- list()
julList <- list()
augList <- list()
sepList <- list()
octList <- list()
novList <- list()
decList <- list()

# for each file in the list
for (i in 1:nFiles) {

#print(inFiles[i])

# get month of file
theMonth <- substr(inFiles[i], 16,17)
if (substr(theMonth, 2,2) == ".") {theMonth <- substr(theMonth, 1, 1)}

# group by month
if (theMonth == "1") {
a <- length(janList)
janList[a+1] <- inFiles[i]
}

if (theMonth == "2") {
a <- length(febList)
febList[a+1] <- inFiles[i]
}

if (theMonth == "3") {
a <- length(marList)
marList[a+1] <- inFiles[i]
}

if (theMonth == "4") {
a <- length(aprList)
aprList[a+1] <- inFiles[i]
}

if (theMonth == "5") {
a <- length(mayList)
mayList[a+1] <- inFiles[i]
}

if (theMonth == "6") {
a <- length(junList)
junList[a+1] <- inFiles[i]
}

if (theMonth == "7") {
a <- length(julList)
julList[a+1] <- inFiles[i]
}

if (theMonth == "8") {
a <- length(augList)
augList[a+1] <- inFiles[i]
}

if (theMonth == "9") {
a <- length(sepList)
sepList[a+1] <- inFiles[i]
}

if (theMonth == "10") {
a <- length(octList)
octList[a+1] <- inFiles[i]
}

if (theMonth == "11") {
a <- length(novList)
novList[a+1] <- inFiles[i]
}

if (theMonth == "12") {
a <- length(decList)
decList[a+1] <- inFiles[i]
}

# end list files
}

print("")
print("PROCESSING h08v04 FILES")
print("")

# for each file in the list
for (f in 1:12) {
if (f == 1) {myList <- janList}
if (f == 2) {myList <- febList}
if (f == 3) {myList <- marList}
if (f == 4) {myList <- aprList}
if (f == 5) {myList <- mayList}
if (f == 6) {myList <- junList}
if (f == 7) {myList <- julList}
if (f == 8) {myList <- augList}
if (f == 9) {myList <- sepList}
if (f == 10) {myList <- octList}
if (f == 11) {myList <- novList}
if (f == 12) {myList <- decList}

print ("")
print ("")

filestoStack <- raster()
for (iCtr in 1 : length(myList))
{
mFile <- myList[iCtr]
print(sprintf("..Reading file: %s",mFile))
myFile <- paste(wkDir, "/", mFile, sep="")
inputRaster <- raster()
inputRaster <- mFile
filestoStack <- c(filestoStack, inputRaster)
}

# now calculate the mean cell values
print("....Stacking Grids")
myStack <- stack(filestoStack)

# Check if file exists
outFile <- paste("Month_", f ,"_ClearDay_Average",".img",sep="")
if (file.exists(outFile) == FALSE) {
print("....Averaging Monthly Values")
meanRaster <- mean(myStack, na.rm=TRUE)
print("....Applying Scale Factor")
scaledRaster <- meanRaster * 0.0005 # apply scaling factor to raster
print(sprintf("......Writing file: %s",outFile))
writeRaster(scaledRaster,filename=outFile,format="HFA",overwrite=TRUE)
}
}

# finished
print("Finished")


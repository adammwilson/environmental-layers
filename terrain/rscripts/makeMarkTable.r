##############################################################################################
#
# makeMarkTable.r
# 
# R script generates table of adjacent ASTER / SRTM / Mosaic Border / CDEM pixel values
# to be used to generate plots of 'delta elevation' vs 'pixel pair proximity' and 'elevation'
# suggested by Mark Schildhauer on May 3. 
#
# Author: Rick Reeves, NCEAS
# May 4, 2011
##############################################################################################
#
makeMarkTable <- function()
{
require(raster)
require(rgdal)

#inputFirstRaster <- raster(sFirstImageName)
#inputSecondRaster <- raster(sSecondImageName)

inputCgiarRaster  <- raster("/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdySRTM_BL.tif")
inputAsterRaster  <- raster("/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdyASTER_BL.tif")
inputMosaicRaster <- raster("/data/project/organisms/rcr/ValidateBoundary/mergeCgiarAsterBdyTuesdayClip.tif")
inputCDEMRaster   <- raster("/data/project/organisms/rcr/ValidateBoundary/CDemMosTuesdayClipMergeSpace.tif")
#
# Difference image for entire merged image takes a while to create, 
# so we created it once, now read it back in.
#
rDeltaWhole <- raster("/data/project/organisms/rcr/ValidateBoundary/DeltaEntireImage.tif")
rDeltaWhole@data@values <-getValues(rDeltaWhole)

# Create extent objects used to extract raster subimges. 
# The object will be centered along the 60 degree North latitude line,
# and have varying depths (number of rows).
# The Western Canada study area runs from -135 (west) to -100 (west) longitude,
# and 55.0 to 64.00 degrees (north) latitude. 
# the ASTER and SRTM/CGIAR image components are merged at the 60 Deg N Latitude line.

#eTestAreaExtent <- extent(-135.2,-100.2, 59.997,60.001) # Creates a 5 row subimage
eTestAreaExtent <- extent(-135.2,-100.2, 59.995,60.005) # Creates a 12 row subimage

# Extract a sub image corresponding to the selected extent.
# Two different alternatives:
# The extract() function returns a vector of cell values, 
# the crop() function returns a complete raster* object.

#vEdgeRegionMosaic <- extract(inputMosaicRaster,eTestAreaExtent)
#vEdgeRegionCDEM <- extract(inputCDEMRaster,eTestAreaExtent)

rEdgeRegionAster <- crop(inputAsterRaster,eTestAreaExtent)
rEdgeRegionCgiar <- crop(inputCgiarRaster,eTestAreaExtent)
rEdgeRegionMosaic <- crop(inputMosaicRaster,eTestAreaExtent)
rEdgeRegionCDEM <- crop(inputCDEMRaster,eTestAreaExtent)

# Important: In order for the image subtraction to work, the extents
#            of the two images must be IDENTICAL. I used ArcMap GIS Raster Crop By Mask
#            to create subimages with identical extents. 

# Compute the difference image  for the entire study area, and for the region along
# the boundary (narrow, maybe 10 pixels either side)

rEdgeRegionDelta <- rEdgeRegionMosaic - rEdgeRegionCDEM  # not used in this version (yet)

# Create this image one time, read it in thereafter.
#rDeltaWhole <- inputMosaicRaster - inputCDEMRaster
#writeRaster(rDeltaWhole,filename="DeltaMosaicCDEMSubmage.tif",format="GTiff",datatype="INT2S",overwrite=TRUE)

# Using the large difference image, compute subimagee statistics for areas
# North (ASTER) and South (SRTM) of the boundary. These give us an idea 
# re: differences between ASTER and CDEM and CGIAR/SRT and CDEM
# what is raster package way of using subscripts to extract? 

# Now, the interesting part: using the boundary difference image, randomly select 
# one-degree N-S strips throughout the image, and compare adjacent pixel pairs 
# above and below the boundary with pixel pairs straddling the boundary. Subtract  
# the pairs, save the collection of (absolute value) of the differences in a vector,
# so that we have a population of differences above, below, and straddling the boundary 
# line. Compare the populations.

# get a vector of random column index numbers, constrained by column dimension of image
# Loop three times, sampling pixel pairs from above, below, across the border

nColsToGet <- 1000
iDiffVecNorth <- vector(mode="integer",length=nColsToGet)
iDiffVecBorder <- vector(mode="integer",length=nColsToGet)
iDiffVecSouth <- vector(mode="integer",length=nColsToGet)

#colsToGet <-sample(1:50,nColsToGet)

# Note: initially, sample the same columns in all regions to get a profile.
#       other 'sample()' calls can be commented out to sample differenct
#       coluns in each 'region'.

# iDiffVecxxxx is a population of differences between adjacent cell pairs. 
# Compute iDiffVecNorth/Border/South on either side of border, and across it. 
# note that North and South samples taken from larger difference image for 
# entire mosaic (sub) image; iDiffBorder taken from the edge region extracted
# from the center of the lerger image.

# Remember, we are sampling a PAIR of pixels (same column from two adjacent rows)

#browser()
# debug
#nColsToGet <- 2
#colsToGet <- c(20,100)
#iFirstRow <- 300
#iCtr = 1
#for (iNextCol in colsToGet)
#{
#  rColVec <- cellFromRowCol(rDeltaWhole,iFirstRow:(iFirstRow+1),iNextCol:iNextCol)
# neighborCells <- rDeltaWhole@data@values[rColVec]
#  iDiffVecNorth[iCtr] <- neighborCells[2] - neighborCells[1]
#iCtr = iCtr + 1
#}
#

# Here is the output matrix, (nColSamples * 3) rows, four columns 
colsToGet <-sample(1:inputMosaicRaster@ncols,nColsToGet)
message("North Sample")

mOutTable <- matrix(nrow=(nColsToGet * 3), ncol=5)
colnames(mOutTable) <- c("ColumnID","elevNorth","elevSouth","cdemNorth","cdemSouth")
# in this difference image, the border edge occurs at column 6
message("Border Sample - different columns")
#browser()
#colsToGet <-sample(1:inputFirstRaster@ncols,nColsToGet)
iFirstRow <- 4 # two rows before the border           
iLastRow  <- 7 # two rows after the border
iRowCtr = 1 # points to latest output table being written
for (iNextCol in colsToGet)
{
  rColVecAster  <- cellFromRowCol(rEdgeRegionAster,iFirstRow:(iLastRow),iNextCol:iNextCol)
  rColVecCgiar  <- cellFromRowCol(rEdgeRegionCgiar,iFirstRow:(iLastRow),iNextCol:iNextCol)
  rColVecMosaic <- cellFromRowCol(rEdgeRegionMosaic,iFirstRow:(iLastRow),iNextCol:iNextCol)
  rColVecCDEM   <- cellFromRowCol(rEdgeRegionCDEM,iFirstRow:(iLastRow),iNextCol:iNextCol)  
  
# Split the column vector into the pairs that Mark requested 
# For each column sampled, the output table has three rows:
#
# Row 1: from (input) Aster layer:  Two pixels above border
# Row 2: from (input) Mosaic layer: Two pixels straddling border
# Row 3: from (input) Cgiar layer:  Two pixels below border
#
# each with four columns, arranged into two column pairs:
#
#  North Pixel and South Pixel elevation, North Pixel and South Pixel CDEM (baseline)
# 
  mOutTable[iRowCtr][1]   <- iNextCol           # The (radomly sampled) col ID (surrogate for longitude)
  mOutTable[iRowCtr][2:3] <- rColVecAster[1:2]  # First column pair from extracted vector: 
  mOutTable[iRowCtr][4:5] <- rColVecCDEM[1:2]   # entirely top (ASTER) image 
  iRowCtr = iRowCtr + 1
#  
  mOutTable[iRowCtr][1]   <- iNextCol           # The (radomly sampled) col ID (surrogate for longitude)
  mOutTable[iRowCtr][2:3] <- rColVecMosaic[3:4] # Second column pair: 
  mOutTable[iRowCtr][4:5] <- rColVecCDEM[3:4]   # straddles border region (from Mosaic image)
  iRowCtr = iRowCtr + 1  
#  
  mOutTable[iRowCtr][1]   <- iNextCol           # The (radomly sampled) col ID (surrogate for longitude)
  mOutTable[iRowCtr][2:3] <- rColVecCgiar[5:6]  # third column pair:
  mOutTable[iRowCtr][4:5] <- rColVecCDEM[5:6]   # entirely bottom (SRTM) image 
  iRowCtr = iRowCtr + 1  
#
}
#
# write the table out as CSV file
# TODO: add the column ID (proximity indicator
message("hit key to write output table..")
browser()
writeCSV(mOutTable,file="tableForMark.csv",row.names=FALSE)
#
#message("South Sample - different columns")
#browser()
#colsToGet <-sample(1:inputFirstRaster@ncols,nColsToGet)
#iFirstRow <- 3600
#iCtr = 1
#for (iNextCol in colsToGet)
#{
#  rColVec <- cellFromRowCol(rDeltaWhole,iFirstRow:(iFirstRow+1),iNextCol:iNextCol)
#  neighborCells <- rDeltaWhole@data@values[rColVec]
#  iDiffVecSouth[iCtr] <- neighborCells[2] - neighborCells[1]
#  iCtr = iCtr + 1 
#}
# Compute iDiffVecs on either side of border, and across it. 
message("Check the cell difference vectors...")
#browser()

# summary stats for each population

#sNorthSum <- sprintf("ASTER sample summary: Min: %f / Median: %d / Mean: %f / Max: %f / Variance: %f sDev: %f",
#                     min(iDiffVecNorth,na.rm=TRUE),median(iDiffVecNorth,na.rm=TRUE),mean(iDiffVecNorth,na.rm=TRUE),
#                     max(iDiffVecNorth,na.rm=TRUE),var(iDiffVecNorth,na.rm=TRUE),sd(iDiffVecNorth,na.rm=TRUE))

sBorderSum <- sprintf("Border sample summary: Min: %f / Median: %d / Mean: %f / Max: %f / Variance: %f sDev: %f",
                     min(iDiffVecBorder,na.rm=TRUE),median(iDiffVecBorder,na.rm=TRUE),mean(iDiffVecBorder,na.rm=TRUE),
                     max(iDiffVecBorder,na.rm=TRUE),var(iDiffVecBorder,na.rm=TRUE),sd(iDiffVecBorder,na.rm=TRUE))

#sSouthSum <- sprintf("STRM sample summary: Min: %f / Median: %d / Mean: %f / Max: %f / Variance: %f sDev: %f",
#                     min(iDiffVecSouth,na.rm=TRUE),median(iDiffVecSouth,na.rm=TRUE),mean(iDiffVecSouth,na.rm=TRUE),
#                     max(iDiffVecSouth,na.rm=TRUE),var(iDiffVecSouth,na.rm=TRUE),sd(iDiffVecSouth,na.rm=TRUE))
#
message(sprintf("statistics for %d N/S adjacent pixel pairs from three mosaic image regions:",nColsToGet))
#message(sNorthSum)
message(sBorderSum)
#mssage(sSouthSum)

}

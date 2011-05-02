##############################################################################################
#
# SampleDemDiffCols
# 
# R script generates (or reads in) the CDEM / ASTER-SRTM mosaic 'difference image',
# and for randomly-selected one column / two row subimages, computes and saves tbe 
# difference between the pixels in the pair to create a distribution of differences.
# Three distributions created: North (ASTER CDEM), South i(SRTM/CGIAR), and 
# Border (boundary between ASTER and SRTM), Summary statistics are created for each 
# distribution.
#
# Author: Rick Reeves, NCEAS
# April 29, 2011
##############################################################################################
#
#ExtractDemRasterSubimages <- function(sFirstImageName,sSecondImageName)
SampleDemDiffCols <- function()
{
require(raster)
require(rgdal)

#inputFirstRaster <- raster(sFirstImageName)
#inputSecondRaster <- raster(sSecondImageName)

inputFirstRaster <- raster("mergeCgiarAsterBdyTuesdayClip.tif")
inputSecondRaster <- raster("CDemMosTuesdayClipMergeSpace.tif")
#
# Difference image for entire merged image takes a while to create, 
# so we created it once, now read it back in.
#
rDeltaWhole <- raster("DeltaEntireImage.tif")
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

vEdgeRegionFirst <- extract(inputFirstRaster,eTestAreaExtent)
rEdgeRegionFirst <- crop(inputFirstRaster,eTestAreaExtent)
vEdgeRegionSecond <- extract(inputSecondRaster,eTestAreaExtent)
rEdgeRegionSecond <- crop(inputSecondRaster,eTestAreaExtent)

# Important: In order for the image subtraction to work, the extents
#            of the two images must be IDENTICAL. I used ArcMap GIS Raster Crop By Mask
#            to create subimages with identical extents. 

# Compute the difference image  for the entire study area, and for the region along
# the boundary (narrow, maybe 10 pixels either side)

rDeltaEdge <- rEdgeRegionFirst - rEdgeRegionSecond

# Create this image one time, read it in thereafter.
#rDeltaWhole <- inputFirstRaster - inputSecondRaster
#writeRaster(rDeltaWhole,filename="DeltaEntireImage.tif",format="GTiff",datatype="INT2S",overwrite=TRUE)

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

colsToGet <-sample(1:inputFirstRaster@ncols,nColsToGet)
message("North Sample")
#browser()
# debug
#nColsToGet <- 2
#colsToGet <- c(20,100)
iFirstRow <- 300
iCtr = 1
for (iNextCol in colsToGet)
{
  rColVec <- cellFromRowCol(rDeltaWhole,iFirstRow:(iFirstRow+1),iNextCol:iNextCol)
  neighborCells <- rDeltaWhole@data@values[rColVec]
  iDiffVecNorth[iCtr] <- neighborCells[1] - neighborCells[2]
  iCtr = iCtr + 1
}
#
message("Border Sample")
#browser()
#colsToGet <-sample(1:inputFirstRaster@ncols,nColsToGet)
iFirstRow <- 6 # straddle the border of 12 row center section 
iCtr = 1
for (iNextCol in colsToGet)
{
  rColVec <- cellFromRowCol(rDeltaEdge,iFirstRow:(iFirstRow+1),iNextCol:iNextCol)
  neighborCells <- rDeltaEdge@data@values[rColVec]
  iDiffVecBorder[iCtr] <- neighborCells[1] - neighborCells[2]
  iCtr = iCtr + 1 
}
#
message("South Sample")
#browser()
#colsToGet <-sample(1:inputFirstRaster@ncols,nColsToGet)
iFirstRow <- 3600
iCtr = 1
for (iNextCol in colsToGet)
{
  rColVec <- cellFromRowCol(rDeltaWhole,iFirstRow:(iFirstRow+1),iNextCol:iNextCol)
  neighborCells <- rDeltaWhole@data@values[rColVec]
  iDiffVecSouth[iCtr] <- neighborCells[1] - neighborCells[2]
  iCtr = iCtr + 1 
}
# Compute iDiffVecs on either side of border, and across it. 
message("Check the cell difference vectors...")
#browser()

# summary stats for each population

sNorthSum <- sprintf("ASTER sample summary: Min: %f / Median: %d / Mean: %f / Max: %f / Variance: %f sDev: %f",
                     min(iDiffVecNorth,na.rm=TRUE),median(iDiffVecNorth,na.rm=TRUE),mean(iDiffVecNorth,na.rm=TRUE),
                     max(iDiffVecNorth,na.rm=TRUE),var(iDiffVecNorth,na.rm=TRUE),sd(iDiffVecNorth,na.rm=TRUE))

sBorderSum <- sprintf("Border sample summary: Min: %f / Median: %d / Mean: %f / Max: %f / Variance: %f sDev: %f",
                     min(iDiffVecBorder,na.rm=TRUE),median(iDiffVecBorder,na.rm=TRUE),mean(iDiffVecBorder,na.rm=TRUE),
                     max(iDiffVecBorder,na.rm=TRUE),var(iDiffVecBorder,na.rm=TRUE),sd(iDiffVecBorder,na.rm=TRUE))

sSouthSum <- sprintf("STRM sample summary: Min: %f / Median: %d / Mean: %f / Max: %f / Variance: %f sDev: %f",
                     min(iDiffVecSouth,na.rm=TRUE),median(iDiffVecSouth,na.rm=TRUE),mean(iDiffVecSouth,na.rm=TRUE),
                     max(iDiffVecSouth,na.rm=TRUE),var(iDiffVecSouth,na.rm=TRUE),sd(iDiffVecSouth,na.rm=TRUE))
#
message(sprintf("statistics for %d N/S adjacent pixel pairs from three mosaic image regions:",nColsToGet))
message(sNorthSum)
message(sBorderSum)
message(sSouthSum)

message("hit key to write output images...")
browser()
# Write the extracted subimage and its difference image to disk
# For now, use 'gdalinfo' to check image statistics

writeRaster(rEdgeRegionFirst,filename="EdgeRegionFirst.tif",format="GTiff",datatype="INT2S",overwrite=TRUE)
            
writeRaster(rEdgeRegionAsterCgiar,filename="EdgeRegionSecond.tif",format="GTiff",datatype="INT2S",overwrite=TRUE)

writeRaster(rDelta,filename="EdgeRegionDelta.tif",format="GTiff",datatype="INT2S",overwrite=TRUE)
}

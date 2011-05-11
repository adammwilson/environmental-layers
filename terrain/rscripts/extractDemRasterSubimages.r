##############################################################################################
#
# extractDemSubimages
# 
# R script extracts and saves subimages from various inputs to the 'image boundary analysis';
# also produces 'difference images' by subtracting CDEM components from other DEM components.
# Output files written as GeoTiff files. 
#
# Author: Rick Reeves, NCEAS
# May 9, 2011
##############################################################################################
#
extractDemSubimages <- function()
{
require(raster)
require(rgdal)

#inputRasterMerge <- raster(sFirstImageName)
#inputSecondRaster <- raster(sSecondImageName)

inputRasterAster <- raster("/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdyASTER_BLEven.tif")
inputRasterSRTM <- raster("/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdySRTM_BLEven.tif")
inputRasterMerge <- raster("/data/project/organisms/rcr/ValidateBoundary/mergeCgiarAsterBdyTuesdayClip.tif")
inputRasterCDEM <- raster("/data/project/organisms/rcr/ValidateBoundary/CDEMMosCgiarAsterBdy_BLEven.tif")

inputCdemAster24Row <- raster("/data/project/organisms/rcr/ValidateBoundary/EdgeAsterCDEM24row.tif")
inputCdemSrtm24Row <- raster("/data/project/organisms/rcr/ValidateBoundary/EdgeSrtmCDEM24row.tif")
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

eTestAreaExtentAster <- extent(-135.0,-105.0, 60.00009,60.010) # Creates 12 row subimage North of border (All ASTER)
eTestAreaExtentBorder <- extent(-135.0,-105.0, 59.995,60.005) # Creates a 12 row subimage centered on border
eTestAreaExtentSRTM <- extent(-135.0,-105.0, 59.990,60.00) # Creates a 12 row subimage South of border (All SRTM)

eTestAreaExtentAster <- extent(-135.0,-105.0, 60.00009,60.020) # Creates 24 row subimage North of border (All ASTER)
eTestAreaExtentBorder <- extent(-135.0,-105.0, 59.99,60.010) # Creates a 24 row subimage centered on border
eTestAreaExtentSRTM <- extent(-135.0,-105.0, 59.98,60.00) # Creates a 24 row subimage South of border (All SRTM)


# Extract a sub image corresponding to the selected extent.
# Two different alternatives:
# The extract() function returns a vector of cell values, 
# the crop() function returns a complete raster* object.
message("extracts")
browser()
vEdgeRegionAster <- extract(inputRasterAster,eTestAreaExtentAster)
rEdgeRegionAster <- crop(inputRasterAster,eTestAreaExtentAster)
vEdgeRegionAsterDelta <- extract(rDeltaWhole,eTestAreaExtentAster)
rEdgeRegionAsterDelta <- crop(rDeltaWhole,eTestAreaExtentAster)
#vEdgeRegionAsterCDEM <- extract(inputRasterCDEM,eTestAreaExtentAster)
#rEdgeRegionAsterCDEM <- crop(inputRasterCDEM,eTestAreaExtentAster)

vEdgeRegionBorder <- extract(inputRasterMerge,eTestAreaExtentBorder)
rEdgeRegionBorder <- crop(inputRasterMerge,eTestAreaExtentBorder)
vEdgeRegionBorderDelta <- extract(rDeltaWhole,eTestAreaExtentBorder)
rEdgeRegionBorderDelta <- crop(rDeltaWhole,eTestAreaExtentBorder)

vEdgeRegionSRTM <- extract(inputRasterSRTM,eTestAreaExtentSRTM)
rEdgeRegionSRTM <- crop(inputRasterSRTM,eTestAreaExtentSRTM)
vEdgeRegionSRTMDelta <- extract(rDeltaWhole,eTestAreaExtentSRTM)
rEdgeRegionSRTMDelta <- crop(rDeltaWhole,eTestAreaExtentSRTM)
#vEdgeRegionSrtmCDEM <- extract(inputRasterCDEM,eTestAreaExtentSRTM)
#rEdgeRegionSrtmCDEM <- crop(inputRasterCDEM,eTestAreaExtentSRTM)
message("differences")
browser()
# 
# write these files to disk, get stats.
#
rDeltaEdgeAster <- rEdgeRegionAster - inputCdemAster24Row
rDeltaEdgeBorder <- rEdgeRegionBorder - rEdgeRegionBorderDelta
rDeltaEdgeSRTM <- rEdgeRegionSRTM - rEdgeRegionSrtmCDEM

# Important: In order for the image subtraction to work, the extents
#            of the two images must be IDENTICAL. I used ArcMap GIS Raster Crop By Mask
#            to create subimages with identical extents. 

# Computeexter the difference image  for the entire study area, and for the region along
# the boundary (narrow, maybe 10 pixels either side)

# rDeltaEdge <- rEdgeRegionFirst - rEdgeRegionSecond
# May 10 2011 note: One of these statements generates the 'different origin' fatal error. 
# my workaround was to create ALL of the subimages used here with gdalwarp, and then 
# read them in and subtract them. 
# see other R script: OnlyImage Differences.r
# I will create a reproducable example, send to Raster authors. 

rDeltaEdgeAster <- rEdgeRegionAster - inputCdemAster24Row
rDeltaEdgeSRTM <- rEdgeRegionSRTM - inputCdemSrtm24Row

#rDeltaWhole <- inputRasterMerge - inputRasterCDEM
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

nColsToGet <-20000
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
# NOTR 5/9: need to rethink these sampling loops for this new version. Comment for now!
#colsToGet <-sample(1:inputRasterMerge@ncols,nColsToGet)
#message("North Sample")
#browser()
# debug
#nColsToGet <- 2
#colsToGet <- c(20,100)
#iFirstRow <- 300
#iCtr = 1
#for (iNextCol in colsToGet)
#{
#  rColVec <- cellFromRowCol(rDeltaWhole,iFirstRow:(iFirstRow+1),iNextCol:iNextCol)
#  neighborCells <- rDeltaWhole@data@values[rColVec]
#  iDiffVecNorth[iCtr] <- neighborCells[2] - neighborCells[1]
#  iCtr = iCtr + 1
#}
#
#message("Border Sample - different columns")
#browser()
#colsToGet <-sample(1:inputRasterMerge@ncols,nColsToGet)
#iFirstRow <- 6 # straddle the border of 12 row center section 
#iCtr = 1
#for (iNextCol in colsToGet)
#{
#  rColVec <- cellFromRowCol(rDeltaEdge,iFirstRow:(iFirstRow+1),iNextCol:iNextCol)
#  neighborCells <- rDeltaEdge@data@values[rColVec]
#  iDiffVecBorder[iCtr] <- neighborCells[2] - neighborCells[1]
#  iCtr = iCtr + 1 
#}
##
#message("South Sample - different columns")
##browser()
#colsToGet <-sample(1:inputRasterMerge@ncols,nColsToGet)
#iFirstRow <- 3600
#iCtr = 1
#for (iNextCol in colsToGet)
#{
#  rColVec <- cellFromRowCol(rDeltaWhole,iFirstRow:(iFirstRow+1),iNextCol:iNextCol)
#  neighborCells <- rDeltaWhole@data@values[rColVec]
#  iDiffVecSouth[iCtr] <- neighborCells[2] - neighborCells[1]
#  iCtr = iCtr + 1 
#}
## Compute iDiffVecs on either side of border, and across it. 
#message("Check the cell difference vectors...")
##browser()

# summary stats for each population

#sNorthSum <- sprintf("ASTER sample summary: Min: %f / Median: %d / Mean: %f / Max: %f / Variance: %f sDev: %f",
#                     min(iDiffVecNorth,na.rm=TRUE),median(iDiffVecNorth,na.rm=TRUE),mean(iDiffVecNorth,na.rm=TRUE),
#                     max(iDiffVecNorth,na.rm=TRUE),var(iDiffVecNorth,na.rm=TRUE),sd(iDiffVecNorth,na.rm=TRUE))
#
#sBorderSum <- sprintf("Border sample summary: Min: %f / Median: %d / Mean: %f / Max: %f / Variance: %f sDev: %f",
#                    min(iDiffVecBorder,na.rm=TRUE),median(iDiffVecBorder,na.rm=TRUE),mean(iDiffVecBorder,na.rm=TRUE),
#                    max(iDiffVecBorder,na.rm=TRUE),var(iDiffVecBorder,na.rm=TRUE),sd(iDiffVecBorder,na.rm=TRUE))

#sSouthSum <- sprintf("STRM sample summary: Min: %f / Median: %d / Mean: %f / Max: %f / Variance: %f sDev: %f",
#                     min(iDiffVecSouth,na.rm=TRUE),median(iDiffVecSouth,na.rm=TRUE),mean(iDiffVecSouth,na.rm=TRUE),
#                     max(iDiffVecSouth,na.rm=TRUE),var(iDiffVecSouth,na.rm=TRUE),sd(iDiffVecSouth,na.rm=TRUE))
##
#message(sprintf("statistics for %d N/S adjacent pixel pairs from three mosaic image regions:",nColsToGet))
#message(sNorthSum)
#message(sBorderSum)
#message(sSouthSum)

message("hit key to write output images...")
browser()

writeRaster(rDeltaEdgeSRTM,filename="/data/project/organisms/rcr/ValidateBoundary/EdgeRegionSRTMOnlyDelta24row.tif",format="GTiff",datatype="INT2S",overwrite=TRUE)
writeRaster(rDeltaEdgeAster,filename="/data/project/organisms/rcr/ValidateBoundary/EdgeRegionAsterOnlyDelta24row.tif",format="GTiff",datatype="INT2S",overwrite=TRUE)

# Create this image one time, read it in thereafter.

}

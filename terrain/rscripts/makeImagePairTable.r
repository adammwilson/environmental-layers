##############################################################################################
#
# makeImagePairTable.r
# 
# R script generates table of adjacent ASTER / SRTM / Mosaic Border / CDEM pixel values
# to be used to generate plots of 'delta elevation' vs 'pixel pair proximity' and 'elevation'
# suggested by Mark Schildhauer on May 3. 
#
# Author: Rick Reeves, NCEAS
# May 4, 2011
# May 17: This version includes raster() extents and input
#         image names used to create the (normalized)boundary  
#         area Difference Image scatterplots used in the most
#         recent 'boundary analysis'
##############################################################################################
#
makeImagePairTable <- function()
{
require(raster)
require(rgdal)

inputCgiarRaster  <- raster("/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdySRTM_BLEven.tif")
inputAsterRaster  <- raster("/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdyASTER_BLEven.tif")
inputMosaicRaster <- raster("/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdyFinalBLEven.tif")
inputCDEMRaster   <- raster("/data/project/organisms/rcr/ValidateBoundary/CDemMosSundayClipMergeSpaceEven.tif")
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
eTestAreaExtent <- extent(-135.0,-105.0, 59.995,60.005) # Creates a 12 row, 36000 col subimage

# Extract a sub image corresponding to the selected extent.
# Two different alternatives:
# The extract() function returns a vector of cell values, 
# the crop() function returns a complete raster* object.

rEdgeRegionAster <- crop(inputAsterRaster,eTestAreaExtent)
rEdgeRegionCgiar <- crop(inputCgiarRaster,eTestAreaExtent)
rEdgeRegionMosaic <- crop(inputMosaicRaster,eTestAreaExtent)
rEdgeRegionCDEM <- crop(inputCDEMRaster,eTestAreaExtent)

# Important: In order for the image subtraction to work, the extents
#            of the two images must be IDENTICAL. I used ArcMap GIS Raster Crop By Mask
#            to create subimages with identical extents. 

# Compute the difference image  for the entire study area, and for the region along
# the boundary (narrow, maybe 10 pixels either side)
#extent(rEdgeRegionMosaic) = extent(rEdgeRegionCDEM) # raster package author suggests this to resolve slight extent differences
rEdgeRegionDelta <- rEdgeRegionMosaic - rEdgeRegionCDEM  

# get a vector of random column index numbers, constrained by column dimension of image
# Loop three times, sampling pixel pairs from above, below, across the border

nColsToGet <- 36000 # Latest input images on even boundary have 36000 columns

iDiffVecNorth <- vector(mode="integer",length=nColsToGet)

iDiffVecBorder <- vector(mode="integer",length=nColsToGet)
iDiffVecSouth <- vector(mode="integer",length=nColsToGet)

# Here is the output matrix, (nColSamples * 3) rows, four columns 
colsToGet <-sample(1:inputMosaicRaster@ncols,nColsToGet)

mOutTable <- matrix(nrow=(nColsToGet * 3), ncol=5)
colnames(mOutTable) <- c("ColumnID","elevNorth","elevSouth","cdemNorth","cdemSouth")
# in this difference image, the border edge occurs at column 6

# NOTE Wed eve: A better start and end for the border: firstRow=5, lastRow = 8....
iFirstRow <- 5 #4 # two rows before the border           
iLastRow  <- 8 #7 # two rows after the border
iRowCtr = 1 # points to latest output table being written
message(sprintf("starting col sample loop"))
for (iNextCol in colsToGet)
{
#message(sprintf("in col sample loop getting col sample %d - hit key..",iNextCol))
#browser()

   rColVecMosaic <- cellFromRowCol(rEdgeRegionMosaic,iFirstRow:(iLastRow),iNextCol:iNextCol)
   rColVecCDEM   <- cellFromRowCol(rEdgeRegionCDEM,iFirstRow:(iLastRow),iNextCol:iNextCol)  
  
# Split the column vector into the pairs that Mark requested 
# For each column sampled, the output table has three rows:
#
# Row 1: from (input) Aster layer:  Two pixels above border
# Row 2: from (input) Mosaic layer: Two pixels straddling border rDeltaWhole@data@values[rColVec]
# Row 3: from (input) Cgiar layer:  Two pixels below border
#
# each with four columns, arranged into two column pairs:
#
#  North Pixel and South Pixel elevation, North Pixel and South Pixel CDEM (baseline)
# 
   mOutTable[iRowCtr,1]   <- iNextCol           # The (radomly sampled) col ID (surrogate for longitude) 
   mOutTable[iRowCtr,2:3] <- rEdgeRegionMosaic@data@values[rColVecMosaic[1:2]]  # First column pair from extracted vector: 
   mOutTable[iRowCtr,4:5] <- rEdgeRegionCDEM@data@values[rColVecCDEM[1:2]]   # entirely top (ASTER part of mosaic) image 
   iRowCtr = iRowCtr + 1
#  
   mOutTable[iRowCtr,1]   <- iNextCol           # The (radomly sampled) col ID (surrogate for longitude)
   mOutTable[iRowCtr,2:3] <- rEdgeRegionMosaic@data@values[rColVecMosaic[2:3]] # Second column pair: 
   mOutTable[iRowCtr,4:5] <- rEdgeRegionCDEM@data@values[rColVecCDEM[2:3]]   # straddles border region 
   iRowCtr = iRowCtr + 1  
#   
   mOutTable[iRowCtr,1]   <- iNextCol           # The (radomly sampled) col ID (surrogate for longitude)
   mOutTable[iRowCtr,2:3] <- rEdgeRegionMosaic@data@values[rColVecMosaic[3:4]]  # third column pair:
   mOutTable[iRowCtr,4:5] <- rEdgeRegionCDEM@data@values[rColVecCDEM[3:4]]   # entirely bottom (SRTM part of mosaic) image 
   iRowCtr = iRowCtr + 1  
}

# write the table out as CSV file

message("end of loop - hit key to write output table..")
browser()
write.csv(mOutTable,file="/data/project/organisms/rcr/ValidateBoundary/pixelPairs36000_5_8Even.csv",row.names=FALSE)
#
}

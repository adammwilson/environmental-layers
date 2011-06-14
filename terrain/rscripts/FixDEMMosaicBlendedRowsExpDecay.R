##################################################################################### 
# FixDEMMosaicBlendedRowsExpDecay: Implements Image Blending treatment to DEM Mosaic image
# Collection of functions that attempt to mitigate ASTER / SRTM the boundary edge artifact
# This version implements the Exponential Decay function to control image blending rate.
# Inputs: 
#   1) DEM Mosaic image containing 'boundary edge' artifact 
#   2) 'Aster-Only' subimage corresponding to the NORTHernmost part (adjacent to the 60 degree Lat 
#     boundary) of the SRTM component of the mosaic.
#   3) 'Aster-Only' subimage corresponding to the SOUTHernmost part (adjacent to the 60 degree Lat 
#     boundary) of the ASTER component of the mosaic.
#   4) numBlendRows: number of number of rows south of 60 degrees N Lat to modify ('blend') by
#      generating (lnear) combination of SRTM and ASTER image pixels. 
#   5) dKc: Empirically-determined exponential decay constant. Rule of thumb: set this such 
#      that exponential decay function approaches zero as row 'numBlendRows' is treated.
#
# Output: 
#   1) A copy of the input DEM mosaic, with 'blended image' boundary edge treatment.
#
# To run:
#
#  1) start R
#  2) > source("FixDEMMosaicBlendedRows.r")
#  3) > FixDEMMosaicBlendedRows()
#
# Note: the input images used by this program located on /jupiter: /data/project/organisms/rcr/BoundaryFixTest
#    
# Author: Rick Reeves, NCEAS
# June 13, 2011
#
##############################################################################################
FixDEMMosaicBlendedRowsExpDecay <- function()
{
  require(raster)
  require(rgdal)
 
# Key parameter: number of rows south of 60 degrees N Lat to modify ('blend') by
# generating (lnear) combination of SRTM and ASTER image pixels. 

  numBlendRows <- 100
  dKc <- .045 # empirically determined exponential decay constant. Select it 
#              so that exp - 1 decay goes to zero in the desired number of image rows.
  
# Read the mosaic image and component sub-images:
#   - Aster/SRTM mosaic image (with boundary edge to be fixed)
#   - 'Aster-only' subimage adjacent to Southern edge of 60 degree bounls -s dary,
#     sampled into a grid with same extent and spatial resolution as image mosaic
#   - 'Aster-only' subimage adjacent to Northern edge of 60 degree boundary,
#     sampled into a grid with same extent and spatial resolution as image mosaic
#     inAsterNorthFG  <- raster("./aster60DegNorthRasterFG.tif")     
#     inAsterSouthFG  <- raster("./aster59DegSouthRasterFG.tif")
#     inBdyAsterBoth  <- raster("./boundaryTestCase115DegBothAsterMos.img")      

     inMosaic      <- raster("./AsterSrtm3ArcSecTestImg.tif") 
     inAsterSouth  <- raster("./boundaryTest115_59DegreeAster.img")   
     inAsterSouthFG  <- raster("./aster59DegSouthRasterFG.tif")     
     inAsterNorth  <- raster("./boundaryTest115_60DegreeAster.img")     

# First, get the extent of the 'below 60' sub image in the (big) input raster

     northAsterEx <- extent(inAsterNorth)
     southAsterEx <- extent(inAsterSouth)

# Create copy of input raster that we will use to create a 'fixed boundary' iamge
# Even SIMPLER, according to raster documentation

     outMosaic <- raster(inMosaic) 
     
     sFixedRasterFile <- "TestOutRasterBdyBlendFixExpDec.tif"
     
#  create the output raster by copying the input file with 'block copy'
     
message("Copy input mosaic: Create output (repaired) mosaic")
#browser()
     bs <- blockSize(inMosaic) 
     outMosaic <- writeStart(outMosaic,sFixedRasterFile, datatype="INT2S", format="GTiff",overwrite=TRUE)
     for (iCtr in 1 : bs$n)
     {
        mosaicVals <- getValues(inMosaic,row=bs$row[iCtr],nrows=bs$nrows[iCtr])
        writeValues(outMosaic,mosaicVals,bs$row[iCtr])
        message(sprintf(".....processed input mosaic block %d",iCtr))        
     }
     outMosaic <- writeStop(outMosaic)
# 
message("Input mosaic copied to output raster: now, process boundary")
browser()

# now, we SHOULD be able to modify outMosaic with new 'column values'.

     southCellsOfInterest <- cellsFromExtent(outMosaic,southAsterEx)
     northCellsOfInterest <- cellsFromExtent(outMosaic,northAsterEx)
     
     firstNorthSubImgRow <- rowFromCell(outMosaic,northCellsOfInterest[1])   
     lastNorthSubImgRow <- rowFromCell(outMosaic,northCellsOfInterest[(length(northCellsOfInterest) - 1)])
 
     firstSouthSubImgRow <- rowFromCell(outMosaic,southCellsOfInterest[1])   
     lastSouthSubImgRow <- rowFromCell(outMosaic,southCellsOfInterest[(length(southCellsOfInterest) - 1)])

# note: last north image and first south image rows are the same.

     northBorderEdgeRow <- lastNorthSubImgRow
     southBorderEdgeRow <- firstSouthSubImgRow + 1

# The border 'edge' row is one row below the top of the south sub image

     northBrdrRowVals <- getValues(outMosaic,northBorderEdgeRow,1)
     southBrdrRowVals <- getValues(outMosaic,southBorderEdgeRow,1)     
 
# Process the mosaic rows:
# Blend the first 'numBlendRows' south of the 60 degree line (first rows of the SRTM component)
# with the underlying and corresponding ASTER image rows.
# For first attempt, use a linear combination; for second attempt, use an exponential function.
#
     nImageCols <- ncol(inMosaic)     
     sRows <- seq(southBorderEdgeRow,((southBorderEdgeRow + numBlendRows) - 1))
# 
     rowVecValuesBlend <- vector(mode="integer",length=nImageCols)
     iRowCtr <- 1
     for (curMosRow in sRows)     
     {
         message(sprintf("transforming cur row: %d",curMosRow))
# exponential decay diminishes impact of the 'delta' edge adjustment with distance from the boundary.

         dK <- exp(-(iRowCtr * dKc))
#         colToMod <- (numBlendRows-iCtr)+1
#               qq <- as.integer(round(colVecValues[colToMod] + (curRowBoundaryOffset * dK)))
#               colVecValues[colToMod] <- as.integer(round(colVecValues[colToMod] + (curRowBoundaryOffset * dK)))
#               colVecValues[iCtr] <- as.integer(round(colVecValues[iCtr] + (curRowBoundaryOffset * dK)))

# even simpler here, as we implement a 'linecar distance decay' function,
# and let each new column be a linear combination of the Aster and CGIAR image layers

#       deltaInc <- iRowCtr / as.numeric(numBlendRows)
        colVecCells <- cellFromRowCol(outMosaic,curMosRow,1:nImageCols) 

# get the current row from the mosaic(SRTM data) and raster images
# We get the ASTER values from a version of the 'South' ASTER image 
# with same extent as the image mosaic.

        rowVecValuesMosaic <- getValuesBlock(outMosaic,row=curMosRow,nrows=1,col=1,ncols=nImageCols) 
        rowVecValuesAster <- getValuesBlock(inAsterSouthFG,row=curMosRow,nrows=1,col=1,ncols=nImageCols)        
#        rowVecValuesBlend <- (rowVecValuesMosaic * deltaInc) + (rowVecValuesAster * (1.0 - deltaInc))
        rowVecValuesBlend <- (rowVecValuesAster * dK) + (rowVecValuesMosaic * (1.0 - dK))

# create the merged row as a linear combination of the Mosaic and Aster image. 
# note: we need to deal with NA values, which are possible. 

# write the blended row to the output mosaic

        outMosaic[colVecCells] <- as.integer(round(rowVecValuesBlend))
        iRowCtr <- iRowCtr + 1
     }
#message(sprintf("cur col: %d about to write to outMosaic",curMosCol))
#browser()

# write the modified outMosaic values to the image file that we created.

message("Done with raster blending : hit key to update raster disk file..")
browser()
     writeRaster(outMosaic, sFixedRasterFile, datatype="INT2S", format="GTiff",overwrite=TRUE)
#browser()
}

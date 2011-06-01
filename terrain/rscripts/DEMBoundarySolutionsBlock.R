##################################################################################### 
# DEMBoundarySolutionsBlock - Block version, uses a BLOCK write to create output mosaic
# 
# Collection of functions that attempt to mitigate ASTER / SRTM the boundary edge problem
# 
# 
#
# Inputs: 
#   1) Blended Aster/SRTM DEM mosaic image.
#   2) 'Aster-Only' subimage corresponding to the NORTHernmost part (adjacent to the 60 degree Lat 
#     boundary) of the SRTM component of the mosaic.
#   3) 'Aster-Only' subimage corresponding to the SOUTHernmost part (adjacent to the 60 degree Lat 
#     boundary) of the ASTER component of the mosaic.
#   4) Other parameters, as they are needed
#
# To run:
#
#  1) start R
#  2) > source("DEMBoundarySolutions.r")
#  3) > BlendAsterDEMPixelsBlock()
#
# Author: Rick Reeves, NCEAS
# May 31, 2011

##############################################################################################
BlendAsterDEMPixelsBlock <- function()
{
  require(raster)
  require(rgdal)
  
# Read the mosaic image and component sub-images:
#   - Aster/SRTM mosaic image (with boundary edge to be fixed)
#   - 'Aster-only' subimage adjacent to Southern edge of 60 degree bounls -s dary,
#     sampled into a grid with same extent and spatial resolution as image mosaic
#   - 'Aster-only' subimage adjacent to Northern edge of 60 degree boundary,
#     sampled into a grid with same extent and spatial resolution as image mosaic
    
#     inMosaic      <- raster("t:/rcr/BoundaryFixTest/AsterSrtmBoth3ArcSecSub.tif")
#     inAsterNorth  <- raster("t:/rcr/BoundaryFixTest/AsterBdyTest3ArcSecFullGridAbove60.tif")     
#    inAsterSouth  <- raster("t:/rcr/BoundaryFixTest/AsterBdyTest3ArcSecFullGridBelow60.tif")    
     inMosaic      <- raster("./AsterSrtmBoth3ArcSecSub.tif")
     
# Use these objects to get the extents of the subsets within the mosaic

     inAsterNorth  <- raster("./AsterBdyTestAbove60Sub.tif")     
     inAsterSouth  <- raster("./AsterBdyTestBelow60Sub.tif")     
     
# Better, read them into a rasterStack

#     mosaicLayers <- stack("./AsterSrtmBoth3ArcSecSub.tif",
#                           "./AsterBdyTest3ArcSecFullGridAbove60.tif",
#                           "./AsterBdyTest3ArcSecFullGridBelow60.tif") # fix by exporting from ArcGIS

     mosaicLayers <- stack("./AsterSRTMBothTestSub.tif",
                           "./AsterBdyTestFGAbove60Sub.tif",
                           "./AsterBdyTestFGBelow60Sub.tif") 
# 
# Create copy of input raster that we will use to create a 'fixed boundary' iamge
# Even SIMPLER, according to raster documentation
#
     outMosaic <- raster(mosaicLayers[[1]])
 
     sFixedRasterFile <- "TestOutputRasterBdyFix.tif"
     
# First, get the extent of the 'below 60' sub image in the (big) input raster

     northAsterEx <- extent(inAsterNorth)
     southAsterEx <- extent(inAsterSouth)
     
# Get the values from the mosaic for this extent

     southCellsOfInterest <- cellsFromExtent(mosaicLayers[[1]],southAsterEx)
     northCellsOfInterest <- cellsFromExtent(mosaicLayers[[1]],northAsterEx)

# Within the large input raster, we need the index of the first row of the 'Below 60 Aster' subimage.
# Our plan: to replace this portion of the input mosaic with a linear combination of the original
# input mosaic COPY (outMosaic) and the 'below 60' raster.

     firstNorthSubImgRow <- rowFromCell(mosaicLayers[[1]],northCellsOfInterest[1])   
     lastNorthSubImgRow <- rowFromCell(mosaicLayers[[1]],northCellsOfInterest[(length(northCellsOfInterest) - 1)])
     northNrowsToProcess <- nrow(inAsterNorth)
  
     firstSouthSubImgRow <- rowFromCell(mosaicLayers[[1]],southCellsOfInterest[1])   
     lastSouthSubImgRow <- rowFromCell(mosaicLayers[[1]],southCellsOfInterest[(length(southCellsOfInterest) - 1)])
     southNrowsToProcess <- nrow(inAsterSouth)

#  create the output raster by copying the input file with 'block copy'
     
message("Create output (fixed) mosaic")
#browser()
     bs <- blockSize(mosaicLayers[[1]]) 
     outMosaic <- writeStart(outMosaic,sFixedRasterFile, datatype="INT2S", format="GTiff",overwrite=TRUE)
     for (iCtr in 1 : bs$n)
     {
        mosaicVals <- getValues(mosaicLayers[[1]],row=bs$row[iCtr],nrows=bs$nrows[iCtr])
        writeValues(outMosaic,mosaicVals,bs$row[iCtr])
        message(sprintf(".....processed input mosaic block %d",iCtr))        
     }
     outMosaic <- writeStop(outMosaic)
message("Input mosaic copied to output raster: now, process boundary")
#browser()

# now, we SHOULD be able to modify outMosaic with new 'column values'.

# note: last north image and first south image rows are the same. 

     northBorderEdgeRow <- lastNorthSubImgRow
     southBorderEdgeRow <- firstSouthSubImgRow + 1

# The border 'edge' row is one row below the top of the south sub image

     southBrdrRowVals <- getValues(outMosaic,southBorderEdgeRow,1)
     northBrdrRowVals <- getValues(outMosaic,northBorderEdgeRow,1)
 
     brdrRowEdgeDelta <- southBrdrRowVals - northBrdrRowVals

# Process the mosaic 'column-by-column'
# For first effort, apply straightforward linear ramp function to the northernmost SRTM image rows.
# In next iteration, we will modify the LAST 'numBlendRows' rows NORTH of the boundary with an 'extinction' function.
#
     numBlendRows <- 100
     srows <- seq(southBorderEdgeRow,((southBorderEdgeRow + numBlendRows) - 1))
# 
     for (curMosCol in 1 : ncol(mosaicLayers[[1]]))
     {
message(sprintf("transforming cur col: %d",curMosCol))
#browser()
        deltaInc <- brdrRowEdgeDelta[curMosCol] / numBlendRows
        colVecCells <- cellFromRowCol(outMosaic,srows,curMosCol) 
        colVecValues <- getValuesBlock(outMosaic,row=southBorderEdgeRow,nrows=numBlendRows,col=curMosCol,ncols=1) 

# note: we need to deal with NA values, which are possible. 

        sumDeltaInc <- 0
        for (iCtr in length(colVecValues) : 1)
        {

# The idea: the closer to the border, the larger an increment we assign.
# in any case, increment the offset so that it is correct for any iupcoming non-NA column value

            sumDeltaInc <- sumDeltaInc + deltaInc

            if (!is.na(colVecValues[1]))
            {
#message(sprintf("cur col: %d Found a NON-NA vector",curMosCol))
#browser()
               colVecValues[iCtr] <- as.integer(round(colVecValues[iCtr] - sumDeltaInc)) 
            }
        }

# Insert this vector back into the mosaic: this technique adopted from raster PDF doc 'replacement' help. 
# Looks like: I have to specify the row index (vector) to insert the values into the mosaic. 
#message("write columns to out mosaic")
#browser()
        outMosaic[colVecCells] <- colVecValues
#        outMosaic[srows,curMosCol] <- colVecValues #  this did not seem to work - june 1

     }
#message(sprintf("cur col: %d about to write to outMosaic",curMosCol))
#browser()

# write the modified outMosaic values to the image file that we created.

message("Done with raster blending : hit key to update raster disk file..")
browser()
     writeRaster(outMosaic, sFixedRasterFile, datatype="INT2S", format="GTiff",overwrite=TRUE)
}

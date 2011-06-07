
# DEMBdyFixExpDecay - Implements Exponential Decay method to 'blend' DEM boundary edges
# 
# General form: decayFactor = e(-(d * DKc))
#                 where d = distance interval from origin (in this case, an image row index)
#                       DKc = empirically derived constant
# 
#
# Inputs: 
#   1) Blended Aster/SRTM DEM mosaic image.
#   2) 'SRTM-Only' subimage corresponding to the NORTHernmost part (adjacent to the 60 degree Lat 
#     boundary) of the SRTM component of the mosaic.
#   3) 'Aster-Only' subimage corresponding to the SOUTHernmost part (adjacent to the 60 degree Lat 
#     boundary) of the ASTER component of the mosaic.
#   4) Other parameters, as they are needed
#
# To run:

 
#  2) > source("DEMBdyFixExpDecay.r")
#  3) > DEMBdyFixExpDecay()
#
# Author: Rick Reeves, NCEAS
# June, 2011

##############################################################################################
DEMBdyFixExpDecay <- function()
{
  require(raster)
  require(rgdal)
  
# Read the mosaic image and component sub-images:
#   - Aster/SRTM mosaic image (with boundary edge to be fixed)
#   - 'Aster-only' subimage adjacent to Southern edge of 60 degree bounls -s dary,
#     sampled into a grid with same extent and spatial resolution as image mosaic
#   - 'Aster-only' subimage adjacent to Northern edge of 60 degree boundary,
#     sampled into a grid with same extent and spatial resolution as image mosaic
    
     inMosaic      <- raster("./AsterSrtmBoth3ArcSecSub.tif")
     
# Use these objects to get the extents of the subsets within the mosaic

     inAsterNorth  <- raster("./AsterBdyTestAbove60Sub.tif")     
     inAsterSouth  <- raster("./AsterBdyTestBelow60Sub.tif")     
     
# Better, read them into a rasterStack

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
# For each column, extract the LAST 'numBlendRows cells adjacent to the bottomi
# (southernmost) edge of the ASTER component, next to the SRTM border. 
# Then, moving 'north' along the colum of values, add the current colunn's 'ledge' 
# offset, attenuated by the exponential decay factor for that row. 
# Thus, the SRTM/ASTER 'ledge' is attenuated, the impact is greatest at the boundary,
# least at the threshold, when the decay factor (empirically determined) goes to zero.
#
# Add Exponential Decay constant: empirically selected (try a few, ee if you like result
# Maybe plot it, see how many rows it takes for the 'effect' of SRTM goes to zero.
 
     dKc <- .045 # empirically determined constant, so that exp - 1 decay goes to zero in the desired number of image rows.
     numBlendRows <- 60
     begBlendRow <- (northBorderEdgeRow - numBlendRows) + 1
#     srows <- seq(southBorderEdgeRow,((southBorderEdgeRow + numBlendRows) - 1))
     srows <- seq((begBlendRow),northBorderEdgeRow)
# 
     for (curMosCol in 1 : ncol(mosaicLayers[[1]]))
     {
message(sprintf("transforming cur col: %d",curMosCol))
#browser()
#        deltaInc <- brdrRowEdgeDelta[curMosCol] / numBlendRows
        colVecCells <- cellFromRowCol(outMosaic,srows,curMosCol) 
        colVecValues <- getValuesBlock(outMosaic,row=begBlendRow,nrows=numBlendRows,col=curMosCol,ncols=1) 
        compareVec <- getValuesBlock(outMosaic,row=begBlendRow,nrows=numBlendRows+2,col=curMosCol,ncols=1) 
        curRowBoundaryOffset <- brdrRowEdgeDelta[curMosCol]

# note: we need to deal with NA values, which are possible. 

#        sumDeltaInc <- 0
#        for (iCtr in numBlendRows : 1) # this moves from 'south to north', decay increases with distance.
        for (iCtr in 1 : numBlendRows) # this moves from 'south to north', decay increases with distance.
#        for (iCtr in length(colVecValues) : 1) # this moves from 'south to north', decay increases with distance.
        {

# The idea: the closer to the border, the larger an increment we assign.
# in any case, increment the offset so that it is correct for any iupcoming non-NA column value

            if (!is.na(colVecValues[1]))
            {
#message(sprintf("cur col: %d Found a NON-NA vector",curMosCol))
#browser()
 
# exponential decay diminishes impact of the 'delta' edge adjustment with distance from the boundary.

               dK <- exp(-(iCtr * dKc))  
               colToMod <- (numBlendRows-iCtr)+1
#               qq <- as.integer(round(colVecValues[colToMod] + (curRowBoundaryOffset * dK)))
               colVecValues[colToMod] <- as.integer(round(colVecValues[colToMod] + (curRowBoundaryOffset * dK)))
#               colVecValues[iCtr] <- as.integer(round(colVecValues[iCtr] + (curRowBoundaryOffset * dK)))
            }
        }

# Insert this vector back into the mosaic: this technique adopted from raster PDF doc 'replacement' help. 
# Looks like: I have to specify the row index (vector) to insert the values into the mosaic. 
message("write columns to out mosaic")
#rowser()
        outMosaic[colVecCells] <- colVecValues

     }

# write the modified outMosaic values to the image file that we created.

message("Done with raster blending : hit key to update raster disk file..")
browser()
     writeRaster(outMosaic, sFixedRasterFile, datatype="INT2S", format="GTiff",overwrite=TRUE)
}

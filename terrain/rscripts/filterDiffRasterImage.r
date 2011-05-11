##############################################################################################
#
# R Script performs edge filtering on Raster files
#
#
##############################################################################################
filterDiffRasterImage <- function(sInCDEMRasterName,sInAstCgiarMosName)
{
require(raster)
require(rgdal)

# Read a raster image file into Raster object

#setwd("/data/project/organisms/rcr/ValidateBoundary")
#setwd("t:/organisms/rcr/ValidateBoundary")

#sInCDEMName <- "CanadaDemMosTuesdayNewWGS84.tif"
#sInAstCgiarMosName <- "mergeCgiarAsterBdyFinalBLMonday.tif"

inputCDEMRaster <- raster(sInCDEMRasterName)
inputAsterCgiarRaster <- raster(sInAstCgiarMosName)
inputAsterCgiarRasterRes <- raster("./mergeCgiarAsterTuesdayCDEMGrid2.tif")

# Create an extent object with the coordinates of the 'edge zone' 
# along the 60 degree North latitude line
# The Western Canada study area runs from -135 (west) to -100 (west) longitude,
# and 55.0 to 64.00 degrees (north) latitude. 

eTestAreaExtent <- extent(-135.2,-100.2, 59.995,60.005) # this results in a 12 row subimage
eTestAreaExtent <- extent(-135.2,-100.2, 59.997,60.001) # Creates a 5 row subimage CENTERED on 60N Latitude

# extract a sub area corresponding to the extent.
# The extract() function returns a vector of cell values, 
# the crop() function returns a raster* object.

vEdgeRegionCDEM <- extract(inputCDEMRaster,eTestAreaExtent)
rEdgeRegionCDEM <- crop(inputCDEMRaster,eTestAreaExtent)
vEdgeRegionAsterCgiar <- extract(inputAsterCgiarRaster,eTestAreaExtent)
rEdgeRegionAsterCgiar <- crop(inputAsterCgiarRaster,eTestAreaExtent)

vEdgeRegionAsterCgiarRes <- extract(inputAsterCgiarRasterRes,eTestAreaExtent)
rEdgeRegionAsterCgiarRes <- crop(inputAsterCgiarRasterRes,eTestAreaExtent)

# for raster subtraction to work, the raster object extracts must work. 
# for some reason, the extent for these two were off by .1 degree. 
# here is a workaround - It resulted in an 'aligned' image pair, for 
# which the difference image values matched the actual differences 
# between ASTER and CDEM values.
extent(rEdgeRegionAsterCgiar) <- extent(rEdgeRegionCDEM) # only do if extents dont match
rDelta <- rEdgeRegionAsterCgiar - rEdgeRegionCDEM
rDeltaRes <- rEdgeRegionAsterCgiarRes - rEdgeRegionCDEM
rBigDeltaRes <- inputAsterCgiarRasterRes - inputCDEMRaster

# compare the statistics between these two difference images

# Construct the filter kernal: a 3 x 3 integer matrix

# Execute the filter

# Compute image stats?

# display the file?

# Write the output file to disk

writeRaster(rEdgeRegionCDEM,filename="EdgeRegionCDEM5RowsS.tif",
            format="GTiff",datatype="INT2S",overwrite=TRUE)
            
writeRaster(rEdgeRegionAsterCgiar,filename="EdgeRegionAsterCgiar5RowsS.tif",
            format="GTiff",datatype="INT2S",overwrite=TRUE)

writeRaster(rEdgeRegionAsterCgiarRes,filename="EdgeRegionAsterCgiar5RowsResamp.tif",
format="GTiff",datatype="INT2S",overwrite=TRUE)

writeRaster(rDelta,filename="EdgeRegionDelta5RowsS.tif",
            format="GTiff",datatype="INT2S",overwrite=TRUE)
writeRaster(rDeltaRes,filename="EdgeRegionDelta5RowsResS.tif",
            format="GTiff",datatype="INT2S",overwrite=TRUE)
            
#writeRaster(rBigDelta,filename="CgiarCDEMRegionDelta.tif",
#            format="GTiff",datatype="INT2S",overwrite=TRUE)
#writeRaster(rBigDeltaRes,filename="CgiarCDEMRegionDeltaRes.tif",
#            format="GTiff",datatype="INT2S",overwrite=TRUE)            
}

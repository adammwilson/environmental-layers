# R code to smooth the boundary area N59 to N60 to the south, by simply taking pixel-wise
# averages of the observed SRTM and ASTER using a distance-based weighting function such
# that the relative contribution of ASTER decays to zero over a few km
# create simple grid indicating distance (in units of pixels) south from
# boundary , starting at 1. 
#
#
# Original author: Jim Regetz 
# [13-JUL-2011]
#	Edits by Yuni to apply ASTER GDEM2, instead of GDEM1, as well as auto-applying 
#	for each nine region.  [9-Dec-2011] 
# 




library(raster)

aster.dir  <- "/data/project/organisms/DEM/Yuni/Data/aster2"
srtm.dir  <- "/data/project/organisms/DEM/Yuni/Data/srtm"
aster.below <- list.files(aster.dir, pattern="^aster2_.*_below.tif")
aster.above <- list.files(aster.dir, pattern="^aster2_.*_above.tif")
srtm.below  <- list.files(srtm.dir, pattern="^srtm_.*_below.tif$")


blendgau <- function(aster.tile.below, aster.tile.above, srtm.tile.below, aster.dir, srtm.dir){
    # load relevant SRTM and ASTER data
    srtm.south <- raster(file.path(srtm.dir,  srtm.tile.below)) 
    aster.south <- raster(file.path(aster.dir, aster.tile.below)) 
    aster.north <- raster(file.path(aster.dir, aster.tile.above))
    
    # create difference raster for area of overlap
    delta.south <- srtm.south - aster.south
    
    aster.south.matrix <- as.matrix(aster.south) 
    ydistS <- row(aster.south.matrix)
    
    r <- -0.001 # weight drops to 0.5 at ~26 cells, or 2.4km at 3" res 
    w <- exp(-0.001*ydistS^2)
    aster.south.smooth <- aster.south
    aster.south.smooth[] <- values(srtm.south) - as.integer(round(t(w *
        as.matrix(delta.south)))) 
    aster.south.smooth[aster.south.smooth <0] <- 0 
    file.name <- paste("aster2_", substr(srtm.tile.below, 6, 13), "_below_blendgau.tif", sep="") 
    writeRaster(aster.south.smooth, file.path(aster.dir, file.name))
} 




num.regions <- 9
makeGau <-lapply(1:num.regions, function(region, aster.dir, srtm.dir){
    aster.tile.below <- aster.below[region]
    aster.tile.above <- aster.above[region]
    srtm.tile.below  <- srtm.below[region]
    blendgau(aster.tile.below, aster.tile.above, srtm.tile.below, aster.dir, srtm.dir)
}, aster.dir=aster.dir, srtm.dir=srtm.dir)




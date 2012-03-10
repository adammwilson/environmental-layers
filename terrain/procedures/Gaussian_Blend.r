# Modificaion of R code to smooth the boundary area N59 to N60 to the south, by simply taking pixel-wise
# averages of the observed SRTM and ASTER using a distance-based weighting function such
# that the relative contribution of ASTER decays to zero over a few km
# create simple grid indicating distance (in units of pixels) south from
# boundary , starting at 1. 
#
#
# Original author: Jim Regetz  (organisms/DEM/Yuni/scripts/toProduceData/gaussian.r)
# [13-JUL-2011]
#  Edits by Natalie to apply to resampled ASTER GDEM2, in 5 x 5 grids with no 1/2 pixel offset, and 
#  shifterd SRTM tiles, with no 1/2 pixel offset. [2-Feb-2012]
# NOTE- A similar version of this script exists in "organisms/DEM/Yuni/scripts/toProduceData/gaussian.r" as 
# written and edited by Jim and Yuni.


library(raster)

#Set working directories and filenames
aster.dir  <- "/data/project/organisms/DEM/asterGdem2/90m_NoPixelOffset/Mosaiced/N59to60"
srtm.dir  <- "/data/project/organisms/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1/Tiles_Resampled/Mosaiced"
out.dir <- "/data/project/organisms/DEM/GlobalProduct"
aster.below.East <- paste (aster.dir, "/N59to60E000_180.tif", sep="")
srtm.East  <- paste (srtm.dir, "/SRTM_N59to60E000_180.tif",sep="")
aster.below.West <- paste(aster.dir, "/N59to60W180_000.tif",sep="")
srtm.West  <- paste(srtm.dir, "/SRTM_N59to60W180_000.tif",sep="")

blendgau <- function(aster.tile, srtm.tile, aster.dir, srtm.dir, out.dir, hemi){
    # load relevant SRTM and ASTER data
    srtm.south <- raster(srtm.tile) 
    aster.south <- raster(aster.tile) 
        
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
    file.name <- paste("/Blended_N59to60_", hemi, ".tif", sep="") 
    writeRaster(aster.south.smooth, file.path(out.dir, file.name))
} 

blendgau(aster.below.East, srtm.East, aster.dir, srtm.dir, out.dir, hemi="East")
blendgau(aster.below.West, srtm.West, aster.dir, srtm.dir, out.dir, hemi="West")
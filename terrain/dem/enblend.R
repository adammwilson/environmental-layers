# Code to produce enblended DEM (i.e., using multiresolution splines as
# described by Burt & Adelson 1983) in the 60N boundary region. After
# appropriately preparing the SRTM and ASTER layers, this code makes a
# system call to run 'enblend' (v. 4.0) on the inputs, then
# post-processes the resulting image to yield a single band geotiff with
# datatype of 16bit signed integer, matching the input data.
#
# Somewhat arbitrarily, in the code below the input DEMs are prepped
# such that the area of SRTM/ASTER overlap is the first 75 rows below
# 60N (i.e., a zone extending ~6.75km south of the boundary).
#
# Jim Regetz
# NCEAS
# Created on 29-Jun-2011

library(raster)

demdir <- "/home/regetz/media/temp/terrain/dem"

# read in aster data
aster <- raster(file.path(demdir, "aster_300straddle.tif"))
# create alpha layer for aster
alpha <- aster
alpha[] <- 0
alpha[1:225, ] <- 1
# write enblend-ready tif out to disk
writeRaster(brick(aster, alpha), file="aster-enblend.tif",
    options="ALPHA=YES")

# read in srtm
srtm.lower <- raster("../srtm_150below.tif")
# expand to match full image (i.e., same extent as aster), holding srtm
# data below 60N and nothing above
srtm <- aster
srtm[] <- NA
srtm[151:300, ] <- values(srtm.lower)
# create alpha layer for srtm
alpha <- srtm
alpha[] <- 0
alpha[151:300, ] <- 1
writeRaster(brick(srtm, alpha), file="srtm-enblend.tif",
    options="ALPHA=YES")

# run 'enblend'
system(paste("enblend --verbose=6 -o enblend.tif",
    "aster-enblend.tif srtm-enblend.tif"))

# post-process enblended DEM
e <- raster("enblend.tif")
e2 <- aster
# round to nearest integer, and write out the result as a geotiff
e2[] <- as.integer(round(values(e), 0))
writeRaster(e2, file=file.path(demdir, "fused_300straddle_enblend.tif"),
    options="COMPRESS=NONE", datatype="INT2S")

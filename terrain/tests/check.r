# CHECK PART1

# R script for verifying the ASTER GDEM2 tilenames. This script will 
# verify that lower lefthand coordinate of tiles matches filenames
# (this is how the files are supposed to be named) in both dem.tif and
# num.tif files. 
# The result of Checks can be found at 
#    "/data/project/organisms/DEM/Yuni/documents/check/check_result.pdf" 
#
#
# Original author: Natalie Robinson
# [08-Nov-2011]
#    Edits by Jim and Yuni, focusing on streamlining and improving
#    runtime efficiency. [9-Dec-2011]
#    If you would like to separate the bad-name tiles in the separate 
#    folder, please see Nathalie's original script, located at 
#    "/data/project/organisms/DEM/asterGdem/R_files/AsterCheck_demAndnum.r."




--------------------------------------------------------------------
library(rgdal)

# path to base directory containing the tiles
aster.dir <- "/data/project/organisms/DEM/asterGdem2"

# get list of relevant ASTER GDEM tile names
aster.list <- list.files(aster.dir, pattern="^ASTGTM2_.*.tif$")




# for a given file, return TRUE if file name matches the internally
# specified lower left corner, otherwise print the bad-named tiles
# with the correct name. 

check.name <- function(tile_num.name, path=".", silent=TRUE) {
    # build expected filenameche
    origin <- round(GDALinfo(file.path(path, tile_num.name),
        silent=silent)[c("ll.x", "ll.y")])
    ly <- origin["ll.y"]
    y <- sprintf("%s%02d", if (ly>=0) "N" else "S", abs(ly))
    lx <- origin["ll.x"]
    x <- sprintf("%s%03d", if (lx>=0) "E" else "W", abs(lx))

    tile.suffix <- substring(tile_num.name, 16)
    expected.name <- paste("ASTGTM2_", y, x, tile.suffix, sep="")

    # compare to actual filename
    if (tile_num.name==expected.name) {
        TRUE
    } else {
        print ("Bad Name:", tile_num.name, "but supposed to be",expected.name)
    }
}


 
title_num.check <- sapply(aster.list, check.name, path=aster.dir)
bad.data <- data.frame(expected=title_num.check[title_num.check!="TRUE"])
bad.data   #this will print list of bad data 







-------------------------------------------------------------------

# CHECK PART2

# R script for calculating the fraction of land mass in each large USGS
# elevation tile, and comparing this to the fraction of land mass across
# all ASTER tiles covering the same spatial extent.
#
# Note that this script assumes all ASTER pixels that have a value of 0
# are ocean cells. This is indeed how ocean is masked out in GDEM, but
# we do not attempt to distinguish these from legitimate land cells that
# happen to have an elevation of 0 meters. We just assume the latter are
# rare enough to be inconsequential...
#
# Original author: Natalie Robinson
# [08-Nov-2011]
#    Edits by Jim and Yuni, focusing on streamlining and improving
#    runtime efficiency of the first batch of code (i.e., for
#    N59to81_W20toE19), which can now easily be generalized to a
#    function that can be reused for all other regions. [9-Dec-2011]
#
# The result of Checks can be found at 
#    "/data/project/organisms/DEM/Yuni/documents/check/check_result.pdf" 


library(raster)
library(rgdal) 

# path to base directory containing the tiles
aster.dir <- "/data/project/organisms/DEM/Yuni/Data/aster2"
usgs.dir <-  "/data/project/organisms/DEM/Yuni/Data/GTOPO30/"

# get list of relevant ASTER GDEM tile names and list of clipped USGS dem
aster.clip <- list.files(aster.dir, pattern="^aster2_.*_82N.tif$")
usgs.clip  <- list.files(usgs.dir) 



compare <- function(aster.tile.names, usgs.tile.name, aster.dir=".", usgs.dir=".") {

# summarize all aster tiles
    p.not.land <- t(sapply(aster.tile.names, function(tile.name, dir) {
        elev <- values(raster(file.path(dir, tile.name)))
        p.ocean <- mean(elev==0)
        p.nodata <- mean(elev==-9999)
        c(p.ocean=p.ocean, p.nodata=p.nodata)
    }, dir=aster.dir))

    # calculate overall proportion of all ASTER pixels containing known land
    # elevations (i.e., neither ocean nor nodata)
    aster.land.fraction <- 1 - mean(rowSums(p.not.land))


    # summarize usgs tiles:
    # calculate proportion of land in the corresponding USGS elevation
    # raster, in which all non-land pixels are automatically read in as NA
    usgs <- values(raster(file.path(usgs.dir, usgs.tile.name)))
    usgs.land.fraction <- mean(!is.na(usgs))

    # compare them:
    # what's the proportional difference between USGS and ASTER?
    diff.aster.usgs <- 1-(usgs.land.fraction/aster.land.fraction)

    # print the comparison details 
    list(
    comparison = c(aster.land.fraction=aster.land.fraction,
    usgs.land.fraction=usgs.land.fraction,
    difference=diff.aster.usgs),
    aster.details = p.not.land
    )
}



num.regions <- 9
comparisons <- lapply(1:num.regions, function(region, aster.dir, usgs.dir) {
    aster.tiles <- aster.clip[region]
    usgs.tile <- usgs.clip[region]
    compare(aster.tiles, usgs.tile, aster.dir, usgs.dir)
}, aster.dir=aster.dir, usgs.dir=usgs.dir)





--------------------------------------------------------------------

# CHECK PART3

# R script for calculating the number of tiles for GDEM2 and GDEM1.
# Quick and rough comparison of DEM tile holdings on eos for ASTER GDEM1
# vs ASTER GDEM2, based purely on file names
#
# Jim Regetz
# NCEAS
# Created on 09-Dec-2011



# list all *_dem.tif files (full paths starting from their respective
# base directories)
gdem1 <- system('find /data/project/organisms/DEM/asterGdem -name "*_dem.tif"', intern=TRUE)
gdem2 <- system('find /data/project/organisms/DEM/asterGdem2 -name "*_dem.tif"', intern=TRUE)

# extract file names, and remove any duplicates
# note: getting rid of spurious "._ASTGTM2xxx" files from GDEM2 list
f1 <- unique(basename(gdem1))
f2 <- unique(grep("\\._", basename(gdem2), value=TRUE, invert=TRUE))

# parse out latitude (always N here) and longitude values
a1 <- data.frame(
    lat = as.numeric(sub(".*N(..).*", "\\1", f1)),
    lon = sub(".*N..(....).*$", "\\1", f1),
    row.names = f1)
a2 <- data.frame(
    lat = as.numeric(sub(".*N(..).*", "\\1", f2)),
    lon = sub(".*N..(....).*$", "\\1", f2),
    row.names = f2)

# count files by latitude, merge results, and compute differences in
# counts
comp <- merge(data.frame(gdem1=table(a1$lat)),
    data.frame(gdem2=table(a2$lat)), by=1, all=TRUE)
comp[2:3][is.na(comp[2:3])] <- 0
names(comp) <- c("latitude", "gdem1count", "gdem2count")
comp$diff <- comp$gdem2count - comp$gdem1count



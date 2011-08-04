# Quick script to determine whether the filename of each downloaded
# ASTER tile is faithful to its spatial location as specified in the
# GeoTIFF itself.
#
# This script is written to run on eos, based on the file system
# location of downloaded ASTER tiles as of 04-Aug-2011
#
# Jim Regetz
# NCEAS
# Created on 04-Aug-2011

library(rgdal)

# for a given file, return TRUE if file name matches the internally
# specified lower left corner, otherwise return string with those
# coordinates
check <- function(tilename, path=".", silent=TRUE) {
    # build expected filename
    origin <- round(GDALinfo(file.path(path, tilename),
        silent=silent)[c("ll.x", "ll.y")])
    ly <- origin["ll.y"]
    y <- sprintf("%s%02d", if (ly>=0) "N" else "S", abs(ly))
    lx <- origin["ll.x"]
    x <- sprintf("%s%03d", if (lx>=0) "E" else "W", abs(lx))
    expected.name <- paste("ASTGTM_", y, x, "_dem.tif", sep="")
    # compare to actual filename
    if (tilename==expected.name) {
        TRUE
    } else {
        paste(y, x)
    }
}

# produce vector of check results, with the actual file names as vector
# element names (takes ~12 minutes on eos)
aster.tiles <- list.files("~organisms/DEM/asterGdem",
    pattern="^ASTGTM.*_dem.tif$")
tilecheck <- sapply(aster.tiles, check)

# report mismatches
data.frame(expected=tilecheck[tilecheck!="TRUE"])
##                       expected
##ASTGTM_N59E069_dem.tif N63 E109
##ASTGTM_N63E113_dem.tif N69 E107
##ASTGTM_N63E117_dem.tif N69 E113
##ASTGTM_N64E098_dem.tif N70 E117
##ASTGTM_N65E104_dem.tif N73 E084
##ASTGTM_N65E111_dem.tif N73 E098
##ASTGTM_N65E117_dem.tif N66 E130

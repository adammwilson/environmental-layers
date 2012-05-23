library(rgdal)
aster.dir <- "DEM/asterGdem"

#Check that path=correct:
a<- GDALinfo("DEM/asterGdem/ASTGTM_N59E069_dem.tif", 
    silent=TRUE,returnRAT=TRUE)

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

aster.tiles <- list.files(aster.dir, pattern="^ASTGTM.*_dem.tif$") 
titlecheck <- sapply(aster.tiles, check, path=aster.dir)

BadAster_df<- data.frame(expected=titlecheck[titlecheck!="TRUE"])

# ASTGTM_N59E069_dem.tif  N63 E109
#ASTGTM_N63E113_dem.tif  N69 E107
# ASTGTM_N63E117_dem.tif	N69 E113
#ASTGTM_N64E098_dem.tif	N70 E117
#ASTGTM_N65E104_dem.tif	N73 E084
#	ASTGTM_N65E111_dem.tif	N73 E098
#	ASTGTM_N65E117_dem.tif	N66 E130

KnownBadcheck <- function(badtilename, path="DEM/asterGdem/incorrectTilesJuly2011", silent=TRUE) {
    # build expected filename
    origin <- round(GDALinfo(file.path(path, badtilename),
        silent=silent)[c("ll.x", "ll.y")])
    ly <- origin["ll.y"]
    y <- sprintf("%s%02d", if (ly>=0) "N" else "S", abs(ly))
    lx <- origin["ll.x"]
    x <- sprintf("%s%03d", if (lx>=0) "E" else "W", abs(lx))
    Badexpected.name <- paste("ASTGTM_", y, x, "_dem.tif", sep="")
    # compare to actual filename
    if (badtilename==Badexpected.name) {
        TRUE
    } else {
        paste(y, x)
    }
}

bad.tiles <- list.files("DEM/asterGdem/incorrectTilesJuly2011", pattern="^ASTGTM.*_dem.tif$") 
badtitlecheck <- sapply(bad.tiles, KnownBadcheck, path="DEM/asterGdem/incorrectTilesJuly2011")

BadTitle_df<- data.frame(expected=badtitlecheck[badtitlecheck!="TRUE"])

#----------------------------------------------------------------------------------------------------------------------
#Check num.tif files
b<- GDALinfo("DEM/asterGdem/ASTGTM_N49E000_num.tif", 
silent=TRUE,returnRAT=TRUE)
b

Numcheck <- function(Numtilename, path=".", silent=TRUE) {
    # build expected filename
    origin <- round(GDALinfo(file.path(path, Numtilename),
        silent=silent)[c("ll.x", "ll.y")])
    ly <- origin["ll.y"]
    y <- sprintf("%s%02d", if (ly>=0) "N" else "S", abs(ly))
    lx <- origin["ll.x"]
    x <- sprintf("%s%03d", if (lx>=0) "E" else "W", abs(lx))
    Numexpected.name <- paste("ASTGTM_", y, x, "_num.tif", sep="")
    # compare to actual filename
    if (Numtilename==Numexpected.name) {
        TRUE
    } else {
        paste(y, x)
    }
}

Num.tiles <- list.files("DEM/asterGdem/NewTiles", pattern="^ASTGTM.*_num.tif$") 
Numtitlecheck <- sapply(Num.tiles, Numcheck, path="DEM/asterGdem/NewTiles")

NumTitle_df<- data.frame(expected=Numtitlecheck[Numtitlecheck!="TRUE"])

#Located one bad num.tif file
N49E007_NumRead<- GDALinfo("DEM/asterGdem/ASTGTM_N49E007_num.tif", 
silent=TRUE,returnRAT=TRUE)

GDALinfo("DEM/asterGdem/ASTGTM_N59E069_dem.tif", 
silent=TRUE,returnRAT=TRUE)

#--------------------------------------------------------------------------------
#Check new tiles uploaded to replace faulty tiles

Newcheck <- function(Newtilename, path=".", silent=TRUE) {
    # build expected filename
    origin <- round(GDALinfo(file.path(path, Newtilename),
        silent=silent)[c("ll.x", "ll.y")])
    ly <- origin["ll.y"]
    y <- sprintf("%s%02d", if (ly>=0) "N" else "S", abs(ly))
    lx <- origin["ll.x"]
    x <- sprintf("%s%03d", if (lx>=0) "E" else "W", abs(lx))
    Newexpected.name <- paste("ASTGTM_", y, x, "_dem.tif", sep="")
    # compare to actual filename
    if (Newtilename==Newexpected.name) {
        TRUE
    } else {
        paste(y, x)
    }
}

New.tiles <- list.files("DEM/asterGdem/NewTiles", pattern="^ASTGTM.*_dem.tif$") 
Newtitlecheck <- sapply(New.tiles, Newcheck, path="DEM/asterGdem/NewTiles")

NewAster_df<- data.frame(Newexpected=Newtitlecheck[Newtitlecheck!="TRUE"])

#Yeah, no mistakes in first 4!

g<- list.files("DEM/asterGdem/N59to81_E60to99", pattern="^ASTGTM.*_dem.tif$")
h<- list.files("DEM/asterGdem/N59to81_E60to99/Non_DEM's", pattern="^ASTGTM.*_num.tif$")

GDALinfo("DEM/asterGdem/N59to81_E60to99/ASTGTM_N64E098_dem.tif", 
silent=TRUE,returnRAT=TRUE)

#--------------------------------------------------------------------------
#new folder check
nfcheck <- function(nftilename, path=".", silent=TRUE) {
    # build expected filename
    origin <- round(GDALinfo(file.path(path, nftilename),
        silent=silent)[c("ll.x", "ll.y")])
    ly <- origin["ll.y"]
    y <- sprintf("%s%02d", if (ly>=0) "N" else "S", abs(ly))
    lx <- origin["ll.x"]
    x <- sprintf("%s%03d", if (lx>=0) "E" else "W", abs(lx))
    nfexpected.name <- paste("ASTGTM_", y, x, "_dem.tif", sep="")
    # compare to actual filename
    if (nftilename==nfexpected.name) {
        TRUE
    } else {
        paste(y, x)
    }
}

nf.tiles <- list.files("DEM/asterGdem/N59to81_E60to99", pattern="^ASTGTM.*_dem.tif$") 
nftitlecheck <- sapply(nf.tiles, nfcheck, path="DEM/asterGdem/N59to81_E60to99")

nfAster_df<- data.frame(nfexpected=nftitlecheck[nftitlecheck!="TRUE"])


Numcheck <- function(Numtilename, path=".", silent=TRUE) {
    # build expected filename
    origin <- round(GDALinfo(file.path(path, Numtilename),
        silent=silent)[c("ll.x", "ll.y")])
    ly <- origin["ll.y"]
    y <- sprintf("%s%02d", if (ly>=0) "N" else "S", abs(ly))
    lx <- origin["ll.x"]
    x <- sprintf("%s%03d", if (lx>=0) "E" else "W", abs(lx))
    Numexpected.name <- paste("ASTGTM_", y, x, "_num.tif", sep="")
    # compare to actual filename
    if (Numtilename==Numexpected.name) {
        TRUE
    } else {
        paste(y, x)
    }
}

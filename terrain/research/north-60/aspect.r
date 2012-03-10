# R code to plot latitudinal profiles of (circular) mean aspect, along
# with both RMSE and (circular) correlation coefficients comparing fused
# layers with both the raw ASTER and with the Canada DEM
#
# Aspect layers were generated from the respect DEMs in this way:
#   $ gdaldem aspect -s 111120 <layer>.tif <layer>_a.tif
# ...where the default azimuthal behavior produces output values ranging
# from 0-360 where 0 is north, and proceeding clockwise
#
# For exploratory plotting, note the following (uses 'circular'
# package):
#  > cx <- circular(as.matrix(a.bg)[151,], units="degrees",
#      rotation="clock", zero=pi/2)
#  > rose.diag(cx, bins=8)
#  > points(mean.circular(cx, na.rm=TRUE), col="red")
#
#
# The results (plots) are saved at 
#   "/data/project/organisms/DEM?Yuni/documents/aspect"
#
# Original author: Jim Regetz
# [13-JUL-2011]
#   Edits by Yuina to auto-apply for nine regions.
#   
#   9-Dec-2011
#   Yuina Nunokawa





library(raster)
library(circular)


# load aspecct rasters
aster.dir      <- "/data/project/organisms/DEM/Yuni/Data/aster2"
srtm.dir       <- "/data/project/organisms/DEM/Yuni/Data/srtm"
aster.straddle <- list.files(aster.dir, pattern="^aster2.*_straddle_a.tif")
srtm.below     <- list.files(srtm.dir, pattern="^srtm_.*_below_a.tif") 
fused.straddle <- list.files(aster.dir, pattern="^fused.*_straddle_a.tif")
fused.bgau     <- list.files(aster.dir, pattern="^fused.*_blendgau_a.tif")



# simple helper function to calculate row-wise means using circular
# mean, patterned after circ.mean in the CircStats package
rowMeansC <- function(r1, na.rm=TRUE) {
    m1 <- as.matrix(r1)
    m1[] <- (m1 * pi)/180
    sinr <- rowSums(sin(m1), na.rm=na.rm)
    cosr <- rowSums(cos(m1), na.rm=na.rm)
    cmeans <- atan2(sinr, cosr)
    (cmeans * 180)/pi
}



aspect <- function(a.aster2, a.srtm, a.uncor, a.bg, file.name, aster.dir, srtm.dir){

        # extract raster latitudes for later
        lats2400 <- yFromRow(a.aster2, 1:nrow(a.aster2))
        lats1200 <- yFromRow(a.srtm, 1:nrow(a.srtm))

        # initialize output pdf device driver
        name <- paste("aspect_", file.name, ".pdf", sep="") 
        pdf(file.path("/data/project/organisms/DEM/Yuni/documents/aspect", file=name), height=8, width=11.5)

        #
        # plot latitudinal profiles of mean aspect
        #
       
        par(mfrow=c(2,2), omi=c(1,1,1,1))
        ylim <- c(-180, 180)


        plot(lats2400, rowMeansC(a.aster2), type="l", xlab="Latitude", ylab="Mean aspect", ylim=ylim)
        axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
        lines(lats1200, rowMeans(as.matrix(a.srtm), na.rm=TRUE), col="red")
        text(min(lats2400), max(ylim)+0.5, pos=4, font=3, labels="Original")
        legend("topright", legend=c("ASTER2", "SRTM"), col=c("blue", "red"), lty=c(1, 1), bty="n")
        abline(v=60, col="red", lty=2)
        mtext(expression(paste("Latitudinal profiles of mean aspect", file.name)), adj=0, line=2, font=2)

        plot(lats2400, rowMeansC(a.uncor), type="l", xlab="Latitude", ylab="Mean aspect", ylim=ylim)
        axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
        legend("topright", legend=c("ASTER2"), col=c("black"), lty=c(1, 1), bty="n")
        text(min(lats2400), max(ylim)+0.5, pos=4, font=3, labels="simple fused")
        abline(v=60, col="red", lty=2)

        plot(lats2400, rowMeansC(a.bg), type="l", xlab="Latitude", ylab="Mean aspect", ylim=ylim)
        legend("topright", legend=c("ASTER2"), col=c("black"), lty=c(1, 1), bty="n")
        text(min(lats2400), max(ylim)+0.5, pos=4, font=3, labels="gaussian blend")
        abline(v=60, col="red", lty=2)

        dev.off () 

}




num.regions <- 9
plotAspect <-lapply(1:num.regions, function(region, aster.dir, srtm.dir){
    a.aster2 <- raster(file.path(aster.dir, aster.straddle[region])) 
    a.srtm <- raster(file.path(srtm.dir, srtm.below[region]))  
    a.uncor <- raster(file.path(aster.dir, fused.straddle[region]))
    a.bg <- raster(file.path(aster.dir, fused.bgau[region])) 
    file.name <- substr(srtm.below[region], 6, 13)
    aspect(a.aster2, a.srtm, a.uncor, a.bg, file.name, aster.dir, srtm.dir)
}, aster.dir=aster.dir, srtm.dir=srtm.dir)



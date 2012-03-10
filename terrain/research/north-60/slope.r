# R code to plot latitudinal profiles of mean slope, comparing fused layers 
# with both the raw ASTER. I did not include the comparison of both RMSE 
# and correlation coefficients,which Jim included in his analysis. 
#
# The results (plots) are saved at 
#   "/data/project/organisms/DEM?Yuni/documents/slope"
#
# Original author: Jim Regetz
# [13-JUL-2011]
#   Edits by Yuina to auto-apply for nine regions.
#   
#   9-Dec-2011
#   Yuina Nunokawa 




library(raster)


# load slope rasters
aster.dir      <- "/data/project/organisms/DEM/Yuni/Data/aster2"
srtm.dir       <- "/data/project/organisms/DEM/Yuni/Data/srtm"
aster.straddle <- list.files(aster.dir, pattern="^aster2.*_straddle_s.tif")
srtm.below     <- list.files(srtm.dir, pattern="^srtm_.*_below_s.tif") 
fused.straddle <- list.files(aster.dir, pattern="^fused.*_straddle_s.tif")
fused.bgau     <- list.files(aster.dir, pattern="^fused.*_blendgau_s.tif")


slope <- function(s.aster2, s.srtm, s.uncor, s.bg, file.name, ylim, aster.dir, srtm.dir){

        # extract raster latitudes for later
        lats2400 <- yFromRow(s.aster2, 1:nrow(s.aster2))
        lats1200 <- yFromRow(s.srtm, 1:nrow(s.srtm))

        # initialize output pdf device driver
        name <- paste("slope_", file.name, ".pdf", sep="") 
        pdf(file.path("/data/project/organisms/DEM/Yuni/documents/slope", file=name), height=8, width=11.5)

        #
        # plot latitudinal profiles of mean slope
        #
        par(mfrow=c(2,2), omi=c(1,1,1,1))
	ylim <- ylim
	plot(lats2400, rowMeans(as.matrix(s.aster2), na.rm=TRUE), type="l", xlab="Latitude", ylab="Mean slope", ylim=ylim)
        lines(lats1200, rowMeans(as.matrix(s.srtm), na.rm=TRUE), col="red")
        text(min(lats2400), max(ylim)-0.5, pos=4, font=3, labels="Original")
        legend("bottomright", legend=c("ASTER2", "SRTM"), col=c("blue", "red"), lty=c(1, 1), bty="n")
        abline(v=60, col="red", lty=2)
        mtext(expression(paste("Latitudinal profiles of mean slope", file.name)), adj=0, line=2, font=2)

        plot(lats2400, rowMeans(as.matrix(s.uncor), na.rm=TRUE), type="l", xlab="Latitude", ylab="Mean slope", ylim=ylim)
        legend("bottomright", legend=c("ASTER2"), col=c("black"), lty=c(1, 1), bty="n")
        text(min(lats2400), max(ylim)-0.5, pos=4, font=3, labels="simple fused")
        abline(v=60, col="red", lty=2)

        plot(lats2400, rowMeans(as.matrix(s.bg), na.rm=TRUE), type="l", xlab="Latitude", ylab="Mean slope", ylim=ylim)
        legend("bottomright", legend=c("ASTER2"), col=c("black"), lty=c(1, 1), bty="n")
        text(min(lats2400), max(ylim)-0.5, pos=4, font=3, labels="gaussian blend")
        abline(v=60, col="red", lty=2)
    
        dev.off () 

}




num.regions <- 9
plotSlope <-lapply(1:num.regions, function(region, aster.dir, srtm.dir){
    s.aster2 <- raster(file.path(aster.dir, aster.straddle[region])) 
    s.srtm <- raster(file.path(srtm.dir, srtm.below[region]))  
    s.uncor <- raster(file.path(aster.dir, fused.straddle[region]))
    s.bg <- raster(file.path(aster.dir, fused.bgau[region])) 
    file.name <- substr(srtm.below[region], 6, 13)
    
    if (region==1){
        ylim <- c(0, 4)
    }else if(region==2){
        ylim <- c(0, 4)
    }else if (region==3){   
        ylim <- c(2, 7)
    }else if (region==4){
        ylim <-c(1, 7)
    }else if (region==5){
        ylim <- c(0, 3)
    }else if (region==6){
        ylim <- c(0, 8)
    }else if (region==7){
        ylim <- c(0, 3)
    }else if (region==8){
        ylim <- c(3, 7)
    }else{
        ylim <- c(0, 6)
    }
    slope(s.aster2, s.srtm, s.uncor, s.bg, file.name, ylim, aster.dir, srtm.dir)
}, aster.dir=aster.dir, srtm.dir=srtm.dir)



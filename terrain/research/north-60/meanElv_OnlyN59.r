# R script for creating a plot of mean elevation profile containing four lines 
# (aster2, srtm, uncorrected fused, blend gaussian fused) for each nine region
# The results (plots) are saved at 
#   "/data/project/organisms/DEM?Yuni/documents/meanElv"
#
# Original author: Jim Regetz
# [13-JUL-2011]
#   Edits by Yuina to auto-apply for nine regions.
#   
#   9-Dec-2011
#   Yuina Nunokawa 



library(raster)

aster.dir      <- "/data/project/organisms/DEM/Yuni/Data/aster2"
srtm.dir       <- "/data/project/organisms/DEM/Yuni/Data/srtm"
aster.straddle <- list.files(aster.dir, pattern="^aster2.*_straddle.tif")
srtm.below     <- list.files(srtm.dir, pattern="^srtm_.*_below.tif") 
fused.straddle <- list.files(aster.dir, pattern="^fused.*_straddle.tif")
fused.bgau     <- list.files(aster.dir, pattern="^fused.*_blendgau.tif")



mElevation <- function(d.aster2, d.srtm, d.uncor2, d.bg2, file.name, ylim, aster.dir, srtm.dir){

    # extract raster latitudes for later
    lats2400 <- yFromRow(d.aster2, 1:nrow(d.aster2))
    lats1200 <- yFromRow(d.srtm, 1:nrow(d.srtm))

    # initialize output pdf device driver
    name <- paste("mElevation_", file.name, ".pdf", sep="") 
    pdf(file.path("/data/project/organisms/DEM/Yuni/documents/meanElv", file=name), height=8, width=11.5)
    
    par(mfrow=c(2,2), omi=c(1,1,1,1))
    ylim <- ylim
   
    # plot latitudinal profiles of mean elevation for aster2 and srtm 
    plot(lats2400, rowMeans(as.matrix(d.aster2), na.rm=TRUE), type="l",
        xlab="Latitude", ylab="Mean elevation", ylim=ylim)
    text(min(lats1200), min(ylim)+0.5, pos=4, font=3, labels="AGDEM2")
    lines(lats1200, rowMeans(as.matrix(d.srtm), na.rm=TRUE), col="blue")
    legend("bottomright", legend=c("SRTM", "ASTER2"), col=c("blue",
        "black"), lty=c(1, 1), bty="n")
    abline(v=60, col="red", lty=2)
    mtext(expression(paste("Latitudinal profiles of mean elevation", file.name)), adj=0, line=2, font=2)
    

    # plot latitudial profiles of mean elevation for Uncorrected fused and blend gaussian fused
    plot(lats2400, rowMeans(as.matrix(d.uncor2), na.rm=TRUE), type="l",
        xlab="Latitude", ylab="Mean elevation", ylim=ylim)
    text(min(lats2400), min(ylim)+0.5, pos=4, font=3, labels="simple fuse and gaussian")
    lines(lats2400, rowMeans(as.matrix(d.bg2), na.rm=TRUE), col="red")
    legend("bottomright", legend=c("Simple Fused", "Gaussian Blend"), col=c("black","red"), lty=c(1, 1), bty="n")
    abline(v=60, col="red", lty=2)
    dev.off () 

}





num.regions <- 9
plotElevation <-lapply(1:num.regions, function(region, aster.dir, srtm.dir){
    d.aster2 <- raster(file.path(aster.dir, aster.straddle[region])) 
    d.srtm <- raster(file.path(srtm.dir, srtm.below[region]))  
    d.uncor2 <- raster(file.path(aster.dir, fused.straddle[region]))
    d.bg2 <- raster(file.path(aster.dir, fused.bgau[region])) 
    file.name <- substr(srtm.below[region], 6, 13)
    
    if (region==1){
        ylim <- c(120, 150)
    }else if(region==2){
        ylim <- c(140, 175)
    }else if (region==3){   
        ylim <- c(350, 550)
    }else if (region==4){
        ylim <-c(50, 450)
    }else if (region==5){
        ylim <- c(60, 200)
    }else if (region==6){
        ylim <- c(0, 90)
    }else if (region==7){
        ylim <- c(60, 100)
    }else if (region==8){
        ylim <- c(600, 750)
    }else{
        ylim <- c(30, 280)
    }

    mElevation(d.aster2, d.srtm, d.uncor2, d.bg2, file.name, ylim, aster.dir, srtm.dir)
}, aster.dir=aster.dir, srtm.dir=srtm.dir)


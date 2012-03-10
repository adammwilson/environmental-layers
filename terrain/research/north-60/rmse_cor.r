# R script for calculating row-wise RMSEs and row-wise correlation coefficients.
#
# The results (plots) are saved at 
#   "/data/project/organisms/DEM?Yuni/documents/rmse_cor"
#
#
# Original author: Jim Regetz 
# [13-JUL-2011]
#	Edits by Yuni to apply ASTER GDEM2, instead of GDEM1, as well as auto-applying 
#	for each nine region.  [9-Dec-2011] 
# 



# simple helper function to calculate row-wise RMSEs
rmse <- function(r1, r2, na.rm=TRUE, use) {
    diffs <- abs(as.matrix(r1) - as.matrix(r2))
    if (!missing(use)) diffs[!use] <- NA
    sqrt(rowMeans(diffs^2, na.rm=na.rm))
}


# simple helper function to calculate row-wise correlation coefficients
corByLat <- function(r1, r2, rows) {
    if (missing(rows)) {
        rows <- 1:nrow(r1)
    }
    m1 <- as.matrix(r1)
    m2 <- as.matrix(r2)
    sapply(rows, function(row) cor(m1[row,], m2[row,],
    use="pairwise.complete.obs"))
}



library(raster)
aster.dir      <- "/data/project/organisms/DEM/Yuni/Data/aster2"
srtm.dir       <- "/data/project/organisms/DEM/Yuni/Data/srtm"
aster.straddle <- list.files(aster.dir, pattern="^aster2.*_straddle_s.tif")
srtm.below     <- list.files(srtm.dir, pattern="^srtm_.*_below_s.tif") 
fused.straddle <- list.files(aster.dir, pattern="^fused.*_straddle_s.tif")
fused.bgau     <- list.files(aster.dir, pattern="^fused.*_blendgau_s.tif")



rmse.corByLat <- function(d.aster2, d.srtm, d.uncor2, d.bg2, file.name,  aster.dir, srtm.dir){

    # extract raster latitudes for later
    lats2400 <- yFromRow(d.aster2, 1:nrow(d.aster2))
    lats1200 <- yFromRow(d.srtm, 1:nrow(d.srtm))

    # initialize output pdf device driver
    name <- paste("rmse.cor_", file.name, ".pdf", sep="") 
    pdf(file.path("/data/project/organisms/DEM/Yuni/documents/rmse_cor", file=name), height=8, width=11.5)

    par(mfrow=c(2,2), omi=c(1,1,1,1))
    ylim <- c(0,30)
 
    # plot rmse
    par(mfrow=c(2,3), omi=c(1,1,1,1))
    ylim <- ylim

    # with uncorrected fusion 
    plot(lats2400, rmse(d.uncor2, d.aster2), type="l", xlab="Latitude", ylab="RMSE", ylim=ylim)
    lines(lats1200, rmse(crop(d.uncor2, extent(d.srtm)), d.srtm), col="blue")
    legend("topright", legend=c("ASTER2", "SRTM"), col=c("black", "blue"), lty=c(1, 1), bty="n")
    text(min(lats2400), max(ylim)-1, pos=4, font=3, labels="simple fuse")
    abline(v=60, col="red", lty=2)
    mtext(expression(paste("Elevation discrepancies with respect to separate ASTER2/SRTM components", file.name)), adj=0, line=2, font=2)

    # with gaussian fusion 
    plot(lats2400, rmse(d.bg2, d.aster2), type="l", xlab="Latitude", ylab="RMSE", ylim=ylim)
    lines(lats1200, rmse(crop(d.bg2, extent(d.srtm)), d.srtm), col="blue")
    legend("topright", legend=c("ASTER2", "SRTM"), col=c("black", "blue"), lty=c(1, 1),
        bty="n")
    text(min(lats2400), max(ylim)-1, pos=4, font=3, labels="gaussian blend")
    abline(v=60, col="red", lty=2)


    # plot latitudinal profiles of correlation coefficients
    par(mfrow=c(2,3), omi=c(1,1,1,1))
    ylim <- c(0.99, 1) 

    plot(lats2400, corByLat(d.uncor2, d.aster2), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
    lines(lats1200, corByLat(crop(d.uncor2, extent(d.srtm)), d.srtm), col="blue")
    legend("bottomright", legend=c("ASTER2", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
    text(min(lats2400), min(ylim), pos=4, font=3, labels="simple fuse")
    abline(v=60, col="red", lty=2)
    mtext(expression(paste("Elevation correlations with respect to separate ASTER2/SRTM components", file.name)), adj=0, line=2, font=2)


    plot(lats2400, corByLat(d.bg2, d.aster2), type="l", xlab="Latitude", 
        ylab="Correlation", ylim=ylim)
    lines(lats1200, corByLat(crop(d.bg2, extent(d.srtm)), d.srtm), col="blue")
    legend("bottomright", legend=c("ASTER2", "SRTM"), col=c("black", "blue"),
        lty=c(1, 1), bty="n")
    text(min(lats2400), min(ylim), pos=4, font=3, labels="gaussian blend")
    abline(v=60, col="red", lty=2)

   dev.off () 
}





num.regions <- 9
plot.rmse.corByLat <-lapply(1:num.regions, function(region, aster.dir, srtm.dir){
    d.aster <- raster(file.path(aster.dir, aster.straddle[region])) 
    d.srtm <- raster(file.path(srtm.dir, srtm.below[region]))  
    d.uncor <- raster(file.path(aster.dir, fused.straddle[region]))
    d.bg <- raster(file.path(aster.dir, fused.bgau[region])) 
    file.name <- substr(srtm.below[region], 6, 13)

   rmse.corByLat(d.aster, d.srtm, d.uncor, d.bg, file.name, aster.dir, srtm.dir)
}, aster.dir=aster.dir, srtm.dir=srtm.dir)


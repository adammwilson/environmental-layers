# R script for ploting a pattern of ASTER2-SRTM deltas as a function of ASTER2 elevation
# The results (plots) are saved at 
#   "/data/project/organisms/DEM?Yuni/documents/meanElv"
#
# Original author: Jim Regetz
# [13-JUL-2011]
#   Edits by Yuina to auto-apply for nine regions.
#   
#   9-Dec-2011
#   Yuina Nunokawa 






# plot pattern of ASTER2-SRTM deltas as a function of ASTER2 elevation
plotMeanDeltaByElev <- function(delta.vals, elev, ...) {
    mean.by.elev <- tapply(delta.vals, elev, mean)
    sd.by.elev <- tapply(delta.vals, elev, sd)
    n.by.elev <- tapply(delta.vals, elev, length)
    se.by.elev <- sd.by.elev/sqrt(n.by.elev)
    na.se.points <- mean.by.elev[is.na(se.by.elev)]
    se.by.elev[is.na(se.by.elev)] <- 0
    elev <- as.numeric(names(mean.by.elev))
    plot(elev, mean.by.elev, pch=16, xlim=c(0, max(elev)), 
        ylim=c(min(mean.by.elev - se.by.elev), max(mean.by.elev + se.by.elev)),
        type="n", ...)
    segments(elev, mean.by.elev-se.by.elev,
        as.numeric(names(mean.by.elev)), mean.by.elev+se.by.elev,col="grey")
    points(elev, mean.by.elev, pch=".")
    points(as.numeric(names(na.se.points)), na.se.points, pch=4,
    col="red", cex=0.5)
}




plotDeltaBins <- function(delta.vals, elev, bin.min, bin.width, bin.max=1500,
    outline=FALSE, ...) {
    breaks <- seq(bin.min, bin.max, by=bin.width)
    midpts <- c(paste("<", bin.min, sep=""), head(breaks, -1) + bin.width/2,
        paste(">", bin.max, sep=""))
    elev <- cut(elev, breaks=c(0, breaks, Inf), labels=midpts)
    bp <- boxplot(delta.vals ~ elev, outline=outline, col="lightgray",
        frame=FALSE, ...)
    text(1:length(bp$n), bp$stats[5,], labels=round(bp$n/1000),
        pos=3, cex=0.5, offset=0.2, font=3, col="gray")
    #axis(3, at=seq_along(bp$n), labels=paste(round(bp$n/1000), "k", sep=""),
    #    cex.axis=0.7, tick=FALSE, font=3, line=-1)
    #mtext("n =", side=3, adj=0, font=3, cex=0.7)
    abline(h=median(delta.vals), col="red", lty=2)
    invisible(bp)
}




library(raster)

aster.dir      <- "/data/project/organisms/DEM/Yuni/Data/aster2"
srtm.dir       <- "/data/project/organisms/DEM/Yuni/Data/srtm"
aster.straddle <- list.files(aster.dir, pattern="^aster2.*_straddle.tif")
srtm.below     <- list.files(srtm.dir, pattern="^srtm_.*_below.tif") 
fused.straddle <- list.files(aster.dir, pattern="^fused.*_straddle.tif")
fused.bgau     <- list.files(aster.dir, pattern="^fused.*_blendgau.tif")





deltas <- function(d.aster, d.srtm, d.uncor, d.bg, file.name, aster.dir, srtm.dir){

    # initialize output pdf device driver
    name <- paste("deltas_", file.name, ".pdf", sep="") 
    pdf(file.path("/data/project/organisms/DEM/Yuni/documents/deltas", file=name), height=8, width=11.5)


    # plot mean delta elevations 
    d.aster2.crop.vals <- values(crop(d.aster, extent(d.srtm)))
    d.srtm.vals <- values(d.srtm)
    delta.vals <- d.aster2.crop.vals - d.srtm.vals
    plotMeanDeltaByElev(delta.vals, d.aster2.crop.vals,
    xlab="ASTER2 elevation (m)", ylab="ASTER2-SRTM difference (m)")

    # plot aster2 srtm bins
    d.aster2.crop.vals <- values(crop(d.aster, extent(d.srtm)))
    d.srtm.vals <- values(d.srtm)
    delta.vals <- d.aster2.crop.vals - d.srtm.vals
    #  d.aster2.crop.vals <- d.aster2.crop.vals[d.srtm.vals>0]
    #  d.srtm.vals <- d.srtm.vals[d.srtm.vals>0]

    plotDeltaBins(delta.vals, d.srtm.vals, 150, 50, 1500, las=2,
    cex.axis=0.8, xlab="Midpoints of SRTM elevation bins (m)",
    ylab="ASTER2 - SRTM difference (m)")


    # plot scatter of aster vs srtm
    plot(jitter(d.srtm.vals), jitter(d.aster2.crop.vals), pch=".",
    xlab="SRTM elevation (m)", ylab="ASTER2 elevation (m)", cex=0.5)
    abline(median(delta.vals), 1, col="red", cex=0.5)
    abline(0, 1, col="blue", lty=2, cex=0.5)

    # add inset histogram of differences
    opar <- par(fig=c(0.55, 0.95, 0.1, 0.6), new=TRUE)
    h <- hist(delta.vals[abs(delta.vals)<60], breaks=48, xlab=NA, main=NULL,
    col=grey(0.8), border=grey(0.3), yaxt="n", ylab=NA, cex.axis=0.5,
    cex.lab=0.5, tcl=-0.25, mgp=c(3,0,0))
    text(10, 0.4*max(h$counts), labels=paste("Entire range:\n(",
    min(delta.vals), ", ", max(delta.vals), ")", sep=""), cex=0.6, adj=c(0,0))
    mtext("ASTER2 - SRTM (m)", side=1, cex=0.5, line=0.6)

    dev.off () 
}



num.regions <- 9
plot.deltas <-lapply(1:num.regions, function(region, aster.dir, srtm.dir){
    d.aster <- raster(file.path(aster.dir, aster.straddle[region])) 
    d.srtm <- raster(file.path(srtm.dir, srtm.below[region]))  
    d.uncor <- raster(file.path(aster.dir, fused.straddle[region]))
    d.bg <- raster(file.path(aster.dir, fused.bgau[region])) 
    file.name <- substr(srtm.below[region],6, 13)

    deltas(d.aster, d.srtm, d.uncor, d.bg, file.name, aster.dir, srtm.dir)
}, aster.dir=aster.dir, srtm.dir=srtm.dir)



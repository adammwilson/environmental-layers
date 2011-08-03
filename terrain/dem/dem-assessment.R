# R code to plot latitudinal profiles of mean elevation, along with both
# RMSE and correlation coefficients comparing fused layers with both the
# raw ASTER and with the Canada DEM
#
# Jim Regetz
# NCEAS
# Created on 08-Jun-2011

library(raster)

datadir <- "/home/regetz/media/temp/terrain/dem"

# load elevation rasters
d.aster <- raster(file.path(datadir, "aster_300straddle.tif"))
d.srtm <- raster(file.path(datadir, "srtm_150below.tif"))
d.uncor <- raster(file.path(datadir, "fused_300straddle.tif"))
d.enblend <- raster(file.path(datadir, "fused_300straddle_enblend.tif"))
d.bg <- raster(file.path(datadir, "fused_300straddle_blendgau.tif"))
d.can <- raster(file.path(datadir, "cdem_300straddle.tif"))

# extract raster latitudes for later
lats300 <- yFromRow(d.aster, 1:nrow(d.aster))
lats150 <- yFromRow(d.srtm, 1:nrow(d.srtm))


#
# plot latitudinal profiles of mean elevation
#

# initialize output pdf device driver
pdf("elevation-assessment.pdf", height=8, width=11.5)

par(mfrow=c(2,2), omi=c(1,1,1,1))

ylim <- c(540, 575)

plot(lats300, rowMeans(as.matrix(d.can), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean elevation", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="Original DEMs")
lines(lats300, rowMeans(as.matrix(d.aster), na.rm=TRUE), col="blue")
lines(lats150, rowMeans(as.matrix(d.srtm), na.rm=TRUE), col="red")
legend("bottomright", legend=c("SRTM", "CDED", "ASTER"), col=c("red",
    "black", "blue"), lty=c(1, 1), bty="n")
abline(v=60, col="red", lty=2)
mtext(expression(paste("Latitudinal profiles of mean elevation (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rowMeans(as.matrix(d.uncor), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean elevation", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="simple fuse")
abline(v=60, col="red", lty=2)

plot(lats300, rowMeans(as.matrix(d.enblend), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean elevation", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, rowMeans(as.matrix(d.bg), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean elevation", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)


#
# plot latitudinal profiles of RMSE
#

# simple helper function to calculate row-wise RMSEs
rmse <- function(r1, r2, na.rm=TRUE, use) {
    diffs <- abs(as.matrix(r1) - as.matrix(r2))
    if (!missing(use)) diffs[!use] <- NA
    sqrt(rowMeans(diffs^2, na.rm=na.rm))
}

par(mfrow=c(2,3), omi=c(1,1,1,1))

ylim <- c(0, 35)

# ...with respect to ASTER
plot(lats300, rmse(d.uncor, d.aster), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
lines(lats150, rmse(crop(d.uncor, extent(d.srtm)), d.srtm), col="blue")
legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-1, pos=4, font=3, labels="simple fuse")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Elevation discrepancies with respect to separate ASTER/SRTM components (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rmse(d.enblend, d.aster), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
lines(lats150, rmse(crop(d.enblend, extent(d.srtm)), d.srtm), col="blue")
legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-1, pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, rmse(d.bg, d.aster), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
lines(lats150, rmse(crop(d.bg, extent(d.srtm)), d.srtm), col="blue")
legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-1, pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# ...with respect to CDEM
plot(lats300, rmse(d.uncor, d.can), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-1, pos=4, font=3, labels="simple fuse")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Elevation discrepancies with respect to Canada DEM (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rmse(d.enblend, d.can), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-1, pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, rmse(d.bg, d.can), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-1, pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)


#
# plot latitudinal profiles of correlation coefficients
#

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

par(mfrow=c(2,3), omi=c(1,1,1,1))

ylim <- c(0.99, 1)

# ...with respect to ASTER
plot(lats300, corByLat(d.uncor, d.aster), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
lines(lats150, corByLat(crop(d.uncor, extent(d.srtm)), d.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="simple fuse")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Elevation correlations with respect to separate ASTER/SRTM components (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, corByLat(d.enblend, d.aster), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
lines(lats150, corByLat(crop(d.enblend, extent(d.srtm)), d.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, corByLat(d.bg, d.aster), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
lines(lats150, corByLat(crop(d.bg, extent(d.srtm)), d.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# ...with respect to CDEM
plot(lats300, corByLat(d.uncor, d.can), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="simple fuse")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Elevation correlations with respect to Canada DEM (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, corByLat(d.enblend, d.can), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, corByLat(d.bg, d.can), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# close pdf device driver
dev.off()

#
# plot pattern of ASTER-SRTM deltas as a function of ASTER elevation
#

plotMeanDeltaByElev <- function(delta.vals, elev, ...) {
    mean.by.elev <- tapply(delta.vals, elev, mean)
    sd.by.elev <- tapply(delta.vals, elev, sd)
    n.by.elev <- tapply(delta.vals, elev, length)
    se.by.elev <- sd.by.elev/sqrt(n.by.elev)
    na.se.points <- mean.by.elev[is.na(se.by.elev)]
    se.by.elev[is.na(se.by.elev)] <- 0
    elev <- as.numeric(names(mean.by.elev))
    plot(elev, mean.by.elev, pch=16,
        xlim=c(0, max(elev)), ylim=c(min(mean.by.elev -
        se.by.elev), max(mean.by.elev + se.by.elev)), type="n", ...)
    segments(elev, mean.by.elev-se.by.elev,
        as.numeric(names(mean.by.elev)), mean.by.elev+se.by.elev,
        col="grey")
    points(elev, mean.by.elev, pch=".")
    points(as.numeric(names(na.se.points)), na.se.points, pch=4,
        col="red", cex=0.5)
}


d.aster.crop.vals <- values(crop(d.aster, extent(d.srtm)))
d.srtm.vals <- values(d.srtm)
delta.vals <- d.aster.crop.vals - d.srtm.vals
plotMeanDeltaByElev(delta.vals, d.aster.crop.vals,
    xlab="ASTER elevation (m)", ylab="ASTER-SRTM difference (m)")

plotDeltaBins <- function(delta.vals, elev, bin.min, bin.width, bin.max=1500,
    outline=FALSE, ...) {
    breaks <- seq(bin.min, bin.max, by=bin.width)
    midpts <- c(
        paste("<", bin.min, sep=""),
        head(breaks, -1) + bin.width/2,
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

d.aster.crop.vals <- values(crop(d.aster, extent(d.srtm)))
d.srtm.vals <- values(d.srtm)
delta.vals <- d.aster.crop.vals - d.srtm.vals
#  d.aster.crop.vals <- d.aster.crop.vals[d.srtm.vals>0]
#  d.srtm.vals <- d.srtm.vals[d.srtm.vals>0]

png("aster-srtm-bins.png", height=5, width=8, units="in", res=300)
plotDeltaBins(delta.vals, d.srtm.vals, 150, 50, 1500, las=2,
    cex.axis=0.8, xlab="Midpoints of SRTM elevation bins (m)",
    ylab="ASTER - SRTM difference (m)")
dev.off()

# plot scatter of aster vs srtm
png("aster-srtm-scatter.png", height=5, width=8, units="in", res=300)
plot(jitter(d.srtm.vals), jitter(d.aster.crop.vals), pch=".",
    xlab="SRTM elevation (m)", ylab="ASTER elevation (m)", cex=0.5)
abline(median(delta.vals), 1, col="red", cex=0.5)
abline(0, 1, col="blue", lty=2, cex=0.5)
dev.off()



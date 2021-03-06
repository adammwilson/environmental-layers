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
# Jim Regetz
# NCEAS

library(raster)
library(circular)

datadir <- "/home/regetz/media/temp/terrain/aspect"

# load aspect rasters
a.aster <- raster(file.path(datadir, "aster_300straddle_a.tif"))
a.srtm <- raster(file.path(datadir, "srtm_150below_a.tif"))
a.uncor <- raster(file.path(datadir, "fused_300straddle_a.tif"))
a.enblend <- raster(file.path(datadir, "fused_300straddle_enblend_a.tif"))
a.bg <- raster(file.path(datadir, "fused_300straddle_blendgau_a.tif"))
a.can <- raster(file.path(datadir, "cdem_300straddle_a.tif"))

# extract raster latitudes for later
lats300 <- yFromRow(a.aster, 1:nrow(a.aster))
lats150 <- yFromRow(a.srtm, 1:nrow(a.srtm))

# initialize output pdf device driver
pdf("aspect-assessment.pdf", height=8, width=11.5)

#
# plot latitudinal profiles of mean aspect
#

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

par(mfrow=c(2,2), omi=c(1,1,1,1))
ylim <- c(-180, 180)

plot(lats300, rowMeansC(a.can), type="l", yaxt="n",
    xlab="Latitude", ylab="Mean aspect", ylim=ylim)
axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="Original DEMs")
lines(lats300, rowMeansC(a.aster), col="blue")
lines(lats150, rowMeansC(a.srtm), col="red")
legend("bottomright", legend=c("ASTER", "SRTM", "CDED"), col=c("blue",
    "red", "black"), lty=c(1, 1), bty="n")
abline(v=60, col="red", lty=2)
mtext(expression(paste("Latitudinal profiles of mean aspect (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rowMeansC(a.uncor), type="l", yaxt="n",
    xlab="Latitude", ylab="Mean aspect", ylim=ylim)
axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)

plot(lats300, rowMeansC(a.enblend), type="l", yaxt="n",
    xlab="Latitude", ylab="Mean aspect", ylim=ylim)
axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, rowMeansC(a.bg), type="l", yaxt="n",
    xlab="Latitude", ylab="Mean aspect", ylim=ylim)
axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)


#
# plot latitudinal profiles of RMSE
#

# simple helper function to calculate row-wise RMSEs, accounting for the
# fact that aspect values are circular (0-360), so the difference
# between e.g. 5 and 355 should only be 10
rmse <- function(r1, r2, na.rm=TRUE, use) {
    diffs <- abs(as.matrix(r1) - as.matrix(r2))
    if (!missing(use)) diffs[!use] <- NA
    diffs[] <- ifelse(diffs>180, 360-diffs, diffs)
    sqrt(rowMeans(diffs^2, na.rm=na.rm))
}

par(mfrow=c(2,3), omi=c(1,1,1,1))

ylim <- c(0, 100)

# ...with respect to ASTER
plot(lats300, rmse(a.uncor, a.aster), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
lines(lats150, rmse(crop(a.uncor, extent(a.srtm)), a.srtm), col="blue")
legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Aspect discrepancies with respect to separate ASTER/SRTM components (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rmse(a.enblend, a.aster), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
lines(lats150, rmse(crop(a.enblend, extent(a.srtm)), a.srtm), col="blue")
legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, rmse(a.bg, a.aster), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
lines(lats150, rmse(crop(a.bg, extent(a.srtm)), a.srtm), col="blue")
legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# ...with respect to CDEM
plot(lats300, rmse(a.uncor, a.can), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Aspect discrepancies with respect to Canada DEM (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rmse(a.enblend, a.can), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, rmse(a.bg, a.can), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)


#
# plot latitudinal profiles of correlation coefficients
#

# simple helper function to calculate row-wise *circular* correlation
# coefficients
corByLat <- function(r1, r2, rows) {
    if (missing(rows)) {
        rows <- 1:nrow(r1)
    }
    m1 <- circular(as.matrix(r1), units="degrees", rotation="clock")
    m2 <- circular(as.matrix(r2), units="degrees", rotation="clock")
    sapply(rows, function(row) {
        p <- cor.circular(m1[row,], m2[row,])
        if (is.null(p)) NA else p
        })
}

par(mfrow=c(2,3), omi=c(1,1,1,1))

ylim <- c(-1, 1)

# ...with respect to ASTER
plot(lats300, corByLat(a.uncor, a.aster), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
lines(lats150, corByLat(crop(a.uncor, extent(a.srtm)), a.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Aspect correlations with respect to separate ASTER/SRTM components (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, corByLat(a.enblend, a.aster), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
lines(lats150, corByLat(crop(a.enblend, extent(a.srtm)), a.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, corByLat(a.bg, a.aster), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
lines(lats150, corByLat(crop(a.bg, extent(a.srtm)), a.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# ...with respect to CDEM
plot(lats300, corByLat(a.uncor, a.can), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Aspect correlations with respect to Canada DEM (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, corByLat(a.enblend, a.can), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, corByLat(a.bg, a.can), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# close pdf device driver
dev.off()

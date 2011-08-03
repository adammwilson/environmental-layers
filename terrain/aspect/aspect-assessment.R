# R code to plot latitudinal profiles of mean aspect, along with both
# RMSE and correlation coefficients comparing fused layers with both the
# raw ASTER and with the Canada DEM
#
# Jim Regetz
# NCEAS
# Created on 08-Jun-2011

library(raster)

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

par(mfrow=c(2,2), omi=c(1,1,1,1))

ylim <- c(160, 180)

plot(lats300, rowMeans(as.matrix(a.can), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean aspect", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="Original DEMs")
lines(lats300, rowMeans(as.matrix(a.aster), na.rm=TRUE), col="blue")
lines(lats150, rowMeans(as.matrix(a.srtm), na.rm=TRUE), col="red")
legend("bottomright", legend=c("ASTER", "SRTM", "CDED"), col=c("blue",
    "red", "black"), lty=c(1, 1), bty="n")
abline(v=60, col="red", lty=2)
mtext(expression(paste("Latitudinal profiles of mean aspect (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rowMeans(as.matrix(a.uncor), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean aspect", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)

plot(lats300, rowMeans(as.matrix(a.enblend), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean aspect", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, rowMeans(as.matrix(a.bg), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean aspect", ylim=ylim)
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

# close pdf device driver
dev.off()

stop("not doing correlations")

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

ylim <- c(0, 1)

# ...with respect to ASTER
plot(lats300, corByLat(a.uncor, a.aster), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
lines(lats150, corByLat(crop(a.uncor, extent(a.srtm)), a.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Aspect correlations with respect to separate ASTER/SRTM components (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, corByLat(a.enblend, a.aster), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
lines(lats150, corByLat(crop(a.enblend, extent(a.srtm)), a.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, corByLat(a.bg, a.aster), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
lines(lats150, corByLat(crop(a.bg, extent(a.srtm)), a.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# ...with respect to CDEM
plot(lats300, corByLat(a.uncor, a.can), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Aspect correlations with respect to Canada DEM (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, corByLat(a.enblend, a.can), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, corByLat(a.bg, a.can), type="l", xlab="Latitude",
    ylab="Correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

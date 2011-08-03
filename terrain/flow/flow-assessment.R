# R code to plot latitudinal profiles of (circular) mean flow direction,
# along with both RMSE and (circular) correlation coefficients comparing
# fused layers with both the raw ASTER and with the Canada DEM
#
# Jim Regetz
# NCEAS

library(raster)
library(circular)

datadir <- "/home/regetz/media/temp/terrain/flow"

# create function to recode terraflow SFD values into degrees, with
# 0=North and proceeding clockwise (this matches gdaldem's default
# azimuth output for aspect calculation)
recode <- function(r) {
    v <- values(r)
    v[v==0] <- NA
    v[v==1] <- 90  ## east
    v[v==2] <- 135
    v[v==4] <- 180  ## south
    v[v==8] <- 225
    v[v==16] <- 270  ## west
    v[v==32] <- 315
    v[v==64] <- 0  ## north
    v[v==128] <- 45
    r[] <- v
    return(r)
}

# load flow direction rasters, recoding on the fly
sfd.aster <- recode(raster(file.path(datadir, "aster_300straddle_sfd.tif")))
sfd.srtm <- recode(raster(file.path(datadir, "srtm_150below_sfd.tif")))
sfd.uncor <- recode(raster(file.path(datadir, "fused_300straddle_sfd.tif")))
sfd.enblend <- recode(raster(file.path(datadir, "fused_300straddle_enblend_sfd.tif")))
sfd.bg <- recode(raster(file.path(datadir, "fused_300straddle_blendgau_sfd.tif")))
sfd.can <- recode(raster(file.path(datadir, "cdem_300straddle_sfd.tif")))

# extract raster latitudes for later
lats300 <- yFromRow(sfd.aster, 1:nrow(sfd.aster))
lats150 <- yFromRow(sfd.srtm, 1:nrow(sfd.srtm))

# initialize output pdf device driver
pdf("flowdir-assessment.pdf", height=8, width=11.5)

#
# plot latitudinal profiles of mean flow direction
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

plot(lats300, rowMeansC(sfd.can), type="l", yaxt="n",
    xlab="Latitude", ylab="Mean flow direction", ylim=ylim)
axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="Original DEMs")
lines(lats300, rowMeansC(sfd.aster), col="blue")
lines(lats150, rowMeansC(sfd.srtm), col="red")
legend("bottomright", legend=c("ASTER", "SRTM", "CDED"), col=c("blue",
    "red", "black"), lty=c(1, 1), bty="n")
abline(v=60, col="red", lty=2)
mtext(expression(paste("Latitudinal profiles of mean flow direction (",
    125*degree, "W to ", 100*degree, "W)")), adj=0, line=2, font=2)

#plot(lats300, rowMeans(as.matrix(sfd.uncor), na.rm=TRUE), type="l",
#    xlab="Latitude", ylab="Mean flow direction", ylim=ylim)
#text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="uncorrected")
#abline(v=60, col="red", lty=2)
#mtext(expression(paste("Latitudinal profiles of mean flow direction (",
#    125*degree, "W to ", 100*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rowMeansC(sfd.uncor), type="l", yaxt="n",
    xlab="Latitude", ylab="Mean flow direction", ylim=ylim)
axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)

plot(lats300, rowMeansC(sfd.enblend), type="l", yaxt="n",
    xlab="Latitude", ylab="Mean flow direction", ylim=ylim)
axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, rowMeansC(sfd.bg), type="l", yaxt="n",
    xlab="Latitude", ylab="Mean flow direction", ylim=ylim)
axis(2, at=c(-180, -90, 0, 90, 180), labels=c("S", "W", "N", "E", "S"))
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)



#
# plot latitudinal profiles of RMSE
#

# simple helper function to calculate row-wise RMSEs, accounting for the
# fact that flow dir values are circular (0-360), so the difference
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
plot(lats300, rmse(sfd.uncor, sfd.aster), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
lines(lats150, rmse(crop(sfd.uncor, extent(sfd.srtm)), sfd.srtm), col="blue")
legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="uncorrected")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Flowdir discrepancies with respect to separate ASTER/SRTM components (",
    125*degree, "W to ", 100*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rmse(sfd.enblend, sfd.aster), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
lines(lats150, rmse(crop(sfd.enblend, extent(sfd.srtm)), sfd.srtm), col="blue")
legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="exponential ramp")
text(mean(lats300), mean(ylim), font=3, labels="(skipped)")
abline(v=60, col="red", lty=2)

plot(lats300, rmse(sfd.bg, sfd.aster), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
lines(lats150, rmse(crop(sfd.bg, extent(sfd.srtm)), sfd.srtm), col="blue")
legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# ...with respect to CDEM
plot(lats300, rmse(sfd.uncor, sfd.can), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="uncorrected")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Flowdir discrepancies with respect to Canada DEM (",
    125*degree, "W to ", 100*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rmse(sfd.enblend, sfd.can), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="exponential ramp")
text(mean(lats300), mean(ylim), font=3, labels="(skipped)")
abline(v=60, col="red", lty=2)

plot(lats300, rmse(sfd.bg, sfd.can), type="l", xlab="Latitude",
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
plot(lats300, corByLat(sfd.uncor, sfd.aster), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
lines(lats150, corByLat(crop(sfd.uncor, extent(sfd.srtm)), sfd.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Flow direction correlations with respect to separate ASTER/SRTM components (",
    125*degree, "W to ", 100*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, corByLat(sfd.enblend, sfd.aster), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
lines(lats150, corByLat(crop(sfd.enblend, extent(sfd.srtm)), sfd.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, corByLat(sfd.bg, sfd.aster), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
lines(lats150, corByLat(crop(sfd.bg, extent(sfd.srtm)), sfd.srtm), col="blue")
legend("bottomright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
    lty=c(1, 1), bty="n")
text(min(lats300), min(ylim), pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# ...with respect to CDEM
plot(lats300, corByLat(sfd.uncor, sfd.can), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="simple fused")
abline(v=60, col="red", lty=2)
mtext(expression(paste(
    "Flow direction correlations with respect to Canada DEM (",
    125*degree, "W to ", 100*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, corByLat(sfd.enblend, sfd.can), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="multires spline")
abline(v=60, col="red", lty=2)

plot(lats300, corByLat(sfd.bg, sfd.can), type="l", xlab="Latitude",
    ylab="Circular correlation", ylim=ylim)
text(min(lats300), min(ylim), pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# close pdf device driver
dev.off()

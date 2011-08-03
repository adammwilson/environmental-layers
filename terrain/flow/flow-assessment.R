# R code to plot latitudinal profiles of mean flow direction, along with
# both RMSE and correlation coefficients comparing fused layers with
# both the raw ASTER and with the Canada DEM
#
# Jim Regetz
# NCEAS
# Created on 08-Jun-2011

library(raster)

datadir <- "/home/regetz/media/temp/terrain/flow"

# create function to recode values into degrees
recode <- function(r) {
    v <- values(r)
    v[v==0] <- NA
    v[v==1] <- 0
    v[v==2] <- 45
    v[v==3] <- 90
    v[v==4] <- 90
    v[v==8] <- 135
    v[v==16] <- 180
    v[v==32] <- 225
    v[v==64] <- 270
    v[v==128] <- 315
    r[] <- v
    return(r)
}

# load flow direction rasters, recoding on the fly
sfd.aster <- recode(raster(file.path(datadir, "aster_300straddle_sfd.tif")))
sfd.srtm <- recode(raster(file.path(datadir, "srtm_150below_sfd.tif")))
sfd.uncor <- recode(raster(file.path(datadir, "fused_300straddle_sfd.tif")))
#sfd.eramp <- recode(raster(file.path(datadir, "fused_300straddle_rampexp_sfd.tif")))
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

par(mfrow=c(2,2), omi=c(1,1,1,1))

ylim <- c(80, 280)

plot(lats300, rowMeans(as.matrix(sfd.uncor), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean flow direction", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="uncorrected")
abline(v=60, col="red", lty=2)
mtext(expression(paste("Latitudinal profiles of mean flow direction (",
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

plot(lats300, rowMeans(as.matrix(sfd.can), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean flow direction", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="Canada DEM")
abline(v=60, col="red", lty=2)

#plot(lats300, rowMeans(as.matrix(sfd.eramp), na.rm=TRUE), type="l",
plot(lats300, rowMeans(as.matrix(sfd.can), na.rm=TRUE), type="n",
    xlab="Latitude", ylab="Mean flow direction", ylim=ylim)
text(min(lats300), min(ylim)+0.5, pos=4, font=3, labels="exponential ramp")
text(mean(lats300), mean(ylim), pos=1, font=3, labels="(skipped)")
#abline(v=60, col="red", lty=2)

plot(lats300, rowMeans(as.matrix(sfd.bg), na.rm=TRUE), type="l",
    xlab="Latitude", ylab="Mean flow direction", ylim=ylim)
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
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

#plot(lats300, rmse(sfd.eramp, sfd.aster), type="l", xlab="Latitude",
plot(lats300, rep(0, 300), type="n", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
#lines(lats150, rmse(crop(sfd.eramp, extent(sfd.srtm)), sfd.srtm), col="blue")
#legend("topright", legend=c("ASTER", "SRTM"), col=c("black", "blue"),
#    lty=c(1, 1), bty="n")
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="exponential ramp")
text(mean(lats300), mean(ylim), font=3, labels="(skipped)")
#abline(v=60, col="red", lty=2)

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
    136*degree, "W to ", 96*degree, "W)")), adj=0, line=2, font=2)

#plot(lats300, rmse(sfd.eramp, sfd.can), type="l", xlab="Latitude",
plot(lats300, rep(0, 300), type="n", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="exponential ramp")
text(mean(lats300), mean(ylim), font=3, labels="(skipped)")
#abline(v=60, col="red", lty=2)

plot(lats300, rmse(sfd.bg, sfd.can), type="l", xlab="Latitude",
    ylab="RMSE", ylim=ylim)
text(min(lats300), max(ylim)-5, pos=4, font=3, labels="gaussian blend")
abline(v=60, col="red", lty=2)

# close pdf device driver
dev.off()

#
# informal concordance analysis of flow accumulation in the 60N boundary
# region of Canada
#
# Jim Regetz
# NCEAS
# Created on 17-Jun-2011

# data directory (local to xander)
datadir <- "~/media/temp/dem/new/flow"

# load flow direction rasters produced by terraflow (mfd)
# Canadian DEM
fl.can <- raster(file.path(datadir, "flowdir_small.tif"))
# uncorrected fused srtm/aster
fl.uncor <- raster(file.path(datadir, "flowdir_smc.tif"))
# gaussian blended srtm/aster
fl.bg <- raster(file.path(datadir, "flowdir_smbg.tif"))

# plot cross-latitude profile of mean direction
lats <- yFromRow(fl.uncor, 1:nrow(fl.uncor))
plot(lats, rowMeans(as.matrix(fl.uncor), na.rm=TRUE), type="l", lwd=2)
lines(lats, rowMeans(as.matrix(fl.bg), na.rm=TRUE), col="red")
lines(lats, rowMeans(as.matrix(fl.can), na.rm=TRUE), col="blue")
plot(lats, rowMeans(as.matrix(fl.bg==fl.can), na.rm=TRUE), col="red")



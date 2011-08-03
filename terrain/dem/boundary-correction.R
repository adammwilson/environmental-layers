# R script to apply several kinds of boundary corrections to ASTER/SRTM
# elevation values near the 60N boundary in Canada, and write out new
# GeoTIFFs.
#
# Jim Regetz
# NCEAS
# Created on 08-Jun-2011

library(raster)

# load relevant SRTM and ASTER data
srtm.south <- raster("srtm_150below.tif")
aster.south <- raster("aster_150below.tif")
aster.north <- raster("aster_150above.tif")

# create difference raster for area of overlap
delta.south <- srtm.south - aster.south

#
# OPTION 1
#

# smooth to the north, by calculating the deltas _at_ the boundary,
# ramping them down to zero with increasing distance from the border,
# and adding them to the north ASTER values

# create simple grid indicating distance (in units of pixels) north from
# boundary, starting at 1 (this is used for both option 1 and option 2)
aster.north.matrix <- as.matrix(aster.north)
ydistN <- nrow(aster.north.matrix) + 1 - row(aster.north.matrix)

# 1b. linear ramp north from SRTM edge
# -- Rick is doing this --

# 2b. exponential ramp north from SRTM edge
# -- Rick is also doing this, but here it is... --
r <- -0.045
w <- exp(ydistN*r)
aster.north.smooth <- aster.north
aster.north.smooth[] <- values(aster.north) + as.integer(round(t(w) *
    as.matrix(delta.south)[1,]))
writeRaster(aster.north.smooth, file="aster_150above_rampexp.tif")

#
# OPTION 2
#

# smooth to the north, by first using LOESS with values south of 60N to
# model deltas as a function of observed ASTER, then applying the model
# to predict pixel-wise deltas north of 60N, then ramping these
# predicted deltas to zero with increasing distance from the border, and
# adding them to the associated ASTER values

# first fit LOESS on a random subsample of data
# note: doing all the data takes too long, and even doing 50k points
#       seems to be too much for calculating SEs during predict step
set.seed(99)
samp <- sample(ncell(aster.south), 10000)
sampdata <- data.frame(delta=values(delta.south)[samp],
    aster=values(aster.south)[samp])
lo.byaster <- loess(delta ~ aster, data=sampdata)

# now create ASTER prediction grid north of 60N
# TODO: deal with NAs in data (or make sure they are passed through
#       properly in the absence of explicit treatment)?
aster.north.pdelta <- aster.north
aster.north.pdelta[] <- predict(lo.byaster, values(aster.north))
# for actual north ASTER values that exceed the max value used to fit
# LOESS, just use the prediction associated with the maximum
aster.north.pdelta[aster.north<min(sampdata$aster)] <- predict(lo.byaster,
    data.frame(aster=min(sampdata$aster)))
# for actual north ASTER value less than the min value used to fit
# LOESS, just use the prediction associated with the minimum
aster.north.pdelta[aster.north>max(sampdata$aster)] <- predict(lo.byaster,
    data.frame(aster=max(sampdata$aster)))

# 2a: exponential distance-weighting of LOESS predicted deltas
r <- -0.045
w <- exp(r*ydistN)
aster.north.smooth <- aster.north
aster.north.smooth[] <- values(aster.north) + as.integer(round(t(w *
    as.matrix(aster.north.pdelta))))
writeRaster(aster.north.smooth, file="aster_150above_predexp.tif")

# 2b: gaussian distance-weighting of LOESS predicted deltas
r <- -0.001  # weight drops to 0.5 at ~26 cells, ie 2.4km at 3" res
w <- exp(r*ydistN^2)
aster.north.smooth <- aster.north
aster.north.smooth[] <- values(aster.north) + as.integer(round(t(w *
    as.matrix(aster.north.pdelta))))
writeRaster(aster.north.smooth, file="aster_150above_predgau.tif")

#
# OPTION 3
#

# smooth to the south, now by simply taking pixel-wise averages of the
# observed SRTM and ASTER using a distance-based weighting function such
# that the relative contribution of ASTER decays to zero over a few km

# create simple grid indicating distance (in units of pixels) south from
# boundary, starting at 1
aster.south.matrix <- as.matrix(aster.south)
ydistS <- row(aster.south.matrix)

# 3a: gaussian weighting function
r <- -0.001  # weight drops to 0.5 at ~26 cells, or 2.4km at 3" res
w <- exp(-0.001*ydistS^2)
aster.south.smooth <- aster.south
aster.south.smooth[] <- values(srtm.south) - as.integer(round(t(w *
    as.matrix(delta.south))))
aster.south.smooth[aster.south.smooth<0] <- 0
writeRaster(aster.south.smooth, file="dem_150below_blendgau.tif")


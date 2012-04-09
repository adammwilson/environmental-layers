# Multiscale raster smoother function designed for use with noisy DEMs.
# Original algorithm and AML implementation by John Gallant. Translation
# to R (relying on raster package functionality) by Jim Regetz, based on
# AML script 'multiscalesmooth9a_clean.aml'.
#
# Jim Regetz
# NCEAS
# Created on 22-Mar-2012

library(raster)

multiscalesmooth <- function(ingrid, sd=0.0001, alpha=0.05, bbox) {
    # ingrid: RasterLayer to smooth
    # sd: numeric constant or RasterLayer
    # alpha: alpha level for chi-square critical value
    # bbox: optional extent object used to subset ingrid

    # subset ingrid if desired
    if (!missing(bbox)) {
        ingrid <- crop(ingrid, bbox)
    }

    # set number of aggregation levels and neighborhood size
    NUM.LEVELS <- 4
    NUM.CELLS <- 3

    # expand grid to accommodate integer number of cells at coarsest
    # level, by adding roungly equal number of cells on either side
    # (with one extra on top/right if an odd number of cells is needed)
    max.size <- NUM.CELLS^NUM.LEVELS
    addx <- if (0 < (extra<-ncol(ingrid)%%max.size)) {
            (max.size-extra)/2
        } else {
            0
        }
    addy <- if (0 < (extra<-nrow(ingrid)%%max.size)) {
            (max.size-extra)/2
        } else {
            0
        }
    full.extent <- extent(
        xmin(ingrid) - floor(addx) * xres(ingrid),
        xmax(ingrid) + ceiling(addx) * xres(ingrid),
        ymin(ingrid) - floor(addy) * yres(ingrid),
        ymax(ingrid) + ceiling(addy) * yres(ingrid)
        )

    # create grids

    # create lists to hold the series of successively coarsened grids of
    # values and variances, respectively; the first element of each is
    # the grid at the original input resolution
    # ...insert initial grid of values, but expanded to full extent
    z <- list(expand(ingrid, full.extent))
    # ...insert initial grid of variances
    if (is.numeric(sd) && length(sd)==1) {
        v <- list(calc(z[[1]], function(x) ifelse(!is.na(x), sd^2, NA)))
    } else if (class(sd)=="RasterLayer") {
        if (identical(extent(sd), extent(ingrid))) {
            v <- list(overlay(z[[1]], sd, fun=function(z, sd)
                ifelse(!is.na(z), sd^2, NA)))
        } else {
            stop("sd raster extent differs from ingrid extent")
        }
    } else {
        stop("sd must be a single number or a RasterLayer")
    }

    # set initial "group variance" to individual msmt variance (noise)
    v.g = v[[1]]
    # weights for aggregation, based on total variance
    w <- calc(v[[1]], function(x) ifelse(is.na(x), 0, 1/x))
    # squared weights
    wsq = w^2
    # effective number of measurements
    n <- calc(z[[1]], function(x) ifelse(!is.na(x), 1, 0))

    # aggregate to broader scales
    for (i in 1+seq.int(NUM.LEVELS)) {

        message("Aggregate from ", i-1, " to ", i)

        # make copies of previous (finer scale) grids
        v.g.prev <- v.g
        w.prev <- w
        wsq.prev <- wsq
        n.prev <- n

        # calc neighborhood weights, num cells, effective num cells
        w <- aggregate(w.prev, 3, sum)
        wsq <- aggregate(wsq.prev, 3, sum)
        n <- aggregate(n.prev, 3, sum)
        n.eff <- w^2 / wsq
        # calc variance-weighted neighborhood mean
        z[[i]] <- aggregate(w.prev * z[[i-1]], 3, sum) / w
        # calc between-cell variance, taken over neighborhood
        zdiff <- z[[i-1]] - disaggregate(z[[i]], 3)
        v.bg <- aggregate(w.prev * zdiff^2, 3, sum) / w
        # calc wtd avg of within-cell variance, taken over neighborhood
        if (i==2) {
            v.wg <- n - n # zero, but with correct window and cell size
        } else {
            v.wg <- aggregate(w.prev * v.g.prev, 3, sum) / w
        }
        # calc total group variance
        # ~ total variance of cell values in the underlying neighborhood
        v.g <- v.bg + v.wg
        # calc variance of the mean for the neighborhood
        v.m <- 1 / w
        # calc mean noise variance (mean of finer scale variances)
        mv <- n / w

        # calc chisq critical values
        chisq <- calc(n.eff, function(n) qchisq(alpha, n-1,
            lower=FALSE)/(n-1))
        # set coarsened cell variances: if group variance is small
        # relative to noise variance, use variance of the mean instead
        # of group variance
        v[[i]] <- overlay(v.g, chisq, mv, v.m,
            fun=function(v.g, chisq, mv, v.m) ifelse(v.g/chisq < mv, v.m, v.g))

    }

    # get arbitrarily large value to fill null variance cells
    bigvar <- cellStats(v[[1]], max) * 10

    # prep for refinement phase
    z.smooth <- z[[NUM.LEVELS+1]]
    v.smooth <- v[[NUM.LEVELS+1]]
    # create weight matrix equivalent to ArcGIS circle with radius 2
    circle2 <- matrix(c(0,0,1,0,0,
                        0,1,1,1,0,
                        1,1,1,1,1,
                        0,1,1,1,0,
                        0,0,1,0,0), nrow=5)
    circle2[circle2==0] <- NA

    # refine, smooth, and combine each layer in turn
    for (j in 1+rev(seq.int(NUM.LEVELS))) {

        message("Refining from ", j, " to ", j-1)

        # create smoothed higher resolution versions of z and v
        zs <- focal(disaggregate(z.smooth, 3), w=circle2, fun=mean,
            pad=TRUE, padValue=NA, na.rm=TRUE)
        vs <- focal(disaggregate(v.smooth, 3), w=circle2, fun=mean,
            pad=TRUE, padValue=NA, na.rm=TRUE)

        # create no-null version of finer z and v
        z_c <- calc(z[[j-1]], function(x) ifelse(is.na(x), 0, x))
        v_c <- calc(v[[j-1]], function(x) ifelse(is.na(x), bigvar, x))

        # explicitly clean up, in case it helps
        z[[j-1]] <- NULL
        v[[j-1]] <- NULL

        # combine two values using least variance
        z.smooth <- (z_c/v_c + zs/vs ) / (1/v_c + 1/vs)
        v.smooth <- 1 / (1/v_c + 1/vs)

    }

    result <- crop(stack(z.smooth, v.smooth, extent(ingrid))
    layerNames(result) <- c("zs", "vs")
    return(result)

}

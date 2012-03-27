# run in GRID, not ARC
# &run .aml ingrid sd prob bbox
# sd 0.0001 prob 0.05
#
# 9a - limit to 4 steps, see if that has any significant deterioration of smoothing performance. Should fix
# the problem with islands and headlands - turns out also need to remove water except
# for one cell adjacent to land, and give that a higher uncertainty. See cluster_multiscalesmooth9a_clean_216
#
# Version 9:
# Focus on implementing identical algorithm to directsmooth2 using multiscale method i.e. aggregating by factor of 3
# from already aggregated data, rather than from original resolution each time.
#
# Version 8:
# WARNING: have not yet checked that the additional weighting of the gaussian smoothing is not messing with
# the calculations of variance etc.
# Replaced simple 3x3 aggregation with gaussian smoothing
# Kernel is chosen to give appropriate level of smoothing and produce a fairly accurate approximation of the smoothed
# surface by interpolation of the coarser scale smoothed values
# Details in gaussian.xls on john's laptop
#
# Version 7:
# Further reworking of how the final values are selected - a mean is a candidate if its associated variance
# is less than the mean sample uncertainty, and the mean with the lowest variance among the candidates is the chosen one.
# Implement that in the nested sense by taking the lowest group variance divided by the chi^2 value, and its associated mean variance,
# and if that is lower than the data point variance the
#
# approximate critical value of chi^2/N with N degrees of freedom at 5% level as 1 + 2.45/sqrt(N) + 0.55/N
# for 1% level use 1 + 3.4/sqrt(N) + 2.4/N
#
# Version 6:
# Done from scratch after careful working through of theory.
# ingrid is the (potentially sparse) grid of data to be smoothed and
# interpolated, which can be of different extent to the output
# (resolution is assumed to be the same, could adapt this to relax that)
#
# var can be a constant or a grid
#
# bbox can be either a grid name or the 'xmin ymin xmax ymax' parameters
# for setwindow

# REGETZ NOTES
# - it looks like Ming used sd=0.001 (based on the Arc 'log' file in the
#   topo experimental directory)

library(raster)

#&type NB - using standard deviation as noise specification now, not variance!

multiscalesmooth <- function(ingrid, sd=0.0001 , prob=0.05, bbox) {
    # ingrid: grid to smooth
    # sd: stdev
    # prob: prob
    # bbox: optional extent object used to subset ingrid

    # set up chisq parameters
    chisqa <- 2.807 - 0.6422 * log10(prob) - 3.410 * prob^0.3411
    chisqb <- -5.871 - 3.675 * log10(prob) + 4.690 * prob^0.3377
    message("chisq parameters: (", chisqa, ", ", chisqb, ")")

    # subset ingrid if desired
    if (!missing(bbox)) {
        ingrid <- crop(ingrid, bbox)
    }

    # set number of levels of aggregation
    NUM.LEVELS <- 4
    # set aggregation factor
    AGG.FACTOR <- 3

    # expand grid to accommodate integer number of cells at coarsest
    # level, by adding roungly equal number of cells on either side
    # (with one extra on top/right if an odd number of cells needs to be
    # added)
    max.size <- AGG.FACTOR^NUM.LEVELS
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
        xmin(ingrid)-floor(addx)*xres(ingrid),
        xmax(ingrid)+ceiling(addx)*xres(ingrid),
        ymin(ingrid)-floor(addy)*yres(ingrid),
        ymax(ingrid)+ceiling(addy)*yres(ingrid)
        )

    # create grids

    # NB - only calculating sample variances here, not variances of estimated means.
    # Also note that v0_bg is an uncertainty, not a sample variance
    # and v1_bg is total variances, but both are labelled as "between-group" to simplify the smoothing

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
    # weighting for aggregation, based on total variance
    w <- calc(v[[1]], function(x) ifelse(is.na(x), 0, 1/x))
    wsq = w^2
    # effective number of measurements
    n <- calc(z[[1]], function(x) ifelse(!is.na(x), 1, 0))

    bigvar <- cellStats(v[[1]], max)

    #setcell minof

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
        hdiff <- z[[i-1]] - disaggregate(z[[i]], 3)
        v.bg <- aggregate(w.prev * hdiff^2, 3, sum) / w
        # calc wtd avg of within-cell variance, taken over neighborhood
        if (i==2) {
            v.wg <- n - n # zero, but with window and cell size set for us
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

        chisq <- 1 + chisqa / sqrt(n.eff - 1) + chisqb / (n.eff - 1)
        # set coarsened cell variances: if group variance is small
        # relative to noise variance, use variance of the mean instead
        # of group variance
        v[[i]] <- overlay(v.g, chisq, mv, v.m,
            fun=function(v.g, chisq, mv, v.m) ifelse(v.g/chisq < mv, v.m, v.g))

    }

    bigvar  <- bigvar * 10

    #copy z[NUM.LEVELS] hs[NUM.LEVELS]
    #copy v[NUM.LEVELS] vs[NUM.LEVELS]
    #kill z[NUM.LEVELS]
    #kill v[NUM.LEVELS]
    #setcell hs[NUM.LEVELS]
    #setwindow hs[NUM.LEVELS]

    # smooth, refine and combine each layer in turn
    #hs <- z[[NUM.LEVELS+1]]
    #vs <- v[[NUM.LEVELS+1]]
    hs <- z
    vs <- v
    #todo: is this the same as circle with radius 2 in arc???
    #circle2 <- matrix(c(0,1,0, 1,1,1, 0,1,0), nrow=3)
    circle2 <- matrix(c(0,0,1,0,0, 0,1,1,1,0, 1,1,1,1,1, 0,1,1,1,0, 0,0,1,0,0), nrow=5)
    circle2[circle2==0] <- NA

    for (j in 1+rev(seq.int(NUM.LEVELS))) {

        message("Refine from ", j, " to ", j-1)

        # for the first stage where the coarser grid is refined and smoothed, set window to the coarse grid
        #setcell z[j-1]
        #setwindow maxof

        # create smoothed higher resolution versions of z and v_bg, hopefully with no nulls!
        # [suppressing warnings to avoid .couldBeLonLat complaints]
        suppressWarnings({
            hs.tmp <- disaggregate(focal(hs[[j]], w=circle2, fun=mean,
                pad=TRUE, padValue=NA, na.rm=TRUE), 3)
            vs.tmp <- disaggregate(focal(vs[[j]], w=circle2, fun=mean,
               pad=TRUE, padValue=NA, na.rm=TRUE), 3)
        })

        # create no-null version of finer z and v
        h_c <- calc(z[[j-1]], function(x) ifelse(is.na(x), 0, x))
        v_c <- calc(v[[j-1]], function(x) ifelse(is.na(x), bigvar, x))

        # combine two values using least variance
        hs[[j-1]] <- (h_c/v_c + hs.tmp/vs.tmp ) / (1/v_c + 1/vs.tmp)
        vs[[j-1]] <- 1 / (1/v_c + 1/vs.tmp)

#todo: mimic all the AML setwindow/setcell stuff???
#        hs[[j-1]] <- expand(hs[[j-1]], 4)
#        vs[[j-1]] <- expand(vs[[j-1]], 4)

    }

    result <- crop(stack(hs[[1]], vs[[1]]), extent(ingrid))
    layerNames(result) <- c("hs", "vs")
    return(result)

}

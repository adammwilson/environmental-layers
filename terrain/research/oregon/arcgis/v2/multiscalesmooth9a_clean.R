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

library(raster)

&args ingrid sd prob bbox:rest

#&type NB - using standard deviation as noise specification now, not variance!

function(ingrid, sd, prob, bbox) {
# ingrid: grid to smooth
# sd: stdev
# prob: prob
# bbox: optional extent object used to subset ingrid

# set up chisq parameters
chisqa <- 2.807 - 0.6422 * log10(prob) - 3.410 * prob^0.3411
chisqb <- -5.871 - 3.675 * log10(prob) + 4.690 * prob^0.3377
#&type chisq parameters %chisqa% %chisqb%

# snap bbox to ingrid
bboxgrid <- alignExtent(bbox, ingrid)
# then union this with the ingrid extent
workgrid <- unionExtent(ingrid, bbox.aligned)
setwindow workgrid

# naming:
#  h - the value being smoothed/interpolated
#  vg - total variance of group of data, or of individual measurement
#  v_bg - variance between groups
#  v_wg - variance within groups
#  wa - weighting for aggregation, based on total variance
#  vm - variance of the calculated mean
#  mv - mean of finer scale variances
#  n - effective number of measurements


# NB - only calculating sample variances here, not variances of estimated means.
# Also note that v0_bg is an uncertainty, not a sample variance
# and v1_bg is total variances, but both are labelled as "between-group" to simplify the smoothing

h[0] <- ingrid
v[0] <- calc(h[0], function(x) ifelse(!is.na(x), sd^2, NA))
vg[0] = v[0]
w[0] <- calc(v[0], function(x) ifelse(is.na(x), 0, 1/x))
wsq[0] = w[0]^2
n[0] <- calc(h[0], function(x) ifelse(!is.na(x), 1, 0))

bigvar <- cellStats(v[0], max)

setcell minof

# aggregate to broader scales
i <- 1
done <- FALSE

while(!done) {

  j <- i-1

  message("Aggregate from ", j, " to ", i)

  cell3  <- xres(h[j]) * 3
  nx0 <- round(xmin(h[0] / cell3 - 0.5)
  ny0 <- round(ymin(h[0] / cell3 - 0.5)
  nx1 <- round(xmax(h[0] / cell3 + 0.5)
  ny1 <- round(ymax(h[0] / cell3 + 0.5)
  x0 <- (nx0 - 0.5) * cell3
  y0 <- (ny0 - 0.5) * cell3
  x1 <- (nx1 + 0.5) * cell3
  y1 <- (ny1 + 0.5) * cell3
  setwindow x0 y0 x1 y1
 #todo:
 #  w? <- expand(w?, 1)

  w[i] = aggregate(w[j], 3, sum)
  wsq[i] = aggregate(wsq[j], 3, sum)
  n[i] = aggregate(n[j], 3, sum)
  neff = w[i] * w[i] / wsq[i]
  h[i] = aggregate(w[j] * h[j], 3, sum) / w[i]
  vbg = aggregate(w[j] * sqr(h[j] - h[i]), 3, sum) / w[i]
  if (i==1) {
      vwg = n[i] - n[i] # zero, but with window and cell size set for us
  } else {
      vwg = aggregate(w[j] * vg[j], 3, sum) / w[i]
  }
  vg[i] = vbg + vwg
  vm = 1 / w[i]
  mv = n[i] / w[i]

  chisq = 1 + chisqa / sqrt(neff - 1) + chisqb / (neff - 1)
  v[i] = con(vg[i] / chisq < mv, vm, vg[i])

  # remove everything except h[i] and v[i]
  kill w[j]
  kill wsq[j]
  kill n[j]
  kill vg[j]

  if (i==4) done <- TRUE

  i <- i + 1
}


maxstep <- i - 1
bigvar  <- bigvar * 10

kill w[maxstep]
kill wsq[maxstep]
kill n[maxstep]
kill vg[maxstep]

# smooth, refine and combine each layer in turn


copy h[maxstep] hs[maxstep]
copy v[maxstep] vs[maxstep]
kill h[maxstep]
kill v[maxstep]
setcell hs[maxstep]
setwindow hs[maxstep]


circle2 <- matrix(c(0,1,1,0, 1,1,1,1, 1,1,1,1, 0,1,1,0), nrow=4)

for (j in maxstep:1) {
  i <- j-1

  message("Refine from ", j, " to ", i)

  # for the first stage where the coarser grid is refined and smoothed, set window to the coarse grid
  setcell h[i]
  setwindow maxof

  # create smoothed higher resolution versions of h and v_bg, hopefully with no nulls!
  hs[j][i] = focal(hs[j], w=circle2, mean)
  vs[j][i] = focal(vs[j], w=circle2, mean)

  setcell h[i]
  cellsize <- xres(h[i])
  setwindow [xmin(bboxgrid) - 4 * cellsize] [ymin(bboxgrid) - 4 * cellsize] [xmax(bboxgrid) + 4 * cellsize] [ymax(bboxgrid) + 4 * cellsize] h[i]

  # create no-null version of finer h and v
  h_c = con(isnull(h[i]), 0, h[i])
  v_c = con(isnull(v[i]), bigvar, v[i])

  # combine two values using least variance
  hs[i] = (h_c / v_c + hs[j][i] / vs[j][i] ) / (1.0 / v_c + 1.0 / vs[j][i])
  vs[i] = 1 / (1.0 / v_c + 1.0 / vs[j][i])

  kill v_c
  kill h_c
  kill v[i]
  kill h[i]
  kill vs[j][i]
  kill hs[j][i]
  kill hs[j]
  kill vs[j]
}

# result is hs[0], with variance vs[0]

kill bboxgrid

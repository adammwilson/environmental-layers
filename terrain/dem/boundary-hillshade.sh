# GDAL commands to generate hillshade images using the different fused
# DEMs we've produced in the 60N Canada boundary region.
#
# Also included below is a bit of R code to generate a PNG of quick
# hillshade visuals of two 1-degree wide swaths (one in the more
# mountainous west, and one in the flatter east) spanning the 60N
# boundary in Canada, for several different fused layers.
#
# Jim Regetz
# NCEAS

export DEMDIR=~/media/temp/terrain/dem
export HSDIR=~/media/temp/terrain/hillshade

gdaldem hillshade -s 111120 $DEMDIR/fused_300straddle.tif \
  $HSDIR/fused_300straddle_h.tif
gdaldem hillshade -s 111120 $DEMDIR/fused_300straddle_rampexp.tif \
  $HSDIR/fused_300straddle_rampexp_h.tif
gdaldem hillshade -s 111120 $DEMDIR/fused_300straddle_predexp.tif \
  $HSDIR/fused_300straddle_predexp_h.tif
gdaldem hillshade -s 111120 $DEMDIR/fused_300straddle_predgau.tif \
  $HSDIR/fused_300straddle_predgau_h.tif
gdaldem hillshade -s 111120 $DEMDIR/fused_300straddle_blendgau.tif \
  $HSDIR/fused_300straddle_blendgau_h.tif
gdaldem hillshade -s 111120 $DEMDIR/fused_300straddle_enblend.tif \
  $HSDIR/fused_300straddle_enblend_h.tif

# Use R to generate a PDF with some visuals 

echo '
  library(raster)
  hsdir <- "/home/regetz/media/temp/terrain/hillshade"
  
  h.uncor <- raster(file.path(hsdir, "fused_300straddle_h.tif"))
  h.re <- raster(file.path(hsdir, "fused_300straddle_rampexp_h.tif"))
  h.enblend <- raster(file.path(hsdir, "fused_300straddle_enblend_h.tif"))
  h.bg <- raster(file.path(hsdir, "fused_300straddle_blendgau_h.tif"))
  
  window1 <- extent(-135, -134, 59.875, 60.125)
  window2 <- extent(-118, -117, 59.875, 60.125)
  
  png("boundary-hillshade.png", height=10, width=7.5, units="in", res=600)
  par(mfrow=c(4,2), mar=c(2,2,3,0))
  plot(crop(h.uncor, window1), legend=FALSE)
  mtext("Simple fuse (hillshade)", adj=0, cex=0.8)
  plot(crop(h.uncor, window2), legend=FALSE)
  plot(crop(h.re, window1), legend=FALSE)
  mtext("North exponential ramp (hillshade)", adj=0, cex=0.8)
  plot(crop(h.re, window2), legend=FALSE)
  plot(crop(h.enblend, window1), legend=FALSE)
  mtext("Multiresolution spline (hillshade)", adj=0, cex=0.8)
  plot(crop(h.enblend, window2), legend=FALSE)
  plot(crop(h.bg, window1), legend=FALSE)
  mtext("Gaussian weighted average (hillshade)", adj=0, cex=0.8)
  plot(crop(h.bg, window2), legend=FALSE)
  dev.off()
' | Rscript --vanilla -

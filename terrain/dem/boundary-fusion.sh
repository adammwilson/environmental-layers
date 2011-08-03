# GDAL commands to produced fused DEMs in the vicinity of the 60N Canada
# boundary, using several "boundary-corrected" variants as well as the
# original uncorrected DEMs.
#
# Jim Regetz
# NCEAS

# uncorrected fused layer
gdalwarp -ot Int16 -te -136 59.875 -96 60.125 -ts 48000 300 \
  srtm_150below.tif aster_150above.tif fused_300straddle.tif

# exponential ramp of boundary delta to the north
gdalwarp -ot Int16 -te -136 59.875 -96 60.125 -ts 48000 300 \
  srtm_150below.tif aster_150above_rampexp.tif fused_300straddle_rampexp.tif

# exponential blend of predicted deltas to the north
gdalwarp -ot Int16 -te -136 59.875 -96 60.125 -ts 48000 300 \
  srtm_150below.tif aster_150above_predexp.tif fused_300straddle_predexp.tif

# gaussian blend of predicted deltas to the north
gdalwarp -ot Int16 -te -136 59.875 -96 60.125 -ts 48000 300 \
  srtm_150below.tif aster_150above_predgau.tif fused_300straddle_predgau.tif

# gaussian blend of SRTM/ASTER to the south
gdalwarp -ot Int16 -te -136 59.875 -96 60.125 -ts 48000 300 \
  dem_150below_blendgau.tif aster_150above.tif fused_300straddle_blendgau.tif

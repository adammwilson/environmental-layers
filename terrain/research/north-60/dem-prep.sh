# GDAL commands for assembling SRTM and ASTER DEM tiles associated with
# the 60N Canada boundary analysis, and resampling into the desired 3"
# (~90m) resolution even-boundary grid.
#
# These commands generate strips of elevation data (GeoTIFFs) along a
# 40-degree longitudinal extent matching (at least one of) Rick Reeves'
# mosaicked CDEM grids. The strips extend 150 pixels south of 60N and
# (in case of ASTER only) 150 pixels north of 60N, which should provide
# a sufficient but not excessive latitudinal range for fixing and
# assessing boundary artifacts.
#
# Note:
#   Working with the original ASTERs yields this warning from GDAL:
#     Warning 1: TIFFReadDirectoryCheckOrder:Invalid TIFF directory;
#     tags are not sorted in ascending order
#   I then ran gdal_translate on several of the offending ASTERs, then
#   repeated the vrt/warp on those -- now without warnings. However, the
#   output data was the same as when I operated on the original files
#   (with warnings), so for the moment I'm just going to ignore the
#   warnings.
#
# Jim Regetz
# NCEAS

# at the time of script creation, these paths were correct on vulcan
export ASTDIR="/home/reeves/active_work/EandO/asterGdem"
export SRTMDIR="/home/reeves/active_work/EandO/CgiarSrtm/SRTM_90m_ASCII_4_1"

# SRTM (also convert to 16bit integer)
gdalbuildvrt srtm.vrt $SRTMDIR/srtm_*_01.asc
gdalwarp -ot Int16 -te -136 59.875 -96 60 -ts 48000 150 -r bilinear \
  srtm.vrt srtm_150below.tif

# ASTER
gdalbuildvrt aster.vrt $ASTDIR/ASTGTM_N59*W*_dem.tif \
  $ASTDIR/ASTGTM_N60*W*_dem.tif
gdalwarp -te -136 59.875 -96 60 -ts 48000 150 -r bilinear \
  aster.vrt aster_150below.tif
gdalwarp -te -136 60 -96 60.125 -ts 48000 150 -r bilinear \
  aster.vrt aster_150above.tif

# note that the top 150 rows of this one are, somewhat surprisingly,
# slightly different from the above!
# gdalwarp -te -136 59.875 -96 60.125 -ts 48000 300 -r bilinear \
#   aster.vrt aster_300straddle.tif
#
# and this yields an even different set of values
# gdalbuildvrt aster_N60.vrt $ASTDIR/ASTGTM_N60*W*_dem.tif
# gdalwarp -te -136 60 -96 60.125 -ts 48000 150 -r bilinear \
#   aster_N60.vrt aster_150above.tif


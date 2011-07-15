#! /bin/sh
################################################################################
# script Name: mosaicNwHemiCgiar30ArcSecBL.sh
# This script constructs the CIGAR SRTM image mosaic for the Northweetern Hemisphere
# quadrant of the global mosaic
# 
# Author: Rick Reeves, NCEAS June 2, 2011
#
#
# NOTE: We are using SRTM version 4.1 data distributed by CGIAR (6001 x 6001 tiles)
# Determining the -ts (row, column # cells): 
# 1) Look at the first line of corresponding .vrt file: contains the spanning extent 
# (# rows, columns) of all image tiles in the virtual mosaic, as well as the pixel
# resolution in 'native units' for the spatial reference frame (in our case, 
# the geographic coordinate system and decimal degrees).
# Example for this file: 
#
# <GeoTransform> -1.8000041666668000e+02,  8.3333333333332894e-04,  0.0000000000000000e+00,
#  6.0000417247802311e+01,  0.0000000000000000e+00, -8.3333333333332894e-04</GeoTransform>
# The values 8.3333333333332894e-04 and -8.3333333333332894e-04 are the cell resolution
# in decimal cegrees: 3 arc-seconds, or approx 90 meters.
# <dataset rasterXSize="156001" rasterYSize="54001">: # cols and rows, respectively.
#
# The extent of the mosaic is fixed. To change the cell size, adjust the X and Y resolution.
# In the present case, to set cellsize at 30 arcseconds, reduce the number of cells in
# X and Y by a factor of 10 - from 156001 and 54001 to 15601 and 5401. 
################################################################################
#
date
rm /home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiCgiar_30ArcSecBL.img
gdalwarp -of HFA -ot Int16   -ts 15601 5401 -r bilinear -srcnodata -9999 -dstnodata -9999 \
/home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiCgiarSrtm.vrt \
/home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiCgiar_30ArcSecBL.img
date

#! /bin/sh
################################################################################
# script Name: mosaicNwHemiAster3ArcSecBL.sh
# This script constructs the CIGAR SRTM image mosaic for the Northweetern Hemisphere
# quadrant of the global mosaic
# 
# Author: Rick Reeves, NCEAS June 9, 2011
#
# NOTE: We are using SRTM version 4.1 data distributed by CGIAR (6001 x 6001 tiles)
# Determining the -ts (row, column # cells): 
# 1) Look at the first line of corresponding .vrt file: contains the spanning extent 
# (# rows, columns) of all image tiles in the virtual mosaic, as well as the pixel
# resolution in 'native units' for the spatial reference frame (in our case, 
# the geographic coordinate system and decimal degrees).
# Example for this file: 
#
#Transform> -1.8000013888888890e+02,  2.7777777777778255e-04,  0.0000000000000000e+00,  
# 8.3000138888888884e+01,  0.0000000000000000e+00, -2.77777777777782 55e-04</GeoTransform>
# in decimal cegrees: 1 arc-seconds, or approx 30 meters.
#<VRTDataset rasterXSize="648001" rasterYSize="82801">
#
# The extent of the mosaic is fixed. To change the cell size, adjust the X and Y resolution.
# In the present case, to set cellsize at 3 arcseconds, reduce the number of cells in
# X and Y by a factor of 3: from 648001 and 82801 to 64801 and 82801. 
################################################################################
#<VRTDataset rasterXSize="648001" rasterYSize="82801">
#
rm /home/reeves/EandO/Results/NorthWestHemiAster3ArcSec.img
rm /home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiAster3ArcSec.img
gdalwarp -of HFA -ot Int16  -ts 216001 27601 -r bilinear -srcnodata -9999 -dstnodata -9999 \
/home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiAster.vrt \
/home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiAster3ArcSec.img
date

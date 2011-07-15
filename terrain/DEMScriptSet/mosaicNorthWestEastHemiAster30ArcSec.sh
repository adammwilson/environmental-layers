#! /bin/sh
# script uses gdalwarp to create mosaic of the  South Western Hemisphere
# (to 60 degrees North Latitude)  with 3ArcSec resolution 
# using CGIAR/SRTM tiles adjacent to 60 Degrees North Latitude.
# 
# 2011/06/02:  -te (georef extent) and 
#  -ts (number of rows, cols to cover w/3 arcsecond pixels) combination 
# to get exact 3 arc second pixels. 
# To get # pixels (ts parameter), multiply image extent, in degrees,
# by 3600 (number of arc seconds per degree)
# Author: Rick Reeves, NCEAS May 11, 2011
# Note: This script accesses the (no longer used) 'even 5 degree' 6000 column version
# of the CGIAR SRTM data set. 
## NOTE: Computing the -ts parameter: 
# 1) Calculate difference between bounding latitude (longitude) pairs,
# 2) multiply by 120 (3600 arc seconds per degree / 30 arc second resolution)
# NOTE: This script uses the SRTM version 4.1 data distributed by CGIAR (6001 x 6001 tiles)
# taset rasterXSize="392401" rasterYSize="79201">
#gdalwarp -of HFA -ot Int16 -ts 295201 79201 -r bilinear -srcnodata -9999 -dstnodata -9999 \
date
rm /home/reeves/active_work/OutProducts/EastHemi/NorthWestEast/NorthWestEastHemiAster_30ArcSec.img
gdalwarp -of HFA -ot Int16 -ts 98391 26401 -r bilinear -srcnodata -9999 -dstnodata -9999 \
/home/reeves/active_work/OutProducts/EastHemi/NorthWestEast/NorthWestEastHemiAster.vrt \
/home/reeves/active_work/OutProducts/EastHemi/NorthWestEast/NorthWestEastHemiAster_30ArcSec.img
date

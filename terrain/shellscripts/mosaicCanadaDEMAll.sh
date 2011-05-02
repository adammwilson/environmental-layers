#! /bin/sh
# script uses gdalwarp to create mosaic  with 9KM resolution using 
# using a second set of Canada DEM tiles adjacent to 60 Degrees North Latitude.
# Author: Rick Reeves, NCEAS April 26, 2011
date
rm ./CanadaDemMosLatest.tif
gdalwarp -of GTiff -ot Int16 -tr .0008333 .0008333 -srcnodata -32767 -dstnodata -9999 \
064demFile.tif \
065demFile.tif \
074demFile.tif \
075demFile.tif \
084demFile.tif \
085demFile.tif \
094demFile.tif \
095demFile.tif \
mergeCanada104105.tif \
./CanadaDemMosLatest.tif
date

#! /bin/sh
# script uses gdalwarp to create mosaic  with 90KM resolution 
# using sub-mosaics created from ASTER GDEM and CGIAR/SRTM images
#  adjacent to 60 Degrees North Latitude.
# Author: Rick Reeves, NCEAS April 26, 2011
date
rm /data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdyFinalBL.tif
gdalwarp -of GTiff -ot Int16 -tr .0008333 .0008333 -r bilinear -dstnodata -9999 \
/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdySRTM_BL.tif \
/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdyASTER_BL.tif \
/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdyFinalBL.tif
date

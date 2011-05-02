#! /bin/sh
# script uses gdalwarp to create mosaic  with 90KM resolution 
# using CGIAR/SRTM tiles adjacent to 60 Degrees North Latitude.
# Author: Rick Reeves, NCEAS April 26, 2011
date
rm /data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdySRTM_BL.tif
gdalwarp -of GTiff -ot Int16 -tr .0008333 .0008333 -r bilinear -srcnodata -9999 -dstnodata -9999 \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_07_01.tif \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_08_01.tif \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_09_01.tif \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_10_01.tif \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_11_01.tif \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_12_01.tif \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_13_01.tif \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_14_01.tif \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_15_01.tif \
/data/project/organisms/CgiarSrtmAll/5_5x5_ascii/srtm_16_01.tif \
/data/project/organisms/rcr/AsterCgiarMerge/mergeCgiarAsterBdySRTM_BL.tif
date

#! /bin/sh
# script builds virtual mosaic table for the  NorthEast Hemisphere
# containing CGIAR SRTM Version 4.1 files/
# using 'gdalbuildvrt' utility
# 
# Author: Rick Reeves, NCEAS June 2, 2011
date
#rm /home/reeves/active_work/OutProducts/EastHemi/NorthEastEast/NorthEastEastHemiCgiarSrtm.vrt 
gdalbuildvrt -input_file_list NorthEastEastCgiarSrtmFileList.txt  -overwrite \
/home/reeves/active_work/OutProducts/EastHemi/NorthEastEast/NorthEastEastHemiCgiarSrtm.vrt
date

#! /bin/sh
# script builds virtual mosaic table for the  SouthEast Hemisphere
# containing CGIAR SRTM Version 4.1 files/
# using 'gdalbuildvrt' utility
# 
# Author: Rick Reeves, NCEAS June 2, 2011
date
rm /home/reeves/active_work/OutProducts/EastHemi/SouthWestEast/SouthWestEastHemiCgiarSrtm.vrt 
gdalbuildvrt -input_file_list SouthWestEastHemiCgiarSrtmFileList.txt -overwrite \
/home/reeves/active_work/OutProducts/EastHemi/SouthWestEast/SouthWestEastHemiCgiarSrtm.vrt 
date

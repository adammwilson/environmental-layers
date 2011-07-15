#! /bin/sh
# script builds virtual mosaic table for the  NorthWest Hemisphere
# containing CGIAR SRTM Version 4.1 files/
# using 'gdalbuildvrt' utility
# 
# Author: Rick Reeves, NCEAS June 8, 2011
date
rm /home/reeves/active_work/OutProducts/EastHemi/NorthWestEast/NorthWestEastHemiAster.vrt 
gdalbuildvrt -input_file_list NorthWestEastHemiAsterFileList.txt -overwrite \
/home/reeves/active_work/OutProducts/EastHemi/NorthWestEast/NorthWestEastHemiAster.vrt 
date

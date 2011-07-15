#! /bin/sh
################################################################################
# script Name: buildNewSwHemiCigarMosaicVrt.sh
# script uses 'gdalbuildvrt' utility to construct  virtual image mosaic table
#  for the SouthWest Hemisphere mosaic componenes containing only CGIAR SRTM 
# Version 4.1 files.
# 
# Author: Rick Reeves, NCEAS June 12, 2011
#
################################################################################
date
rm /home/reeves/active_work/OutProducts/WestHemi/SouthWest/SouthWestHemiCgiariSrtm.vrt 
gdalbuildvrt -input_file_list SouthWestHemiCgiarSrtmFileList.txt -overwrite \
/home/reeves/active_work/OutProducts/WestHemi/SouthWest/SouthWestHemiCgiarSrtm.vrt 
date

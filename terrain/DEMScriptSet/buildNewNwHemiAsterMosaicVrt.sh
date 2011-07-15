#! /bin/sh
################################################################################
# script Name: buildNewNwHemiAsterMosaicVrt.sh
# script uses 'gdalbuildvrt' utility to construct  virtual image mosaic table
#  for the NorthWest Hemisphere mosaic componenes containing only ASTER GDEM
# Version 4.1 files.
# 
# Author: Rick Reeves, NCEAS June 8, 2011
#
################################################################################
# 
date
rm /home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiAster.vrt 
gdalbuildvrt -input_file_list  NorthWestHemiAsterFileList.txt -overwrite \
/home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiAster.vrt 
date

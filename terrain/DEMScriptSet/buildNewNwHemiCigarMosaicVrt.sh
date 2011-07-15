#! /bin/sh
################################################################################
# script Name: buildNewNwHemiCigarMosaicVrt.sh
# script uses 'gdalbuildvrt' utility to construct  virtual image mosaic table
#  for the NorthWest Hemisphere mosaic componenes containing only CGIAR SRTM 
# Version 4.1 files.
# 
# input:  NorthWestHemiCgiarSrtmFileList.txt: Text file contains list of 
#        CIGAR/SRTM image 'tile' files that comprise the image mosaic.
# Author: Rick Reeves, NCEAS June 8, 2011
#
################################################################################
date
rm /home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiCgiarSrtm.vrt 
gdalbuildvrt -input_file_list NorthWestHemiCgiarSrtmFileList.txt -overwrite \
/home/reeves/active_work/OutProducts/WestHemi/NorthWest/NorthWestHemiCgiarSrtm.vrt 
date

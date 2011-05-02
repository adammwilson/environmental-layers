#!/bin/sh
# Script extracts .tif-format image files from SRTM .zip archives
# Author: Rick Reeves, NCEAS
#
iNames=`ls ./srtm_0[2-9]*.zip`

for iName in $iNames
do
  echo " unpacking: $iName"
  unzip -o $iName
done

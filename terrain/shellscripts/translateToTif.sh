#!/bin/sh
# script translates SRTM files in ESRI .ASC format files into .tif format
# Author: Rick Reeves, NCEAS April 26, 2011

iNames=`ls ./srtm_0[2-9]*.asc`

for iName in $iNames
do
  echo " translating: $iName"
  gdal_translate -ot Int16 -of GTiff  $iName $iName.tif
done

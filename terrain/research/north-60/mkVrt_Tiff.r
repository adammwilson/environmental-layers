# This scripts are to make SRTM vrt file and ASTER GDEM2 vrt file (full extent N82).
# Then make tiff for each region (9). With ASTER, we will make two tiff for each region,
# one for the boundary analysis purpose (_60N.tif) and the other for full-extent (_82N.tif).
# All the files should be saved at /data/project/organisms/DEM/Yuni/vrt




---------------------------------------------------------------------
#SRTM
export SRTMDIR="/data/project/organisms/DEM/Yuni/Data/srtm"


# Create SRTM Tiff files for each region (also convert to 16bit integer)
gdalbuildvrt $SRTMDIR/srtm2.vrt  /data/project/organisms/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1/srtm_*_01.asc


gdalwarp -ot Int16  -te   20 59  59.99  60  -ts 48000 1200 -r bilinear  $SRTMDIR/srtm.vrt  $SRTMDIR/srtm_e020e059_below.tif
gdalwarp -ot Int16  -te   60 59  99.99  60  -ts 48000 1200 -r bilinear  $SRTMDIR/srtm.vrt  $SRTMDIR/srtm_e060e099_below.tif
gdalwarp -ot Int16  -te  100 59 139.99  60  -ts 48000 1200 -r bilinear  $SRTMDIR/srtm.vrt  $SRTMDIR/srtm_e100e139_below.tif
gdalwarp -ot Int16  -te  140 59 179.99  60  -ts 48000 1200 -r bilinear  $SRTMDIR/srtm.vrt  $SRTMDIR/srtm_e140e179_below.tif
gdalwarp -ot Int16  -te  -20 59  19.99  60  -ts 48000 1200 -r bilinear  $SRTMDIR/srtm.vrt  $SRTMDIR/srtm_w020e019_below.tif
gdalwarp -ot Int16  -te  -60 59 -21.99  60  -ts 48000 1200 -r bilinear  $SRTMDIR/srtm.vrt  $SRTMDIR/srtm_w060w021_below.tif
gdalwarp -ot Int16  -te -100 59 -61.99  60  -ts 48000 1200 -r bilinear  $SRTMDIR/srtm.vrt  $SRTMDIR/srtm_w100w061_below.tif
gdalwarp -ot Int16  -te -140 59 -101.99 60  -ts 48000 1200 -r bilinear  $SRTMDIR/srtm.vrt  $SRTMDIR/srtm_w140w101_below.tif
gdalwarp -ot Int16  -te -180 59 -141.99 60  -ts 48000 1200 -r bilinear  $SRTMDIR/srtm.vrt  $SRTMDIR/srtm_w180w141_below.tif




---------------------------------------------------------------------

# ASTER
export ASTDIR="/data/project/organisms/DEM/Yuni/Data/aster2"



gdalbuildvrt $ASTDIR/aster.vrt  /data/project/organisms/DEM/asterGdem2/ASTGTM2*_dem.tif 



# Create ASTER Tiff files for each region FOR Below(N60) 
gdalwarp -ot Int16  -te   20 59  59.99  60  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e020e059_below.tif
gdalwarp -ot Int16  -te   60 59  99.99  60  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e060e099_below.tif
gdalwarp -ot Int16  -te  100 59 139.99  60  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e100e139_below.tif
gdalwarp -ot Int16  -te  140 59 179.99  60  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e140e179_below.tif
gdalwarp -ot Int16  -te  -20 59  19.99  60  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w020e019_below.tif
gdalwarp -ot Int16  -te  -60 59 -21.99  60  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w060w021_below.tif
gdalwarp -ot Int16  -te -100 59 -61.99  60  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w100w061_below.tif
gdalwarp -ot Int16  -te -140 59 -101.99 60  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w140w101_below.tif
gdalwarp -ot Int16  -te -180 59 -141.99 60  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w180w141_below.tif


# Create ASTER Tiff files for each region FOR above N60 (61N)  
gdalwarp -ot Int16  -te   20 60  59.99  61  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e020e059_above.tif
gdalwarp -ot Int16  -te   60 60  99.99  61  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e060e099_above.tif
gdalwarp -ot Int16  -te  100 60 139.99  61  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e100e139_above.tif
gdalwarp -ot Int16  -te  140 60 179.99  61  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e140e179_above.tif
gdalwarp -ot Int16  -te  -20 60  19.99  61  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w020e019_above.tif
gdalwarp -ot Int16  -te  -60 60 -21.99  61  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w060w021_above.tif
gdalwarp -ot Int16  -te -100 60 -61.99  61  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w100w061_above.tif
gdalwarp -ot Int16  -te -140 60 -101.99 61  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w140w101_above.tif
gdalwarp -ot Int16  -te -180 60 -141.99 61  -ts 48000 1200 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w180w141_above.tif


# Create ASTER Tiff files for each region for straddle (N59 to 61) 
# This equals to normal straddle, introduced by Jim as aster_300straddle.tif 
gdalwarp -ot Int16  -te   20 59  59.99  61  -ts 48000 2400 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e020e059_straddle.tif
gdalwarp -ot Int16  -te   60 59  99.99  61  -ts 48000 2400 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e060e099_straddle.tif
gdalwarp -ot Int16  -te  100 59 139.99  61  -ts 48000 2400 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e100e139_straddle.tif
gdalwarp -ot Int16  -te  140 59 179.99  61  -ts 48000 2400 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e140e179_straddle.tif
gdalwarp -ot Int16  -te  -20 59  19.99  61  -ts 48000 2400 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w020e019_straddle.tif
gdalwarp -ot Int16  -te  -60 59 -21.99  61  -ts 48000 2400 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w060w021_straddle.tif
gdalwarp -ot Int16  -te -100 59 -61.99  61  -ts 48000 2400 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w100w061_straddle.tif
gdalwarp -ot Int16  -te -140 59 -101.99 61  -ts 48000 2400 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w140w101_straddle.tif
gdalwarp -ot Int16  -te -180 59 -141.99 61  -ts 48000 2400 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w180w141_straddle.tif



# Create ASTER Tiff files for each region FOR FULL EXTENTION (82N)
gdalwarp -ot Int16  -te   20 59  59.99  82  -ts 48000 27600 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e020e059_82N.tif
gdalwarp -ot Int16  -te   60 59  99.99  82  -ts 48000 27600 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e060e099_82N.tif
gdalwarp -ot Int16  -te  100 59 139.99  82  -ts 48000 27600 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e100e139_82N.tif
gdalwarp -ot Int16  -te  140 59 179.99  82  -ts 48000 27600 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_e140e179_82N.tif
gdalwarp -ot Int16  -te  -20 59  19.99  82  -ts 48000 27600 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w020e019_82N.tif
gdalwarp -ot Int16  -te  -60 59 -21.99  82  -ts 48000 27600 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w060w021_82N.tif
gdalwarp -ot Int16  -te -100 59 -61.99  82  -ts 48000 27600 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w100w061_82N.tif
gdalwarp -ot Int16  -te -140 59 -101.99 82  -ts 48000 27600 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w140w101_82N.tif
gdalwarp -ot Int16  -te -180 59 -141.99 82  -ts 48000 27600 -srcnodata "-9999" -r bilinear  $ASTDIR/aster.vrt $ASTDIR/aster2_w180w141_82N.tif












 


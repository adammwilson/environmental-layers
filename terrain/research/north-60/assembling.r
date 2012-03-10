# Script for assembling boundary-corrected DEM components above and below N60 with respect to 
# both uncorrected fused layer (straddle) and Gaussian blend (blendgau) of SRTM/ASTER to the south. 
# Then make GeoTiff for each nine region.
# All the files should be saved at  "/data/project/organisms/DEM/Yuni/Data"
#
# 9-Dec-2011
# Yuina Nunokawa 



----------------------------------------------------------------------

# Create Tiff files of uncorrected fused layer (straddle) for each nine region 

export SRTMDIR="/data/project/organisms/DEM/Yuni/Data/srtm"
export ASTDIR="/data/project/organisms/DEM/Yuni/Data/aster2"

gdalwarp -ot Int16  -te   20 59  59.99  61  -ts 48000 2400  $SRTMDIR/srtm_e020e059_below.tif  $ASTDIR/aster2_e020e059_above.tif $ASTDIR/fused_e020e059_straddle.tif
gdalwarp -ot Int16  -te   60 59  99.99  61  -ts 48000 2400  $SRTMDIR/srtm_e060e099_below.tif  $ASTDIR/aster2_e060e099_above.tif $ASTDIR/fused_e060e099_straddle.tif
gdalwarp -ot Int16  -te  100 59 139.99  61  -ts 48000 2400  $SRTMDIR/srtm_e100e139_below.tif  $ASTDIR/aster2_e100e139_above.tif $ASTDIR/fused_e100e139_straddle.tif
gdalwarp -ot Int16  -te  140 59 179.99  61  -ts 48000 2400  $SRTMDIR/srtm_e140e179_below.tif  $ASTDIR/aster2_e140e179_above.tif $ASTDIR/fused_e140e179_straddle.tif
gdalwarp -ot Int16  -te  -20 59  19.99  61  -ts 48000 2400  $SRTMDIR/srtm_w020e019_below.tif  $ASTDIR/aster2_w020e019_above.tif $ASTDIR/fused_w020e019_straddle.tif
gdalwarp -ot Int16  -te  -60 59 -21.99  61  -ts 48000 2400  $SRTMDIR/srtm_w060w021_below.tif  $ASTDIR/aster2_w060w021_above.tif $ASTDIR/fused_w060w021_straddle.tif
gdalwarp -ot Int16  -te -100 59 -61.99  61  -ts 48000 2400  $SRTMDIR/srtm_w100w061_below.tif  $ASTDIR/aster2_w100w061_above.tif $ASTDIR/fused_w100w061_straddle.tif
gdalwarp -ot Int16  -te -140 59 -101.99 61  -ts 48000 2400  $SRTMDIR/srtm_w140w101_below.tif  $ASTDIR/aster2_w140w101_above.tif $ASTDIR/fused_w140w101_straddle.tif
gdalwarp -ot Int16  -te -180 59 -141.99 61  -ts 48000 2400  $SRTMDIR/srtm_w180w141_below.tif  $ASTDIR/aster2_w180w141_above.tif $ASTDIR/fused_w180w141_straddle.tif




------------------------------------------------------------------------

# Create Tiff files of blend gaussian fused layer (blendgau) for each nine region

gdalwarp -ot Int16  -te   20 59  59.99  61  -ts 48000 2400  $ASTDIR/aster2_e020e059_below_blendgau.tif  $ASTDIR/aster2_e020e059_above.tif $ASTDIR/fused_e020e059_blendgau.tif
gdalwarp -ot Int16  -te   60 59  99.99  61  -ts 48000 2400  $ASTDIR/aster2_e060e099_below_blendgau.tif  $ASTDIR/aster2_e060e099_above.tif $ASTDIR/fused_e060e099_blendgau.tif
gdalwarp -ot Int16  -te  100 59 139.99  61  -ts 48000 2400  $ASTDIR/aster2_e100e139_below_blendgau.tif  $ASTDIR/aster2_e100e139_above.tif $ASTDIR/fused_e100e139_blendgau.tif
gdalwarp -ot Int16  -te  140 59 179.99  61  -ts 48000 2400  $ASTDIR/aster2_e140e179_below_blendgau.tif  $ASTDIR/aster2_e140e179_above.tif $ASTDIR/fused_e140e179_blendgau.tif
gdalwarp -ot Int16  -te  -20 59  19.99  61  -ts 48000 2400  $ASTDIR/aster2_w020e019_below_blendgau.tif  $ASTDIR/aster2_w020e019_above.tif $ASTDIR/fused_w020e019_blendgau.tif
gdalwarp -ot Int16  -te  -60 59 -21.99  61  -ts 48000 2400  $ASTDIR/aster2_w060w021_below_blendgau.tif  $ASTDIR/aster2_w060w021_above.tif $ASTDIR/fused_w060w021_blendgau.tif
gdalwarp -ot Int16  -te -100 59 -61.99  61  -ts 48000 2400  $ASTDIR/aster2_w100w061_below_blendgau.tif  $ASTDIR/aster2_w100w061_above.tif $ASTDIR/fused_w100w061_blendgau.tif
gdalwarp -ot Int16  -te -140 59 -101.99 61  -ts 48000 2400  $ASTDIR/aster2_w140w101_below_blendgau.tif  $ASTDIR/aster2_w140w101_above.tif $ASTDIR/fused_w140w101_blendgau.tif
gdalwarp -ot Int16  -te -180 59 -141.99 61  -ts 48000 2400  $ASTDIR/aster2_w180w141_below_blendgau.tif  $ASTDIR/aster2_w180w141_above.tif $ASTDIR/fused_w180w141_blendgau.tif







 


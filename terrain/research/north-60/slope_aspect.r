# Script for creating Slope and Aspect dem (tif) for SRTM aster straddle, and both uncorrected
# fused layer (straddle) and blend gaussian fused layer (blendgau) for each nine region.
#
# 9-Dec-2011
# Yuina Nunokawa






------------------------------------------------------------------------

# SRTM

export SRTMDIR="/data/project/organisms/DEM/Yuni/Data/srtm"


# SRTM Slope 
gdaldem slope -s 111120  $SRTMDIR/srtm_e020e059_below.tif $SRTMDIR/srtm_e020e059_below_below_s.tif 
gdaldem slope -s 111120  $SRTMDIR/srtm_e060e099_below.tif $SRTMDIR/srtm_e060e099_below_below_s.tif
gdaldem slope -s 111120  $SRTMDIR/srtm_e100e139_below.tif $SRTMDIR/srtm_e100e139_below_below_s.tif
gdaldem slope -s 111120  $SRTMDIR/srtm_e140e179_below.tif $SRTMDIR/srtm_e140e179_below_below_s.tif
gdaldem slope -s 111120  $SRTMDIR/srtm_w020e019_below.tif $SRTMDIR/srtm_w020e019_below_below_s.tif
gdaldem slope -s 111120  $SRTMDIR/srtm_w060w021_below.tif $SRTMDIR/srtm_w060w021_below_below_s.tif
gdaldem slope -s 111120  $SRTMDIR/srtm_w100w061_below.tif $SRTMDIR/srtm_w100w061_below_below_s.tif
gdaldem slope -s 111120  $SRTMDIR/srtm_w140w101_below.tif $SRTMDIR/srtm_w140w101_below_below_s.tif
gdaldem slope -s 111120  $SRTMDIR/srtm_w180w141_below.tif $SRTMDIR/srtm_w180w141_below_below_s.tif


# SRTM Aspect 
gdaldem aspect -s 111120  $SRTMDIR/srtm_e020e059_below.tif $SRTMDIR/srtm_e020e059_below_a.tif 
gdaldem aspect -s 111120  $SRTMDIR/srtm_e060e099_below.tif $SRTMDIR/srtm_e060e099_below_a.tif
gdaldem aspect -s 111120  $SRTMDIR/srtm_e100e139_below.tif $SRTMDIR/srtm_e100e139_below_a.tif
gdaldem aspect -s 111120  $SRTMDIR/srtm_e140e179_below.tif $SRTMDIR/srtm_e140e179_below_a.tif
gdaldem aspect -s 111120  $SRTMDIR/srtm_w020e019_below.tif $SRTMDIR/srtm_w020e019_below_a.tif
gdaldem aspect -s 111120  $SRTMDIR/srtm_w060w021_below.tif $SRTMDIR/srtm_w060w021_below_a.tif
gdaldem aspect -s 111120  $SRTMDIR/srtm_w100w061_below.tif $SRTMDIR/srtm_w100w061_below_a.tif
gdaldem aspect -s 111120  $SRTMDIR/srtm_w140w101_below.tif $SRTMDIR/srtm_w140w101_below_a.tif
gdaldem aspect -s 111120  $SRTMDIR/srtm_w180w141_below.tif $SRTMDIR/srtm_w180w141_below_a.tif


------------------------------------------------------------------------

# ASTER

export ASTDIR="/data/project/organisms/DEM/Yuni/Data/aster2"

# Create slop files of aster straddle layer (straddle) for each nine region 
gdaldem slope -s 111120  $ASTDIR/aster2_e020e059_straddle.tif $ASTDIR/aster2_e020e059_straddle_s.tif 
gdaldem slope -s 111120  $ASTDIR/aster2_e060e099_straddle.tif $ASTDIR/aster2_e060e099_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/aster2_e100e139_straddle.tif $ASTDIR/aster2_e100e139_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/aster2_e140e179_straddle.tif $ASTDIR/aster2_e140e179_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/aster2_w020e019_straddle.tif $ASTDIR/aster2_w020e019_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/aster2_w060w021_straddle.tif $ASTDIR/aster2_w060w021_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/aster2_w100w061_straddle.tif $ASTDIR/aster2_w100w061_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/aster2_w140w101_straddle.tif $ASTDIR/aster2_w140w101_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/aster2_w180w141_straddle.tif $ASTDIR/aster2_w180w141_straddle_s.tif



# Create aspect files of aster straddle layer (straddle) for each nine region 
gdaldem aspect -s 111120  $ASTDIR/aster2_e020e059_straddle.tif $ASTDIR/aster2_e020e059_straddle_a.tif 
gdaldem aspect -s 111120  $ASTDIR/aster2_e060e099_straddle.tif $ASTDIR/aster2_e060e099_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/aster2_e100e139_straddle.tif $ASTDIR/aster2_e100e139_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/aster2_e140e179_straddle.tif $ASTDIR/aster2_e140e179_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/aster2_w020e019_straddle.tif $ASTDIR/aster2_w020e019_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/aster2_w060w021_straddle.tif $ASTDIR/aster2_w060w021_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/aster2_w100w061_straddle.tif $ASTDIR/aster2_w100w061_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/aster2_w140w101_straddle.tif $ASTDIR/aster2_w140w101_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/aster2_w180w141_straddle.tif $ASTDIR/aster2_w180w141_straddle_a.tif




# Create slop files of uncorrected fused layer (straddle) for each nine region
gdaldem slope -s 111120  $ASTDIR/fused_e020e059_straddle.tif $ASTDIR/fused_e020e059_straddle_s.tif 
gdaldem slope -s 111120  $ASTDIR/fused_e060e099_straddle.tif $ASTDIR/fused_e060e099_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_e100e139_straddle.tif $ASTDIR/fused_e100e139_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_e140e179_straddle.tif $ASTDIR/fused_e140e179_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w020e019_straddle.tif $ASTDIR/fused_w020e019_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w060w021_straddle.tif $ASTDIR/fused_w060w021_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w100w061_straddle.tif $ASTDIR/fused_w100w061_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w140w101_straddle.tif $ASTDIR/fused_w140w101_straddle_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w180w141_straddle.tif $ASTDIR/fused_w180w141_straddle_s.tif




# Create aspect files of uncorrected fused layer (straddle) for each nine region 

gdaldem aspect -s 111120  $ASTDIR/fused_e020e059_straddle.tif $ASTDIR/fused_e020e059_straddle_a.tif 
gdaldem aspect -s 111120  $ASTDIR/fused_e060e099_straddle.tif $ASTDIR/fused_e060e099_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_e100e139_straddle.tif $ASTDIR/fused_e100e139_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_e140e179_straddle.tif $ASTDIR/fused_e140e179_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w020e019_straddle.tif $ASTDIR/fused_w020e019_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w060w021_straddle.tif $ASTDIR/fused_w060w021_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w100w061_straddle.tif $ASTDIR/fused_w100w061_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w140w101_straddle.tif $ASTDIR/fused_w140w101_straddle_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w180w141_straddle.tif $ASTDIR/fused_w180w141_straddle_a.tif




# Create slope files of blend gaussian fused layer (blendgau) for nine each region

gdaldem slope -s 111120  $ASTDIR/fused_e020e059_blendgau.tif $ASTDIR/fused_e020e059_blendgau_s.tif 
gdaldem slope -s 111120  $ASTDIR/fused_e060e099_blendgau.tif $ASTDIR/fused_e060e099_blendgau_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_e100e139_blendgau.tif $ASTDIR/fused_e100e139_blendgau_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_e140e179_blendgau.tif $ASTDIR/fused_e140e179_blendgau_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w020e019_blendgau.tif $ASTDIR/fused_w020e019_blendgau_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w060w021_blendgau.tif $ASTDIR/fused_w060w021_blendgau_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w100w061_blendgau.tif $ASTDIR/fused_w100w061_blendgau_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w140w101_blendgau.tif $ASTDIR/fused_w140w101_blendgau_s.tif
gdaldem slope -s 111120  $ASTDIR/fused_w180w141_blendgau.tif $ASTDIR/fused_w180w141_blendgau_s.tif




# Create aspect files of blend gaussian fused layer (blendgau) for each nine region 

gdaldem aspect -s 111120  $ASTDIR/fused_e020e059_blendgau.tif $ASTDIR/fused_e020e059_blendgau_a.tif 
gdaldem aspect -s 111120  $ASTDIR/fused_e060e099_blendgau.tif $ASTDIR/fused_e060e099_blendgau_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_e100e139_blendgau.tif $ASTDIR/fused_e100e139_blendgau_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_e140e179_blendgau.tif $ASTDIR/fused_e140e179_blendgau_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w020e019_blendgau.tif $ASTDIR/fused_w020e019_blendgau_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w060w021_blendgau.tif $ASTDIR/fused_w060w021_blendgau_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w100w061_blendgau.tif $ASTDIR/fused_w100w061_blendgau_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w140w101_blendgau.tif $ASTDIR/fused_w140w101_blendgau_a.tif
gdaldem aspect -s 111120  $ASTDIR/fused_w180w141_blendgau.tif $ASTDIR/fused_w180w141_blendgau_a.tif



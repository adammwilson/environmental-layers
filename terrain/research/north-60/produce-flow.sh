# GRASS commands for running terraflow on raw and fused DEMs in the
# 60N Canada boundary region, currently for the purposes of assessing
# and comparing flow direction.
#
# Jim Regetz
# NCEAS

export DEMDIR=~/media/temp/terrain/dem
export FLOWDIR=~/media/temp/terrain/flow

#
# Canada near 60N, from 136W to 96WW
#

# load sample data for testing flow stuff
r.in.gdal input=$DEMDIR/cdem_300straddle.tif output=cdem_300straddle
r.in.gdal input=$DEMDIR/aster_300straddle.tif output=aster_300straddle
r.in.gdal input=$DEMDIR/srtm_150below.tif output=srtm_150below
r.in.gdal input=$DEMDIR/fused_300straddle.tif output=fused_300straddle
r.in.gdal input=$DEMDIR/fused_300straddle_rampexp.tif output=fused_300straddle_rampexp
r.in.gdal input=$DEMDIR/fused_300straddle_blendgau.tif output=fused_300straddle_blendgau
r.in.gdal input=$DEMDIR/fused_300straddle_enblend.tif output=fused_300straddle_enblend

# oops -- region is too big for terraflow default of using
# dimension_type (i.e., short), which means nrows and ncols are both
# capped at ~30K (2^15):
#  ERROR: [nrows=300, ncols=48000] dimension_type overflow -- change
#         dimension_type and recompile
# so let's restrict it to a smaller lon range for now...
g.region n=60.125 s=59.875 w=-125 e=-100

# do flow
# each took ~1.5 min on xander (22-Jun-2011)
r.terraflow.short elevation=cdem_300straddle filled=filled_cdem \
  direction=direction_cdem swatershed=swatershed_cdem \
  accumulation=accumulation_cdem tci=tci_cdem
r.terraflow.short elevation=aster_300straddle filled=filled_aster \
  direction=direction_aster swatershed=swatershed_aster \
  accumulation=accumulation_aster tci=tci_aster
r.terraflow.short elevation=fused_300straddle filled=filled_fused \
  direction=direction_fused swatershed=swatershed_fused \
  accumulation=accumulation_fused tci=tci_fused
r.terraflow.short elevation=fused_300straddle_blendgau filled=filled_fused_bg \
  direction=direction_fused_bg swatershed=swatershed_fused_bg \
  accumulation=accumulation_fused_bg tci=tci_fused_bg
r.terraflow.short elevation=fused_300straddle_enblend filled=filled_fused_mrs \
  direction=direction_fused_mrs swatershed=swatershed_fused_mrs \
  accumulation=accumulation_fused_mrs tci=tci_fused_mrs

# now with SFD (D8) algorithm
# each took ~1 min on xander (22-Jun-2011)
r.terraflow.short -s elevation=cdem_300straddle filled=filled_cdem_sfd \
  direction=direction_cdem_sfd swatershed=swatershed_cdem_sfd \
  accumulation=accumulation_cdem_sfd tci=tci_cdem_sfd
r.terraflow.short -s elevation=aster_300straddle filled=filled_aster_sfd \
  direction=direction_aster_sfd swatershed=swatershed_aster_sfd \
  accumulation=accumulation_aster_sfd tci=tci_aster_sfd
r.terraflow.short -s elevation=fused_300straddle filled=filled_fused_sfd \
  direction=direction_fused_sfd swatershed=swatershed_fused_sfd \
  accumulation=accumulation_fused_sfd tci=tci_fused_sfd
r.terraflow.short -s elevation=fused_300straddle_blendgau filled=filled_fused_bg_sfd \
  direction=direction_fused_bg_sfd swatershed=swatershed_fused_bg_sfd \
  accumulation=accumulation_fused_bg_sfd tci=tci_fused_bg_sfd
r.terraflow.short -s elevation=fused_300straddle_enblend filled=filled_fused_mrs_sfd \
  direction=direction_fused_mrs_sfd swatershed=swatershed_fused_mrs_sfd \
  accumulation=accumulation_fused_mrs_sfd tci=tci_fused_mrs_sfd

# export flow dir rasters as geotiffs
r.out.gdal input=direction_cdem output=$FLOWDIR/cdem_300straddle_mfd.tif
r.out.gdal input=direction_cdem_sfd output=$FLOWDIR/cdem_300straddle_sfd.tif
r.out.gdal input=direction_aster output=$FLOWDIR/aster_300straddle_mfd.tif
r.out.gdal input=direction_aster_sfd output=$FLOWDIR/aster_300straddle_sfd.tif
r.out.gdal input=direction_fused output=$FLOWDIR/fused_300straddle_mfd.tif
r.out.gdal input=direction_fused_sfd output=$FLOWDIR/fused_300straddle_sfd.tif
r.out.gdal input=direction_fused_bg output=$FLOWDIR/fused_300straddle_blendgau_mfd.tif
r.out.gdal input=direction_fused_bg_sfd output=$FLOWDIR/fused_300straddle_blendgau_sfd.tif
r.out.gdal input=direction_fused_mrs output=$FLOWDIR/fused_300straddle_enblend_mfd.tif
r.out.gdal input=direction_fused_mrs_sfd output=$FLOWDIR/fused_300straddle_enblend_sfd.tif

# export flow accumulation
r.out.gdal input=accumulation_fused_bg output=$FLOWDIR/fused_300straddle_blendgau_fa.tif
r.out.gdal input=accumulation_cdem output=$FLOWDIR/cdem_300straddle_fa.tif
r.out.gdal input=accumulation_aster output=$FLOWDIR/aster_300straddle_fa.tif

# do the above for SRTM, but only in southern half of region
g.region n=60 s=59.875 w=-125 e=-100
r.terraflow.short elevation=srtm_150below filled=filled_srtm \
  direction=direction_srtm swatershed=swatershed_srtm \
  accumulation=accumulation_srtm tci=tci_srtm
r.terraflow.short -s elevation=srtm_150below filled=filled_srtm_sfd \
  direction=direction_srtm_sfd swatershed=swatershed_srtm_sfd \
  accumulation=accumulation_srtm_sfd tci=tci_srtm_sfd
r.out.gdal input=direction_srtm output=$FLOWDIR/srtm_150below_mfd.tif
r.out.gdal input=direction_srtm_sfd output=$FLOWDIR/srtm_150below_sfd.tif
r.out.gdal input=accumulation_srtm output=$FLOWDIR/srtm_150below_fa.tif
# don't forget to set the region back to include cells above the 60N boundary...
g.region n=60.125 s=59.875 w=-125 e=-100



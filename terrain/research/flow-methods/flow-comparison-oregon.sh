# GRASS terraflow execution for Oregon case study area, done for
# comparison to the pre-existing Arc GRID flowaccumulation results.
#
# Jim Regetz
# NCEAS
# Created 16-Jun-2011

export DEMDIR=~/media/temp/dem/new/flow
export OUTDIR=~/media/temp/dem/new/flow

#
# Oregon case study
#

# create new mapset for oregon case study
g.mapset -c mapset=oregon

# read in smoothed srtm
# ming did smoothing? note that data type is float
r.in.gdal input=$DEMDIR/srtmv41_smth.tif output=srtmv41_smth

# set region based on this srtm raster
g.region rast=srtmv41_smth

# compute flow
# [took ~3 minutes on xander (16-Jun-2011)]
r.terraflow elevation=srtmv41_smth filled=filled_smth \
  direction=direction_smth swatershed=swatershed_smth \
  accumulation=accumulation_smth tci=tci_smth

# compute flow again, but this time using SFD (D8)
# [took ~1.5 minutes on xander (16-Jun-2011)]
r.terraflow -s elevation=srtmv41_smth filled=filled_smth_sfd \
  direction=direction_smth_sfd swatershed=swatershed_smth_sfd \
  accumulation=accumulation_smth_sfd tci=tci_smth_sfd

# write geotiffs out to disk
r.out.gdal input=direction_smth output=$OUTDIR/flowdir_OR_grass.tif
r.out.gdal input=direction_smth_sfd output=$OUTDIR/flowdir_OR_grass_sfd.tif

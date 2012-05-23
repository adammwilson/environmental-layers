# GRASS commands -- testing some stuff for flow direction
#
# Jim Regetz
# NCEAS
# Created 15-Jun-2011

export DEMDIR=~/media/temp/dem/new/flow
export OUTDIR=~/media/temp/dem/new/flow

#
# Canada 60N test area (small)
#

# adjust region to get the resolution exactly right
g.region nsres=0:00:03 ewres=0:00:03

# load sample data for testing flow stuff
r.in.gdal input=$DEMDIR/fused_small.tif output=fused_small
r.in.gdal input=$DEMDIR/fused_small_blendgau.tif output=fused_small_blendgau
r.in.gdal input=$DEMDIR/cdem_small.tif output=cdem_small

# oops -- global 3" is too big for terraflow?
# let's restrict it to the specific test swatch extent for now...
g.region n=60.125 s=59.875 w=-135 e=-134

# do flow -- took ~5 sec on xander (14-Jun-2011)
r.terraflow.short elevation=fused_small filled=filled_small \
  direction=direction_small swatershed=swatershed_small \
  accumulation=accumulation_small tci=tci_small
r.terraflow.short elevation=fused_small_blendgau filled=filled_smbg \
  direction=direction_smbg swatershed=swatershed_smbg \
  accumulation=accumulation_smbg tci=tci_smbg
r.terraflow.short elevation=cdem_small filled=filled_smc \
  direction=direction_smc swatershed=swatershed_smc \
  accumulation=accumulation_smc tci=tci_smc

# now with SFD (D8) algorithm
r.terraflow.short -s elevation=fused_small_blendgau filled=filled_smbg_sfd \
  direction=direction_smbg_sfd swatershed=swatershed_smbg_sfd \
  accumulation=accumulation_smbg_sfd tci=tci_smbg_sfd
r.terraflow.short -s elevation=cdem_small filled=filled_smc_sfd \
  direction=direction_smc_sfd swatershed=swatershed_smc_sfd \
  accumulation=accumulation_smc_sfd tci=tci_smc_sfd

# export flow dir rasters as tif
r.out.gdal input=direction_small output=$OUTDIR/flowdir_small.tif
r.out.gdal input=direction_smbg output=$OUTDIR/flowdir_smbg.tif
r.out.gdal input=direction_smbg_sfd output=$OUTDIR/flowdir_smbg_sfd.tif
r.out.gdal input=direction_smc output=$OUTDIR/flowdir_smc.tif

# for fun, map arrows by recoding SFD directions as AGNPS
# first recode flowdirs to use AGNPS directional coding
r.recode input=direction_smbg_sfd output=agnps_smbg << EOF
1:1:3:3
2:2:4:4
4:4:5:5
8:8:6:6
16:16:7:7
32:32:8:8
64:64:1:1
128:128:2:2
EOF
# now make a hillshade map
r.shaded.relief map=fused_small_blendgau shadedmap=fused_small_bg_hs \
  units=meters
d.mon start=x0
#g.region n=60:03:00N s=59:59:00N w=134:40:00W e=134:35:00W
g.region n=60:05:00N s=59:55:00N w=134:40:00W e=134:30:00W
d.erase
d.rast fused_small_bg_hs
d.rast.arrow map=agnps_smbg type=agnps grid_color=none
# add ~5km gridlines, including 60N
d.grid -b size=00:02:30 origin=0,60 color=red
# write out to png
d.out.png output=$OUTDIR/flowarrows_blended_sm.png
g.region rast=fused_small_blendgau

# now with canada dem
r.recode input=direction_smc_sfd output=agnps_smc << EOF
1:1:3:3
2:2:4:4
4:4:5:5
8:8:6:6
16:16:7:7
32:32:8:8
64:64:1:1
128:128:2:2
EOF
r.shaded.relief map=cdem_small shadedmap=cdem_small_hs units=meters
d.mon start=x1
g.region n=60:05:00N s=59:55:00N w=134:40:00W e=134:30:00W
d.erase
d.rast cdem_small_hs
d.rast.arrow map=agnps_smc type=agnps grid_color=none
g.region rast=fused_small_blendgau

# Exploratory assessment of differences between ESRI's flow direction
# output and GRASS Terraflow's flow direction output, with focus here on
# the SFD (D8) Terraflow output, which is (I think) basically the same
# as what ESRI uses.
#
# The input DEM in all cases is what I gather to be the Oregon case
# study DEM, which is this 3x3 degree, 3" resolution, smoothed (somehow?
# by someone?) SRTM:
#   jupiter:~organisms/flow/experimental/srtmv41_smth
# 
# The ESRI flowdirection raster used here is what I gather was produced by 
# layers.aml, which applies the GRID flowdirection function to the
# smoothed DEM to produce this output:
#   jupiter:~organisms/flow/experimental/flowdir
#
# I converted both of the above rasters to GeoTIFFs to bring them over
# to my machine to work with.
#
# I then generated the two Terraflow output flow direction grids (SFD
# and MFD) using the commands contained in grass-terraflow.sh.
#
# Other resources:
# * ESRI ArcGIS 9.3 Flow Direction documentation
#   http://webhelp.esri.com/arcgisdesktop/9.3/index.cfm?topicname=flow%20direction
# * GRASS terraflow documentation
#   http://grass.osgeo.org/grass64/manuals/html64_user/r.terraflow.html
#
# Jim Regetz
# NCEAS
# Created 16-Jun-2011


# data directory (local to xander)
datadir <- "~/media/temp/dem/new/flow"

# load data

# ESRI flowdirection output (see notes above)
or.flowdir.esri <- raster(file.path(datadir, "flowdir_OR_esri.tif"))
# GRASS terraflow (MFD) output 
or.flowdir.grass.mfd <- raster(file.path(datadir,
    "flowdir_OR_grass.tif"))
# GRASS terraflow (SFD) output 
or.flowdir.grass.sfd <- raster(file.path(datadir,
    "flowdir_OR_grass_sfd.tif"))

# create raster indicating where ESRI and terraflow(SFD) differ
flow.disagreement <- or.flowdir.esri
flow.disagreement[] <- ifelse(values(or.flowdir.esri !=
    or.flowdir.grass.sfd), 1, NA)

# illustrated differences
plot(or.hs, col=grey.colors(100))
mp <- prod(dim(flow.disagreement))
plot(flow.disagreement, add=TRUE, col="blue", legend=FALSE, maxpixels=mp)

# now isolate differences in cases where terraflow is not zero (i.e.,
# places calculated as pits?
flow.disagreement[or.flowdir.grass.sfd==0] <- NA
plot(flow.disagreement, add=TRUE, col="red", legend=FALSE, maxpixels=mp)

# notice what happens at the edges...

# in northernmost row, esri routes most cells east or west (i.e, along
# border) plus some north, and a few other misc directions 
table(as.matrix(or.flowdir.esri)[1,])
##    1    2    4    8   16   32   64 
## 1574    4    1    3 1600    1  425 

# whereas terraflow SFD routes all cells north
table(as.matrix(or.flowdir.grass.sfd)[1,])
##   64 
## 3608 

# (analogous patterns for the other 3 raster edges)

# quantify proportion in concordance, excluding the edges
edges <- c(1, 3608)
mean(as.matrix(or.flowdir.esri)[-edges, -edges] ==
    as.matrix(or.flowdir.grass.sfd)[-edges, -edges])
## [1] 0.9086855

# ...and what proportion are either in concordance or have 0 values
# associated with terraflow SFD? (means pits?)
mean((as.matrix(or.flowdir.esri)[-edges, -edges] ==
  as.matrix(or.flowdir.grass.sfd)[-edges, -edges]) |
  as.matrix(or.flowdir.grass.sfd)[-edges, -edges]==0)
## [1] 0.9755789


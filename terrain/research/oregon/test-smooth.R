# Experimental R procedural code to run multiscalesmooth function on
# srtm_oregon binary grid produced by Ming
#
# Jim Regetz
# NCEAS
# Crated on 27-Mar-2012

library(raster)

# set up some local paths on atlas
src.dir <- "/home/regetz/git/environmental-layers/terrain/research/oregon"
data.dir <- "/home/layers/commons/topo/v2/experimental"

source(file.path(src.dir, "multiscalesmooth.R"))

srtm_oregon <- raster(file.path(data.dir, "srtm_oregon"))

# create a subset of data to work with
srtm.sub <- crop(srtm_oregon, extent(srtm_oregon, r1=4001, r2=6000,
    c1=4001, c2=6000))

srtm.sub.sm <- multiscalesmooth(srtm.sub, sd=0.001)


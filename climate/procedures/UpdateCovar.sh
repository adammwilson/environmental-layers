## simple script to upload data from various places such as acrobates (YALE) and atlas (NCEAS) to pleiades for interpolation project

## Set working directory
cd /nobackupp1/awilso10/interp/covar/

## get distance to coast
wget http://oceancolor.gsfc.nasa.gov/DOCS/DistFromCoast/GMT_intermediate_coast_distance_01d.zip
unzip GMT_intermediate_coast_distance_01d.zip

## topographical variables
mkdir topo
scp adamw@acrobates.eeb.yale.edu:/data2/dem_variables/altitude/median/altitude_median.tif topo/
scp adamw@acrobates.eeb.yale.edu:/data2/dem_variables/slope/median/slope_median.tif topo/
scp adamw@acrobates.eeb.yale.edu:/data2/dem_variables/aspect/median/aspect_median_*.tif topo

## consensus landcover
mkdir landcover
scp adamw@acrobates.eeb.yale.edu:/data/jetzlab/Data/environ/global/landcover/Consensus_new/Layers/*.tif landcover/

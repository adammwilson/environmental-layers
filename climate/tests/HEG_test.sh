#! /bin/bash

tempdir=~/acrobates/adamw/projects/tmp
mkdir $tempdir
cd $tempdir

url="ftp://ladsweb.nascom.nasa.gov/allData/6/MOD35_L2/2009/029/"
## two granules that still show interpolation artifacts
granule="MOD35_L2.A2009029.0500.006.2012245113542.hdf"
granule="MOD35_L2.A2009029.0320.006.2012245113606.hdf"

## get swath data
wget $url$granule

## build parameter file
echo "
NUM_RUNS = 1

BEGIN
INPUT_FILENAME = $granule
OBJECT_NAME = mod35
FIELD_NAME = Cloud_Mask|
BAND_NUMBER = 1
OUTPUT_PIXEL_SIZE_X = 1013.0
OUTPUT_PIXEL_SIZE_Y = 1013.0
SPATIAL_SUBSET_UL_CORNER = ( 89.929451 -179.998932 )
SPATIAL_SUBSET_LR_CORNER = ( 64.8638 179.998108 )
RESAMPLING_TYPE = NN
OUTPUT_PROJECTION_TYPE = SIN
ELLIPSOID_CODE = WGS84
OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0  )
OUTPUT_FILENAME = out_$granule
OUTPUT_TYPE = HDFEOS
END
" > params.txt

### run it
/usr/local/heg/2.12b/bin/swtif -p params.txt -d  -tmpLatLondir $tempdir

## now view the output file in 



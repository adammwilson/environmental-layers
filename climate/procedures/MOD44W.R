### Script to download and process the MOD44W water mask from the USGS

setwd("/home/adamw/acrobates/Global/land")
url="http://e4ftl01.cr.usgs.gov/MOLT/MOD44W.005/2000.02.24/"


### download the 250m land tiles
system(paste("wget -np -nd --no-clobber -r ",url," -P tiles -R html -A hdf"))

## modis projection
mproj="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

## destination projection
dproj="+proj=cea +ellps=WGS84 +lat_ts=30"

### Get list of tiles
files=list.files("tiles",recursive=T,full=T)
maskfiles=paste("HDF4_EOS:EOS_GRID:\"",files,"\":MOD44W_250m_GRID:water_mask",sep="")
## write out a table of files to process into VRT
write.table(maskfiles,file="tiles.txt",row.names=F,col.names=F,quote=F)
## how many are there?
length(files)

## check them out
system(paste("gdalinfo ",files[1]))
system(paste("gdalinfo ",maskfiles[1]))

## test conversion of one tile
system(paste("gdalwarp -overwrite -multi -s_srs \"",mproj,"\" -t_srs \"",dproj,"\" -co COMPRESS=LZW -r near ",maskfiles[307]," land_behrmann.tif"))

### create VRT
system(paste("gdalbuildvrt -overwrite -input_file_list tiles.txt land.vrt"))
system(paste("gdalwarp -overwrite -multi -s_srs \"",mproj,"\" -t_srs \"",dproj,"\" -of GTiff -co COMPRESS=LZW land.vrt land.tif"))

## mosaic directly from tiles to tif
system(paste("gdal_merge.py -of GTiff -co COMPRESS=LZW -o land.tif ",paste(maskfiles,collapse=" ")))

system(paste("gdalwarp -overwrite -multi -s_srs \"",mproj,"\" -t_srs \"",dproj,"\" -of vrt -r near land.vrt land_behrmann.vrt"))
system(paste("gdal_translate -of GTiff -co COMPRESS=LZW land_behrmann.vrt land_behrmann.tif"))

system("gdalinfo land_behrmann.vrt")

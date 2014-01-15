### summarize MOD35 data
setwd("/home/adamw/acrobates/adamw/projects/interp/")

library(sp)
library(spgrass6)
library(rgdal)
library(reshape)
library(ncdf4)
library(geosphere)
library(rgeos)
library(multicore)
library(raster)
library(lattice)
library(rgl)
library(hdf5)
library(rasterVis)
library(heR.Misc)
library(car)

X11.options(type="Xlib")
ncores=20  #number of threads to use

psin=CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

tile="h11v08"

## get MODLAND tile information
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
save(tb,file="modlandTiles.Rdata")

tile="h11v08"   #can move this to submit script if needed
#tile="h09v04"   #oregon

tile_bb=tb[tb$tile==tile,] ## identify tile of interest
roi_ll=extent(tile_bb$lon_min,tile_bb$lon_max,tile_bb$lat_min,tile_bb$lat_max) 
#roi=spTransform(roi,psin)
#roil=as(roi,"SpatialLines")

dmod06="data/modis/mod06/summary"
dmod35="data/modis/mod35/summary"




##################################################
### generate KML file for viewing in google earth
file=paste(dmod35,"/MOD35_",tile,"_ymonmean.nc",sep="")
m=1

cat("
0 green
50 grey 
90 blue
100 red
nv     0   0   0   0 
",file=paste(dmod35,"/mod35_colors.txt",sep=""))

system(paste("gdalinfo ",file,sep=""))

system(paste("gdalwarp -multi -r cubicspline -srcnodata 255 -dstnodata 255 -s_srs '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs' -t_srs 'EPSG:4326' ",file, " MOD35_",tile,".tif",sep=""))
system(paste("gdaldem color-relief -b ",m," ",dmod35,"/MOD35_",tile,".tif mod35_colors.txt ",dmod35,"/MOD35_",tile,"_",m,".tif",sep=""))
system(paste("gdalinfo MOD35_",tile,"_",m,".tif",sep=""))

#system(paste("gdal_translate -b ",m," MOD35_",tile,".tif MOD35_",tile,"_",m,".tif",sep=""))
40075=256*(2^z))  #find zoom level

system(paste("gdal2tiles.py -z 1-8 -k ",dmod35,"/MOD35_",tile,"_",m,".tif",sep=""))

### Wind Direction
winddir="home/adamw/acrobates/adamw/projects/interp/data/ncep/"
dir.create(winddir)
system(paste("wget ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.derived/surface/vwnd.mon.ltm.nc -P ",winddir))

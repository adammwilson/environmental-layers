## explore the MOD35 data downloaded and gridded by the DAAC
setwd("~/acrobates/projects/interp/data/modis/mod35")

library(raster)
library(rgdal)

f=list.files(pattern="*.hdf")

Sys.setenv(GEOL_AS_GCPS = "PARTIAL")

GDALinfo(f[1])
system(paste("gdalinfo",f[1]))
GDALinfo("HDF4_EOS:EOS_SWATH:\"MOD35_L2.A2000100.1445.006.2012252024758.hdf\":mod35:Cloud_Mask")
system("gdalinfo HDF4_EOS:EOS_SWATH:\"MOD35_L2.A2000100.1445.006.2012252024758.hdf\":mod35:Cloud_Mask | tail -n 200")

system("gdalwarp -overwrite -geoloc -order 2 -r near -s_srs \"EPSG:4326\" HDF4_EOS:EOS_SWATH:\"MOD35_L2.A2000100.1445.006.2012252024758.hdf\":mod35:Cloud_Mask cloudmask.tif")
system("gdalwarp -overwrite -r near -s_srs \"EPSG:4326\" HDF4_EOS:EOS_SWATH:\"MOD35_L2.A2000100.1445.006.2012252024758.hdf\":mod35:Cloud_Mask:1 cloudmask2.tif")


## get tile
tile=raster("~/acrobates/projects/interp/data/modis/mod06/summary/MOD06_h09v04.nc",varname="CER")
h11v08=extent(tile)

r=raster(f[1])
extent(r)


st=lapply(f[1:10],raster)
str=lapply(2:length(st),function(i) union(extent(st[[i-1]]),extent(st[[i]])))[[length(st)-1]]
str=union(extent(h11v08),str)

b1=brick(lapply(st,function(stt) {
  x=crop(alignExtent(stt,str),h11v08)
  return(x)
}))



c=brick(f[1:10])

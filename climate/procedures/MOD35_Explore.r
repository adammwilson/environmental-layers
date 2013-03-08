## explore the MOD35 data downloaded and gridded by the DAAC
setwd("~/acrobates/projects/interp/data/modis/mod35")

library(raster)
library(rgdal)

f=list.files(pattern="Cloud_Mask_1_1")

GDALinfo(f[1])

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

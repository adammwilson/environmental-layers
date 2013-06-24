### this script generates empty tifs that align with the MODIS land tiles, but are available globally


## get MODIS gring coordingates
tb=read.table("http://modis-land.gsfc.nasa.gov/pdf/sn_gring_10deg.txt",skip=7,nrows=648,header=F,sep="")
colnames(tb)=c("iv","ih","ll_lon","ll_lat","ul_lon","ul_lat","ur_lon","ur_lat","lr_lon","lr_lat")
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
## drop null tiles
tb=tb[!is.na(tb$ll_lon)&tb$ll_lon!=-999,]
## transform to polygons
getgrid=function(i){
  print(i)
  x=tb[i,]
  gcols=matrix(as.numeric(c(x[3:10],x[3:4])),ncol=2,byrow=T)
  gpol= Polygons(list(Polygon(gcols)),tb[i,"tile"])
return(gpol)
}

modgrid=SpatialPolygons(lapply(1:nrow(tb),getgrid))
proj4string(modgrid)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## transform to sinusoidal
modgrids=spTransform(modgrid,CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))

## two tiles for testing
tiles=c("h22v02","h03v11")

## path to MOD11A1 file for this tile to align grid/extent
gridfile=list.files("/nobackupp4/datapool/modis/MOD11A2.005/2009.01.01",pattern=paste(tiles[1],".*[.]hdf$",sep=""),recursive=T,full=T)[1]
td=raster(paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_8Day_1km_LST:LST_Day_1km",sep=""))
#projection(td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs +datum=WGS84 +ellps=WGS84 "

gridfile2=list.files("/nobackupp4/datapool/modis/MOD11A2.005/2009.01.01",pattern=paste(tiles[2],".*[.]hdf$",sep=""),recursive=T,full=T)[1]
td2=raster(paste("HDF4_EOS:EOS_GRID:\"",gridfile2,"\":MODIS_Grid_8Day_1km_LST:LST_Day_1km",sep=""))

  

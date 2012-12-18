### Script to explore selection of Case Study Regions

library(rgdal)
library(latticeExtra)
library(maptools)

## make local copy of ghcn database on atlas
#  pg_dump ghcn > ghcn
#  copy file to local server
#  createdb ghcn
#  psql -d ghcn < ghcn


### get modis tiles
modt=readOGR("/home/adamw/acrobates/Global/modis_sinusoidal","modis_sinusoidal_grid_world",)
tiles=c("H18V1","H18V2","H18V3","H11V8","H19V12","H20V12","H21V9","H21V8","H31V11","H31V10","H29V5","H28V4","H28V5")
modt$roi=as.factor(modt$HV%in%tiles)

## get coastline data
#coast=readOGR("/media/data/globallayers/GSHHS/GSHHS_shp/c/","GSHHS_c_L2")
coast=Rgshhs("/media/data/globallayers/GSHHS/gshhs/gshhs_c.b", ylim=c(-60,90),xlim=c(0.01,359.99), level = 4, minarea = 1, shift = T, verbose = TRUE, checkPolygons=T)[["SP"]]
coast2=nowrapSpatialPolygons(coast)
coast=getRgshhsMap("/media/data/globallayers/GSHHS/gshhs/gshhs_c.b", ylim=c(-60,90),xlim=c(-180,180), level=4,shift = T, verbose = TRUE, checkPolygons=T)

## create union version
coast2=gUnionCascaded(coast)  #dissolve any subparts of coast
coast2=SpatialPolygonsDataFrame(coast2,data=data.frame(id=1))
## create spatial lines version
coastl=as(coast,"SpatialLines")


### transform to dproj
coast=spTransform(coast,projection(modt))
coastl=spTransform(coastl,CRS(proj4string(modt)))


spplot(modt,zcol="roi",col.regions=c("transparent","red"))+layer(sp.lines(coastl))

## Bounding box of region in lat/lon
roi_ll=spTransform(roi,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
roi_bb=bbox(roi_ll)

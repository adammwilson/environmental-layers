## explore the MOD35 data downloaded and gridded by the DAAC
setwd("~/acrobates/adamw/projects/interp/data/modis/mod35")

library(raster)
library(rasterVis)
library(rgdal)
library(plotKML)

#f=list.files(pattern="*.hdf")

#Sys.setenv(GEOL_AS_GCPS = "PARTIAL")
## try swath-grid with gdal
#GDALinfo(f[1])
#system(paste("gdalinfo",f[2]))
#GDALinfo("HDF4_EOS:EOS_SWATH:\"MOD35_L2.A2000100.1445.006.2012252024758.hdf\":mod35:Cloud_Mask")
#system("gdalinfo HDF4_EOS:EOS_SWATH:\"data/modis/mod35/MOD35_L2.A2000100.1445.006.2012252024758.hdf\":mod35:Cloud_Mask")
#system("gdalwarp -overwrite -geoloc -order 2 -r near -s_srs \"EPSG:4326\" HDF4_EOS:EOS_SWATH:\"MOD35_L2.A2000100.1445.006.2012252024758.hdf\":mod35:Cloud_Mask cloudmask.tif")
#system("gdalwarp -overwrite -r near -s_srs \"EPSG:4326\" HDF4_EOS:EOS_SWATH:\"MOD35_L2.A2000100.1445.006.2012252024758.hdf\":mod35:Cloud_Mask:1 cloudmask2.tif")
#r=raster(f[1])
#extent(r)
#st=lapply(f[1:10],raster)
#str=lapply(2:length(st),function(i) union(extent(st[[i-1]]),extent(st[[i]])))[[length(st)-1]]
#str=union(extent(h11v08),str)
#b1=brick(lapply(st,function(stt) {
#  x=crop(alignExtent(stt,str),h11v08)
#  return(x)
#}))
#c=brick(f[1:10])

## get % cloudy
v5=stack(brick("../mod06/summary/MOD06_h11v08_ymoncld01.nc",varname="CLD01"))
projection(v5)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
v6=stack(brick("summary/MOD35_h11v08.nc",varname="PCloud"))
projection(v6)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
v6=setZ(v6,as.Date(paste("2011-",1:12,"-15",sep="")))
names(v6)=month.name

## generate means
v6m=mean(v6)
v5m=mean(v5)


## landcover
lulc=raster("~/acrobatesroot/jetzlab/Data/environ/global/landcover/MODIS/MCD12Q1_IGBP_2005_v51.tif")
projection(lulc)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
lulc=crop(lulc,v6)

Mode <- function(x,na.rm=T) {  #get MODE
  x=na.omit(x)
  ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
  }
## aggregate to 1km resolution
lulc2=aggregate(lulc,2,fun=function(x,na.rm=T) Mode(x))
## convert to factor table
lulcf=lulc2
lulcf=ratify(lulcf)
levels(lulcf)
table(as.matrix(lulcf))
data(worldgrids_pal)  #load palette
IGBP=data.frame(ID=0:16,col=worldgrids_pal$IGBP[-c(18,19)],
  lulc_levels2=c("Water","Forest","Forest","Forest","Forest","Forest","Shrublands","Shrublands","Savannas","Savannas","Grasslands","Permanent wetlands","Croplands","Urban and built-up","Cropland/Natural vegetation mosaic","Snow and ice","Barren or sparsely vegetated"),stringsAsFactors=F)
IGBP$class=rownames(IGBP);rownames(IGBP)=1:nrow(IGBP)
levels(lulcf)=list(IGBP)


### load WORLDCLIM elevation 
dem=raster("../../tiles/h11v08/dem_h11v08.tif",format="GTiff")
projection(dem)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

dif=v6-v5
names(dif)=month.name

difm=v6m-v5m
v5v6compare=stack(v5m,v6m,difm)
names(v5v6compare)=c("Collection 5","Collection 6","Difference (C6-C5)")

tile=extent(v6)

### compare differences between v5 and v6 by landcover type
lulcm=as.matrix(lulc)
forest=lulcm>=1&lulcm<=5


#####################################
### compare MOD43 and MOD17 products

## MOD17
mod17=raster("../MOD17/Npp_1km_C5.1_mean_00_to_06.tif",format="GTiff")
NAvalue(mod17)=32767
mod17=crop(projectRaster(mod17,v6,method="bilinear"),v6)

mod17qc=raster("../MOD17/Npp_QC_1km_C5.1_mean_00_to_06.tif",format="GTiff")
mod17qc=crop(projectRaster(mod17qc,v6,method="bilinear"),v6)
mod17qc[mod17qc<0|mod17qc>100]=NA

## MOD43 via earth engine
mod43=raster("../mod43/3b21aa90cc657523ff31e9559f36fb12.EVI_MEAN.tif",format="GTiff")
mod43=crop(projectRaster(mod43,v6,method="bilinear"),v6)

mod43qc=raster("../mod43/3b21aa90cc657523ff31e9559f36fb12.Percent_Cloudy.tif",format="GTiff")
mod43qc=crop(projectRaster(mod43qc,v6,method="bilinear"),v6)
mod43qc[mod43qc<0|mod43qc>100]=NA

## Summary plot of mod17 and mod43
modprod=stack(mod17/cellStats(mod17,max)*100,mod17qc,mod43,mod43qc)
names(modprod)=c("MOD17","MOD17qc","MOD43","MOD43qc")


###

n=100
at=seq(0,100,len=n)
cols=grey(seq(0,1,len=n))
cols=rainbow(n)
bgyr=colorRampPalette(c("blue","green","yellow","red"))
cols=bgyr(n)

#levelplot(lulcf,margin=F,layers="LULC")

m=3
mcompare=stack(subset(v5,m),subset(v6,m))

mdiff=subset(v5,m)-subset(v6,m)
names(mcompare)=c("Collection_5","Collection_6")
names(mdiff)=c("Collection_5-Collection_6")


CairoPDF("output/mod35compare.pdf",width=11,height=8.5)
#CairoPNG("output/mod35compare_%d.png",units="in", width=11,height=8.5,pointsize=4000,dpi=1200,antialias="subpixel")

### LANDCOVER
levelplot(lulcf,col.regions=levels(lulcf)[[1]]$col,colorkey=list(space="right",at=0:16,labels=list(at=seq(0.5,16.5,by=1),labels=levels(lulcf)[[1]]$class,cex=2)),margin=F)


levelplot(mcompare,col.regions=cols,at=at,margin=F,sub="Frequency of MOD35 Clouds in March")
#levelplot(dif,col.regions=bgyr(20),margin=F)
levelplot(mdiff,col.regions=bgyr(100),at=seq(mdiff@data@min,mdiff@data@max,len=100),margin=F)


boxplot(as.matrix(subset(dif,subset=1))~forest,varwidth=T,notch=T);abline(h=0)


levelplot(modprod,main="Missing Data (%) in MOD17 (NPP) and MOD43 (BRDF Reflectance)",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at)




levelplot(modprod,main="Missing Data (%) in MOD17 (NPP) and MOD43 (BRDF Reflectance)",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          xlim=c(-7300000,-6670000),ylim=c(0,600000))

levelplot(v5m,main="Missing Data (%) in MOD17 (NPP) and MOD43 (BRDF Reflectance)",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          xlim=c(-7200000,-6670000),ylim=c(0,400000),margin=F)


levelplot(subset(v5v6compare,1:2),main="Proportion Cloudy Days (%) in Collection 5 and 6 MOD35",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          margin=F)

levelplot(subset(v5v6compare,1:2),main="Proportion Cloudy Days (%) in Collection 5 and 6 MOD35",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          xlim=c(-7200000,-6670000),ylim=c(0,400000),margin=F)

levelplot(subset(v5v6compare,1:2),main="Proportion Cloudy Days (%) in Collection 5 and 6 MOD35",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          xlim=c(-7500000,-7200000),ylim=c(700000,1000000),margin=F)


dev.off()

### smoothing plots
## explore smoothed version
td=subset(v6,m)
## build weight matrix
s=3
w=matrix(1/(s*s),nrow=s,ncol=s)
#w[s-1,s-1]=4/12; w
td2=focal(td,w=w)
td3=stack(td,td2)

levelplot(td3,col.regions=cols,at=at,margin=F)

dev.off()
plot(stack(difm,lulc))

### ROI
tile_ll=projectExtent(v6, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

62,59
0,3



#### export KML timeseries
library(plotKML)
tile="h11v08"
file=paste("summary/MOD35_",tile,".nc",sep="")
system(paste("gdalwarp -overwrite -multi -ot INT16 -r cubicspline -srcnodata 255 -dstnodata 255 -s_srs '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs' -t_srs 'EPSG:4326' NETCDF:",file,":PCloud  MOD35_",tile,".tif",sep=""))

v6sp=brick(paste("MOD35_",tile,".tif",sep=""))
v6sp=readAll(v6sp)

## wasn't working with line below, perhaps Z should just be text? not date?
v6sp=setZ(v6sp,as.Date(paste("2011-",1:12,"-15",sep="")))
names(v6sp)=month.name

kml_open("output/mod35.kml")


kml_layer.RasterBrick(v6sp,
     plot.legend = TRUE, dtime = "", tz = "GMT",
    z.lim = c(0,100),colour_scale = get("colour_scale_numeric", envir = plotKML.opts))
#    home_url = get("home_url", envir = plotKML.opts),
#    metadata = NULL, html.table = NULL,
#    altitudeMode = "clampToGround", balloon = FALSE,
)

logo = "http://static.tumblr.com/t0afs9f/KWTm94tpm/yale_logo.png"
kml_screen(image.file = logo, position = "UL", sname = "YALE logo",size=c(.1,.1))
kml_close("mod35.kml")
kml_compress("mod35.kml",files=c(paste(month.name,".png",sep=""),"obj_legend.png"),zip="/usr/bin/zip")

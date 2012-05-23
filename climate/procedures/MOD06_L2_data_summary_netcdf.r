###################################################################################
###  R code to aquire and process MOD06_L2 cloud data from the MODIS platform


## connect to server of choice
#system("ssh litoria")
#R

library(sp)
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

X11.options(type="Xlib")
ncores=20  #number of threads to use

setwd("/home/adamw/personal/projects/interp")
setwd("/home/adamw/acrobates/projects/interp")

roi=readOGR("data/regions/Test_sites/Oregon.shp","Oregon")
roi=spTransform(roi,CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))


### Get processed MOD06 nc files
datadir="data/modis/MOD06_nc"

fs=data.frame(
  path=list.files(datadir,full=T,recursive=T,pattern="nc$"),
  file=basename(list.files(datadir,full=F,recursive=T,pattern="nc$")))
fs$date=as.Date(substr(fs$file,11,17),"%Y%j")
fs$time=substr(fs$file,19,22)
fs$datetime=as.POSIXct(strptime(paste(substr(fs$file,11,17),substr(fs$file,19,22)), '%Y%j %H%M'))
fs$path=as.character(fs$path)
fs$file=as.character(fs$file)


### get station data
load("data/ghcn/roi_ghcn.Rdata")
load("data/allstations.Rdata")


d2=d[d$variable=="ppt"&d$date%in%fs$date,]
d2=d2[,-grep("variable",colnames(d2)),]
st2=st[st$id%in%d$id,]
d2[,c("lon","lat")]=coordinates(st2)[match(d2$id,st2$id),]

## generate list split by day to merge with MOD06 data
d2l=split(d2,d2$date)

ds=do.call(rbind.data.frame,mclapply(d2l,function(td){
  print(td$date[1])
  tfs=fs[fs$date==td$date[1],]
  ## make spatial object
  tdsp=td
  coordinates(tdsp)=c("lon","lat")
  projection(tdsp)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  tdsp=spTransform(tdsp, CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))

  td2=do.call(rbind.data.frame,lapply(1:nrow(tfs),function(i){
    tnc1=brick(
      raster(tfs$path[i],varname="Cloud_Water_Path"),
      raster(tfs$path[i],varname="Cloud_Effective_Radius"),
      raster(tfs$path[i],varname="Cloud_Optical_Thickness"))
    projection(tnc1)=CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0")
    td3=as.data.frame(extract(tnc1,tdsp))
    if(ncol(td3)==1) return(NULL)
    colnames(td3)=
      c("Cloud_Water_Path","Cloud_Effective_Radius","Cloud_Optical_Thickness")
      ## drop negative values (need to check why these exist)
      td3[td3<0]=NA
    td3[,c("date","id")]=td[,c("date","id")]
      return(td3)
    }))
      td2=melt(td2,id.vars=c("date","id"))
      td2=cast(td2,date+id~variable,fun=mean,na.rm=T)
      td2=merge(td,td2)
  td2$id=as.character(td2$id)
  td2$date=as.character(td2$date)
      return(td2)
}))

colnames(ds)[grep("value",colnames(ds))]="ppt"
ds$ppt=as.numeric(ds$ppt)
ds$Cloud_Water_Path=as.numeric(ds$Cloud_Water_Path)
ds$Cloud_Effective_Radius=as.numeric(ds$Cloud_Effective_Radius)
ds$Cloud_Optical_Thickness=as.numeric(ds$Cloud_Optical_Thickness)
ds$date=as.Date(ds$date)
ds$year=as.numeric(ds$year)
ds$lon=as.numeric(ds$lon)
ds$lat=as.numeric(ds$lat)
ds=ds[!is.na(ds$ppt),]

save(ds,file="data/modis/pointsummary.Rdata")

load("data/modis/pointsummary.Rdata")


dsl=melt(ds,id.vars=c("id","date","ppt","lon","lat"),measure.vars=  c("Cloud_Water_Path","Cloud_Effective_Radius","Cloud_Optical_Thickness"))

dsl=dsl[!is.nan(dsl$value),]


summary(lm(ppt~Cloud_Effective_Radius,data=ds))
summary(lm(ppt~Cloud_Water_Path,data=ds))
summary(lm(ppt~Cloud_Optical_Thickness,data=ds))
summary(lm(ppt~Cloud_Effective_Radius+Cloud_Water_Path+Cloud_Optical_Thickness,data=ds))


####
## mean annual precip
dp=d[d$variable=="ppt",]
dp$year=format(dp$date,"%Y")
dm=tapply(dp$value,list(id=dp$id,year=dp$year),sum,na.rm=T)
dms=apply(dm,1,mean,na.rm=T)
dms=data.frame(id=names(dms),ppt=dms/10)

dslm=tapply(dsl$value,list(id=dsl$id,variable=dsl$variable),mean,na.rm=T)
dslm=data.frame(id=rownames(dslm),dslm)

dms=merge(dms,dslm)
dmsl=melt(dms,id.vars=c("id","ppt"))

summary(lm(ppt~Cloud_Effective_Radius,data=dms))
summary(lm(ppt~Cloud_Water_Path,data=dms))
summary(lm(ppt~Cloud_Optical_Thickness,data=dms))
summary(lm(ppt~Cloud_Effective_Radius+Cloud_Water_Path+Cloud_Optical_Thickness,data=dms))


#### draw some plots
#pdf("output/MOD06_summary.pdf",width=11,height=8.5)
png("output/MOD06_summary_%d.png",width=1024,height=780)

 ## daily data
xyplot(value~ppt/10|variable,data=dsl,
       scales=list(relation="free"),type=c("p","r"),
       pch=16,cex=.5,layout=c(3,1))


densityplot(~value|variable,groups=cut(dsl$ppt,c(0,50,100,500)),data=dsl,auto.key=T,
            scales=list(relation="free"),plot.points=F)

## annual means

xyplot(value~ppt|variable,data=dmsl,
       scales=list(relation="free"),type=c("p","r"),pch=16,cex=0.5,layout=c(3,1),
       xlab="Mean Annual Precipitation (mm)",ylab="Mean value")

densityplot(~value|variable,groups=cut(dsl$ppt,c(0,50,100,500)),data=dmsl,auto.key=T,
            scales=list(relation="free"),plot.points=F)


## plot some swaths

nc1=raster(fs$path[3],varname="Cloud_Effective_Radius")
nc2=raster(fs$path[4],varname="Cloud_Effective_Radius")
nc3=raster(fs$path[5],varname="Cloud_Effective_Radius")

nc1[nc1<=0]=NA
nc2[nc2<=0]=NA
nc3[nc3<=0]=NA

plot(roi)
plot(nc3)

plot(nc1,add=T)
plot(nc2,add=T)


dev.off()



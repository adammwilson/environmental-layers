###################################################################################
###  R code to aquire and process MOD06_L2 cloud data from the MODIS platform


## connect to server of choice
#system("ssh litoria")
#R

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

X11.options(type="Xlib")
ncores=20  #number of threads to use

setwd("/home/adamw/personal/projects/interp")
setwd("/home/adamw/acrobates/projects/interp")

roi=readOGR("data/regions/Test_sites/Oregon.shp","Oregon")
roi=spTransform(roi,CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))
roil=as(roi,"SpatialLines")

summarydatadir="data/modis/MOD06_climatologies"



##########################
#### explore the data

## load data
months=seq(as.Date("2000-01-15"),as.Date("2000-12-15"),by="month")
cerfiles=list.files(summarydatadir,pattern="CER_mean_.*tif$",full=T); cerfiles
cer=brick(stack(cerfiles))
setZ(cer,months,name="time")
cer@z=list(months)
cer@zname="time"
layerNames(cer) <- as.character(format(months,"%b"))
#cer=projectRaster(from=cer,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",method="ngb")
### TODO: change to bilinear!

cotfiles=list.files(summarydatadir,pattern="COT_mean_.*tif$",full=T); cotfiles
cot=brick(stack(cotfiles))
setZ(cot,months,name="time")
cot@z=list(months)
cot@zname="time"
layerNames(cot) <- as.character(format(months,"%b"))
#cot=projectRaster(from=cot,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",method="ngb")
cotm=mean(cot,na.rm=T)
### TODO: change to bilinear!

### get station data
load("data/ghcn/roi_ghcn.Rdata")
load("data/allstations.Rdata")

d2=d[d$variable=="ppt",]
d2=d2[,-grep("variable",colnames(d2)),]
st2=st[st$id%in%d$id,]
st2=spTransform(st2,CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))
d2[,c("lon","lat")]=coordinates(st2)[match(d2$id,st2$id),]
d2$month=format(d2$date,"%m")

### extract MOD06 data for each station
stcer=extract(cer,st2)#;colnames(stcer)=paste("cer_mean_",1:12,sep="")
stcot=extract(cot,st2)#;colnames(stcot)=paste("cot_mean_",1:12,sep="")
mod06=cbind.data.frame(id=st2$id,lat=st2$lat,lon=st2$lon,stcer,stcot)
mod06l=melt(mod06,id.vars=c("id","lon","lat"))
mod06l[,c("variable","moment","month")]=do.call(rbind,strsplit(as.character(mod06l$variable),"_"))
mod06l=cast(mod06l,id+lon+lat+month~variable+moment,value="value")

### generate monthly means of station data
dc=cast(d2,id+lon+lat~month,value="value",fun=function(x) mean(x,na.rm=T)*30)
dcl=melt(dc,id.vars=c("id","lon","lat"),value="ppt")

## merge station data with mod06
mod06s=merge(dcl,mod06l)
mod06s$lvalue=log(mod06s$value+1)


###################################################################
###################################################################
### draw some plots

bgyr=colorRampPalette(c("blue","green","yellow","red"))

pdf("output/MOD06_summary.pdf",width=11,height=8.5)

# CER maps
title="Cloud Effective Radius (microns)"
at=quantile(as.matrix(cer),seq(0,1,len=100),na.rm=T)
p=levelplot(cer,xlab.top=title,at=at,col.regions=bgyr(length(at)))+layer(sp.lines(roil, lwd=1.2, col='black'))
print(p)
bwplot(cer,main=title,ylab="Cloud Effective Radius (microns)")

# COT maps
title="Cloud Optical Thickness (%)"
at=quantile(as.matrix(cot),seq(0,1,len=100),na.rm=T)
p=levelplot(cot,xlab.top=title,at=at,col.regions=bgyr(length(at)))+layer(sp.lines(roil, lwd=0.8, col='black'))
print(p)
bwplot(cot,xlab.top=title,ylab="Cloud Optical Thickness (%)")


### Comparison at station values
at=quantile(as.matrix(cotm),seq(0,1,len=100),na.rm=T)
p=levelplot(cotm, layers=1,at=at,col.regions=bgyr(length(at)),main="Mean Annual Cloud Optical Thickness",FUN.margin=function(x) 0)+
  layer(sp.lines(roil, lwd=1.2, col='black'))+layer(sp.points(st2, pch=16, col='black'))
print(p)

### monthly comparisons of variables
mod06sl=melt(mod06s,measure.vars=c("value","COT_mean","CER_mean"))
bwplot(value~month|variable,data=mod06sl,cex=.5,pch=16,col="black",scales=list(y=list(relation="free")),layout=c(1,3))
splom(mod06s[grep("CER|COT",colnames(mod06s))],cex=.2,pch=16)

xyplot(value~CER_mean,data=mod06s,cex=.5,pch=16,col="black",scales=list(y=list(log=T)),main="Comparison of monthly mean CER and precipitation",ylab="Precipitation (log axis)")
xyplot(value~COT_mean,data=mod06s,cex=.5,pch=16,col="black",scales=list(y=list(log=T)),main="Comparison of monthly mean COT and precipitation",ylab="Precipitation (log axis)")

xyplot(value~CER_mean|month,data=mod06s,cex=.5,pch=16,col="black",scales=list(log=T,relation="free"))
xyplot(value~COT_mean|month,data=mod06s,cex=.5,pch=16,col="black",scales=list(log=T,relation="free"))

#xyplot(value~CER_mean|month+cut(COT_mean,breaks=c(0,10,20)),data=mod06s,cex=.5,pch=16,col="black")#,scales=list(relation="fixed"))


summary(lm(lvalue~COT_mean*month,data=mod06s))


dev.off()












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



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
library(heR.Misc)

X11.options(type="Xlib")
ncores=20  #number of threads to use

setwd("/home/adamw/personal/projects/interp")
setwd("/home/adamw/acrobates/projects/interp")

roi=readOGR("data/regions/Test_sites/Oregon.shp","Oregon")
roi_geo=as(roi,"SpatialLines")
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

cldfiles=list.files(summarydatadir,pattern="CLD_mean_.*tif$",full=T); cldfiles
cld=brick(stack(cldfiles))
cld[cld==0]=NA
setZ(cld,months,name="time")
cld@z=list(months)
cld@zname="time"
layerNames(cld) <- as.character(format(months,"%b"))
#cot=projectRaster(from=cot,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",method="ngb")
cldm=mean(cld,na.rm=T)
### TODO: change to bilinear if reprojecting!

### load PRISM data for comparison
prism=brick("data/prism/prism_climate.nc",varname="ppt")
## project to sinusoidal
projection(prism)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
prism=projectRaster(prism,cer)
prism[prism<0]=NA  #for some reason NAvalue() wasn't working
setZ(prism,months,name="time")
prism@z=list(months)
prism@zname="time"
layerNames(prism) <- as.character(format(months,"%b"))

####  build a pixel by variable matrix
vars=c("cer","cld","cot","prism")
bd=melt(as.matrix(vars[1]))
colnames(bd)=c("cell","month",vars[1])
for(v in vars[-1]) {print(v); bd[,v]=melt(as.matrix(get(v)))$value}
bd=bd[!is.na(bd$cer)|is.na(bd$prism),]

## Summarize annual metrics for full rasters

### get all variables from all months
c01=brick(mclapply(vars,function(v) projectRaster(get(v),crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")))
layerNames(m01)=paste(vars,months,sep="_")

m01=brick(mclapply(vars,function(v) mean(get(v))))#mean(cer),mean(cld),mean(cot),mean(prism))
layerNames(m01)=vars
m01=projectRaster(from=m01,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
m01=crop(m01,extent(-125,-115,41,47))
### get station data, subset to stations in region, and transform to sinusoidal
load("data/ghcn/roi_ghcn.Rdata")
load("data/allstations.Rdata")

st2_sin=spTransform(st2,CRS(projection(cer)))

d2=d[d$variable=="ppt"&d$date>=as.Date("2000-01-01"),]
d2=d2[,-grep("variable",colnames(d2)),]
st2=st[st$id%in%d$id,]
#st2=spTransform(st2,CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"))
d2[,c("lon","lat")]=coordinates(st2)[match(d2$id,st2$id),]
d2$elev=st2$elev[match(d2$id,st2$id)]
d2$month=format(d2$date,"%m")
d2$value=d2$value/10 #convert to mm

### extract MOD06 data for each station
stcer=extract(cer,st2)#;colnames(stcer)=paste("cer_mean_",1:12,sep="")
stcot=extract(cot,st2)#;colnames(stcot)=paste("cot_mean_",1:12,sep="")
stcld=extract(cld,st2)#;colnames(stcld)=paste("cld_mean_",1:12,sep="")
mod06=cbind.data.frame(id=st2$id,lat=st2$lat,lon=st2$lon,stcer,stcot,stcld)
mod06l=melt(mod06,id.vars=c("id","lon","lat"))
mod06l[,c("variable","moment","month")]=do.call(rbind,strsplit(as.character(mod06l$variable),"_"))
mod06l=as.data.frame(cast(mod06l,id+lon+lat+month~variable+moment,value="value"))

### Identify stations that have < 10 years of data
cnts=cast(d2,id~.,fun=function(x) length(x[!is.na(x)]),value="count");colnames(cnts)[colnames(cnts)=="(all)"]="count"
summary(cnts)
## drop them
d2=d2[d2$id%in%cnts$id[cnts$count>=365*10],]


### generate monthly means of station data
dc=cast(d2,id+lon+lat+elev~month,value="value",fun=function(x) mean(x,na.rm=T)*30)
dcl=melt(dc,id.vars=c("id","lon","lat","elev"),value="ppt")
colnames(dcl)[colnames(dcl)=="value"]="ppt"



## merge station data with mod06
mod06s=merge(dcl,mod06l)


### draw some plots
gq=function(x,n=10,cut=F) {
  if(!cut) return(unique(quantile(x,seq(0,1,len=n+1),na.rm=T)))
  if(cut)  return(cut(x,unique(quantile(x,seq(0,1,len=n+1),na.rm=T))))
}

### add some additional variables
mod06s$month=factor(mod06s$month,labels=format(as.Date(paste("2000",1:12,"15",sep="-")),"%b"))
mod06s$lppt=log(mod06s$ppt)
mod06s$glon=cut(mod06s$lon,gq(mod06s$lon,n=5),include.lowest=T,ordered=T)#gq(mod06s$lon,n=3))
mod06s$glon2=cut(mod06s$lon,breaks=c(-125,-122,-115),labels=c("Coastal","Inland"),include.lowest=T,ordered=T)#gq(mod06s$lon,n=3))
mod06s$gelev=cut(mod06s$elev,breaks=gq(mod06s$elev,n=3),labels=c("Low","Mid","High"),include.lowest=T,ordered=T)
mod06s$gbin=factor(paste(mod06s$gelev,mod06s$glon2,sep="_"),levels=c("Low_Coastal","Mid_Coastal","High_Coastal","Low_Inland","Mid_Inland","High_Inland"),ordered=T)


## melt it
mod06sl=melt(mod06s[,!grepl("lppt",colnames(mod06s))],id.vars=c("id","lon","lat","elev","month","ppt","glon","glon2","gelev","gbin"))
levels(mod06sl$variable)=c("Effective Radius (um)","Cloudy Days (%)","Optical Thickness (%)")

###################################################################
###################################################################

bgyr=colorRampPalette(c("blue","green","yellow","red"))

pdf("output/MOD06_summary.pdf",width=11,height=8.5)

# % cloudy maps
title="Cloudiness (% cloudy days) "
at=unique(quantile(as.matrix(cld),seq(0,1,len=100),na.rm=T))
p=levelplot(cld,xlab.top=title,at=at,col.regions=bgyr(length(at)))+layer(sp.lines(roil, lwd=1.2, col='black'))
print(p)
#bwplot(cer,main=title,ylab="Cloud Effective Radius (microns)")

# CER maps
title="Cloud Effective Radius (microns)"
at=quantile(as.matrix(cer),seq(0,1,len=100),na.rm=T)
p=levelplot(cer,xlab.top=title,at=at,col.regions=bgyr(length(at)))+layer(sp.lines(roil, lwd=1.2, col='black'))
print(p)
#bwplot(cer,main=title,ylab="Cloud Effective Radius (microns)")

# COT maps
title="Cloud Optical Thickness (%)"
at=quantile(as.matrix(cot),seq(0,1,len=100),na.rm=T)
p=levelplot(cot,xlab.top=title,at=at,col.regions=bgyr(length(at)))+layer(sp.lines(roil, lwd=0.8, col='black'))
print(p)
#bwplot(cot,xlab.top=title,ylab="Cloud Optical Thickness (%)")

###########################################
#### compare with PRISM data

at=quantile(as.matrix(subset(m01,subset=1)),seq(0,1,len=100),na.rm=T)
p1=levelplot(subset(m01,subset=1),xlab.top="Effective Radius (um)",at=at,col.regions=bgyr(length(at)),margin=F,
  )+layer(sp.lines(roi_geo, lwd=1.2, col='black'))
at=quantile(as.matrix(subset(m01,subset=2)),seq(0,1,len=100),na.rm=T)
p2=levelplot(subset(m01,subset=2),xlab.top="Cloudy days (%)",at=at,col.regions=bgyr(length(at)),margin=F,
   )+layer(sp.lines(roi_geo, lwd=1.2, col='black'))
at=quantile(as.matrix(subset(m01,subset=3)),seq(0,1,len=100),na.rm=T)
p3=levelplot(subset(m01,subset=3),xlab.top="Optical Thickness (%)",at=at,col.regions=bgyr(length(at)),margin=F,
   )+layer(sp.lines(roi_geo, lwd=1.2, col='black'))
at=quantile(as.matrix(subset(m01,subset=4)),seq(0,1,len=100),na.rm=T)
p4=levelplot(subset(m01,subset=4),xlab.top="PRISM MAP",at=at,col.regions=bgyr(length(at)),margin=F,
    )+layer(sp.lines(roi_geo, lwd=1.2, col='black'))

print(p1,split=c(1,1,2,2))
print(p2,split=c(1,2,2,2),new=F)
print(p3,split=c(2,1,2,2),new=F)
print(p4,split=c(2,2,2,2),new=F)

### compare COT and PRISM
print(p3,split=c(1,1,2,1))
print(p4,split=c(2,1,2,1),new=F)


### sample to speed up processing
s=sample(1:nrow(bd),10000)

## melt it to ease comparisons
bdl=melt(bd[s,],measure.vars=c("cer","cld","cot"))

combineLimits(useOuterStrips(xyplot(prism~value|variable+month,data=bdl,pch=16,cex=.2,scales=list(y=list(log=T),x=list(relation="free")),
                                    ylab="PRISM Monthly mean precipitation (mm)",xlab="MOD06 metric",main="PRISM vs. MOD06 (mean monthly ppt)")))+
  layer(panel.abline(lm(y~x),col="red"))+
  layer(panel.text(0,2.5,paste("R2=",round(summary(lm(y~x))$r.squared,2))))


### Comparison at station values
at=quantile(as.matrix(cotm),seq(0,1,len=100),na.rm=T)
p=levelplot(cotm, layers=1,at=at,col.regions=bgyr(length(at)),main="Mean Annual Cloud Optical Thickness",FUN.margin=function(x) 0)+
  layer(sp.lines(roil, lwd=1.2, col='black'))+layer(sp.points(st2_sin, pch=16, col='black'))
print(p)

### monthly comparisons of variables
mod06sl=melt(mod06s,measure.vars=c("value","COT_mean","CER_mean"))
bwplot(value~month|variable,data=mod06sl,cex=.5,pch=16,col="black",scales=list(y=list(relation="free")),layout=c(1,3))
splom(mod06s[grep("CER|COT|CLD",colnames(mod06s))],cex=.2,pch=16,main="Scatterplot matrix of MOD06 products")

### run some regressions
#plot(log(ppt)~COT_mean,data=mod06s)
#summary(lm(log(ppt)~COT_mean*month,data=mod06s))

## ppt~metric with longitude bins
 xyplot(ppt~value|variable,groups=glon,data=mod06sl,
       scales=list(y=list(log=T),x=list(relation="free")),
       par.settings = list(superpose.symbol = list(col=bgyr(5),pch=16,cex=.5)),auto.key=list(space="right",title="Station Longitude"),
       main="Comparison of MOD06_L2 and Precipitation Monthly Climatologies",ylab="Precipitation",xlab="MOD06_L2 Product",layout=c(3,1))+
  layer(panel.text(9,2.5,label="Coastal stations",srt=30,cex=1.3,col="blue"),columns=1)+
  layer(panel.text(13,.9,label="Inland stations",srt=10,cex=1.3,col="red"),columns=1)

## with elevation
# xyplot(ppt~value|variable,groups=gbin,data=mod06sl,
#       scales=list(y=list(log=T),x=list(relation="free")),
#       par.settings = list(superpose.symbol = list(col=c(rep("blue",3),rep("red",3)),pch=rep(c(3,4,8),2),cex=.5)),auto.key=list(space="right",title="Station Longitude"),
#       main="Comparison of MOD06_L2 and Precipitation Monthly Climatologies",ylab="Precipitation",xlab="MOD06_L2 Product",layout=c(3,1))+
#  layer(panel.text(9,2.5,label="Coastal stations",srt=30,cex=1.3,col="blue"),columns=1)+
#  layer(panel.text(13,.9,label="Inland stations",srt=10,cex=1.3,col="red"),columns=1)

## with elevation and longitude bins
combineLimits(useOuterStrips(xyplot(ppt~value|variable+gbin,data=mod06sl,
       scales=list(y=list(log=T),x=list(relation="free")),col="black",pch=16,cex=.5,type=c("p","r"),
       main="Comparison of MOD06_L2 and Precipitation Monthly Climatologies",ylab="Precipitation",xlab="MOD06_L2 Product")))+
  layer(panel.xyplot(x,y,type="r",col="red"))

## *** MOD06 vars vs precipitation by month, colored by longitude
combineLimits(useOuterStrips(xyplot(ppt~value|month+variable,groups=glon,data=mod06sl,cex=.5,pch=16,
                      scales=list(y=list(log=T),x=list(relation="free")),
                      par.settings = list(superpose.symbol = list(pch =16, col=bgyr(5),cex=1)),auto.key=list(space="top",title="Station Longitude"),
                      main="Comparison of MOD06_L2 and Precipitation Monthly Climatologies",ylab="Precipitation",xlab="MOD06_L2 Product")),
              margin.x=1,adjust.labels=F)+
  layer(panel.abline(lm(y~x),col="red"))+
  layer(panel.text(0,0,paste("R2=",round(summary(lm(y~x))$r.squared,2)),pos=4,cex=.5,col="grey"))


 xyplot(ppt~CLD_mean|id,data=mod06s,panel=function(x,y,group){
  panel.xyplot(x,y,type=c("r"),cex=.5,pch=16,col="red")
  panel.xyplot(x,y,type=c("p"),cex=.5,pch=16,col="black")
} ,scales=list(y=list(log=T)),strip=F,main="Monthly Mean Precipitation and % Cloudy by station",
        sub="Each panel is a station, each point is a monthly mean",
        ylab="Precipitation (mm, log axis)",xlab="% of Cloudy Days")+
  layer(panel.text(.5,.5,round(summary(lm(y~x))$r.squared,2),pos=4,cex=.75,col="grey"))

 xyplot(ppt~CER_mean|id,data=mod06s,panel=function(x,y,group){
  panel.xyplot(x,y,type=c("r"),cex=.5,pch=16,col="red")
  panel.xyplot(x,y,type=c("p"),cex=.5,pch=16,col="black")
} ,scales=list(y=list(log=T)),strip=F,main="Monthly Mean Precipitation and Cloud Effective Radius by station",sub="Each panel is a station, each point is a monthly mean",ylab="Precipitation (mm, log axis)",xlab="Mean Monthly Cloud Effective Radius (mm)")+
  layer(panel.text(10,.5,round(summary(lm(y~x))$r.squared,2),pos=4,cex=.75,col="grey"))

 xyplot(ppt~COT_mean|id,data=mod06s,panel=function(x,y,group){
  panel.xyplot(x,y,type=c("r"),cex=.5,pch=16,col="red")
  panel.xyplot(x,y,type=c("p"),cex=.5,pch=16,col="black")
} ,scales=list(y=list(log=T)),strip=F,main="Monthly Mean Precipitation and Cloud Optical Thickness by station",
        sub="Each panel is a station, each point is a monthly mean \n Number in lower right of each panel is R^2",
        ylab="Precipitation (mm, log axis)",xlab="Mean Monthly Cloud Optical Thickness (%)")+
  layer(panel.text(10,.5,round(summary(lm(y~x))$r.squared,2),pos=4,cex=.75,col="grey"))


### Calculate the slope of each line
mod06s.sl=dapply(mod06s,list(id=mod06s$id),function(x){
  lm1=lm(log(x$ppt)~x$CER_mean,)
  data.frame(lat=x$lat[1],lon=x$lon[1],elev=x$elev[1],intcpt=coefficients(lm1)[1],cer=coefficients(lm1)[2],r2=summary(lm1)$r.squared)
})
mod06s.sl$cex=gq(mod06s.sl$r2,n=5,cut=T)
mod06s.sl$cer.s=gq(mod06s.sl$cer,n=5,cut=T)

###  and plot it on a map
xyplot(lat~lon,group=cer.s,data=mod06s.sl,par.settings = list(superpose.symbol = list(pch =16, col=bgyr(5),cex=1)),auto.key=list(space="right",title="Slope Coefficient"),asp=1,
       main="Slopes of linear regressions {log(ppt)~CloudEffectiveRadius}")+
  layer(sp.lines(roi_geo, lwd=1.2, col='black'))

### look for relationships with longitude
xyplot(cer~lon,group=cut(mod06s.sl$elev,gq(mod06s.sl$elev,n=5)),data=mod06s.sl,
       par.settings = list(superpose.symbol = list(col=bgyr(5),pch=16,cex=1)),auto.key=list(space="right",title="Station Elevation"),
       ylab="Slope of lm(ppt~EffectiveRadius)",xlab="Longitude",main="Precipitation~Effective Radius relationship by latitude")


############################################################
### simple regression to get spatial residuals
m="01"
mod06s2=mod06s#[mod06s$month==m,]

lm1=lm(log(ppt)~CER_mean*month*lon,data=mod06s2); summary(lm1)
mod06s2$pred=exp(predict(lm1,mod06s2))
mod06s2$resid=mod06s2$pred-mod06s2$ppt
mod06s2$residg=gq(mod06s2$resid,n=5,cut=T)
mod06s2$presid=mod06s2$resid/mod06s2$ppt

for(l in c(F,T)){
## all months
  xyplot(pred~ppt,groups=gelev,data=mod06s2,
       par.settings = list(superpose.symbol = list(col=bgyr(3),pch=16,cex=.75)),auto.key=list(space="right",title="Station Elevation"),
       scales=list(log=l),
       ylab="Predicted Mean Monthly Precipitation (mm)",xlab="Observed Mean Monthly Precipitation (mm)",main="Predicted vs. Observed for Simple Model",
       sub="Red line is y=x")+
  layer(panel.abline(0,1,col="red"))

## month by month
  print(xyplot(pred~ppt|month,groups=gelev,data=mod06s2,
       par.settings = list(superpose.symbol = list(col=bgyr(3),pch=16,cex=.75)),auto.key=list(space="right",title="Station Elevation"),
       scales=list(log=l),
       ylab="Predicted Mean Monthly Precipitation (mm)",xlab="Observed Mean Monthly Precipitation (mm)",main="Predicted vs. Observed for Simple Model",
       sub="Red line is y=x")+
  layer(panel.abline(0,1,col="red"))
)}

## residuals by month
xyplot(lat~lon|month,group=residg,data=mod06s2,
       par.settings = list(superpose.symbol = list(pch =16, col=bgyr(5),cex=.5)),
       auto.key=list(space="right",title="Residuals"),
       main="Spatial plot of monthly residuals")+
    layer(sp.lines(roi_geo, lwd=1.2, col='black'))


dev.off()


####################################
#### build table comparing various metrics
mods=data.frame(
  models=c(
    "log(ppt)~CER_mean",
    "log(ppt)~CLD_mean",
    "log(ppt)~COT_mean",
    "log(ppt)~CER_mean*month",
    "log(ppt)~CLD_mean*month",
    "log(ppt)~COT_mean*month",
    "log(ppt)~CER_mean*month*lon",
    "log(ppt)~CLD_mean*month*lon",
    "log(ppt)~COT_mean*month*lon",
    "ppt~CER_mean*month*lon",
    "ppt~CLD_mean*month*lon",
    "ppt~COT_mean*month*lon"),stringsAsFactors=F)
  
mods$r2=
  do.call(rbind,lapply(1:nrow(mods),function(i){
    lm1=lm(as.formula(mods$models[i]),data=mod06s2)
    summary(lm1)$r.squared}))

mods








load("data/modis/pointsummary.Rdata")


dsl=melt(ds,id.vars=c("id","date","ppt","lon","lat"),measure.vars=  c("Cloud_Water_Path","Cloud_Effective_Radius","Cloud_Optical_Thickness"))

dsl=dsl[!is.nan(dsl$value),]




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



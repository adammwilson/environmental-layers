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
library(car)

X11.options(type="Xlib")
ncores=20  #number of threads to use

setwd("/home/adamw/acrobates/projects/interp")

psin=CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

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


##########################
#### explore the data

months=seq(as.Date("2000-01-15"),as.Date("2000-12-15"),by="month")

getmod06<-function(variable){
  d=brick(list.files(dmod06,pattern=paste("MOD06_",tile,".nc",sep=""),full=T),varname=toupper(variable))
  projection(d)=psin
  setZ(d,format(as.Date(d@z$Date),"%m"),name="time")
#  d@z=list(months)
  layerNames(d) <- as.character(format(as.Date(d@z$Date),"%b"))
  return(d)
}

cer=getmod06("cer")
cld=getmod06("cld")
cot=getmod06("cot")

pcol=colorRampPalette(c("brown","red","yellow","darkgreen"))
#levelplot(cer,col.regions=pcol(20))

## load WorldClim data for comparison (download then uncompress)
#system("wget -P data/worldclim/ http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/prec_30s_bil.zip",wait=F)
#system("wget -P data/worldclim/ http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_30s_bil.zip",wait=F)

### load WORLDCLIM data for comparison
wc=stack(list.files("data/worldclim/prec_30s_bil/",pattern="bil$",full=T)[c(4:12,1:3)])
projection(wc)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
wc=crop(wc,roi_ll)
wc[wc==55537]=NA
wc=projectRaster(wc,cer)#crs=projection(psin))
setZ(wc,months,name="time")
wc@z=list(months)
layerNames(wc) <- as.character(format(months,"%b"))
writeRaster(wc,file=paste("data/tiles/",tile,"/worldclim_",tile,".tif",sep=""),format="GTiff")

### load WORLDCLIM elevation 
dem=raster(list.files("data/worldclim/alt_30s_bil/",pattern="bil$",full=T))
projection(dem)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
dem=crop(dem,roi_ll)
dem[dem>60000]=NA
dem=projectRaster(dem,cer)
writeRaster(dem,file=paste("data/tiles/",tile,"/dem_",tile,".tif",sep=""),format="GTiff")


### get station data, subset to stations in region, and transform to sinusoidal
dm=readOGR(paste("data/tiles/",tile,sep=""),paste("station_monthly_",tile,"_PRCP",sep=""))
xyplot(latitude~longitude|month,data=dm@data)
dm2=spTransform(dm,CRS(projection(cer)))
dm2@data[,c("x","y")]=coordinates(dm2)

### extract MOD06 data for each station
stcer=extract(cer,dm2,fun=mean);colnames(stcer)=paste("cer_mean_",as.numeric(format(as.Date(cer@z$Date),"%m")),sep="")
#stcer20=extract(cer20,st2)#;colnames(stcer)=paste("cer_mean_",1:12,sep="")
stcot=extract(cot,dm2);colnames(stcot)=paste("cot_mean_",as.numeric(format(as.Date(cot@z$Date),"%m")),sep="")
stcld=extract(cld,dm2);colnames(stcld)=paste("cld_mean_",as.numeric(format(as.Date(cld@z$Date),"%m")),sep="")
stdem=extract(dem,dm2)
mod06=cbind.data.frame(station=dm$station,stcer,stcot,stcld)
mod06l=melt(mod06,id.vars=c("station"));colnames(mod06l)[grep("value",colnames(mod06l))]="mod06"
mod06l[,c("variable","moment","month")]=do.call(rbind,strsplit(as.character(mod06l$variable),"_"))
mod06l=unique(mod06l)
mod06l=cast(mod06l,station+moment+month~variable,value="mod06")
mod06l=merge(dm2@data,mod06l,by=c("station","month"))
mod06l=mod06l[!is.na(mod06l$cer),]

                                        #mod06l=melt(mod06,id.vars=c("station","longitude","latitude","elevation","month","count","value"))
#mod06l[,c("variable","moment","month2")]=do.call(rbind,strsplit(as.character(mod06l$variable),"_"))
#mod06l=as.data.frame(cast(mod06l,station+longitude+latitude+month~variable,value="value.1"))
#mod06l=mod06l[mod06l$month==mod06l$month2&!is.na(mod06l$value.1),]
mod06l=mod06l[order(mod06l$month),]

xyplot(value~cer|month,data=mod06l,scales=list(relation="free"),pch=16,cex=.5)
xyplot(value~cer|station,data=mod06l[mod06l$count>400,],pch=16,cex=.5)

xyplot(cot~month,groups=station,data=mod06l,type="l")


## explore fit of simple model
m=11
cor(mod06l[mod06l$month==m,c("value","cer","cld","cot")])
lm1=lm(value~latitude+longitude+elevation+cer+cld+cot,data=mod06l[mod06l$month==m,])
summary(lm1)
crPlots(lm1)

plot(mod06l$value[mod06l$month==m],as.vector(predict(lm1,data=mod06l[mod06l$month==m,])))

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
mod06s$LWP_mean=(2/3)*mod06s$CER_mean*mod06s$COT_mean

## melt it
mod06sl=melt(mod06s[,!grepl("lppt",colnames(mod06s))],id.vars=c("id","lon","lat","elev","month","ppt","glon","glon2","gelev","gbin"))
levels(mod06sl$variable)=c("Effective Radius (um)","Very Cloudy Days (%)","Cloudy Days (%)","Optical Thickness (%)","Liquid Water Path")

###################################################################
###################################################################

bgyr=colorRampPalette(c("blue","green","yellow","red"))

X11.options(type="cairo")
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
#mod06sl=melt(mod06s,measure.vars=c("ppt","COT_mean","CER_mean","CER_P20um"))
#bwplot(value~month|variable,data=mod06sl,cex=.5,pch=16,col="black",scales=list(y=list(relation="free")),layout=c(1,3))
#splom(mod06s[grep("CER|COT|CLD|ppt",colnames(mod06s))],cex=.2,pch=16,main="Scatterplot matrix of MOD06 products")

### run some regressions
#plot(log(ppt)~COT_mean,data=mod06s)
#summary(lm(log(ppt)~COT_mean*month,data=mod06s))

## ppt~metric with longitude bins
 xyplot(ppt~value|variable,groups=glon,data=mod06sl,
       scales=list(y=list(log=T),x=list(relation="free",log=F)),
       par.settings = list(superpose.symbol = list(col=bgyr(5),pch=16,cex=.5)),auto.key=list(space="top",title="Station Longitude"),
       main="Comparison of MOD06_L2 and Precipitation Monthly Climatologies",ylab="Mean Monthly Station Precipitation (mm)",xlab="MOD06_L2 Product",layout=c(5,1))+
  layer(panel.text(9,2.5,label="Coastal stations",srt=30,cex=1.3,col="blue"),columns=1)+
  layer(panel.text(13,.9,label="Inland stations",srt=10,cex=1.3,col="red"),columns=1)+
  layer(panel.abline(lm(y~x),col="red"))+
  layer(panel.text(0,0,paste("R2=",round(summary(lm(y~x))$r.squared,2)),pos=4,cex=.5,col="grey"))


## ppt~metric with longitude bins
#CairoPNG("output/COT.png",width=10,height=5,units="in",dpi=300,pointsize=20)
#png("output/COT.png",width=10,height=5,units="in",res=150)
#trellis.par.set("fontsize",12)
at=quantile(as.matrix(subset(m01,subset=3)),seq(0,1,len=100),na.rm=T)
p1=levelplot(subset(m01,subset=3),xlab.top="Optical Thickness (%)",at=at,col.regions=bgyr(length(at)),margin=F,
   )+layer(sp.lines(roi_geo, lwd=1.2, col='black'))+layer(sp.points(st2, cex=.5,col='black'))
at=quantile(as.matrix(subset(m01,subset=3)),seq(0,1,len=100),na.rm=T)

at=quantile(as.matrix(subset(m01,subset=4)),seq(0,1,len=100),na.rm=T)
p2=levelplot(subset(m01,subset=4),xlab.top="PRISM MAP",at=at,col.regions=bgyr(length(at)),margin=F,
    )+layer(sp.lines(roi_geo, lwd=1.2, col='black'))+layer(sp.points(st2, cex=.5, col='black'))

p3=xyplot(ppt~value,groups=glon,data=mod06sl[mod06sl$variable=="Optical Thickness (%)",],
       scales=list(y=list(log=T),x=list(relation="free",log=F)),
       par.settings = list(superpose.symbol = list(col=bgyr(5),pch=16,cex=.3)),auto.key=list(space="right",title="Station \n Longitude"),
ylab="Mean Monthly Station Precipitation (mm)",xlab="Cloud Optical Thickness from MOD06_L2 (%)",layout=c(1,1))+
  layer(panel.text(9,2.6,label="Coastal stations",srt=10,cex=1.3,col="blue"),columns=1)+
  layer(panel.text(13,.95,label="Inland stations",srt=10,cex=1.3,col="red"),columns=1)

p4=xyplot(ppt~value,groups=glon,data=mod06sl[mod06sl$variable=="Very Cloudy Days (%)",],
       scales=list(y=list(log=T),x=list(relation="free",log=F)),
       par.settings = list(superpose.symbol = list(col=bgyr(5),pch=16,cex=.3)),auto.key=list(space="right",title="Station \n Longitude"),
ylab="Mean Monthly Station Precipitation (mm)",xlab="Proportion days with Cloud Effective Radius >20um from MOD06_L2 (%)",layout=c(1,1))+
  layer(panel.text(9,2.6,label="Coastal stations",srt=10,cex=1.3,col="blue"),columns=1)+
  layer(panel.text(13,.95,label="Inland stations",srt=10,cex=1.3,col="red"),columns=1)

#save(p1,p2,p3,file="plotdata.Rdata")
#load("plotdata.Rdata")

#CairoPDF("output/MOD06_Summaryfig.pdf",width=11,height=8.5)
print(p3,position=c(0,0,1,.5),save.object=F)
#print(p4,position=c(0,0,1,.5),save.object=F)
print(p1,split=c(1,1,2,2),new=F)
print(p2,split=c(2,1,2,2),new=F)
#dev.off()
#system("convert output/MOD06_Summaryfig.pdf output/MOD06_Summaryfig.png")
                                        #dev.off()

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

 xyplot(ppt~LWP_mean|id,data=mod06s,panel=function(x,y,group){
  panel.xyplot(x,y,type=c("r"),cex=.5,pch=16,col="red")
  panel.xyplot(x,y,type=c("p"),cex=.5,pch=16,col="black")
} ,scales=list(y=list(log=T)),strip=F,main="Monthly Mean Precipitation and Liquid Water Path by station",
        sub="Each panel is a station, each point is a monthly mean \n Number in lower right of each panel is R^2",
        ylab="Precipitation (mm, log axis)",xlab="Mean Monthly Liquid Water Path")+
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



### Figures and tables for MOD09 Cloud Manuscript

setwd("~/acrobates/adamw/projects/cloud/")


## libraries
library(rasterVis)
library(latticeExtra)
library(xtable)
library(reshape)
library(caTools)

## read in data
cld=read.csv("data/NDP026D/cld.csv")
cldm=read.csv("data/NDP026D/cldm.csv")
cldy=read.csv("data/NDP026D/cldy.csv")
clda=read.csv("data/NDP026D/clda.csv")
st=read.csv("data/NDP026D/stations.csv")


## add lulc factor information
require(plotKML); data(worldgrids_pal)  #load IGBP palette
IGBP=data.frame(ID=0:16,col=worldgrids_pal$IGBP[-c(18,19)],stringsAsFactors=F)
IGBP$class=rownames(IGBP);rownames(IGBP)=1:nrow(IGBP)

## month factors
cld$month2=factor(cld$month,labels=month.name)
cldm$month2=factor(cldm$month,labels=month.name)

coordinates(st)=c("lon","lat")
projection(st)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

##make spatial object
cldms=cldm
coordinates(cldms)=c("lon","lat")
projection(cldms)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

##make spatial object
cldys=cldy
coordinates(cldys)=c("lon","lat")
projection(cldys)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#### Evaluate MOD35 Cloud data
mod09=brick("data/cloud_daily.nc")
mod09c=brick("data/cloud_ymonmean.nc",varname="CF");names(mod09c)=month.name
mod09a=brick("data/cloud_mean.nc",varname="CF_annual")#;names(mod09c)=month.name

mod09min=raster("data/cloud_min.nc",varname="CFmin")
mod09max=raster("data/cloud_max.nc",varname="CFmax")
mod09sd=raster("data/cloud_std.nc",varname="CFsd")
#mod09mean=raster("data/mod09_clim_mac.nc")
#names(mod09d)=c("Mean","Minimum","Maximum","Standard Deviation")

#plot(mod09a,layers=1,margin=F,maxpixels=100)

## calculated differences
cldm$dif=cldm$mod09-cldm$cld
clda$dif=clda$mod09-clda$cld

## read in global coasts for nice plotting
library(maptools)

data(wrld_simpl)
coast <- unionSpatialPolygons(wrld_simpl, rep("land",nrow(wrld_simpl)), threshold=5)
coast=as(coast,"SpatialLines")


## Figures
n=100
  at=seq(0,100,length=n)
colr=colorRampPalette(c("black","green","red"))
cols=colr(n)


pdf("output/Figures.pdf",width=11,height=8.5)

## Figure 1: 4-panel summaries
#- Annual average
levelplot(mod09a,col.regions=colr(100),cuts=100,at=seq(0,100,len=100),colorkey=list(space="bottom",adj=1),
  margin=F,maxpixels=1e6,ylab="Latitude",xlab="Longitude",useRaster=T)+
  layer(sp.lines(coast,col="black"),under=F)
#- Monthly minimum
#- Monthly maximum
#- STDEV or Min-Max
p_mac=levelplot(mac,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.75),xlab="",ylab="",main=names(regs)[r],useRaster=T)
p_min=levelplot(mod09min,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.75),useRaster=T)
p_max=levelplot(mod09max,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.75),useRaster=T)
p_sd=levelplot(mod09sd,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.75),useRaster=T)
p3=c("Mean Cloud Frequency (%)"=p_mac,"Max Cloud Frequency (%)"=p_max,"Min Cloud Frequency (%)"=p_min,"Cloud Frequency Variability (SD)"=p_sd,x.same=T,y.same=T,merge.legends=T,layout=c(2,2))
print(p3)


### maps of mod09 and NDP
## map of stations
p_mac=levelplot(mod09a,col.regions=colr(100),cuts=100,at=seq(0,100,len=100),colorkey=list(space="bottom",adj=1),
  margin=F,maxpixels=1e6,ylab="Latitude",xlab="Longitude",useRaster=T)+
  layer(panel.xyplot(lon,lat,pch=16,cex=.3,col="black"),data=data.frame(coordinates(st)))+
  layer(sp.lines(coast,col="black"),under=F)

p_mace=xyplot(lat~dif,data=cldm[cldm$lat>-60,],panel=function(x,y,subscripts=T){
  x2=x[order(y)]
  y2=y[order(y)]
  win=8000
  Q50=runquantile(x2,win,probs=c(.25,.5,.75))
  ## polygon
  panel.polygon(c(Q50[,1],rev(Q50[,3])),c(y2,rev(y2)),type="l",col=grey(.8))
### hist
  n=150
  xbins=seq(-70,50,len=n)
  ybins=seq(-60,90,len=n)
  tb=melt(as.matrix(table(
    x=cut(x,xbins,labels=xbins[-1]),
    y=cut(y,ybins,labels=ybins[-1]))))
  qat=unique(tb$value)
  print(qat)
  panel.levelplot(tb$x,tb$y,tb$value,at=qat,col.regions=c("transparent",hmcols(length(qat))),subscripts=1:nrow(tb))
###
#  panel.xyplot(x,y,pch=16,cex=.1,col=)
#  cuts=cut(y,lats,labels=lats[-10])
#  qs=do.call(rbind,tapply(x,cuts,quantile,c(.25,.50,.75),na.rm=T))
  colnames(qs)=c("Q25","Q50","Q75")
  panel.lines(Q50[,1],y2,type="l",col=grey(.5))
  panel.lines(Q50[,2],y2,type="l",col="black")
  panel.lines(Q50[,3],y2,type="l",col=grey(.5))
},asp=1,xlab="Difference (MOD09-Observed)")+layer(panel.abline(v=0,lty="dashed",col="red"))

print(p_mac,position=c(0,0,.75,1),more=T)
print(p_mace,position=c(0.75,0,1,1))


p1=c("MODIS Cloud Frequency and NDP-026D Validation Stations"=p_mac,"Difference (MOD09-NDP026D)"=p_mace,x.same=F,y.same=T,layout=c(2,1))
resizePanels(p1,w=c(.75,.25))

quantile(cldm$dif,seq(0,1,len=6),na.rm=T)
at=c(-70,-50,-25,-10,-5,0,5,10,25,50,70)
bwr=colorRampPalette(c("blue","grey","red"))
xyplot(lat~lon|month2,data=cldm,groups=cut(cldm$dif,at),
       par.settings=list(superpose.symbol=list(col=bwr(length(at)-1))),pch=16,cex=.25,
       auto.key=list(space="right",title="Difference\n(MOD09-NDP026D)",cex.title=1),asp=1,
       main="NDP-026D Cloud Climatology Stations",ylab="Latitude",xlab="Longitude")+
  layer(sp.lines(coast,col="black",lwd=.1),under=F)



### heatmap of mod09 vs. NDP for all months
hmcols=colorRampPalette(c("grey","blue","red"))
#hmcols=colorRampPalette(c(grey(.8),grey(.3),grey(.2)))
tr=c(0,120)
colkey <- draw.colorkey(list(col = hmcols(tr[2]), at = tr[1]:tr[2],height=.25))

xyplot(cld~mod09,data=cld[cld$Nobs>10,],panel=function(x,y,subscripts){
  n=150
  bins=seq(0,100,len=n)
  tb=melt(as.matrix(table(
    x=cut(x,bins,labels=bins[-1]),
    y=cut(y,bins,labels=bins[-1]))))
  qat=tr[1]:tr[2]#unique(tb$value)
  print(qat)
  panel.levelplot(tb$x,tb$y,tb$value,at=qat,col.regions=c("transparent",hmcols(length(qat))),subscripts=subscripts)
  },asp=1,scales=list(at=seq(0,100,len=6)),ylab="NDP Mean Cloud Amount (%)",xlab="MOD09 Cloud Frequency (%)",
       legend= list(right = list(fun = colkey,title="Station Count")))+
  layer(panel.abline(0,1,col="black",lwd=2))
#  layer(panel.ablineq(lm(y ~ x), r.sq = TRUE,at = 0.6,pos=1, offset=22,digits=2,col="blue"), style = 1)


xyplot(cld~mod09|month2,data=cld[cld$Nobs>50,],panel=function(x,y,subscripts){
  n=50
  bins=seq(0,100,len=n)
  tb=melt(as.matrix(table(
    x=cut(x,bins,labels=bins[-1]),
    y=cut(y,bins,labels=bins[-1]))))
  qat=unique(tb$value)
  qat=0:78
  qat=tr[1]:tr[2]#unique(tb$value)
  panel.levelplot(tb$x,tb$y,tb$value,at=qat,col.regions=c("transparent",hmcols(length(qat))),subscripts=1:nrow(tb))
  panel.abline(0,1,col="black",lwd=2)
#  panel.ablineq(lm(y ~ x), r.sq = TRUE,at = 0.6,pos=1, offset=0,digits=2,col="blue")
  panel.text(70,10,bquote(paste(R^2,"=",.(round(summary(lm(y ~ x))$r.squared,2)))),cex=1.2)
},asp=1,scales=list(at=seq(0,100,len=6),useRaster=T,colorkey=list(width=.5,title="Number of Stations")),
          ylab="NDP Mean Cloud Amount (%)",xlab="MOD09 Cloud Frequency (%)",
              legend= list(right = list(fun = colkey)))+ layer(panel.abline(0,1,col="black",lwd=2))


## Monthly Climatologies
for(i in 1:2){
 p1=xyplot(cld~mod09|month2,data=cldm,cex=.2,pch=16,subscripts=T,ylab="NDP Mean Cloud Amount",xlab="MOD09 Cloud Frequency (%)")+
  layer(panel.lines(1:100,predict(lm(y~x),newdata=data.frame(x=1:100)),col="green"))+
  layer(panel.lines(1:100,predict(lm(y~x+I(x^2)),newdata=data.frame(x=1:100)),col="blue"))+
  layer(panel.abline(0,1,col="red"))
    if(i==2){
     p1=p1+layer(panel.segments(mod09[subscripts],cld[subscripts]-cldsd[subscripts],mod09[subscripts],cld[subscripts]+cldsd[subscripts],subscripts=subscripts,col="grey"),data=cldm,under=T,magicdots=T)
     p1=p1+layer(panel.segments(mod09[subscripts]-mod09sd[subscripts],cld[subscripts],mod09[subscripts]+mod09sd[subscripts],cld[subscripts],subscripts=subscripts,col="grey"),data=cldm,under=T,magicdots=T) 
       }
print(p1)
}

bwplot(lulcc~dif,data=cldm,horiz=T,xlab="Difference (MOD09-Observed)",varwidth=T,notch=T)+layer(panel.abline(v=0))


dev.off()


summary(lm(cld~mod09,data=cld))

## explore validation error
cldm$lulcc=as.factor(IGBP$class[match(cldm$lulc,IGBP$ID)])

## Table of RMSE's by lulc by month
lulcrmsel=ddply(cldm,c("month","lulc"),function(x) c(count=nrow(x),rmse=sqrt(mean((x$mod09-x$cld)^2,na.rm=T))))
lulcrmsel=lulcrmsel[!is.na(lulcrmsel$lulc),]
lulcrmsel$lulcc=as.factor(IGBP$class[match(lulcrmsel$lulc,IGBP$ID)])

lulcrmse=cast(lulcrmsel,lulcc~month,value="rmse")
lulcrmse
print(xtable(lulcrmse,digits=1),"html")
  
levelplot(rmse~lulc*month,data=lulcrmsel,col.regions=heat.colors(20))


### Linear models
summary(lm(dif~as.factor(lulc)+lat+month2,data=cldm))


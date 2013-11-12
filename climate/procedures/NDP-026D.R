#! /bin/R
### Script to download and process the NDP-026D station cloud dataset
setwd("~/acrobates/adamw/projects/interp/data/NDP026D")

library(multicore)
library(latticeExtra)
library(doMC)
library(rasterVis)
library(rgdal)
library(reshape)
library(hexbin)
## register parallel processing
registerDoMC(10)


## available here http://cdiac.ornl.gov/epubs/ndp/ndp026d/ndp026d.html

## Get station locations
system("wget -N -nd http://cdiac.ornl.gov/ftp/ndp026d/cat01/01_STID -P data/")
st=read.table("data/01_STID",skip=1)
colnames(st)=c("StaID","LAT","LON","ELEV","ny1","fy1","ly1","ny7","fy7","ly7","SDC","b5c")
st$lat=st$LAT/100
st$lon=st$LON/100
st$lon[st$lon>180]=st$lon[st$lon>180]-360
st=st[,c("StaID","ELEV","lat","lon")]
colnames(st)=c("id","elev","lat","lon")
write.csv(st,"stations.csv",row.names=F)

## download data
system("wget -N -nd ftp://cdiac.ornl.gov/pub/ndp026d/cat67_78/* -A '.tc.Z' -P data/")

system("gunzip data/*.Z")

## define FWF widths
f162=c(5,5,4,7,7,7,4) #format 162
c162=c("StaID","YR","Nobs","Amt","Fq","AWP","NC")

## use monthly timeseries
cld=do.call(rbind.data.frame,mclapply(sprintf("%02d",1:12),function(m) {
  d=read.fwf(list.files("data",pattern=paste("MNYDC.",m,".tc",sep=""),full=T),skip=1,widths=f162)
  colnames(d)=c162
  d$month=as.numeric(m)
  print(m)
  return(d)}
  ))

## add lat/lon
cld[,c("lat","lon")]=st[match(cld$StaID,st$StaID),c("lat","lon")]

## drop missing values
cld$Amt[cld$Amt<0]=NA
cld$Fq[cld$Fq<0]=NA
cld$AWP[cld$AWP<0]=NA
cld$NC[cld$NC<0]=NA
cld=cld[cld$Nobs>0,]

## calculate means and sds
cldm=do.call(rbind.data.frame,by(cld,list(month=as.factor(cld$month),StaID=as.factor(cld$StaID)),function(x){
  data.frame(
             month=x$month[1],
             StaID=x$StaID[1],
             cld=mean(x$Amt[x$Nobs>10]/100,na.rm=T),
             cldsd=sd(x$Amt[x$Nobs>10]/100,na.rm=T))}))
cldm[,c("lat","lon")]=st[match(cldm$StaID,st$StaID),c("lat","lon")]

## means by year
cldy=do.call(rbind.data.frame,by(cld,list(year=as.factor(cld$YR),StaID=as.factor(cld$StaID)),function(x){
  data.frame(
             year=x$YR[1],
             StaID=x$StaID[1],
             cld=mean(x$Amt[x$Nobs>10]/100,na.rm=T),
             cldsd=sd(x$Amt[x$Nobs>10]/100,na.rm=T))}))
cldy[,c("lat","lon")]=st[match(cldy$StaID,st$StaID),c("lat","lon")]

## add the MOD09 data to cld
#### Evaluate MOD35 Cloud data
mod09=brick("~/acrobates/adamw/projects/cloud/data/mod09.nc")

## overlay the data with 5km radius buffer
mod09st=extract(mod09,st,buffer=5000,fun=mean,na.rm=T,df=T)
mod09st$id=st$id
mod09stl=melt(mod09st[,-1],id.vars="id")
mod09stl[,c("year","month")]=do.call(rbind,strsplit(sub("X","",mod09stl$variable),"[.]"))[,1:2]
## add it to cld
cld$mod09=mod09stl$value[match(paste(cld$StaID,cld$YR,cld$month),paste(mod09stl$id,mod09stl$year,as.numeric(mod09stl$month)))]

## write out the tables
write.csv(cld,file="cld.csv",row.names=F)
write.csv(cldy,file="cldy.csv")
write.csv(cldm,file="cldm.csv")

#########################################################################
##################
###
cld=read.csv("cld.csv")
cldm=read.csv("cldm.csv")
cldy=read.csv("cldy.csv")
st=read.csv("stations.csv")

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
mod09=brick("~/acrobates/adamw/projects/cloud/data/mod09.nc")


## LULC
#system(paste("gdalwarp -r near -co \"COMPRESS=LZW\" -tr ",paste(res(mod09),collapse=" ",sep=""),
#             "-tap -multi -t_srs \"",   projection(mod09),"\" /mnt/data/jetzlab/Data/environ/global/landcover/MODIS/MCD12Q1_IGBP_2005_v51.tif ../modis/mod12/MCD12Q1_IGBP_2005_v51.tif"))
lulc=raster("../modis/mod12/MCD12Q1_IGBP_2005_v51.tif")
#lulc=ratify(lulc)
require(plotKML); data(worldgrids_pal)  #load IGBP palette
IGBP=data.frame(ID=0:16,col=worldgrids_pal$IGBP[-c(18,19)],stringsAsFactors=F)
IGBP$class=rownames(IGBP);rownames(IGBP)=1:nrow(IGBP)
levels(lulc)=list(IGBP)
#lulc=crop(lulc,mod09)

n=100
at=seq(0,100,length=n)
colr=colorRampPalette(c("black","green","red"))
cols=colr(n)


hexbinplot(Amt/100~mod09,data=cld[cld$Nobs>100,])+
  layer(panel.abline(lm(y~x),col="blue"))+
  layer(panel.abline(0,1,col="red"))

xyplot(Amt/100~mod09,grpups="month",data=cld[cld$Nobs>75,],cex=.2,pch=16)+
  layer(panel.abline(lm(y~x),col="blue"))+
#  layer(panel.lines(x,predict(lm(y~x),type="prediction")))+
  layer(panel.abline(0,1,col="red"))

xyplot(Amt/100~mod09|month,data=cld[cld$Nobs>75,],cex=.2,pch=16)+
  layer(panel.abline(lm(y~x),col="blue"))+
#  layer(panel.lines(x,predict(lm(y~x),type="prediction")))+
  layer(panel.abline(0,1,col="red"))


d$lulc=unlist(extract(lulc,d))
d$lulc_10=unlist(extract(lulc,d,buffer=10000,fun=mode,na.rm=T))
d$lulc=factor(d$lulc,labels=IGBP$class)

save(d,file="annualsummary.Rdata")



load("annualsummary.Rdata")

## quick model to explore fit
xyplot(cld~mod35c5_10,groups=lulc,data=d@data)
summary(lm(cld~mod35c5_10+as.factor(lulc),data=d@data))
summary(lm(Amt~mod09,data=cld))
summary(lm(cld~mod09_10+as.factor(lulc),data=d))
summary(lm(cld~mod09_10+as.factor(lulc),data=d))

### exploratory plots
xyplot(cld~mod09_10,groups=lulc,data=d@data,pch=16,cex=.5)+layer(panel.abline(0,1,col="red"))
xyplot(cld~mod09_10+mod35c5_10|as.factor(lulc),data=d@data,type=c("p","r"),pch=16,cex=.25,auto.key=T)+layer(panel.abline(0,1,col="green"))
xyplot(cld~mod35_10|as.factor(lulc),data=d@data,pch=16,cex=.5)+layer(panel.abline(0,1,col="red"))
xyplot(mod35_10~mod09_10|as.factor(lulc),data=d@data,pch=16,cex=.5)+layer(panel.abline(0,1,col="red"))

densityplot(stack(mod35,mod09))
boxplot(mod35,lulc)

bwplot(mod09~mod35|cut(y,5),data=stack(mod09,mod35))

## month factors
cldm$month2=factor(cldm$month,labels=month.name)
## add a color key
breaks=seq(0,100,by=25)
cldm$cut=cut(cldm$cld,breaks)
cp=colorRampPalette(c("blue","orange","red"))
cols=cp(length(at))

## read in global coasts for nice plotting
library(maptools)

data(wrld_simpl)
coast <- unionSpatialPolygons(wrld_simpl, rep("land",nrow(wrld_simpl)), threshold=5)
coast=as(coast,"SpatialLines")
#coast=spTransform(coast,CRS(projection(mod35)))


## write a pdf
#dir.create("output")
pdf("output/NDP026d.pdf",width=11,height=8.5)

## map of stations
 xyplot(lat~lon,data=st,pch=16,cex=.5,col="black",auto.key=T,
       main="NDP-026D Cloud Climatology Stations",ylab="Latitude",xlab="Longitude")+
  layer(sp.lines(coast,col="grey"),under=T)

xyplot(lat~lon|month2,groups=cut,data=cldm,pch=".",cex=.2,auto.key=T,
       main="Mean Monthly Cloud Coverage",ylab="Latitude",xlab="Longitude",
        par.settings = list(superpose.symbol= list(pch=16,col=c("blue","green","yellow","red"))))+
  layer(sp.lines(coast,col="grey"),under=T)


## Validation
m=10
zlim=c(40,100)
dr=subset(mod35,subset=m);projection(dr)=projection(mod35)
ds=cldms[cldms$month==m,]
plot(dr,col=cp(100),zlim=zlim,main="Comparison of MOD35 Cloud Frequency and NDP-026D Station Cloud Climatologies",
     ylab="Northing (m)",xlab="Easting (m)",sub="MOD35 is proportion of cloudy days, while NDP-026D is Mean Cloud Coverage")
plot(ds,add=T,pch=21,cex=3,lwd=2,fg="black",bg=as.character(cut(ds$cld,breaks=seq(zlim[1],zlim[2],len=5),labels=cp(4))))
#legend("topright",legend=seq(zlim[1],zlim[2],len=5),pch=16,col=cp(length(breaks)))


xyplot(mod35~cld,data=mod35v,subscripts=T,auto.key=T,panel=function(x,y,subscripts){
   td=mod35v[subscripts,]
#   panel.segments(x-td$cldsd[subscripts],y,x+td$cldsd[subscripts],y,subscripts=subscripts)
   panel.xyplot(x,y,subscripts=subscripts,type=c("p","smooth"),pch=16,col="black")
#   panel.segments(x-td$cldsd[subscripts],y,x+td$cldsd[subscripts],y,subscripts=subscripts)
 },ylab="MOD35 Proportion Cloudy Days",xlab="NDP-026D Mean Monthly Cloud Amount",
        main="Comparison of MOD35 Cloud Mask and Station Cloud Climatologies")

#xyplot(mod35~cld|month,data=mod35v,subscripts=T,auto.key=T,panel=function(x,y,subscripts){
#   td=mod35v[subscripts,]
#   panel.segments(x-td$cldsd[subscripts],y,x+td$cldsd[subscripts],y,subscripts=subscripts)
#   panel.xyplot(x,y,subscripts=subscripts,type=c("p","smooth"),pch=16,col="black")
#   panel.segments(x-td$cldsd[subscripts],y,x+td$cldsd[subscripts],y,subscripts=subscripts)
# },ylab="MOD35 Proportion Cloudy Days",xlab="NDP-026D Mean Monthly Cloud Amount",
#        main="Comparison of MOD35 Cloud Mask and Station Cloud Climatologies")


dev.off()

graphics.off()

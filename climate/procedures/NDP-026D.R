#! /bin/R
### Script to download and process the NDP-026D station cloud dataset
setwd("~/acrobates/adamw/projects/interp/data/NDP026D")

library(multicore)
library(latticeExtra)
library(doMC)
library(rasterVis)
library(rgdal)
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

## download data
system("wget -N -nd ftp://cdiac.ornl.gov/pub/ndp026d/cat67_78/* -A '.tc.Z' -P data/")
system("gunzip data/*.Z")

## get monthly mean cloud amount MMCF
#system("wget -N -nd ftp://cdiac.ornl.gov/pub/ndp026d/cat08_09/* -A '.tc.Z' -P data/")
#system("gunzip data/*.Z")
#f121=c(6,6,6,7,6,7,6,2) #format 121
#c121=c("StaID","NobD","AvgDy","NobN","AvgNt","NobDN","AvgDN","Acode")
#cld=do.call(rbind.data.frame,lapply(sprintf("%02d",1:12),function(m) {
#  d=read.fwf(list.files("data",pattern=paste("MMCA.",m,".tc",sep=""),full=T),skip=1,widths=f162)
#  colnames(d)=c121
#  d$month=as.numeric(m)
#  return(d)}
#  ))

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


#cldm=foreach(m=unique(cld$month),.combine='rbind')%:%
#  foreach(s=unique(cld$StaID),.combine="rbind") %dopar% {
#    x=cld[cld$month==m&cld$StaID==s,]
#    data.frame(
#               month=x$month[1],
#               StaID=x$StaID[1],
#               Amt=mean(x$Amt[x$Nobs>10],na.rm=T)/100)}
 

## write out the tables
write.csv(cldy,file="cldy.csv")
write.csv(cldm,file="cldm.csv")

#########################################################################
##################
###
cldm=read.csv("cldm.csv")
cldy=read.csv("cldy.csv")


##make spatial object
cldms=cldm
coordinates(cldms)=c("lon","lat")
projection(cldms)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

##make spatial object
cldys=cldy
coordinates(cldys)=c("lon","lat")
projection(cldys)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#### Evaluate MOD35 Cloud data
mod35c6=brick("~/acrobates/adamw/projects/MOD35C5/data/MOD35C6_2009_new.tif")
#projection(mod35c6)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"


### use data from google earth engine
mod35c5=raster("../modis/mod09/global_2009/MOD35_2009.tif")
mod09=raster("../modis/mod09/global_2009/MOD09_2009.tif")

## LULC
#system(paste("gdalwarp -r near -co \"COMPRESS=LZW\" -tr ",paste(res(mod09),collapse=" ",sep=""),
#             "-tap -multi -t_srs \"",   projection(mod09),"\" /mnt/data/jetzlab/Data/environ/global/landcover/MODIS/MCD12Q1_IGBP_2005_v51.tif ../modis/mod12/MCD12Q1_IGBP_2005_v51.tif"))
lulc=raster("../modis/mod12/MCD12Q1_IGBP_2005_v51.tif")
lulc=ratify(lulc)
require(plotKML); data(worldgrids_pal)  #load IGBP palette
IGBP=data.frame(ID=0:16,col=worldgrids_pal$IGBP[-c(18,19)],stringsAsFactors=F)
IGBP$class=rownames(IGBP);rownames(IGBP)=1:nrow(IGBP)
levels(lulc)=list(IGBP)
lulc=crop(lulc,mod09)

n=100
at=seq(0,100,length=n)
colr=colorRampPalette(c("black","green","red"))
cols=colr(n)

#dif=mod35-mod09
#bwplot(dif~as.factor(lulc))

#levelplot(mod35,col.regions=cols,at=at,margins=F,maxpixels=1e6)#,xlim=c(-100,-50),ylim=c(0,10))
#levelplot(lulc,att="class",col.regions=levels(lulc)[[1]]$col,margin=F,maxpixels=1e6)

#cldys=spTransform(cldys,CRS(projection(mod35)))

#mod35v=foreach(m=unique(cldm$month),.combine="rbind") %do% {
#  dr=subset(mod35,subset=m);projection(dr)=projection(mod35)
#  dr2=subset(mod35sd,subset=m);projection(dr2)=projection(mod35)
#  ds=cldms[cldms$month==m,]
#  ds$mod35=unlist(extract(dr,ds,buffer=10,fun=mean,na.rm=T))
#  ds$mod35sd=extract(dr2,ds,buffer=10)
#  print(m)
#  return(ds@data[!is.na(ds$mod35),])}

y=2009
d=cldys[cldys$year==y,]

d$mod35c6_10=unlist(extract(mod35c6,d,buffer=10000,fun=mean,na.rm=T))
d$mod35c5_10=unlist(extract(mod35c5,d,buffer=10000,fun=mean,na.rm=T))
d$mod09_10=unlist(extract(mod09,d,buffer=10000,fun=mean,na.rm=T))
#d$dif=d$mod35_10-d$mod09_10
#d$dif2=d$mod35_10-d$cld

d$lulc=unlist(extract(lulc,d))
d$lulc_10=unlist(extract(lulc,d,buffer=10000,fun=mode,na.rm=T))
d$lulc=factor(d$lulc,labels=IGBP$class)

save(d,file="annualsummary.Rdata")

## quick model to explore fit
plot(cld~mod35,groups=lulc,data=d)
summary(lm(cld~mod35+as.factor(lulc),data=d))
summary(lm(cld~mod09_10,data=d))
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

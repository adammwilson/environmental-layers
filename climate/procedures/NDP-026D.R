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
#registerDoMC(10)
#beginCluster(10)

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
coordinates(st)=c("lon","lat")
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
cld[,c("lat","lon")]=st[match(cld$StaID,st$id),c("lat","lon")]

## drop missing values
cld=cld[,!grepl("Fq|AWP|NC",colnames(cld))]
cld$Amt[cld$Amt<0]=NA
#cld$Fq[cld$Fq<0]=NA
#cld$AWP[cld$AWP<0]=NA
#cld$NC[cld$NC<0]=NA
#cld=cld[cld$Nobs>0,]

## add the MOD09 data to cld
#### Evaluate MOD35 Cloud data
mod09=brick("~/acrobates/adamw/projects/cloud/data/mod09.nc")

## overlay the data with 32km diameter (16km radius) buffer
## buffer size from Dybbroe, et al. (2005) doi:10.1175/JAM-2189.1.
buf=16000
#mod09sta=lapply(1:nlayers(mod09),function(l) {print(l); extract(mod09[[l]],st,buffer=buf,fun=mean,na.rm=T,df=T)[,2]})
bins=cut(1:nrow(st),100)
mod09sta=lapply(levels(bins),function(lb) {
  l=which(bins==lb)
  td=extract(mod09,st[l,],buffer=buf,fun=mean,na.rm=T,df=T)
  td$id=st$id[l]
  print(lb)#as.vector(c(l,td[,1:4])))
  write.table(td,"valid.csv",append=T,col.names=F,quote=F,sep=",",row.names=F)
  td
})#,mc.cores=3)

#mod09sta=extract(mod09,st,buffer=buf,fun=mean,na.rm=T,df=T)
mod09st=read.csv("valid.csv",header=F)[,-c(1,2)]

#mod09st=do.call(rbind.data.frame,mod09sta)
#mod09st=mod09st[,!is.na(colnames(mod09st))]
colnames(mod09st)=c(names(mod09),"id")
#mod09st$id=st$id
mod09stl=melt(mod09st,id.vars="id")
mod09stl[,c("year","month")]=do.call(rbind,strsplit(sub("X","",mod09stl$variable),"[.]"))[,1:2]
## add it to cld
cld$mod09=mod09stl$value[match(paste(cld$StaID,cld$YR,cld$month),paste(mod09stl$id,mod09stl$year,as.numeric(mod09stl$month)))]


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
  Mode <- function(x) {
      ux <- na.omit(unique(x))
        ux[which.max(tabulate(match(x, ux)))]
      }
lulcst=extract(lulc,st,fun=Mode,buffer=buf,df=T)
colnames(lulcst)=c("id","lulc")
## add it to cld
cld$lulc=lulcst$lulc[match(cld$StaID,lulcst$id)]
#cld$lulc=factor(as.integer(cld$lulc),labels=IGBP$class[sort(unique(cld$lulc))])

## update cld column names
colnames(cld)[grep("Amt",colnames(cld))]="cld"
cld$cld=cld$cld/100

## calculate means and sds
cldm=do.call(rbind.data.frame,by(cld,list(month=as.factor(cld$month),StaID=as.factor(cld$StaID)),function(x){
  data.frame(
             month=x$month[1],
             lulc=x$lulc[1],
             StaID=x$StaID[1],
             mod09=mean(x$mod09,na.rm=T),
             mod09sd=sd(x$mod09,na.rm=T),
             cld=mean(x$cld[x$Nobs>10],na.rm=T),
             cldsd=sd(x$cld[x$Nobs>10],na.rm=T))}))
cldm[,c("lat","lon")]=coordinates(st)[match(cldm$StaID,st$id),c("lat","lon")]

## means by year
cldy=do.call(rbind.data.frame,by(cld,list(year=as.factor(cld$YR),StaID=as.factor(cld$StaID)),function(x){
  data.frame(
             year=x$YR[1],
             StaID=x$StaID[1],
             lulc=x$lulc[1],
             mod09=mean(x$mod09,na.rm=T),
             mod09sd=sd(x$mod09,na.rm=T),
             cld=mean(x$Amt[x$Nobs>10]/100,na.rm=T),
             cldsd=sd(x$Amt[x$Nobs>10]/100,na.rm=T))}))
cldy[,c("lat","lon")]=coordinates(st)[match(cldy$StaID,st$id),c("lat","lon")]

## overall mean
clda=do.call(rbind.data.frame,by(cld,list(StaID=as.factor(cld$StaID)),function(x){
  data.frame(
             StaID=x$StaID[1],
             lulc=x$lulc[1],
             mod09=mean(x$mod09,na.rm=T),
             mod09sd=sd(x$mod09,na.rm=T),
             cld=mean(x$cld[x$Nobs>10],na.rm=T),
             cldsd=sd(x$cld[x$Nobs>10],na.rm=T))}))
clda[,c("lat","lon")]=coordinates(st)[match(clda$StaID,st$id),c("lat","lon")]


## write out the tables
write.csv(cld,file="cld.csv",row.names=F)
write.csv(cldy,file="cldy.csv",row.names=F)
write.csv(cldm,file="cldm.csv",row.names=F)
write.csv(clda,file="clda.csv",row.names=F
)
#########################################################################
##################
###
cld=read.csv("cld.csv")
cldm=read.csv("cldm.csv")
cldy=read.csv("cldy.csv")
clda=read.csv("clda.csv")
st=read.csv("stations.csv")

### remove mod09==0 due to mosaic problem (remove when fixed)
cld=cld[!is.na(cld$lat)&cld$mod09!=0,]
cldm=cldm[!is.na(cldm$lat)&cldm$mod09!=0,]
cldy=cldy[!is.na(cldy$lat)&cldy$mod09!=0,]

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
mod09=brick("~/acrobates/adamw/projects/cloud/data/mod09.nc")
mod09c=brick("~/acrobates/adamw/projects/cloud/data/mod09_clim.nc",varname="CF");names(mod09c)=month.name
mod09c2=raster("~/acrobates/adamw/projects/cloud/data/mod09_clim.nc",varname="CF",nl=1)

### get monthly climatologies for each station 
#cldc=do.call(rbind.data.frame,by(cld,list(id=cld$StaID,month=cld$month),function(x){
#  x$mod09[x$mod09==0]=NA
#  data.frame(id=x$StaID[1],month=x$month[1],Nobs=sum(x$Nobs,na.rm=T),Amt=mean(x$Amt,na.rm=T),mod09=mean(x$mod09,na.rm=T))
#  }))

## read in global coasts for nice plotting
library(maptools)

data(wrld_simpl)
coast <- unionSpatialPolygons(wrld_simpl, rep("land",nrow(wrld_simpl)), threshold=5)
coast=as(coast,"SpatialLines")
#coast=spTransform(coast,CRS(projection(mod35)))


n=100
  at=seq(0,100,length=n)
colr=colorRampPalette(c("black","green","red"))
cols=colr(n)


pdf("/home/adamw/acrobates/adamw/projects/cloud/output/validation.pdf",width=11,height=8.5)

### maps of mod09 and NDP
## map of stations
xyplot(lat~lon,data=data.frame(coordinates(st)),pch=16,cex=.5, main="NDP-026D Cloud Climatology Stations",ylab="Latitude",xlab="Longitude")+
  layer(sp.lines(coast,col="grey"),under=T)

levelplot(mod09c,col.regions=colr(100),at=seq(0,100,len=100),margin=F,maxpixels=1e5,main="MOD09 Cloud Frequency",ylab="Latitude",xlab="Longitude")

#p2=xyplot(lat~lon|month2,data=cldm,col=as.character(cut(cldm$cld,seq(0,100,len=100),labels=colr(99))),pch=16,cex=.1,auto.key=T,asp=1,
#       main="NDP-026D Cloud Climatology Stations",ylab="Latitude",xlab="Longitude",layout=c(12,1))+
#  layer(sp.lines(coast,col="black",lwd=.1),under=F)
#v_month=c(p1,p2,layout=c(12,2),x.same=T,y.same=T,merge.legends=T)
#print(v_month)


#xyplot(lat~lon|month2,groups=cut(cldm$cld,seq(0,100,len=5)),data=cldm,pch=".",cex=.2,auto.key=T,
#       main="Mean Monthly Cloud Coverage",ylab="Latitude",xlab="Longitude",
#        par.settings = list(superpose.symbol= list(pch=16,col=c("blue","green","yellow","red"))))+
#  layer(sp.lines(coast,col="grey"),under=T)

### heatmap of mod09 vs. NDP for all months
hmcols=colorRampPalette(c("grey","blue","red"))
tr=c(0,27)
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
  layer(panel.abline(0,1,col="black",lwd=2))+
  layer(panel.ablineq(lm(y ~ x), r.sq = TRUE,at = 0.6,pos=1, offset=22,digits=2,col="blue"), style = 1)



xyplot(cld~mod09|month2,data=cld[cld$Nobs>10,],panel=function(x,y,subscripts){
  n=50
  bins=seq(0,100,len=n)
  tb=melt(as.matrix(table(
    x=cut(x,bins,labels=bins[-1]),
    y=cut(y,bins,labels=bins[-1]))))
  qat=unique(tb$value)
  print(qat)
  qat=0:26
  qat=tr[1]:tr[2]#unique(tb$value)
  panel.levelplot(tb$x,tb$y,tb$value,at=qat,col.regions=c("transparent",hmcols(length(qat))),subscripts=1:nrow(tb))
  layer(panel.abline(0,1,col="black",lwd=2))+
  layer(panel.ablineq(lm(y ~ x), r.sq = TRUE,at = 0.6,pos=1, offset=0,digits=2,col="blue"), style = 1)
},asp=1,scales=list(at=seq(0,100,len=6),useRaster=T,colorkey=list(width=.5,title="Number of Stations")),
          ylab="NDP Mean Cloud Amount (%)",xlab="MOD09 Cloud Frequency (%)",
              legend= list(right = list(fun = colkey)))+ layer(panel.abline(0,1,col="black",lwd=2))


xyplot(cld~mod09,data=clda,cex=0.5,pch=16)+
  layer(panel.abline(lm(y~x),col="blue"))+
#  layer(panel.lines(x,predict(lm(y~x),type="prediction")))+
  layer(panel.loess(x,y,col="blue",span=.2))+
  layer(panel.abline(0,1,col="red"))+
  layer(panel.segments(mod09,cld-cldsd,mod09,cld+cldsd,col="grey"),data=clda,under=T,magicdots=T)

## all monthly values
#xyplot(cld~mod09|as.factor(month),data=cld[cld$Nobs>75,],cex=.2,pch=16,subscripts=T)+
#  layer(panel.abline(lm(y~x),col="blue"))+
#  layer(panel.abline(0,1,col="red"))

## Monthly Climatologies
for(i in 1:2){
 p1=xyplot(cld~mod09|month2,data=cldm,cex=.2,pch=16,subscripts=T,ylab="NDP Mean Cloud Amount",xlab="MOD09 Cloud Frequency (%)")+
  layer(panel.lines(1:100,predict(lm(y~x),newdata=data.frame(x=1:100)),col="green"))+
  layer(panel.lines(1:100,predict(lm(y~x+I(x^3)),newdata=data.frame(x=1:100)),col="blue"))+
  layer(panel.abline(0,1,col="red"))
    if(i==2){
     p1=p1+layer(panel.segments(mod09[subscripts],cld[subscripts]-cldsd[subscripts],mod09[subscripts],cld[subscripts]+cldsd[subscripts],subscripts=subscripts,col="grey"),data=cldm,under=T,magicdots=T)
     p1=p1+layer(panel.segments(mod09[subscripts]-mod09sd[subscripts],cld[subscripts],mod09[subscripts]+mod09sd[subscripts],cld[subscripts],subscripts=subscripts,col="grey"),data=cldm,under=T,magicdots=T)
        }
print(p1)
}

dev.off()


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

## add a color key
breaks=seq(0,100,by=25)
cldm$cut=cut(cldm$cld,breaks)
cp=colorRampPalette(c("blue","orange","red"))
cols=cp(length(at))



## write a pdf
#dir.create("output")
pdf("output/NDP026d.pdf",width=11,height=8.5)



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

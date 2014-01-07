### Script to download and process the NDP-026D station cloud dataset

setwd("~/acrobates/adamw/projects/cloud/data/NDP026D")

library(multicore)
library(doMC)
library(rasterVis)
library(rgdal)
library(reshape)


## Data available here http://cdiac.ornl.gov/epubs/ndp/ndp026d/ndp026d.html

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
projection(st)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

## download data
system("wget -N -nd ftp://cdiac.ornl.gov/pub/ndp026d/cat67_78/* -A '.tc.Z' -P data/")

system("gunzip data/*.Z")

## define FWF widths
f162=c(5,5,4,7,7,7,4) #format 162
c162=c("StaID","YR","Nobs","Amt","Fq","AWP","NC")

## use monthly timeseries
cld=do.call(rbind.data.frame,mclapply(sprintf("%02d",1:12),function(m) {
  d=read.fwf(list.files("data",pattern=paste("MNYDC.",m,".tc$",sep=""),full=T),skip=1,widths=f162)
  colnames(d)=c162
  d$month=as.numeric(m)
  print(m)
  return(d)}
  ))

## add lat/lon
cld[,c("lat","lon")]=coordinates(st)[match(cld$StaID,st$id),]

## drop missing values
cld=cld[,!grepl("Fq|AWP|NC",colnames(cld))]
cld$Amt[cld$Amt<0]=NA
#cld$Fq[cld$Fq<0]=NA
#cld$AWP[cld$AWP<0]=NA
#cld$NC[cld$NC<0]=NA
#cld=cld[cld$Nobs>0,]

## add the MOD09 data to cld
#### Evaluate MOD35 Cloud data
mod09=brick("~/acrobates/adamw/projects/cloud/data/cloud_ymonmean.nc")

## overlay the data with 32km diameter (16km radius) buffer
## buffer size from Dybbroe, et al. (2005) doi:10.1175/JAM-2189.1.
buf=16000
bins=cut(1:nrow(st),100)
if(file.exists("valid.csv")) file.remove("valid.csv")
mod09sta=lapply(levels(bins),function(lb) {
  l=which(bins==lb)
  td=extract(mod09,st[l,],buffer=buf,fun=mean,na.rm=T,df=T)
  td$id=st$id[l]
  print(lb)#as.vector(c(l,td[,1:4])))
  write.table(td,"valid.csv",append=T,col.names=F,quote=F,sep=",",row.names=F)
  td
})#,mc.cores=3)

## read it back in
mod09st=read.csv("valid.csv",header=F)[,-c(1,2)]

colnames(mod09st)=c(names(mod09)[-1],"id")
mod09stl=melt(mod09st,id.vars="id")
mod09stl[,c("year","month")]=do.call(rbind,strsplit(sub("X","",mod09stl$variable),"[.]"))[,1:2]

## add it to cld
cld$mod09=mod09stl$value[match(paste(cld$StaID,cld$YR,cld$month),paste(mod09stl$id,mod09stl$year,as.numeric(mod09stl$month)))]


## LULC
#system(paste("gdalwarp -r near -co \"COMPRESS=LZW\" -tr ",paste(res(mod09),collapse=" ",sep=""),
#             "-tap -multi -t_srs \"",   projection(mod09),"\" /mnt/data/jetzlab/Data/environ/global/landcover/MODIS/MCD12Q1_IGBP_2005_v51.tif ../modis/mod12/MCD12Q1_IGBP_2005_v51.tif"))
lulc=raster("~/acrobates/adamw/projects/interp/data/modis/mod12/MCD12Q1_IGBP_2005_v51.tif")
require(plotKML); data(worldgrids_pal)  #load IGBP palette
IGBP=data.frame(ID=0:16,col=worldgrids_pal$IGBP[-c(18,19)],stringsAsFactors=F)
IGBP$class=rownames(IGBP);rownames(IGBP)=1:nrow(IGBP)
levels(lulc)=list(IGBP)
## function to get modal lulc value
Mode <- function(x) {
      ux <- na.omit(unique(x))
        ux[which.max(tabulate(match(x, ux)))]
      }
lulcst=extract(lulc,st,fun=Mode,buffer=buf,df=T)
colnames(lulcst)=c("id","lulc")
## add it to cld
cld$lulc=lulcst$lulc[match(cld$StaID,lulcst$id)]
cld$lulcc=IGBP$class[match(cld$lulc,IGBP$ID)]

## update cld column names
colnames(cld)[grep("Amt",colnames(cld))]="cld"
cld$cld=cld$cld/100
cld[,c("lat","lon")]=coordinates(st)[match(cld$StaID,st$id),c("lat","lon")]

## calculate means and sds
cldm=do.call(rbind.data.frame,by(cld,list(month=as.factor(cld$month),StaID=as.factor(cld$StaID)),function(x){
  data.frame(
             month=x$month[1],
             lulc=x$lulc[1],
             StaID=x$StaID[1],
             mod09=mean(x$mod09,na.rm=T),
             mod09sd=sd(x$mod09,na.rm=T),
             cld=mean(x$cld[x$Nobs>50],na.rm=T),
             cldsd=sd(x$cld[x$Nobs>50],na.rm=T))}))
cldm[,c("lat","lon")]=coordinates(st)[match(cldm$StaID,st$id),c("lat","lon")]

## means by year
cldy=do.call(rbind.data.frame,by(cld,list(year=as.factor(cld$YR),StaID=as.factor(cld$StaID)),function(x){
  data.frame(
             year=x$YR[1],
             StaID=x$StaID[1],
             lulc=x$lulc[1],
             mod09=mean(x$mod09,na.rm=T),
             mod09sd=sd(x$mod09,na.rm=T),
             cld=mean(x$cld[x$Nobs>50]/100,na.rm=T),
             cldsd=sd(x$cld[x$Nobs>50]/100,na.rm=T))}))
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
write.csv(clda,file="clda.csv",row.names=F)

#########################################################################


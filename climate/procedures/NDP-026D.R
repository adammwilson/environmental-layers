### Script to download and process the NDP-026D station cloud dataset
### to validate MODIS cloud frequencies

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
st@data[,c("lon","lat")]=coordinates(st)

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
cld$Amt=cld$Amt/100

## calculate means and sds for full record (1970-2009)
Nobsthresh=20 #minimum number of observations to include 

cldm=do.call(rbind.data.frame,by(cld,list(month=as.factor(cld$month),StaID=as.factor(cld$StaID)),function(x){
  data.frame(
      month=x$month[1],
      StaID=x$StaID[1],
      cld_all=mean(x$Amt[x$Nobs>=Nobsthresh],na.rm=T),  # full record
      cldsd_all=sd(x$Amt[x$Nobs>=Nobsthresh],na.rm=T),
      cld=mean(x$Amt[x$YR>=2000&x$Nobs>=Nobsthresh],na.rm=T), #only MODIS epoch
      cldsd=sd(x$Amt[x$YR>=2000&x$Nobs>=Nobsthresh],na.rm=T))}))
cldm[,c("lat","lon")]=coordinates(st)[match(cldm$StaID,st$id),c("lat","lon")]



## add the MOD09 data to cld
#### Evaluate MOD35 Cloud data
mod09=brick("~/acrobates/adamw/projects/cloud/data/cloud_ymonmean.nc")
mod09std=brick("~/acrobates/adamw/projects/cloud/data/cloud_ymonstd.nc")

## overlay the data with 32km diameter (16km radius) buffer
## buffer size from Dybbroe, et al. (2005) doi:10.1175/JAM-2189.1.
buf=16000
bins=cut(st$lat,10)
rerun=F
if(rerun&file.exists("valid.csv")) file.remove("valid.csv")
mod09sta=lapply(levels(bins),function(lb) {
  l=which(bins==lb)
  ## mean
  td=extract(mod09,st[l,],buffer=buf,fun=mean,na.rm=T,df=T)
  td$id=st$id[l]
  td$type="mean"
  ## std
  td2=extract(mod09std,st[l,],buffer=buf,fun=mean,na.rm=T,df=T)
  td2$id=st$id[l]
  td2$type="sd"
  print(lb)#as.vector(c(l,td[,1:4])))
  write.table(rbind(td,td2),"valid.csv",append=T,col.names=F,quote=F,sep=",",row.names=F)
  td
})#,mc.cores=3)

## read it back in
mod09st=read.csv("valid.csv",header=F)[,-c(1)]
colnames(mod09st)=c(names(mod09),"id","type")
mod09stl=melt(mod09st,id.vars=c("id","type"))
mod09stl[,c("year","month")]=do.call(rbind,strsplit(sub("X","",mod09stl$variable),"[.]"))[,1:2]
mod09stl$value[mod09stl$value<0]=NA
mod09stl=cast(mod09stl,id+year+month~type,value="value")

## add it to cld
cldm$mod09=mod09stl$mean[match(paste(cldm$StaID,cldm$month),paste(mod09stl$id,as.numeric(mod09stl$month)))]
cldm$mod09sd=mod09stl$sd[match(paste(cldm$StaID,cldm$month),paste(mod09stl$id,as.numeric(mod09stl$month)))]


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
cldm$lulc=lulcst$lulc[match(cldm$StaID,lulcst$id)]
cldm$lulcc=IGBP$class[match(cldm$lulc,IGBP$ID)]


### Add biome data
if(!file.exists("../teow/biomes.shp")){
    teow=readOGR("/mnt/data/jetzlab/Data/environ/global/teow/official/","wwf_terr_ecos")
    teow=teow[teow$BIOME<90,]
    biome=unionSpatialPolygons(teow,teow$BIOME, threshold=5)
    biomeid=read.csv("/mnt/data/jetzlab/Data/environ/global/teow/official/biome.csv",stringsAsFactors=F)
    biome=SpatialPolygonsDataFrame(biome,data=biomeid[as.numeric(row.names(biome)),])
    writeOGR(biome,"../teow","biomes",driver="ESRI Shapefile",overwrite=T)
}
biome=readOGR("../teow/","biomes")
projection(biome)=projection(st)
#st$biome=over(st,biome,returnList=F)$BiomeID
dists=apply(gDistance(st,biome,byid=T),2,which.min)
st$biome=biome$BiomeID[dists]

cldm$biome=st$biome[match(cldm$StaID,st$id)]


## write out the tables
write.csv(cld,file="cld.csv",row.names=F)
write.csv(cldm,file="cldm.csv",row.names=F)
writeOGR(st,dsn=".",layer="stations",driver="ESRI Shapefile",overwrite_layer=T)
#########################################################################


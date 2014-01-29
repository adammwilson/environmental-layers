##
## Summarize clouds by biome

setwd("~/acrobates/adamw/projects/cloud/")


library(rgdal)
library(raster)
library(geosphere)
library(rgeos)
library(maptools)
library(multicore)

## create Biome shapefile
if(!file.exists("data/teow/biomes.shp")){
    teow=readOGR("/mnt/data/jetzlab/Data/environ/global/teow/official/","wwf_terr_ecos")
    biome=unionSpatialPolygons(teow,paste(teow$REALM,teow$BIOME,sep="_"), threshold=5)
    biomeid=read.csv("/mnt/data/jetzlab/Data/environ/global/teow/biome.csv",stringsAsFactors=F)
    realmid=read.csv("/mnt/data/jetzlab/Data/environ/global/teow/realm.csv",stringsAsFactors=F,na.strings = "TTTT")
    dt=data.frame(code=row.names(biome),stringsAsFactors=F)
    dt[,c("realmid","biomeid")]=do.call(rbind,strsplit(sub(" ","",dt$code),"_"))
    dt$realm=realmid$realm[match(dt$realmid,realmid$realmid)]
    dt$biome=biomeid$BiomeID[match(dt$biomeid,biomeid$Biome)]
    row.names(dt)=row.names(biome)
    biome=SpatialPolygonsDataFrame(biome,data=dt)
    ## add area and centroid to each polygon
    biome$areakm=do.call(c,mclapply(1:length(biome),function(i) {print(i); return(areaPolygon(biome[i,])/1000000)}))
    biome@data[,c("lon","lat")]=coordinates(gCentroid(biome,byid=T))
    writeOGR(biome,"data/teow","biomes",driver="ESRI Shapefile",overwrite=T)
}


biome=readOGR("data/teow/","biomes")
projection(biome)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


### load cloud data
#mod09s=brick("data/cloud_yseasmean.nc",varname="CF");names(mod09s)=c("DJF","MAM","JJA","SON")
#mod09c=brick("data/cloud_ymonmean.nc",varname="CF");names(mod09c)=month.name
#mod09s=brick("data/cloud_ymonstd.nc",varname="CFsd");names(mod09s)=month.name

mod09a=brick("data/cloud_mean.nc",varname="CF");names(mod09a)="Mean Annual Cloud Frequency"
mod09min=brick("data/cloud_min.nc",varname="CF");names(mod09min)="Min Cloud Frequency"
mod09max=brick("data/cloud_max.nc",varname="CF");names(mod09max)="Max Cloud Frequency"

mod09inter=brick("data/cloud_std_inter.nc",varname="CF_sd");names(mod09inter)="Inter-annual SD"
mod09intra=brick("data/cloud_std_intra.nc",varname="CFsd");names(mod09intra)="Intra-annual SD"


## Extract biome-level summaries

fsum=function(x,na.rm=T) paste(mean(x,na.rm=T),sd(x,na.rm=T),min(x,na.rm=T),max(x,na.rm=T),sep="_")
fs=list(min=mod09min,max=mod09max,intersd=mod09inter,intrasd=mod09intra,mean=mod09a)
fs=brick(list(min=mod09min,max=mod09max,intersd=mod09inter,intrasd=mod09intra,mean=mod09a))

biomed=do.call(rbind.data.frame,
    mclapply(1:length(fs),function(i) {
        print(i)
        td=extract(fs[[i]],biome,fun=fsum,df=F,sp=F)
        td2=do.call(rbind.data.frame,strsplit(td,"_"))
        td3=as.data.frame(apply(td2,2,as.numeric))
        colnames(td3)=c("mean","sd","min","max")
        td3$metric=names(fs)[i]
        td3$biome=biome$code
        return(td3)
    }))
write.csv(biomed,file="output/biomesummary.csv",row.names=F)

################################################################
## get stratified sample of points from biomes for illustration
 if(!file.exists("output/biomesamplepoints.csv")){
          biome=readOGR("data/teow/","biomes")
               n_biomesamples=1000
               library(multicore)
               biomesample=do.call(rbind.data.frame,mclapply(1:length(biome),function(i)
                            data.frame(code=biome$code[i],coordinates(spsample(biome[i,],n=n_biomesamples,type="stratified",nsig=2)))))
               colnames(biomesample)[2:3]=c("lon","lat")
               biomesample[,c("biome","realm")]=biome@data[match(biomesample$code,biome$code),c("biome","realm")]
               write.csv(biomesample,"output/biomesamplepoints.csv",row.names=F)
      }

     ## Extract data for points
 if(!file.exists("output/biomesamplepoints_cloud.csv")){
          biomesample=read.csv("output/biomesamplepoints.csv")
               biomep=raster::extract(mod09c,biomesample,sp=T)
               biomep$lon=biomesample$lon
               biomep@data[,c("lon","lat")]=coordinates(biomep)
               write.csv(biomep@data,"output/biomesamplepoints_cloud.csv",row.names=F)
      }
biomep=read.csv("output/biomesamplepoints_cloud.csv")
coordinates(biomep)=c("lon","lat")
projection(biomep)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"                     

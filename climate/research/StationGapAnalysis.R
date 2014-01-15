
setwd("~/acrobates/adamw/projects/interp/data")
library(reshape)
library(sp);library(rgdal);
library(reshape)
library(ncdf4)
library(geosphere)
library(rgeos)
library(multicore);library(RSQLite)

stInfo=function(file,inventory,vars=vars){
  ### read in station location data
    cs=c(11,-1,8,-1,9,-1,6,-4,30,-1,3,-1,3,-1,5) #column widths, negative means skip
      print("Importing station data")
      st=read.fwf(file,widths=cs,colClasses="character",comment.char = "",header=F,skip=0)
      colnames(st)=c("id","lat","lon","elev","name","gsnflag","hcnflag","wmoid")
      print("Updating factors")
      for(i in c(5,6,7,8)) st[,i]=as.factor(st[,i])
      for(i in 2:4) st[,i]=as.numeric(st[,i])
    ### inventory
      print("Getting station start-stop years from station inventory file")
      cs=c(11,-1,8,-1,9,-1,4,-1,4,-1,4) #column widths, negative means skip
      stinv=read.fwf(inventory,widths=cs,colClasses="character",comment.char = "",header=F,skip=0)
      colnames(stinv)=c("id","lat","lon","variable","yearstart","yearstop")
      stinv=stinv[stinv$variable%in%vars,]
    ### reshape yearstart
      stinvw1=cast(stinv,id+lat+lon~variable,value="yearstart")
      colnames(stinvw1)[!grepl("id|lat|lon",colnames(stinvw1))]=
            paste(colnames(stinvw1)[!grepl("id|lat|lon",colnames(stinvw1))],".start",sep="")
    ### reshape yearstop
      stinvw2=cast(stinv,id+lat+lon~variable,value="yearstop")
      colnames(stinvw2)[!grepl("id|lat|lon",colnames(stinvw2))]=
            paste(colnames(stinvw2)[!grepl("id|lat|lon",colnames(stinvw2))],".stop",sep="")
      stinvw=merge(stinvw1,stinvw2)
      for(i in colnames(stinvw[,!grepl("id|lat|lon",colnames(stinvw))])) stinvw[,i]=as.numeric(as.character( stinvw[,i]))
      colnames(stinvw)=sub("TMAX","tmax",colnames(stinvw))
      colnames(stinvw)=sub("TMIN","tmin",colnames(stinvw))
      colnames(stinvw)=sub("PRCP","ppt",colnames(stinvw))
    ### merge the data
      print("Merging start-stop dates with station locations")
      st2=merge(st,stinvw[,!grepl("lat|lon",colnames(stinvw))],all.x=T,by=c("id"))
    ## add station type field
    st2$type=ifelse((!is.na(st2$tmax.start)|!is.na(st2$tmin.start))&!is.na(st2$ppt.start),"T&P",
      ifelse((!is.na(st2$tmax.start)|!is.na(st2$tmin.start))&is.na(st2$ppt.start),"T",
             ifelse((is.na(st2$tmax.start)|is.na(st2$tmin.start))&!is.na(st2$ppt.start),"P","None")))
    ### Convert to spatial points and return
      print("Convert to spatial points")
      coordinates(st2)=c("lon","lat")
      st2@data[,c("lon","lat")]=coordinates(st2)
      proj4string(st2)=CRS("+proj=longlat +datum=WGS84")
      return(st2)
  }


### Process the station data
st=stInfo(file="ghcn/ghcnd-stations.txt",inventory="ghcn/ghcnd-inventory.txt",
    vars=c("PRCP","TMAX","TMIN"))

writeOGR(st,".","stationlocations",driver="ESRI Shapefile",overwrite=T)

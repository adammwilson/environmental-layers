#! /bin/R
### Script to download and process the NDP-026D station cloud dataset
setwd("~/acrobates/projects/interp/data/NDP026D")

## available here http://cdiac.ornl.gov/epubs/ndp/ndp026d/ndp026d.html


## Get station locations
system("wget -N -nd http://cdiac.ornl.gov/ftp/ndp026d/cat01/01_STID -P data/")
st=read.table("data/01_STID",skip=1)
colnames(st)=c("StaID","LAT","LON","ELEV","ny1","fy1","ly1","ny7","fy7","ly7","SDC","b5c")
st$lat=st$LAT/100
st$lon=st$LON/100
st$lon[st$lon>180]=st$lon[st$lon>180]-360

## check a plot
plot(lat~lon,data=st,pch=16,cex=.5)


## get monthly mean cloud amount MMCA
system("wget -N -nd ftp://cdiac.ornl.gov/pub/ndp026d/cat67_78/* -A '.tc.Z' -P data/")
system("gunzip data/*.Z")

#f121=c(6,6,6,7,6,7,6,2) #format 121
#c121=c("StaID","NobD","AvgDy","NobN","AvgNt","NobDN","AvgDN","Acode")
f162=c(5,5,4,7,7,7,4) #format 121
c162=c("StaID","YR","Nobs","Amt","Fq","AWP","NC")

cld=do.call(rbind.data.frame,lapply(sprintf("%02d",1:12),function(m) {
  d=read.fwf(list.files("data",pattern=paste("MNYDC.",m,".tc",sep=""),full=T),skip=1,widths=f162)
  colnames(d)=c162
  d$month=as.numeric(m)
  return(d)}
  ))

cld[,c("lat","lon")]=st[match(st$StaID,cld$StaID),c("lat","lon")]

## drop missing values
cld$Amt[cld$Amt<0]=NA
cld$Fq[cld$Fq<0]=NA
cld$AWP[cld$AWP<0]=NA
cld$NC[cld$NC<0]=NA

## calculate means
cldm=do.call(rbind.data.frame,by(cld,list(month=as.factor(cld$month),StaID=as.factor(cld$StaID)),function(x){
  data.frame(month=x$month[1],StaID=x$StaID[1],Amt=mean(x$Amt[x$Nobs>20],na.rm=T))}))
cldm[,c("lat","lon")]=st[match(st$StaID,cldm$StaID),c("lat","lon")]



## write out the table
write.csv(cldm,file="cldm.csv")


##################
###
cldm=read.csv("cldm.csv")

## add a color key
cldm$col=cut(cldm$Amt/100,quantile(cldm$Amt/100,seq(0,1,len=5),na.rm=T))

library(lattice)
xyplot(lat~lon|+month,groups=col,data=cldm,pch=16,cex=.2,auto.key=T)


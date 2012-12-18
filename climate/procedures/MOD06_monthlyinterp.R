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
library(latticeExtra)
library(rgl)
library(hdf5)
library(rasterVis)
library(heR.Misc)
library(car)
library(mgcv)
library(sampling)

X11.options(type="Xlib")
ncores=20  #number of threads to use

setwd("/home/adamw/acrobates/projects/interp")

## get MODLAND tile information
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
save(tb,file="modlandTiles.Rdata")

tile="h11v08"   #can move this to submit script if needed
#tile="h09v04"   #oregon

psin=CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

tile_bb=tb[tb$tile==tile,] ## identify tile of interest
roi_ll=extent(tile_bb$lon_min,tile_bb$lon_max,tile_bb$lat_min,tile_bb$lat_max) 
#roi=spTransform(roi,psin)
#roil=as(roi,"SpatialLines")

dmod06="data/modis/mod06/summary"


##########################
#### Organize the data
months=seq(as.Date("2000-01-15"),as.Date("2000-12-15"),by="month")

getmod06<-function(variable){
  d=brick(list.files(dmod06,pattern=paste("MOD06_",tile,".nc",sep=""),full=T),varname=toupper(variable))
#  d=dropLayer(d,1)
  projection(d)=psin
  setZ(d,format(as.Date(d@z$Date),"%m"),name="time")
#  d@z=as.Date(d@z$Date)
  layerNames(d) <- as.character(format(as.Date(d@z$Date),"%b")) #paste(variable,format(as.Date(d@z$Date),"%m"))
  return(d)
}

# drop #1?

cer=getmod06("cer")
cld=getmod06("cld")
cot=getmod06("cot")
cer20=getmod06("cer20")

pcol=colorRampPalette(c("brown","red","yellow","darkgreen"))
#levelplot(cer,col.regions=pcol(20))

## load WorldClim data for comparison (download then uncompress)
#system("wget -P data/worldclim/ http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/prec_30s_bil.zip",wait=F)
#system("wget -P data/worldclim/ http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_30s_bil.zip",wait=F)

### load WORLDCLIM elevation 
#dem=raster(list.files("data/worldclim/alt_30s_bil/",pattern="bil$",full=T))
#projection(dem)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#dem=crop(dem,roi_ll)
#dem[dem>60000]=NA
#dem=projectRaster(dem,cer)
#writeRaster(dem,file=paste("data/tiles/",tile,"/dem_",tile,".tif",sep=""),format="GTiff")
dem=raster(paste("data/tiles/",tile,"/dem_",tile,".tif",sep=""))

### get station data, subset to stations in region, and transform to sinusoidal
dm=readOGR(paste("data/tiles/",tile,sep=""),paste("station_monthly_",tile,"_PRCP",sep=""))
colnames(dm@data)[grep("elevation",colnames(dm@data))]="dem"
colnames(dm@data)[grep("value",colnames(dm@data))]="ppt"

                                        #xyplot(latitude~longitude|month,data=dm@data)
dm2=spTransform(dm,CRS(projection(cer)))
dm2@data[,c("x","y")]=coordinates(dm2)

### extract MOD06 data for each station
stcer=extract(cer,dm2,fun=mean);colnames(stcer)=paste("cer_mean_",as.numeric(format(as.Date(cer@z$Date),"%m")),sep="")
stcer20=extract(cer20,dm2,fun=mean);colnames(stcer20)=paste("cer20_mean_",as.numeric(format(as.Date(cer20@z$Date),"%m")),sep="")
stcot=extract(cot,dm2);colnames(stcot)=paste("cot_mean_",as.numeric(format(as.Date(cot@z$Date),"%m")),sep="")
stcld=extract(cld,dm2);colnames(stcld)=paste("cld_mean_",as.numeric(format(as.Date(cld@z$Date),"%m")),sep="")
stdem=extract(dem,dm2)
#mod06=cbind.data.frame(station=dm$station,stcer[,-1],stcot[,-1],stcld[,-1],stcer20[,-1])
mod06=cbind.data.frame(station=dm$station,stcer,stcot,stcld,stcer20)
mod06l=melt(mod06,id.vars=c("station"));colnames(mod06l)[grep("value",colnames(mod06l))]="mod06"
mod06l[,c("variable","moment","month")]=do.call(rbind,strsplit(as.character(mod06l$variable),"_"))
mod06l=unique(mod06l)
mod06l=cast(mod06l,station+moment+month~variable,value="mod06")
mod06l=merge(dm2@data,mod06l,by=c("station","month"))
mod06l=mod06l[!is.na(mod06l$cer),]

mod06l=mod06l[order(mod06l$month),]

#xyplot(value~cer|month,data=mod06l,scales=list(relation="free"),pch=16,cex=.5)
#xyplot(value~cer|station,data=mod06l[mod06l$count>400,],pch=16,cex=.5)
#xyplot(cot~month,groups=station,data=mod06l,type="l")

### create monthly raster bricks for prediction
m=2

pdata=stack(
  subset(cer,subset=m),
  subset(cot,subset=m),
  subset(cer20,subset=m),
  subset(cld,subset=m)
  )

## Set up models to compare
####################################
#### build table comparing various metrics
models=c(
  "ppt~s(y)+ s(x)",
  "ppt~s(y,x)",
  "ppt~s(y,x) + s(dem)",
  "ppt~s(y,x)+s(dem)+cer+cld+cot+cer20",
  "ppt~s(y,x)+s(dem)+s(cer)",
  "ppt~s(y,x)+s(dem)+s(cer20)",
  "ppt~s(y,x)+s(dem)+s(cld)",
  "ppt~s(y,x)+s(dem)+s(cot)",
  "ppt~s(y,x)+s(dem)+s(cer20,cld)",
  "ppt~s(y,x)+s(dem)+s(cer20,cot)",
  "ppt~s(y,x)+s(dem)+s(cer,cld)",
  "ppt~s(dem)+s(cer,cot)")
months=1:12

## build the list of models/months to process
mm=expand.grid(model=models,month=months)
mm$model=as.character(mm$model)
mm$mid=match(mm$model,models)
  
#  mod1<- gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM), data=data_s)
#  mod2<- gam(tmax~ s(lat,lon,ELEV_SRTM), data=data_s)
#  mod3<- gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) +  s (Northness)+ s (Eastness) + s(DISTOC), data=data_s)
#  mod4<- gam(tmax~ s(lat) + s (lon) + s(ELEV_SRTM) + s(Northness) + s (Eastness) + s(DISTOC) + s(LST), data=data_s)
#  mod5<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST), data=data_s)
#  mod6<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC1), data=data_s)
#  mod7<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC3), data=data_s)
#  mod8<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST) + s(LC1), data=data_s)

### Sample validation stations
prop=0.1
mod06l$validation=F
mod06l$validation[as.numeric(rownames(strata(mod06l,stratanames="month",size=as.integer(prop*table(mod06l$month)),method="srswor")))]=T

### run it
mods=lapply(1:nrow(mm),function(i){
    mod=try(gam(as.formula(mm$model[i]),data=mod06l[mod06l$month==mm$month[i]&!mod06l$validation,]))
  ## add some attributes to the object to help ID it later
  attr(mod,"month")=mm$month[i]
  attr(mod,"model")=mm$model[i]
  attr(mod,"modelid")=mm$mid[i]
  print(paste("Finished model ",mm$model[i]," for month",mm$month[i]))
  return(mod)
})

## Get summary stats
sums=do.call(rbind.data.frame,lapply(mods,function(m,plot=F){
  if(class(m)=="try-error") {
    return(data.frame(model=attr(m,"model"),
                      modelid=attr(m,"modelid"),
                      month=attr(m,"month"),
                      r2=NA,
                      dev=NA,
                      aic=NA,
                      rmse=NA,
                      vr2=NA))
  }

  ## make validation predictions
  vd=mod06l[mod06l$validation&mod06l$month==attr(m,"month"),]
  y=predict(m,mod06l[mod06l$validation&mod06l$month==attr(m,"month"),])
  vlm=lm(y~vd$ppt)

    ## Draw some plots
  if(plot){
    ## partial residual plots
    var=2
    fv <- predict(m,type="terms") ## get term estimates
    ## compute partial residuals for first smooth...
    prsd1 <- residuals(m,type="working") + fv[,var]
    plot(m,select=var) ## plot first smooth
    points(mod06l$cot[mod06l$month==mm$month[i]&!mod06l$validation],prsd1,pch=16,col="red")
  }

  ## summarize validation
  s1=summary(m)
  data.frame(
             model=attr(m,"model"),
             modelid=attr(m,"modelid"),
             month=attr(m,"month"),
             r2=s1$r.sq,
             dev=s1$dev.expl,
             aic=AIC(m),
             rmse=sqrt(mean((vd$ppt-predict(vlm,vd))^2)),
             vr2=summary(vlm)$r.squared)
}))

### Summary Figures

sumsl=melt(sums,id.vars=c("model","modelid","month"))

sum2=cast(sumsl,model~variable, fun=function(x) c(mean=mean(x,na.rm=T),sd=sd(x,na.rm=T)))

#combineLimits(useOuterStrips(xyplot(value~month|model+variable,data=sumsl,scale=list(relation="free"))))
bwplot(model~value|variable,data=sumsl,scale=list(x=list(relation="free")))

xyplot(ppt~cld|station,groups=month,data=mod06l,cex=.5,pch=16)

round(cor(mod06l[,c("ppt","dem","cer","cer20","cld","cot")]),2)


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

bgyr=colorRampPalette(c("blue","green","yellow","red","purple"))

X11.options(type="cairo")
pdf("output/MOD06_summary.pdf",width=11,height=8.5)

# % cloudy maps
title="Cloudiness (% cloudy days) "
at=unique(quantile(as.matrix(cld),seq(0,1,len=100),na.rm=T))
p=levelplot(cld,xlab.top=title,at=at,col.regions=bgyr(length(at)))#+layer(sp.lines(roil, lwd=1.2, col='black'))
print(p)
#bwplot(cer,main=title,ylab="Cloud Effective Radius (microns)")

# CER maps
title="Cloud Effective Radius (microns)"
at=quantile(as.matrix(cer),seq(0,1,len=100),na.rm=T)
p=levelplot(cer,xlab.top=title,at=at,col.regions=bgyr(length(at)))#+layer(sp.lines(roil, lwd=1.2, col='black'))
print(p)
#bwplot(cer,main=title,ylab="Cloud Effective Radius (microns)")

# CER20 maps
title="% Days with Cloud Effective Radius > 20 microns"
at=unique(quantile(as.matrix(cer20),seq(0,1,len=100),na.rm=T))
p=levelplot(cer20,xlab.top=title,at=at,col.regions=bgyr(length(at)))#+layer(sp.lines(roil, lwd=1.2, col='black'))
print(p)
#bwplot(cer,main=title,ylab="Cloud Effective Radius (microns)")

# COT maps
title="Cloud Optical Thickness (%)"
at=quantile(as.matrix(cot),seq(0,1,len=100),na.rm=T)
p=levelplot(cot,xlab.top=title,at=at,col.regions=bgyr(length(at)))#+layer(sp.lines(roil, lwd=0.8, col='black'))
print(p)
#bwplot(cot,xlab.top=title,ylab="Cloud Optical Thickness (%)")
dev.off()






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



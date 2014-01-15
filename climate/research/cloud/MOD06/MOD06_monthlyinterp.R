###################################################################################
###  R code to interpolate monthly climatologies 


## connect to server of choice
#system("ssh litoria")
#R

library(sp)
#library(spgrass6)
library(rgdal)
library(reshape)
library(ncdf4)
#library(geosphere)
#library(rgeos)
library(multicore)
library(raster)
library(rasterVis)
library(lattice)
library(latticeExtra)
#library(rgl)
#library(hdf5)
#library(heR.Misc)
#library(car)
library(mgcv)
library(sampling)

X11.options(type="Xlib")
ncores=20  #number of threads to use

setwd("/home/adamw/acrobates/projects/interp")

## get MODLAND tile information
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
save(tb,file="modlandTiles.Rdata")

#tile="h11v08"   #can move this to submit script if needed
tile="h21v09"  #Kenya
tile="h09v04"   #oregon
tiles=c("h11v08","h09v04")
  
psin=CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

tile_bb=tb[tb$tile==tile,] ## identify tile of interest
roi_ll=extent(tile_bb$lon_min,tile_bb$lon_max,tile_bb$lat_min,tile_bb$lat_max) 

dmod06="data/modis/mod06/summary"
outdir=paste("data/tiles/",tile,"/",sep="")


##########################
#### Organize the data
months=seq(as.Date("2000-01-15"),as.Date("2000-12-15"),by="month")

getmod06<-function(variable,month=NA){
  d=brick(list.files(dmod06,pattern=paste("MOD06_",tile,".nc$",sep=""),full=T),varname=toupper(variable))
  if(!is.na(month)) {
    d=subset(d,subset=month)
    names(d)=variable
  }
  if(is.na(month)){
    setZ(d,format(as.Date(d@z$Date),"%m"),name="time")
    layerNames(d) <- as.character(format(as.Date(d@z$Date),"%b")) #paste(variable,format(as.Date(d@z$Date),"%m"))
  }
  projection(d)=psin
  return(d)
}

cer=getmod06("cer")
cld=getmod06("cld")
cot=getmod06("cot")
cer20=getmod06("cer20")



pcol=colorRampPalette(c("brown","red","yellow","darkgreen"))

### create data dir for tiled data
ddir=paste("data/tiles/",tile,sep="")
if(!file.exists(ddir)) dir.create(ddir)

## load WorldClim data for comparison (download then uncompress)
#system("wget -P data/worldclim/ http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/prec_30s_bil.zip",wait=F)
#system("wget -P data/worldclim/ http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_30s_bil.zip",wait=F)

### load WORLDCLIM elevation 
if(!file.exists(paste(ddir,"/dem_",tile,".tif",sep=""))){
  dem=raster(list.files("data/worldclim/alt_30s_bil/",pattern="bil$",full=T))
  projection(dem)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  dem=crop(dem,roi_ll)
  dem[dem>30000]=NA
  dem=projectRaster(dem,cer)
  writeRaster(dem,file=paste("data/tiles/",tile,"/dem_",tile,".tif",sep=""),format="GTiff",overwrite=T)
}
dem=raster(paste("data/tiles/",tile,"/dem_",tile,".tif",sep=""))
names(dem)="dem"

### get station data, subset to stations in region, and transform to sinusoidal
dm=readOGR(ddir,paste("station_monthly_",tile,"_PRCP",sep=""))
colnames(dm@data)[grep("elevation",colnames(dm@data))]="elev"
colnames(dm@data)[grep("value",colnames(dm@data))]="ppt"

#xyplot(latitude~longitude|month,data=dm@data)

## transform to sinusoidal to overlay with raster data
dm2=spTransform(dm,CRS(projection(cer)))
dm2@data[,c("x","y")]=coordinates(dm2)

## limit data to station-months with at least 300 daily observations (~10 years)
dm2=dm2[dm2$count>300,]

## create coordinate rasters

### create monthly raster bricks for prediction
getd<-function(vars=c("cer","cot","cer20","cld"),month){
  pdata=stack(lapply(vars,function(v){getmod06(v,month=month)}))
  x=rasterFromXYZ(coordinates(dem)[,c(1,2,1)])
  y=rasterFromXYZ(coordinates(dem)[,c(1,2,2)])
  pdata=stack(pdata,dem,x,y)
  return(pdata)
}

## Set up models to compare
####################################
#### build table comparing various metrics
models=data.frame(model=c(
#  "ppt~s(y)+ s(x)",
#  "ppt~s(y,x)",
  "lppt~s(y,x)",
  "lppt~s(y,x)+s(dem)",
  "lppt~s(y,x,dem)",
  "lppt~s(y,x,dem)+s(cld)",
  "lppt~s(y,x)+s(dem)+s(cld)",
  "lppt~s(y,x)+s(cld)",
  "lppt~s(y,x)+s(cot)",
  "lppt~s(y,x)+s(dem)+s(cot)",
  "lppt~s(y,x,dem)+s(cot)",
  "lppt~s(y,x)+s(cer20)",
  "lppt~s(y,x)+s(dem)+cld+cot+cer20",
  "lppt~s(y,x,dem)+cld+cot+cer20",
  "lppt~s(y,x)+s(dem)+s(cld,cot,cer20)",
  "lppt~s(y,x)+s(dem)+s(cld)+s(cot)+s(cer20)"))
  months=1:12
## add category for each model
models$type="Spatial"
models$type[grepl("cer|cot|cer20",models$model)]="MOD06"
models$type[grepl("cld",models$model)&!grepl("cer|cot|cer20",models$model)]="MOD35"

## build the list of models/months to process
mm=expand.grid(model=models$model,months=months,stringsAsFactors=F)
mm$mid=match(mm$model,models$model)
  
### Sample validation stations
prop=0.1

### run it
fitmod<-function(i,prop=0.1,predict=F, nv,...){
  model=as.character(mm$model[i])
  month=mm$month[i]
  mid=mm$mid[i]
### extract data
  dr=getd(month=month)
  ##  subset data for this month
  dt=dm2[dm2$month==month,]
  ## add log+1 ppt
  dt$lppt=log(dt$ppt+1)
  ## extract for points from dm2
  dr2=subset(dr,subset=grep("^x$|^y$",names(dr),invert=T))
  ds=cbind.data.frame(dt@data,extract(dr2,dt))
### flag stations to hold out for validation
  ts2=do.call(rbind.data.frame,lapply(1:nv,function(r) {
    ds$validation=F
    ds$validation[sample(1:nrow(ds),size=as.integer(prop*nrow(ds)))]=T
### fit model
    mod<<-try(gam(as.formula(model),data=ds[!ds$validation,]),silent=T)
      ## if fitting fails, return a NULL
      if(class(mod)[[1]]=="try-error")  return(NULL)
### Model Validation
      vd=ds[ds$validation,]  # extract validation points
      y=as.data.frame(predict(mod,ds,se.fit=T))      # make predictions
      if(attr(terms(mod),"variables")[[2]]=="lppt"){  #if modeling log(ppt+1), transform back to real units
        y$fit=exp(y$fit)-1
        y$se.fit=exp(y$se.fit)-1
      }
      ds[,c("pred","pred.se")]=cbind(y$fit,y$se.fit)
      ds$model=model
      ds$modelid=mid
      vlm=lm(pred~ppt,data=ds[ds$validation,])       # lm() to summarize predictive fit
    if(r==nv) ds<<-ds  #if on last iteration, save predictions
### summarize validation
      s1=summary(mod)
      s2=data.frame(
        model.r2=s1$r.sq,
        model.dev=s1$dev.expl,
        model.aic=AIC(mod),
        valid.rmse=sqrt(mean((vd$ppt-y$fit[ds$validation])^2,na.rm=T)),
        valid.nrmse=sqrt(mean((vd$ppt-y$fit[ds$validation])^2,na.rm=T))/mean(vd$ppt+.1,na.rm=T),
        valid.mer=mean(vd$ppt-y$fit[ds$validation],na.rm=T),
        valid.mae=mean(abs(vd$ppt-y$fit[ds$validation]),na.rm=T),
        valid.r2=summary(vlm)$r.squared)
      return(s2)}))
  ## summarize the gcv datasets
  if(nrow(ts2)==0) return(NULL)
  s2a=data.frame(mean=apply(ts2,2,mean,na.rm=T))
  s2b=t(apply(ts2,2,quantile,c(0.025,0.975),na.rm=T))
  colnames(s2b)=paste("Q",c(2.5,97.5),sep="")
  s2=cbind.data.frame(tile=tile, model=model, modelid=mid, month=month,
    metric=rownames(s2a),s2a,s2b)
  rownames(s2)=1:nrow(s2)
### add some attributes to the object to help ID it later
      attr(mod,"month")=ds$month[1]
      attr(mod,"model")=model
      attr(mod,"modelid")=mid
### generate predictions?
  if(predict){
    ## write prediction
    ncfile=paste(outdir,tile,"_pred_",ds$month[1],"_",mid[1],".nc",sep="")
    predict(dr,mod,filename=ncfile,format="CDF",overwrite=T)
    ## add some attributes to nc file
    ncopath=""
    ## add other attributes
    system(paste(ncopath,"ncrename -v variable,ppt ",ncfile,sep=""))
    system(paste(ncopath,"ncatted -a units,ppt,o,c,\"mm\" ",
                 "-a model,ppt,o,c,\"",model,"\" ",
                 "-a long_name,ppt,o,c,\"Mean Monthly Precipitation\" ",
                 ncfile,sep=""))
    ## create NC file to hold summary data and then append it to output file
    ## add table for validation metrics
    s2_dim1=ncdim_def("metrics1",units="",vals=1:ncol(s2),unlim=FALSE,create_dimvar=T,longname="Model Fitting and Validation Metrics")
    s2_dim2=ncdim_def("metrics2",units="",vals=1:nrow(s2),unlim=FALSE,create_dimvar=T,longname="Model Fitting and Validation Metrics")
    s2_var=ncvar_def("modelmetrics",units="varies",list(s2_dim1,s2_dim2),missval=-999,longname="Model Fitting and Validation Metrics",
      prec="float")
    ## simplify data table for incorporation into netcdf file
    ds2=ds[,-1];ds2$validation=ifelse(ds2$validation,1,0)
    ds2=as.matrix(ds2)
    data_dim1=ncdim_def("data",units="",vals=1:ncol(ds2),unlim=FALSE,create_dimvar=T,longname="Data used in model fitting")
    data_dim2=ncdim_def("stations",units="",vals=1:nrow(ds2),unlim=FALSE,create_dimvar=T,longname="Stations used in model fitting")
    data_var=ncvar_def("modeldata",units="varies",list(data_dim1,data_dim2),missval=-999,longname="Data used in model fitting",
      prec="float")
    tncf=paste(tempdir(),"/",tile,month,mid,"metrics.nc",sep="") #temp file to hold metric data
    nc_create(filename=tncf,vars=list(data_var,s2_var))
    tnc=nc_open(tncf,write=T)
#    ncvar_put(tnc,s2_var,vals=s2)  ## put in validatation data
    ncvar_put(tnc,data_var,vals=ds2)  ## put in validatation data
    nc_close(tnc)  ## close the file
  system(paste(ncopath,"ncks -A ",tncf," ",ncfile,sep=""))
  system(paste(ncopath,"ncatted -a value,modelmetrics,o,c,\"",paste(1:ncol(s2),":",colnames(s2),collapse="
",sep=""),"\" ",ncfile,sep=""))
  ## also add metrics as variable attributes for easier viewing
  for(c in 1:ncol(s2))  system(paste(ncopath,"ncatted -a ",colnames(s2)[c],",ppt,o,",
                                    ifelse(class(s2[1,c])!="numeric","c,\"","d,"),s2[1,c],
                                    ifelse(class(s2[1,c])!="numeric","\" "," "),ncfile,sep=""))
  ## Add time dimension to ease merging files later
  system(paste(ncopath,"ncecat -O -u time ",ncfile," ",ncfile,sep=""))
  cat(paste("
    netcdf time {
      dimensions:
        time = 1 ;
      variables:
        int time(time) ;
      time:units = \"months since 2000-01-01 00:00:00\" ;
      time:calendar = \"gregorian\";
      time:long_name = \"time of observation\"; 
    data:
      time=",as.integer(ds$month[1]-1),";
    }"),file=paste(tempdir(),"/time.cdl",sep=""))
  system(paste("ncgen -o ",tempdir(),"/time.nc ",tempdir(),"/time.cdl",sep=""))
  system(paste(ncopath,"ncks -A ",tempdir(),"/time.nc ",ncfile,sep=""))
  ## global attributes
  system(paste(ncopath,"ncatted -a title,global,o,c,\"Interpolated Monthly Precipitation\" ",
               "-a institution,global,o,c,\"Yale University\" ",
               "-a source,global,o,c,\"Interpolated from station data\" ",
               "-a contact,global,o,c,\"adam.wilson@yale.edu\" ",
               ncfile,sep=""))
  }
  print(paste("Finished model ",model," for month",ds$month[1]))
  return(list(model=mod,summary=s2,data=ds))
}

mods=lapply(1:nrow(mm),fitmod,predict=F,nv=100)

## extract summary tables and write them
sums=do.call(rbind.data.frame,lapply(mods,function(m) return(m$summary)))
write.csv(sums,paste(outdir,tile,"_validation.csv",sep=""))
pred=lapply(mods,function(m) return(m$data))
### messy error handling to remove tables with incorrect number of columns...
nullpred=which(!do.call(c,lapply(pred,function(x) ncol(x)==20&!is.null(x))))
pred=pred[nullpred]
pred=do.call(rbind.data.frame,pred)
pred$tile=tile
write.csv(pred,paste(outdir,tile,"_predictions.csv",sep=""))

### read them back

#sums=do.call(rbind.data.frame,lapply(tiles,function(tile) read.csv(paste("data/tiles/",tile,"/",tile,"_validation.csv",sep=""))))
sums=read.csv(paste(outdir,tile,"_validation.csv",sep=""))
pred=read.csv(paste(outdir,tile,"_predictions.csv",sep=""))

#sums=rbind.data.frame(sums,read.csv(paste("data/tiles/",tiles[1],"/",tiles[1],"_validation.csv",sep="")))
#pred=read.csv(paste(outdir,tiles[1],"_predictions.csv",sep=""))


## reshape summary validation data
#sumsl=melt(sums,id.vars=c("tile","model","modelid","month"))
sum2=cast(sums[,-1],model+modelid~metric,value="mean",fun=function(x) c(median=round(median(x,na.rm=T),2),sd=round(sd(x,na.rm=T),2)));sum2

#by(sum2,tile,function(x) rank(apply(cbind(rank(x$valid.rmse_median),rank(-x$valid.r2_median)),1,mean,na.rm=T)))
#sum2$rank=rank(apply(cbind(rank(sum2$valid.rmse_median),rank(-sum2$valid.r2_median)),1,mean,na.rm=T))


## add sorting variables across months
sum2$rank=rank(apply(cbind(rank(sum2$valid.rmse_median),rank(-sum2$valid.r2_median)),1,mean,na.rm=T))
sum2[order(sum2$rank),c("model","modelid","valid.r2_median","valid.rmse_median","valid.nrmse_median","rank"),] #"valid.mer_mean",
sums$fmodel=factor(sums$model,ordered=T,levels=rev(sum2$model[order(sum2$rank)]))

## add sorting variables for each month
sumsl2=cast(model~month~metric,value="mean",data=sums)
sumsl2[,,"valid.r2"]

#combineLimits(useOuterStrips(xyplot(value~month|model+variable,data=sumsl,scale=list(relation="free"))))
sums2=sums[sums$metric%in%c("valid.r2","valid.rmse"),]



###############################################################
###############################################################
### Draw some plots
library(maptools)
coast=getRgshhsMap(fn="/home/adamw/acrobates/Global/GSHHS_shp/gshhs/gshhs_l.b",
  xlim=c(360+roi_ll@xmin,360+roi_ll@xmax),ylim=c(roi_ll@ymin,roi_ll@ymax),level=1)
coast=as(coast,"SpatialLines")
coast=spTransform(coast,psin)

roi=spTransform(roi,psin)
roil=as(roi,"SpatialLines")


### quantile function
gq=function(x,n=10,cut=F) {
  if(!cut) return(unique(quantile(x,seq(0,1,len=n+1),na.rm=T)))
  if(cut)  return(cut(x,unique(quantile(x,seq(0,1,len=n+1),na.rm=T))))
}
bgyr=colorRampPalette(c("brown","yellow","green","darkgreen","blue"))
X11.options(type="cairo")


png(paste("output/ModelComparisonRasters_",tile,"_%02d.png",sep=""),width=3000,height=2000,res=300,bg="transparent",type="cairo-png")

## COT climatologies
at=unique(seq(0,30,len=100))
p=levelplot(cot,xlab.top=title,at=at,col.regions=bgyr(length(at)))+layer(sp.lines(coast, lwd=1.2, col='black'))
print(p)

## all monthly predictions
p1=brick(stack(list.files(outdir,pattern="pred_.*_10.nc$",full=T),varname="ppt"))
projection(p1)=psin
names(p1)=month.name
title=""#"Interpolated Precipitation"
at=unique(quantile(as.matrix(p1),seq(0,1,len=100),na.rm=T))
at=unique(seq(min(p1@data@min),max(p1@data@max),len=100))
p=levelplot(p1,xlab.top=title,at=at,col.regions=bgyr(length(at)))#+layer(sp.lines(roil, lwd=1.2, col='black'))
print(p)

## compare two models
p2=brick(stack(list.files(outdir,pattern="pred_3_3.nc|pred_3_12.nc",full=T)[2:1],varname="ppt"))
projection(p2)=psin
names(p2)=c("X+Y+Elevation","X+Y+Elevation+MOD06")
at=unique(seq(min(p2@data@min),max(p2@data@max),len=100))
p=levelplot(p2,xlab.top=title,at=at,col.regions=bgyr(length(at)))+layer(sp.lines(coast, lwd=1.2, col='black'))
print(p)
dev.off()

png(paste("output/dem_",tile,".png",sep=""),width=3000,height=3000,res=300,bg="transparent")
at=unique(seq(min(dem@data@min),max(dem@data@max),len=100))
st=unique(coordinates(dm2))
levelplot(dem,col.regions=terrain.colors(100),at=at,margin=F)+
  layer(panel.xyplot(st[,1],st[,2],cex=.5,pch=3,col="black"))
dev.off()

pdf(paste("output/ModelComparison_",tile,".pdf",sep=""),width=11,height=5,useDingbats=F)

bwplot(fmodel~mean|metric,data=sums2,scale=list(x=list(relation="free")),subscripts=T,panel=function(x,y,subscripts){
  panel.abline(v=mean(x))
  types=models$type[match(sort(unique(y)),models$model)]
  panel.bwplot(x,y,fill=ifelse(types=="Spatial","grey",ifelse(types=="MOD35","dodgerblue","orangered")))
  #  panel.text(x,jitter(as.numeric(y),factor=.5),labels=as.character(sums2$month[subscripts]),col="grey40",cex=.3)
  panel.xyplot(x,y,pch=16,col="grey48",cex=.5)
},
                                        #strip=strip.custom(factor.levels=c("Validation R^2","Validation RMSE")),
       main="Comparison of Validation Metrics",sub="Vertical line indicates overall mean",
       key=list(space="right",rect=list(col=c("orangered","dodgerblue","grey")),text=list(c("MOD06","MOD35","Spatial"))))

dev.off()


### illustration of GAM for IBS poster
#mm
i=56
mm[i,]
tm=fitmod(i,predict=F,nv=1)

    ## partial residual plots
var=4
    fv <- predict(tm$mod,type="terms") ## get term estimates
    v=sub("[)]","",sub("s[(]","",colnames(fv)))[var]
    ## compute partial residuals for first smooth...
    prsd1 <- residuals(tm$mod,type="working") + fv[,var]

pdf(paste("output/GAMexample.pdf"),width=11,height=6,useDingbats=F)
plot(tm$mod,select=var,las=1,rug=F,cex.lab=2,cex.axis=2) ## plot first smooth
    points(tm$mod$model[,v],prsd1,pch=16,cex=.5,col="red")
dev.off()


xyplot(mean~month|metric,groups=model,type="l",data=sums,scale=list(y=list(relation="free")),auto.key=list(space="right"))#+layer(panel.xyplot(x,y,pch=16,col="grey"),under=T)+layer(panel.abline(v=mean(x)))

sums$type=as.factor(models$type[match(sums$model,models$model)])

xyplot(mean~month|metric,groups=model,type="l",data=sums2,panel=function(x,y,subscripts,groups){
  tsums=sums[subscripts,]
  panel.xyplot(x,y,groups=groups,type="l",subscripts=subscripts,col=ifelse(tsums$type=="Spatial","grey20",ifelse(tsums$type=="MOD35","blue","red")))
  panel.segments(tsums$month,tsums$Q2.5,tsums$month,tsums$Q97.5,groups=groups,subscripts=subscripts)
},subscripts=T,scale=list(y=list(relation="free")),
       key=list(space="right",text=list(c("Spatial","MOD35","MOD06")),lines=list(col=c("grey20","blue","red"))))

                                        #+layer(panel.xyplot(x,y,pch=16,col="grey"),under=T)+layer(panel.abline(v=mean(x)))


xyplot(pred~ppt|as.factor(month),groups=validation,data=pred,panel=function(x,y,subscripts,groups){
  panel.xyplot(x,y,groups=groups,subscripts=subscripts)
  panel.abline(0,1,col="red",lwd=2)
},scales=list(relation="free"),
       main="Predicted vs. Observed mean monthly precipitation at validation stations",
       sub="Line is y=x",auto.key=T)

## plot prediction rasters

p=raster(ncfile,varname="ppt")

plot3D(dem, maxpixels=100000,zfac=.5, drape=p, rev=FALSE, adjust=TRUE)



###################################################################
###################################################################

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








####################
####################  OLD JUNK BELOW!
####################


#levelplot(cer)#+points(x,y,data=dm2@data)

### extract MOD06 data for each station
stcer=extract(cer,dm2,fun=mean);colnames(stcer)=paste("cer_mean_",as.numeric(format(as.Date(cer@z$Date),"%m")),sep="")
stcerp=extract(cerp,dm2,fun=mean);colnames(stcerp)=paste("cerp_mean_",as.numeric(format(as.Date(cerp@z$Date),"%m")),sep="")
stcer20=extract(cer20,dm2,fun=mean);colnames(stcer20)=paste("cer20_mean_",as.numeric(format(as.Date(cer20@z$Date),"%m")),sep="")
stcot=extract(cot,dm2);colnames(stcot)=paste("cot_mean_",as.numeric(format(as.Date(cot@z$Date),"%m")),sep="")
stcld=extract(cld,dm2);colnames(stcld)=paste("cld_mean_",as.numeric(format(as.Date(cld@z$Date),"%m")),sep="")
stdem=extract(dem,dm2)
### generate new design matrix
mod06=cbind.data.frame(station=dm$station,stcer,stcerp,stcot,stcld,stcer20)
mod06l=melt(mod06,id.vars=c("station"));colnames(mod06l)[grep("value",colnames(mod06l))]="mod06"
mod06l[,c("variable","moment","month")]=do.call(rbind,strsplit(as.character(mod06l$variable),"_"))
mod06l=unique(mod06l)
mod06l=cast(mod06l,station+moment+month~variable,value="mod06")
mod06l=merge(dm2@data,mod06l,by=c("station","month"))
mod06l=mod06l[!is.na(mod06l$cer),]

mod06l=mod06l[order(mod06l$month),]
mod06l$lppt=log(mod06l$ppt+1)

#xyplot(latitude~longitude|as.factor(month),groups=cut(mod06l$ppt,breaks=quantile(mod06l$ppt,seq(0,1,len=10))),data=mod06l,auto.key=T,col=grey(seq(0,1,len=10)),pch=16,cex=.5)
#plot(cer)

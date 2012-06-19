#! /bin/r


#system("ssh -X turaco")
#export LD_LIBRARY_PATH=${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}/home/adamw/local/lib
#export KMP_DUPLICATE_LIB_OK=TRUE
#R
### Download and clean up GHCN data for CFR
#install.packages("ncdf4",lib="/home/adamw/rlib",configure.args="--with-nc-config=/home/adamw/local/bin/nc-config")

library(sp);library(rgdal);
library(reshape)
library(ncdf4)
library(geosphere)
library(rgeos)
library(multicore);library(RSQLite)
library(spBayes)
library(geoR) #for conventional kriging
library(raster)
## for review/analysis
library(coda)
library(lattice)
library(MBA)

###  Load functions
#source("code/interpolationfunc.r")

X11.options(type="Xlib")
ncores=20  #number of threads to use

setwd("/home/adamw/acrobates/projects/interp")

### read in full dataset
load("data/ghcn/roi_ghcn.Rdata")
load("stroi.Rdata")
source("code/GHCN_functions.r")

## rescale units to C and mm
d$value=d$value/10.0

## add flag for Benoit's training stations to identify training/validation
stt=readOGR("data/ghcn/tempdata/ghcn_1507_s.shp","ghcn_1507_s")$STAT_ID
d$fit=d$id%in%stt

### read in and subset to Benoit's dates
dates=as.Date(as.character(
  read.table("data/ghcn/tempdata/dates_interpolation_03052012.txt")$V1),"%Y%m%d")

##########################################################
#### load region of interest
  roi=readOGR("data/boundaries/statesp020.shp","statesp020")
  proj4string(roi)=CRS("+proj=longlat +datum=WGS84")
  roi=roi[roi$STATE=="Oregon",]
  roi=gUnionCascaded(roi)  #dissolve any subparts of roi
  
  ## distance from ROI to buffer for border stations
  dis=100 #km
  ## buffer roi (transform to azimuthal equidistant with centroid of roi, add 'dis' buffer, then transform back)
  roic=centroid(roi)
  
  roib=spTransform(
    gBuffer(spTransform(roi,
                        CRS(paste("+proj=aeqd +lat_0=",roic[2]," +lon_0=",
                                  roic[1]," +ellps=WGS84 +datum=WGS84 +units=m +no_defs ",sep=""))),
            width=dis*1000),
    CRS("+proj=longlat +datum=WGS84"))

### generate knots
knots=makegrid(roib,cellsize=0.5)
coordinates(knots)=c("x1","x2")
proj4string(knots)=proj4string(roi)
knots=knots[!is.na(overlay(knots,roi))]


###########################################################3
#### interpolation function

interp<-function(parms,niter=100,istart=60,ithin=1){
  ## load variables from parameter list
  idate=as.Date(parms$date)
  ivar=parms$variable
  type=parms$type
  model=parms$model
  
  ## subset single date from data
  td=d[d$date==idate,]
  td=data.frame(cast(td,id+fit~variable,value="value"))
  td$elev=stroi$elev[match(td$id,stroi$id)]
  td[c("lon","lat")]=stroi@data[match(td$id,stroi$id),c("lon","lat")]
  td$intercept=1

  ## drop any points without coordinates
  td=td[!is.na(td$lat)&!is.na(td$lon),]
  
###########################
  ## apply climate correction
  if(type=="anom"){
    cppt=brick("data/prism/prism_climate.nc",varname="ppt")
    cppt=subset(cppt,subset=as.numeric(format(idate,"%m")))
    cppt[cppt==-99.99]=NA
    td$cppt=as.numeric(extract(cppt,td[,c("lon","lat")],method="bilinear"))
    ## Tmax
    ctmax=brick("data/prism/prism_climate.nc",varname="tmax")
    ctmax=subset(ctmax,subset=as.numeric(format(idate,"%m")))
    ctmax[ctmax==-99.99]=NA
    td$ctmax=as.numeric(extract(ctmax,td[,c("lon","lat")],method="bilinear"))
    ## Tmin
    ctmin=brick("data/prism/prism_climate.nc",varname="tmin")
    ctmin=subset(ctmin,subset=as.numeric(format(idate,"%m")))
    ctmin[ctmin==-99.99]=NA
    td$ctmin=as.numeric(extract(ctmin,td[,c("lon","lat")],method="bilinear"))
    ## Calculate anomalies
    td$atmax=td$tmax-td$ctmax
    td$atmin=td$tmin-td$ctmin
    ## Scale for precipitation (add 1 to climate to avoid dividing by 0, will be subtracted off later)
    ## Add 1 to final value to avoid transformation troubles (geoR will only transform if >0
    td$appt=(td$ppt/(ifelse(td$cppt<0,0,td$cppt)+1))+1
  }

### Add region flag to data
  td$region=ifelse(!is.na(overlay(SpatialPoints(td[,c("lon","lat")]),roi)),"roi","roib")
  td=td[!is.na(overlay(SpatialPoints(td[,c("lon","lat")]),roib)),]

#### Extract data for validation
fd=td[td$fit&td$region=="roi",]  #fitting dataset 
pd=td[!td$fit&td$region=="roi",] #training dataset
od=td[!td$fit&td$region!="roi",] #stations outside roi


### drop missing values
fd=fd[!is.na(fd[,as.character(ivar)]),]  #&!is.na(fd$tmin)

### define the formula
if(type=="raw"&ivar=="tmax")  f1=formula(tmax~intercept)
if(type=="anom"&ivar=="tmax"&model=="intercept")  f1=formula(atmax~intercept)
if(type=="anom"&ivar=="tmax"&model=="full")  f1=formula(atmax~lon+lat+elev)
  
### Run the model
  t1=Sys.time()
  m1=spLM(f1,data=fd,coords=as.matrix(fd[,c("lon","lat")]), 
    starting=list("phi"=2.5,"sigma.sq"=4, "tau.sq"=1),
    sp.tuning=list("phi"=0.8, "sigma.sq"=0.1, "tau.sq"=0.2),
    priors=list("phi.Unif"=c(0.1, 4), "sigma.sq.IG"=c(2, 1),
      "tau.sq.IG"=c(2, 1)),
    cov.model="exponential",
    knots=coordinates(knots),
    n.samples=niter, verbose=TRUE, n.report=1000)
  t2=Sys.time()

  
#m1s=mcmc(m1$p.samples)
#summary(m1s)
#xyplot(m1s,scales=list(y=list(rot=0)),main="Posterior Samples of Model Parameters")

### Run the model
#q=3
#nltr=q*(q-1)/2+q

#m1=spMvLM(list(atmin~1,atmax~1),data=fd,coords=as.matrix(fd[,c("lon","lat")]), 
#  starting=list("phi"=0.6,"sigma.sq"=1, "tau.sq"=1),
#  sp.tuning=list("phi"=0.01, "sigma.sq"=0.05, "tau.sq"=0.05),
#  priors=list("phi.Unif"=c(0.1, 3), "sigma.sq.IG"=c(2, 1),
#    "tau.sq.IG"=c(2, 1)),
#  cov.model="exponential",
#  knots=coordinates(knots),
#  n.samples=100, verbose=TRUE, n.report=100)

#save to play with later
#save(m1,file="m1.Rdata")


#load("m1.Rdata")

##############################
#### make predictions

### Prediction grid
#  pgrid=data.frame(coordinates(ctmax))
#  ## crop to bbox
#  pgrid=pgrid[pgrid$x>=bbox(roi)[1,1]&pgrid$x<=bbox(roi)[1,2]
#    &pgrid$y<=bbox(roi)[2,2]&pgrid$y>=bbox(roi)[2,1],]
#  ##  Crop to ROI polygon?
#  #pgrid=pgrid[!is.na(overlay(SpatialPoints(pgrid),roi)),]
#  rownames(pgrid)=1:nrow(pgrid)
#  ncluster=200
#  pgrid$cluster=cut(as.numeric(rownames(pgrid)),ncluster,labels=1:ncluster)
#  ## predict for the full grid
#  m1pg=lapply(1:ncluster,function(i){
#    print(paste("Starting cluster:",i))
#    m1pg=spPredict(m1,pred.coords=pgrid[pgrid$cluster==i,c("x","y")],
#              pred.covars=cbind(intercept=rep(1,nrow(pgrid[pgrid$cluster==i,]))),start=istart,thin=ithin)
#    ## Generate summaries of y.pred
#    ty=t(apply(m1pg$y.pred,1,function(i) c(mean=mean(i),sd=sd(i),quantile(i,c(0.025,0.5,0.975)))))
#    colnames(ty)=paste("y.",c("mean","sd","Q2.5","Q50","Q97.5"),sep="")
 #   ## Generate summaries of w.pred
  #  tw=t(apply(m1pg$w.pred,1,function(i) c(mean=mean(i),sd=sd(i),quantile(i,c(0.025,0.5,0.975)))))
  #  colnames(tw)=paste("w.",c("mean","sd","Q2.5","Q50","Q97.5"),sep="")
#    return(cbind(ty,tw))
#  })
#  m1pgs=do.call(rbind.data.frame,m1pg)
  
  
########### Predict only to validation locations
m1p=spPredict(m1,pred.coords=pd[,c("lon","lat")],pred.covars=model.matrix(lm(f1,data=pd)),start=istart,thin=ithin)#, 
  
m1p.y=mcmc(t(m1p$y.pred),start=istart,thin=ithin)
m1p.w=mcmc(t(m1$sp.effects.knots),start=istart,thin=ithin)
m1p.ys=t(apply(m1p.y,2,quantile,c(0.025,0.5,0.975),na.rm=T))
m1p.ws=t(apply(m1p.w,2,quantile,c(0.025,0.5,0.975),na.rm=T))

### recover original scale if necessary
 resp=as.character(attr(terms.formula(f1),"variables"))[2]
  ## if modeling anomalies, add the climate back on
  if(type=="anom") {
    pd2=cbind.data.frame(pd,aQ2.5=m1p.ys[,"2.5%"],aQ50=m1p.ys[,"50%"],aQ97.5=m1p.ys[,"97.5%"])
# FIXME!  ctmax below needs to be made general for other variables!
    pd2$Q2.5=pd2$ctmax+pd2$aQ2.5
    pd2$Q50=pd2$ctmax+pd2$aQ50
    pd2$Q97.5=pd2$ctmax+pd2$aQ97.5
  }
  ## if modeling raw values, don't add the climate back on
    if(type!="anom") {
    pd2=cbind.data.frame(pd,Q2.5=m1p.ys[,"2.5%"],Q50=m1p.ys[,"50%"],Q97.5=m1p.ys[,"97.5%"])
  }


    ### save model output
  save(m1,m1p,pd2,file=paste("output/modeloutput_",
                gsub("-","",idate),"_",ivar,"_",type,"_",model,".Rdata",sep=""))



####################################
#### Accuracy Assement
m1.dic=spDiag(m1,start=istart,thin=ithin);m1.dic
m1.accuracy=accuracy(pd2[,as.character(ivar)],pd2$Q50,ppt=ivar=="ppt")$summary

  #### Write summary file
  m1.summary=cbind.data.frame(date=idate,model=model,var=ivar,type=type,
    formula=paste(terms(f1),collapse=" "),
    t(m1.dic$DIC),t(m1.accuracy),n.iter=niter,runtime.hours=as.numeric(difftime(t1,t2,units="hours")))
  write.csv(m1.summary,file=paste("output/modeloutput_summary_",gsub("-","",idate),"_",ivar,"_",type,".csv",sep=""))
  
##################################################
### some summary plots
pdf(width=11,height=8.5,file=paste("output/modeloutput_",gsub("-","",idate),"_",ivar,"_",type,".pdf",sep=""))

## Show knots and stations
plot(roib);axis(1);axis(2)
plot(roi,add=T)
#points(knots,pch=3,cex=.5)
points(unique(fd[,c('lon','lat')]),cex=.8,pch=16,col="red")
  points(unique(pd[,c('lon','lat')]),cex=.5,pch=16,col="blue")
points(unique(od[,c('lon','lat')]),cex=.5,pch=16,col="black")
  legend(-118,42,legend=c("Fitting","Validation","Border Stations"),pch=c(16,16,16),col=c("red","blue","black"),bg="white")
  
### Chain behavior
  m1s=mcmc(m1$p.samples)
  print(xyplot(m1s,scales=list(y=list(rot=0)),main="All Posterior Samples of Model Parameters"))
  m1s=mcmc(m1$p.samples,start=istart,thin=ithin)
  print(xyplot(m1s,scales=list(y=list(rot=0)),main="Post-burnin, thinned, Posterior Samples of Model Parameters"))


### Pred vs. obs  plots
plot(pd2[,as.character(ivar)],pd2$Q50,pch=16,cex=.8,ylim=quantile(c(pd2$Q2.5,pd2$Q97.5),c(0.01,0.995)),xlim=quantile(c(pd2$Q2.5,pd2$Q97.5),c(0.01,0.995)))
segments(pd2[,as.character(ivar)],pd2$Q2.5,pd2[,as.character(ivar)],pd2$Q97.5,col="grey")
points(pd2[,as.character(ivar)],pd2$Q50,pch=16,cex=.8);abline(0,1,col="red") #xlim=c(0,10)

### Maps
par(mfrow=c(1,3))
## observed
obs.surf <- mba.surf(cbind(fd[,c("lon","lat")],fd$tmax), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image(obs.surf, xaxs = "r", yaxs = "r", main="Observed response",asp=1)
points(fd[,c("lon","lat")])
plot(roi,add=T)
#contour(obs.surf, add=T)
plot(roi,add=T,border="darkgreen",lwd=3)
## spatial effects
  w.pred.surf <-
mba.surf(cbind(coordinates(knots), m1p.ws[,3]), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image(w.pred.surf, xaxs = "r", yaxs = "r", main="Spatial Effects",asp=1)
#points(pd[,c("lon","lat")], pch=1, cex=1)
points(knots, pch=3, cex=1)
#contour(w.pred.surf, add=T)
plot(roi,add=T,border="darkgreen",lwd=3)
legend(1.5,2.5, legend=c("Obs.", "Knots", "Pred."),
pch=c(1,3,19), bg="white")
## fitted
y.pred.surf <-
mba.surf(cbind(pd2[,c("lon","lat")], pd2$Q50), no.X=100, no.Y=100, extend=TRUE)$xyz.est
image(y.pred.surf, xaxs = "r", yaxs = "r", main="Predicted response",asp=1)
points(pd[,c("lon","lat")], pch=1, cex=1)
#points(knots, pch=3, cex=1)
#contour(y.pred.surf, add=T)
plot(roi,add=T,border="darkgreen",lwd=3)
legend(1.5,2.5, legend=c("Obs.", "Knots", "Pred."),
pch=c(1,3,19), bg="white")

dev.off()

  ### Return objects
  return(m1.summary)
}



###########################################################
### define models to run
ms=expand.grid(variable="tmax",type=c("anom","raw"),date=dates,model=c("intercept","full"))
## drop some
ms[!ms$type=="raw"&ms$model=="intercept",]


msl=apply(ms,1,as.list)


### test it out
parms=msl[[9]]
interp(parms,niter=5000,istart=2000,ithin=3)

#mresults=lapply(msl[1:3],interp)

### run it!
mresults =mclapply(msl,interp,niter=1000,istart=500,ithin=1,mc.cores=1)

### drop any errors
mresults=mresults[!sapply(mresults,function(x) grepl("Error",x[[1]]))]

res=do.call(rbind.data.frame,mresults)

save(mresults,res,file="output/mresults.Rdata")

q("no")






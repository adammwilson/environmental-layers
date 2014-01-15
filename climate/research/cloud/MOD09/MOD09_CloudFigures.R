### Figures and tables for MOD09 Cloud Manuscript

setwd("~/acrobates/adamw/projects/cloud/")


## libraries
library(rasterVis)
library(latticeExtra)
library(xtable)
library(texreg)
library(reshape)
library(caTools)
library(rgeos)

## read in data
#cld=read.csv("data/NDP026D/cld.csv")
cldm=read.csv("data/NDP026D/cldm.csv")
#cldy=read.csv("data/NDP026D/cldy.csv")
#clda=read.csv("data/NDP026D/clda.csv")
st=read.csv("data/NDP026D/stations.csv")


## add lulc factor information
require(plotKML); data(worldgrids_pal)  #load IGBP palette
IGBP=data.frame(ID=0:16,col=worldgrids_pal$IGBP[-c(18,19)],stringsAsFactors=F)
IGBP$class=rownames(IGBP);rownames(IGBP)=1:nrow(IGBP)

## month factors
#cld$month2=factor(cld$month,labels=month.name)
cldm$month2=factor(cldm$month,labels=month.name)

coordinates(st)=c("lon","lat")
projection(st)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

##make spatial object
#cldys=cldy
#coordinates(cldys)=c("lon","lat")
#projection(cldys)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#### Evaluate MOD35 Cloud data
mod09=brick("data/cloud_monthly.nc")
mod09s=brick("data/cloud_yseasmean.nc",varname="CF");names(mod09s)=c("DJF","MAM","JJA","SON")
mod09c=brick("data/cloud_ymonmean.nc",varname="CF");names(mod09c)=month.name
mod09a=brick("data/cloud_mean.nc",varname="CF");names(mod09a)="Mean Annual Cloud Frequency"

mod09min=brick("data/cloud_min.nc",varname="CF")
mod09max=brick("data/cloud_max.nc",varname="CF")
mod09sd=brick("data/cloud_std.nc",varname="CFsd")
mod09metrics=stack(mod09a,mod09min,mod09max,mod09sd)
names(mod09metrics)=c("Mean","Minimum","Maximum","Standard Deviation")

#plot(mod09a,layers=1,margin=F,maxpixels=100)

## calculated differences
cldm$difm=cldm$mod09-cldm$cld_all
cldm$difs=cldm$mod09sd+cldm$cldsd_all

#clda$dif=clda$mod09-clda$cld

## read in global coasts for nice plotting
library(maptools)
library(rgdal)

#coast=getRgshhsMap("/mnt/data/jetzlab/Data/environ/global/gshhg/gshhs_h.b", xlim = NULL, ylim = NULL, level = 4) 
land=readShapePoly("/mnt/data/jetzlab/Data/environ/global/gshhg/GSHHS_shp/c/GSHHS_c_L1.shp",force_ring=TRUE)
projection(land)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
CP <- as(extent(-180, 180, -60, 84), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(land))
coast=as(land[land$area>50,],"SpatialLines")
## Clip the map
land <- gIntersection(land, CP, byid=F)
coast <- gIntersection(coast, CP, byid=F)

## get stratified sample of points from biomes for illustration
## if(!file.exists("output/biomesamplepoints.csv")){
##     n_biomesamples=1000
##     library(multicore)
##     biomesample=do.call(rbind.data.frame,mclapply(1:length(biome),function(i)
##         data.frame(biome=biome$BiomeID[i],coordinates(spsample(biome[i,],n=n_biomesamples,type="stratified",nsig=2)))))
##     write.csv(biomesample,"output/biomesamplepoints.csv",row.names=F)
## }
## biomesample=read.csv("output/biomesamplepoints.csv")
## coordinates(biomesample)=c("x1","x2")

#biomesample=extract(biomesample,mod09c)

## Figures
n=100
  at=seq(0,100,length=n)
colr=colorRampPalette(c("black","green","red"))
cols=colr(n)


#pdf("output/Figures.pdf",width=11,height=8.5)
png("output/CF_Figures_%03d.png",width=5000,height=4000,res=600,pointsize=36,bg="transparent")

res=1e4
greg=list(ylim=c(-60,84),xlim=c(-180,180))
    
## Figure 1: 4-panel summaries
#- Annual average
levelplot(mod09a,col.regions=colr(n),cuts=100,at=seq(0,100,len=100),colorkey=list(space="bottom",adj=1),
  margin=F,maxpixels=res,ylab="",xlab="",useRaster=T,ylim=greg$ylim)+
  layer(sp.lines(coast,col="black"),under=F)
## Mean annual with validation stations
levelplot(mod09a,col.regions=colr(n),cuts=100,at=seq(0,100,len=100),colorkey=list(space="bottom",adj=1),
  margin=F,maxpixels=res,ylab="",xlab="",useRaster=T,ylim=greg$ylim)+
  layer(panel.xyplot(lon,lat,pch=16,cex=.3,col="black"),data=data.frame(coordinates(st)))+
  layer(sp.lines(coast,col="black"),under=F)

## Seasonal Means
levelplot(mod09s,col.regions=colr(n),cuts=100,at=seq(0,100,len=100),colorkey=list(space="bottom",adj=2),
  margin=F,maxpixels=res,ylab="",xlab="",useRaster=T,ylim=greg$ylim)+
  layer(sp.lines(coast,col="black"),under=F)

## four metics
levelplot(mod09metrics,col.regions=colr(n),cuts=100,at=seq(0,100,len=100),colorkey=list(space="bottom",adj=2),
  margin=F,maxpixels=res,ylab="",xlab="",useRaster=T,ylim=greg$ylim)+
  layer(sp.lines(coast,col="black"),under=F)

## Monthly Means
levelplot(mod09c,col.regions=colr(n),cuts=100,at=seq(0,100,len=100),colorkey=list(space="bottom",adj=1),
  margin=F,maxpixels=res,ylab="Latitude",xlab="Longitude",useRaster=T,ylim=greg$ylim)+
  layer(sp.lines(coast,col="black"),under=F)

#- Monthly minimum
#- Monthly maximum
#- STDEV or Min-Max
p_mac=levelplot(mod09a,col.regions=colr(n),cuts=99,at=seq(0,100,length=100),margin=F,maxpixels=res/10,colorkey=list(space="bottom",height=.75),xlab="",ylab="",useRaster=T)+
      layer(sp.lines(coast,col="black"),under=F)
p_min=levelplot(mod09min,col.regions=colr(n),cuts=99,margin=F,maxpixels=res/10,colorkey=list(space="bottom",height=.75),useRaster=T)+
      layer(sp.lines(coast,col="black"),under=F)
p_max=levelplot(mod09max,col.regions=colr(n),cuts=99,margin=F,maxpixels=res/10,colorkey=list(space="bottom",height=.75),useRaster=T)+
      layer(sp.lines(coast,col="black"),under=F)
p_sd=levelplot(mod09sd,col.regions=colr(n),cuts=99,at=seq(0,100,length=100),margin=F,maxpixels=res/10,colorkey=list(space="bottom",height=.75),useRaster=T)+
      layer(sp.lines(coast,col="black"),under=F)
p3=c("Mean Cloud Frequency (%)"=p_mac,"Max Cloud Frequency (%)"=p_max,"Min Cloud Frequency (%)"=p_min,"Cloud Frequency Variability (SD)"=p_sd,x.same=T,y.same=T,merge.legends=T,layout=c(2,2))
print(p3)

bgr=function(x,n=100,br=0,c1=c("darkblue","blue","grey"),c2=c("grey","red","purple")){
    at=unique(c(seq(min(x,na.rm=T),max(x,na.rm=T),len=n)))
    bg=colorRampPalette(c1)
    gr=colorRampPalette(c2)
    return(list(at=at,col=c(bg(sum(at<br)),gr(sum(at>=br)))))
}

colat=bgr(cldm$difm)
phist=histogram(cldm$difm,breaks=colat$at,border=NA,col=colat$col,xlim=c(-50,40),type="count",xlab="Difference (MOD09-NDP026D)")#,seq(0,1,len=6),na.rm=T)
pmap=xyplot(lat~lon|month2,data=cldm,groups=cut(cldm$difm,rev(colat$at)),
       par.settings=list(superpose.symbol=list(col=colat$col)),pch=16,cex=.25,
       auto.key=F,#list(space="right",title="Difference\n(MOD09-NDP026D)",cex.title=1),asp=1,
       ylab="Latitude",xlab="Longitude")+
  layer(sp.lines(coast,col="black",lwd=.1),under=F)
print(phist,position=c(0,.75,1,1),more=T)
print(pmap,position=c(0,0,1,.78))

### heatmap of mod09 vs. NDP for all months
hmcols=colorRampPalette(c("grey","blue","red","purple"))
#hmcols=colorRampPalette(c(grey(.8),grey(.3),grey(.2)))
tr=c(0,80)
colkey <- draw.colorkey(list(col = hmcols(tr[2]), at = tr[1]:tr[2],height=.25))

xyplot(cld_all~mod09|month2,data=cldm,panel=function(x,y,subscripts){
  n=50
  bins=seq(0,100,len=n)
  tb=melt(as.matrix(table(
    x=cut(x,bins,labels=bins[-1]),
    y=cut(y,bins,labels=bins[-1]))))
  qat=unique(tb$value)
  print(max(qat))
  qat=tr[1]:tr[2]#unique(tb$value)
  panel.levelplot(tb$x,tb$y,tb$value,at=qat,col.regions=c("transparent",hmcols(length(qat))),subscripts=1:nrow(tb))
#  panel.abline(0,1,col="black",lwd=2)
  panel.abline(lm(y ~ x),col="black",lwd=2)
#  panel.ablineq(lm(y ~ x), r.sq = TRUE,at = 0.6,pos=1, offset=0,digits=2,col="blue")
  panel.text(70,10,bquote(paste(R^2,"=",.(round(summary(lm(y ~ x))$r.squared,2)))),cex=1.2)
},asp=1,scales=list(at=seq(0,100,len=6),useRaster=T,colorkey=list(width=.5,title="Number of Stations")),
          ylab="NDP Mean Cloud Amount (%)",xlab="MOD09 Cloud Frequency (%)",
              legend= list(right = list(fun = colkey)))#+ layer(panel.abline(0,1,col="black",lwd=2))


## Monthly Climatologies
## for(i in 1:2){
##  p1=xyplot(cld~mod09|month2,data=cldm,cex=.2,pch=16,subscripts=T,ylab="NDP Mean Cloud Amount",xlab="MOD09 Cloud Frequency (%)")+
##   layer(panel.lines(1:100,predict(lm(y~x),newdata=data.frame(x=1:100)),col="green"))+
##   layer(panel.lines(1:100,predict(lm(y~x+I(x^2)),newdata=data.frame(x=1:100)),col="blue"))+
##   layer(panel.abline(0,1,col="red"))
##     if(i==2){
##      p1=p1+layer(panel.segments(mod09[subscripts],cld[subscripts]-cldsd[subscripts],mod09[subscripts],cld[subscripts]+cldsd[subscripts],subscripts=subscripts,col="grey"),data=cldm,under=T,magicdots=T)
##      p1=p1+layer(panel.segments(mod09[subscripts]-mod09sd[subscripts],cld[subscripts],mod09[subscripts]+mod09sd[subscripts],cld[subscripts],subscripts=subscripts,col="grey"),data=cldm,under=T,magicdots=T) 
##        }
## print(p1)
## }

bwplot(lulcc~difm,data=cldm,horiz=T,xlab="Difference (MOD09-Observed)",varwidth=T,notch=T)+layer(panel.abline(v=0))
bwplot(biome~difm,data=cldm,horiz=T,xlab="Difference (MOD09-Observed)",varwidth=T,notch=T)+layer(panel.abline(v=0))

#library(BayesFactor)
#coplot(difm~month2|lulcc,data=cldm[!is.na(cldm$dif)&!is.na(cldm$lulcc),],panel = panel.smooth)


dev.off()

####################################################################
### Regional Comparisons
## Compare with worldclim and NPP
#wc=stack(as.list(paste("/mnt/data/jetzlab/Data/environ/global/worldclim/prec_",1:12,".bil",sep="")))
wc_map=stack(as.list(paste("/mnt/data/jetzlab/Data/environ/global/worldclim/bio_12.bil",sep="")))


pdf("output/mod09_worldclim.pdf",width=11,height=8.5)
regs=list(
  Cascades=extent(c(-122.8,-118,44.9,47)),
  Hawaii=extent(c(-156.5,-154,18.75,20.5)),
  Boliva=extent(c(-71,-63,-20,-15)),
  Venezuela=extent(c(-69,-59,0,7)),
  CFR=extent(c(17.75,22.5,-34.8,-32.6)),
  Madagascar=extent(c(46,52,-17,-12))
  #reg2=extent(c(-81,-70,-4,10))
  )
for(r in 1:length(regs)){
tmap=crop(wc_map,regs[[r]])
p_map=levelplot(tmap,col.regions=grey(seq(0,1,len=100)),cuts=100,at=seq(tmap@data@min,tmap@data@max,len=100),margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.75),xlab="",ylab="",main=names(regs)[r],useRaster=T)
tmac=crop(mod09a,regs[[r]])
p_mac=levelplot(tmac,col.regions=grey(seq(0,1,len=100)),cuts=100,at=seq(tmac@data@min,tmac@data@max,len=100),margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.75),useRaster=T)
#p_npp=levelplot(crop(npp,regs[[r]]),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.5),zscaleLog=T,useRaster=T)  #"NPP"=p_npp,
p3=c("MOD09 Cloud Frequency (%)"=p_mac,"WorldClim Mean Annual Precip (mm)"=p_map,x.same=T,y.same=T,merge.legends=T,layout=c(2,1))
print(p3)
}
dev.off()


## reduced resolution

## read in GEWEX 1-degree data
gewex=mean(brick("data/gewex/CA_PATMOSX_NOAA.nc",varname="a_CA"))

mod09_8km=aggregate(mod09_mac,8)

pdf("output/mod09_resolution.pdf",width=11,height=8.5)
p1=levelplot(mod09_mac,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e5)
#p2=levelplot(mod09_8km,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e5)
p3=levelplot(gewex,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e5)
print(c(p1,p3,x.same=T,y.same=T,merge.legends=F))

p1=levelplot(crop(mac,regs[["Venezuela"]]),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e5)
#p2=levelplot(crop(mod09_8km,reg2),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e5)
p3=levelplot(crop(gewex,regs[["Venezuela"]]),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e5)
print(c(MOD09=p1,GEWEX=p3,x.same=T,y.same=T,merge.legends=F))

p1=levelplot(crop(mod09_mac,reg3),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e5)
#p2=levelplot(crop(mod09_8km,reg3),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e5)
p3=levelplot(crop(mod09_1deg,reg3),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e5)
print(c(p1,p3,x.same=T,y.same=T,merge.legends=F))

dev.off()



## Validation table construction
quantile(cldm$difm,na.rm=T)

summary(lm(cld_all~mod09+lat,data=cldm))

               
## assess latitude bias
cldm$abslat=abs(cldm$lat)
cldm$absdif=abs(cldm$difm)

###################################################################
### validation by biome
bs=biome$BiomeID
mod_bs=lapply(bs,function(bs) lm(cld_all~mod09,data=cldm[cldm$biome==bs,]))
names(mod_bs)=biome$Biome

bt=do.call(rbind,tapply(cldm$difm,list(cldm$month,cldm$biome),function(x) c(n=length(x),mean=round(mean(x,na.rm=T),1),sd=round(sd(x,na.rm=T),1))))
bt
cast(cldm,biome~month,fun=function(x) paste(round(mean(x,na.rm=T),1)," (",round(sd(x,na.rm=T),1),")",sep=""),value="difm")
melt(cast(cldm,biome~month,fun=function(x) c(mean=mean(x,na.rm=T),sd=sd(x,na.rm=T)),value="difm"))

bt=melt(cast(cldm,biome~month2,fun=function(x) c(mean=mean(x,na.rm=T),sd=sd(x,na.rm=T)),value="mod09"))
xyplot(value~month2,data=bt)

print(xtable(bt),type="html")

#lm_all=lm(cld_all~mod09,data=cldm[!is.na(cldm$cld),])
lm_mod=lm(cld~mod09+biome,data=cldm)

screenreg(lm_mod,digits=2,single.row=F)

####################################################################
## assess temporal stability

## spatialy subset data to stations at least 10km apart
st2=remove.duplicates(st,zero=10)

## Subset data
## drop missing observations
cldm.t=cldm[!is.na(cldm$cld_all)&!is.na(cldm$mod09)&!is.na(cldm$biome),]
cldm.t=cldm.t[cldm.t$lat>=-60,]
#  make sure all stations have all mod09 data
stdrop=names(which(tapply(cldm.t$month,cldm.t$StaID,length)!=12))
cldm.t=cldm.t[!cldm.t$StaID%in%stdrop,]
# Keep only stations at least 10km apart 
cldm.t=cldm.t[cldm.t$StaID%in%st2$id,]
## Subset to only some months, if desired
#cldm.t=cldm.t[cldm.t$month%in%1:3,]


## Select Knots
knots=spsample(land,500,type="regular")

                                        #  reshape data
m.cld=cast(cldm.t,StaID+lat+lon+biome~month,value="cld_all");colnames(m.cld)[-(1:4)]=paste("cld.",colnames(m.cld)[-(1:4)],sep="")
m.mod09=cast(cldm.t,StaID~month,value="mod09");colnames(m.mod09)[-1]=paste("mod09.",colnames(m.mod09)[-1],sep="")
mdata=cbind(m.cld,m.mod09)

## cast to 
coords <- as.matrix(m.cld[,c("lon","lat")])#as.matrix(ne.temp[,c("UTMX", "UTMY")]/1000)
max.d <- max(iDist(coords))

##make symbolic model formula statement for each month
mods <- lapply(paste(paste(paste("cld.",1:N.t,sep=''),paste("mod09.",1:N.t,sep=''),sep='~'),"",sep=""), as.formula)

tlm=model.matrix(lm(mods[[1]],data=mdata))

N.t <- ncol(m.mod09)-1 ##number of months
n <- nrow(m.cld) ##number of observation per months
p <- ncol(tlm) #number of regression parameters in each month

starting <- list("beta"=rep(0,N.t*p), "phi"=rep(3/(0.5*max.d), N.t),
                 "sigma.sq"=rep(2,N.t), "tau.sq"=rep(1, N.t),
                 "sigma.eta"=diag(rep(0.01, p)))
tuning <- list("phi"=rep(5, N.t))

priors <- list("beta.0.Norm"=list(rep(0,p), diag(1000,p)),
               "phi.Unif"=list(rep(3/(0.9*max.d), N.t), rep(3/(0.05*max.d), N.t)),
               "sigma.sq.IG"=list(rep(2,N.t), rep(10,N.t)),
               "tau.sq.IG"=list(rep(2,N.t), rep(5,N.t)),
               "sigma.eta.IW"=list(2, diag(0.001,p)))
cov.model <- "exponential"

## Run the model
n.samples <- 500
m.1=spDynLM(mods,data=mdata,coords=coords,knots=coordinates(knots),n.samples=n.samples,starting=starting,tuning=tuning,priors=priors,cov.model=cov.model,get.fitted=T,n.report=25)

save(m.1,file="output/m.1.Rdata")
## summarize
burn.in <- floor(0.75*n.samples)
quant <- function(x){quantile(x, prob=c(0.5, 0.025, 0.975))}
beta <- apply(m.1$p.beta.samples[burn.in:n.samples,], 2, quant)
beta.0 <- beta[,grep("Intercept", colnames(beta))]
beta.1 <- beta[,grep("mod09", colnames(beta))]









library(texreg)
 extract.lm <- function(model) {
     s <- summary(model)
     names <- rownames(s$coef)
     co <- s$coef[, 1]
     se <- s$coef[, 2]
     pval <- s$coef[, 4]
     rs <- s$r.squared
     n <- nobs(model)
     rmse=sqrt(mean((residuals(s)^2)))
     gof <- c(rs, rmse, n)
     gof.names <- c("R-Squared","RMSE","n")
     tr <- createTexreg(coef.names = names, coef = co, se = se, 
                        pvalues = pval, gof.names = gof.names, gof = gof)
     return(tr)
 }
setMethod("extract", signature = className("lm", "stats"),definition = extract.lm)

forms=c("cld~mod09+month2+lat")
lm_all=lm(cld_all~mod09+lat,data=cldm[!is.na(cldm$cld),])


### Compare two time periods
lm_all1=lm(cld_all~mod09,data=cldm[!is.na(cldm$cld),])
lm_mod=lm(cld~mod09,data=cldm)
mods=list("1970-2000 complete"=lm_all1,"2000-2009"=lm_mod)

screenreg(mods,digits=2,single.row=T,custom.model.names=names(mods))
htmlreg(mods,file = "output/tempstab.doc",
        custom.model.names = names(mods),
        single.row = T, inline.css = FALSE,doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)



abslm=lm(absdif~abslat:I(abslat^2),data=cldm)

plot(absdif~abslat,data=cldm,cex=.25,pch=16)
lines(0:90,predict(abslm,newdata=data.frame(abslat=0:90),type="response"),col="red")

bf=anovaBF(dif~lulcc+month2,data=cldm[!is.na(cldm$dif)&!is.na(cldm$lulcc),])
ch=posterior(bf, iterations = 1000)
summary(bf)
plot(bf)

## explore validation error
cldm$lulcc=as.factor(IGBP$class[match(cldm$lulc,IGBP$ID)])

## Table of RMSE's by lulc by month
lulctl=ddply(cldm,c("month","lulc"),function(x) c(count=nrow(x),rmse=sqrt(mean((x$mod09-x$cld)^2,na.rm=T))))
lulctl=lulctl[!is.na(lulctl$lulc),]
lulctl$lulcc=as.factor(IGBP$class[match(lulctl$lulc,IGBP$ID)])

lulctl=ddply(cldm,c("lulc"),function(x) c(count=nrow(x),mean=paste(round(mean(x$difm,na.rm=T),2)," (",round(sd(x$difm,na.rm=T),2),")",sep=""),rmse=round(sqrt(mean((x$difm)^2,na.rm=T)),2)))
lulctl$lulcc=as.factor(IGBP$class[match(lulctl$lulc,IGBP$ID)])
    print(xtable(lulctl[order(lulctl$rmse),c("lulcc","count","mean","rmse")],digits=1),type="html",include.rownames=F,file="output/lulcc.doc",row.names=F)
    

lulcrmse=cast(lulcrmsel,lulcc~month,value="rmse")
lulcrmse

lulcrmse.q=round(do.call(rbind,apply(lulcrmse,1,function(x) data.frame(Min=min(x,na.rm=T),Mean=mean(x,na.rm=T),Max=max(x,na.rm=T),SD=sd(x,na.rm=T)))),1)#quantile,c(0.025,0.5,.975),na.rm=T)),1)
lulcrmse.q=lulcrmse.q[order(lulcrmse.q$Mean,decreasing=T),]
lulcrmse.q

print(xtable(lulcrmse,digits=1),"html")

bgyr=colorRampPalette(c("blue","green","yellow","red"))
levelplot(rmse~month*lulcc,data=lulcrmsel,col.regions=bgyr(1000),at=quantile(lulcrmsel$rmse,seq(0,1,len=100),na.rm=T))


### Linear models
summary(lm(dif~as.factor(lulc)+lat+month2,data=cldm))


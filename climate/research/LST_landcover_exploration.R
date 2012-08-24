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
library(rgl)                                                                                                                                                                         
library(hdf5)                                                                                                                                                                        
library(rasterVis)                                                                                                                                                                   
library(heR.Misc)
library(spBayes)
library(xtable)
library(ellipse) # for correlation matrix
library(maptools) # for rgshhs

X11.options(type="X11")

ncores=20  #number of threads to use


### copy lulc data to litoria
#setwd("data/lulc")
#system("scp atlas:/home/parmentier/data_Oregon_stations/W_Layer* .")


setwd("/home/adamw/acrobates/projects/interp")       

projs=CRS("+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
months=format(ISOdate(2004,1:12,1),"%b")

### load station data, subset to stations in region, and transform to sinusoidal
load("data/ghcn/roi_ghcn.Rdata")
load("data/allstations.Rdata")

d2=d[d$variable=="tmax"&d$date>=as.Date("2000-01-01"),]
d2=d2[,-grep("variable",colnames(d2)),]
st2=st[st$id%in%d$id,]
st2=spTransform(st2,projs)
d2[,c("lon","lat")]=coordinates(st2)[match(d2$id,st2$id),]
d2$elev=st2$elev[match(d2$id,st2$id)]
d2$month=format(d2$date,"%m")
d2$value=d2$value/10 #convert to mm


## load topographical data
topo=brick(as.list(list.files("data/regions/oregon/topo",pattern="SRTM.*rst$",full=T)))
topo=calc(topo,function(x) ifelse(x<0,NA,x))
names(topo)=c("aspect","dem","slope")
colnames(topo@data@values)=c("aspect","dem","slope")
projection(topo)=projs
NS=sin(subset(topo,subset="slope")*pi/180)*cos(subset(topo,subset="aspect")*pi/180);names(NS)="northsouth"
EW=sin(subset(topo,subset="slope")*pi/180)*sin(subset(topo,subset="aspect")*pi/180);names(EW)="eastwest"
slope=sin(subset(topo,subset="slope")*pi/180);names(slope)="slope"

## load coastline data
roill=bbox(projectExtent(topo,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
coast=getRgshhsMap("/home/adamw/acrobates/Global/GSHHS_shp/gshhs/gshhs_f.b",xlim=c(roill[1,1],roill[1,2]),ylim=c(roill[2,1],roill[2,2]))
coast=as(as(coast,"SpatialLines"),"SpatialLinesDataFrame")
coast@data=data.frame(id=1:length(coast@lines))  #convert to dataframe
coast=spTransform(coast,projs)
rcoast=rasterize(coast,topo)
dist=distance(rcoast)/1000 #get distance to coast and convert to km
### transform distance to coast using b-log(dist), where b \approx log(DTC of the farthest point on earth from the sea)
### Garcia-Castellanos, Daniel, and Umberto Lombardo. 2007. “Poles of Inaccessibility: A Calculation Algorithm for the Remotest Places on Earth.” Scottish Geographical Journal 123 (3): 227–233. doi:10.1080/14702540801897809.
### farthest point is ~2510km from ocean
log(2510) #round up to 8 to be sure we won't get negative numbers
dist2=brick(dist,calc(dist,function(x) (8-log(x+1))))
layerNames(dist2)=c("DTCkm","logDTCkm")
colnames(dist2@data@values)=c("DTCkm","logDTCkm")
topo2=stack(subset(topo,"dem"),EW,NS,slope,dist2)

## create binned elevation for stratified sampling
demc=quantile(subset(topo,subset="dem"))
demb=calc(subset(topo,subset="dem"),function(x) as.numeric(cut(x,breaks=demc)))
names(demb)="demb"
##topo=brick(list(topo,demb))


### load the lulc data as a brick
lulc=brick(as.list(list.files("data/regions/oregon/lulc",pattern="rst$",full=T)))
#projection(lulc)=
#plot(lulc)

## Enter lulc types (from Nakaegawa 2011)
lulct=as.data.frame(matrix(c(
  "Forest",1,
  "Shrub",2,
  "Grass",3,
  "Crop",4,
  "Mosaic",5,
  "Urban",6,
  "Barren",7,
  "Snow",8,
  "Wetland",9,
  "Water",10),byrow=T,ncol=2,dimnames=list(1:10,c("class","id"))),stringsAsFactors=F)
colnames(lulc@data@values)=lulct$class[as.numeric(gsub("[a-z]|[A-Z]|[_]|83","",layerNames(lulc)))]
layerNames(lulc)=lulct$class[as.numeric(gsub("[a-z]|[A-Z]|[_]|83","",layerNames(lulc)))]
lulc=calc(lulc,function(x) ifelse(is.na(x),0,x))
projection(lulc)=projs

### reclass/sum classes
ShrubGrass=subset(lulc,"Shrub")+subset(lulc,"Grass");layerNames(ShrubGrass)="ShrubGrass"
Other=subset(lulc,"Mosaic")+subset(lulc,"Barren")+subset(lulc,"Snow")+subset(lulc,"Wetland");layerNames(Other)="Other"
lulc2=stack(subset(lulc,"Forest"),subset(lulc,"Urban"),subset(lulc,"Crop"),Other,ShrubGrass)

### load the LST data
lst=brick(as.list(list.files("data/regions/oregon/lst",pattern="rescaled.rst$",full=T)[c(4:12,1:3)]))
lst=lst-273.15
colnames(lst@data@values)=format(as.Date(paste("2000-",as.numeric(gsub("[a-z]|[A-Z]|[_]|83","",layerNames(lst))),"-15",sep="")),"%b")
layerNames(lst)=format(as.Date(paste("2000-",as.numeric(gsub("[a-z]|[A-Z]|[_]|83","",layerNames(lst))),"-15",sep="")),"%b")
projection(lst)=projs


######################################
## compare LULC with station data
st2=st2[!is.na(extract(demb,st2)),]
st2=SpatialPointsDataFrame(st2,data=cbind.data.frame(st2@data,demb=extract(demb,st2),extract(topo,st2),extract(topo2,st2),extract(lulc2,st2,buffer=1500,fun=mean),extract(lst,st2)))
stlulc=extract(lulc2,st2,buffer=1500,fun=mean) #overlay stations and LULC values
st2$lulc=do.call(c,lapply(apply(stlulc,1,function(x) which.max(x)),function(x) ifelse(is.null(names(x)),NA,names(x))))


###  add MODIS metric to station data for month corresponding to that date
### reshape for easy merging
sdata.ul=melt(st2@data,id.vars=c("id","lat","lon","Forest","ShrubGrass","Crop","Urban","Other","lulc"),measure.vars=format(as.Date(paste("2000-",1:12,"-15",sep="")),"%b"))

### generate sample of points to speed processing
n=10000/length(unique(demb))
n2=30  #number of knots
s=sampleStratified(demb,size=n,sp=T)
#s=spsample(as(topo,"SpatialGrid"),n=n,type="regular")
s2=spsample(as(topo,"SpatialGrid"),n=n2,type="regular")

         
s=SpatialPointsDataFrame(s,data=cbind.data.frame(x=coordinates(s)[,1],y=coordinates(s)[,2],demb=extract(demb,s),extract(topo,s),extract(topo2,s),extract(lulc,s),extract(lst,s)))
### drop areas with no LST data
#s=s[!is.na(s$Aug),]
s=s[apply(s@data,1,function(x) all(!is.na(x))),]

###  add majority rules lulc for exploration
s$lulc=apply(s@data[,colnames(s@data)%in%lulct$class],1,function(x) (colnames(s@data)[colnames(s@data)%in%lulct$class])[which.max(x)])
save(s,file=paste("output/mod_sample.Rdata",sep=""))


### spatial regression to fit lulc coefficients
Sys.setenv(MKL_NUM_THREADS=24)
system("export MKL_NUM_THREADS=24")

niter=12500
lapply(months,function(m){
  print(paste("###################################   Starting month ",m))
  f1=formula(paste(m,"~logDTCkm+y+Shrub+Grass+Crop+Mosaic+Urban+Barren+Snow+Wetland+dem+eastwest+northsouth",sep=""))
  m1=spLM(f1,data=s@data,coords=coordinates(s),knots=coordinates(s2),
    starting=list("phi"=0.6,"sigma.sq"=1, "tau.sq"=1),
    sp.tuning=list("phi"=0.01, "sigma.sq"=0.05, "tau.sq"=0.05),
    priors=list("phi.Unif"=c(0.3, 3), "sigma.sq.IG"=c(2, 1), "tau.sq.IG"=c(2, 1)),
    cov.model="exponential",
    n.samples=niter,sub.samples=c(2500,niter,10),verbose=TRUE, n.report=100)
  assign(paste("mod_",m,sep=""),m1)
  save(list=paste("mod_",m,sep=""),file=paste("output/mod_",m,".Rdata",sep=""))
})

### Read in results
load(paste("output/mod_sample.Rdata",sep=""))
ms=lapply(months,function(m) {print(m); load(paste("output/mod_",m,".Rdata",sep="")) ; get(paste("mod_",m,sep="")) })

### generate summaries
ms1=do.call(rbind.data.frame,lapply(1:length(ms),function(i){
  mi=ms[[i]]
  m1.s=mcmc(t(mi$p.samples),start=round(niter/4),thin=1)
  m1.s2=as.data.frame(t(apply(m1.s,1,quantile,c(0.025,0.5,0.975))))
  colnames(m1.s2)=c("Q2.5","Q50","Q97.5")
  m1.s2$parm=rownames(m1.s)
  m1.s2$month=factor(months[i],levels=months,ordered=T)
  m1.s2$type=ifelse(m1.s2$parm%in%c("tau.sq","sigma.sq","phi"),"Spatial",ifelse(m1.s2$parm%in%c("(Intercept)","eastwest","northsouth","dem","logDTCkm","y"),"Topography","LULC"))
  return(m1.s2)
}))
## drop spatial parameters
#ms1=ms1[!ms1$type%in%"Spatial",]
## Convert dem to degrees/km to make it comparable to other topographical parameters
#ms1[ms1$parm%in%c("x","y","dem"),c("Q2.5","Q50","Q97.5")]=ms1[ms1$parm%in%c("x","y","dem"),c("Q2.5","Q50","Q97.5")]


#######################################################################
#### look at interaction of tmax~lst*lulc using monthly means
### add monthly data to sdata table by matching unique station_month ids.
d2$month=as.numeric(format(d2$date,"%m"))
### get monthly means and sd's
dm=melt(cast(d2,id+lon+lat+elev~month,value="value",fun.aggregate=mean,na.rm=T),id.vars=c("id","lon","lat","elev"));colnames(dm)[grep("value",colnames(dm))]="mean"
ds=melt(cast(d2,id+lon+lat+elev~month,value="value",fun.aggregate=sd,na.rm=T),id.vars=c("id","lon","lat","elev"))  #sd of tmax
dn=melt(cast(d2,id+lon+lat+elev~month,value="value",fun.aggregate=length),id.vars=c("id","lon","lat","elev"))  #number of observations
dm$sd=ds$value
dm$n=dn$value[match(paste(dm$month,dm$id),paste(dn$month,dn$id))]/max(dn$value)  # % complete record

#get lulc classes
lcs=layerNames(lulc2)

dm$lst=sdata.ul$value[match(paste(dm$id,format(as.Date(paste("2000-",dm$month,"-15",sep=""),"%Y-%m-%d"),"%b"),sep="_"),paste(sdata.ul$id,sdata.ul$variable,sep="_"))]
dm[,lcs]=sdata.ul[match(dm$id,sdata.ul$id),lcs]
dm=dm[!is.na(dm$ShrubGrass),]
dm$class=lcs[apply(dm[,lcs],1,which.max)]
## update month names
dm$m2=format(as.Date(paste("2000-",dm$month,"-15",sep="")),"%B")
dm$m2=factor(as.character(dm$m2),levels=format(as.Date(paste("2000-",1:12,"-15",sep="")),"%B"),ordered=T)

xyplot(mean~lst|m2,groups=class,data=dm,panel=function(x,y,subscripts,groups){  #+cut(dm$elev,breaks=quantile(dm$elev,seq(0,1,len=4)),labels=c("low","medium","high"))
  dt=dm[subscripts,]
  #panel.segments(dt$lst,dt$mean-dt$sd,dt$lst,dt$mean+dt$sd,groups=groups,lwd=.5,col="#C1CDCD")
  panel.xyplot(dt$lst,dt$mean,groups=groups,subscripts=subscripts,type=c("p","r"),cex=0.5)
  panel.abline(0,1,col="black",lwd=2)
},par.settings = list(superpose.symbol = list(pch=1:6,col=c("lightgreen","darkgreen","grey","brown","red"))),
       auto.key=list(space="right"),scales=list(relation="free"),
       sub="Each point represents a monthly mean (climatology) for a single station \n Points are colored by LULC class with largest % \n Heavy black line is y=x",main="Monthly Mean LST and Monthly Mean Tmax",
       ylab="Mean Monthly Tmax (C)",xlab="Mean Monthly LST")


mods=data.frame(
  form=c(
    "mean~lst+elev",
    "mean~lst+elev+ShrubGrass+Urban+Crop+Other",
    "mean~lst+elev+lst*ShrubGrass+lst*Urban+lst*Crop+lst*Other"
    ),
  type=c("lst","intercept","interact"),
  stringsAsFactors=F)
mods2=expand.grid(form=mods$form,month=1:12)
mods2$type=mods$type[match(mods2$form,mods$form)]


#summary(lm(mods$form[4],data=dm,weight=dm$n))

ms=lapply(1:nrow(mods2),function(i) {
  m=lm(as.formula(as.character(mods2$form[i])),data=dm[dm$month==mods2$month[i],],weight=dm$n[dm$month==mods2$month[i]])
  return(list(model=m,
              res=data.frame(
                Formula=as.character(mods2$form[i]),
                Month=mods2$month[i],
                type=mods2$type[i],
                AIC=AIC(m),
                R2=summary(m)$r.squared)))
})

### identify lowest AIC per month
ms1=do.call(rbind.data.frame,lapply(ms,function(m) m$res))
aicw= cast(ms1,Month~type,value="AIC")
aicwt=as.data.frame(t(apply(aicw[,-1],1,function(x) ifelse(x==min(x),"Minimum",ifelse((x-min(x))<7,"NS Minimum","NS")))));colnames(aicwt)=colnames(aicw)[-1];aicwt$Month=aicw$Month
aic=melt(aicwt,id.vars="Month");colnames(aic)=c("Month","type","minAIC")
aic$minAIC=factor(aic$minAIC,ordered=F)

xyplot(AIC~as.factor(Month),groups=Formula,data=ms1,type=c("p","l"),pch=16,auto.key=list(space="top"),main="Model Comparison across months",
       par.settings = list(superpose.symbol = list(pch=16,cex=1)),xlab="Month")


ms2=lapply(ms,function(m) m$model)

mi=rep(c(1:12),each=3)  #month indices

fs=do.call(rbind.data.frame,lapply(1:12,function(i){
  it=which(mi==i)
  x=anova(ms2[[it[1]]],ms2[[it[2]]],ms2[[it[3]]])
  fs=c(
    paste(as.character(formula(ms2[[it[1]]]))[c(2,1,3)],collapse=" "),
    paste(as.character(formula(ms2[[it[2]]]))[c(2,1,3)],collapse=" "),
    paste(as.character(formula(ms2[[it[3]]]))[c(2,1,3)],collapse=" "))
  data.frame(month=rep(i,3),model=fs,p=as.data.frame(x)[,6],sig=ifelse(as.data.frame(x)[,6]<0.05,T,F))
}))

table(fs$sig,fs$model)
which(fs$sig)

### load oregon boundary for comparison
roi=spTransform(as(readOGR("data/regions/Test_sites/Oregon.shp","Oregon"),"SpatialLines"),projs)


bgyr=colorRampPalette(c("blue","green","yellow","red"))
pdf("output/lst_lulc.pdf",width=11,height=8.5)

### Summary plots of covariates
## LULC
at=seq(0.1,100,length=100)
levelplot(lulc2,at=at,col.regions=bgyr(length(at)),
          main="Land Cover Classes",sub="Sub-pixel %")+
  layer(sp.lines(roi, lwd=1.2, col='black'))

## TOPO
at=unique(quantile(as.matrix(subset(topo2,c("eastwest","northsouth","slope"))),seq(0,1,length=100),na.rm=T))
levelplot(subset(topo2,c("eastwest","northsouth","slope")),at=at,col.regions=bgyr(length(at)),
          main="Topographic Variables",sub="")+
  layer(sp.lines(roi, lwd=1.2, col='black'))


## DEM
at=unique(quantile(as.matrix(subset(topo2,c("dem"))),seq(0,1,length=100),na.rm=T))
levelplot(subset(topo2,c("dem")),at=at,col.regions=bgyr(length(at)),
          main="Elevation",sub="",margin=F)+
  layer(sp.lines(roi, lwd=1.2, col='black'))

## DTC
at=unique(quantile(as.matrix(subset(topo2,c("logDTCkm"))),seq(0,1,length=100),na.rm=T))
levelplot(subset(topo2,c("logDTCkm")),at=at,col.regions=bgyr(length(at)),
          main="Elevation",sub="",margin=F)+
  layer(sp.lines(roi, lwd=1.2, col='black'))

#LST
at=quantile(as.matrix(lst),seq(0,1,length=100),na.rm=T)
levelplot(lst,at=at,col.regions=bgyr(length(at)),
                      main="MOD11A1 Mean Monthly LST")+
  layer(sp.lines(roi, lwd=1.2, col='black'))


xyplot(value~dem|variable,groups=lulc,melt(s@data[,c("dem","lulc",months)],id.vars=c("dem","lulc")),pch=16,cex=.5,
       main="Month-by-month scatterplots of Elevation and LST, grouped by LULC",
       sub="One point per pixel, per month",ylab="LST",
       par.settings = list(superpose.symbol = list(pch=16,cex=.5)),auto.key=list(space="right",title="LULC"))+
  layer(panel.abline(lm(y~x),col="red"))+
  layer(panel.text(500,50,bquote(beta==.(round(coefficients(lm(y~x))[2],4)))))

## just forest, grouped by distance to coast
xyplot(value~dem|variable,groups=cut(DTCkm,breaks=c(0,300,1000)),melt(s@data[s$lulc=="Forest",c("DTCkm","dem","lulc",months)],id.vars=c("DTCkm","dem","lulc")),pch=16,cex=.5,
       main="Month-by-month scatterplots of Elevation and LST, grouped by Distance to Coast",
       sub="Showing only forest class",ylab="LST",
       par.settings = list(superpose.symbol = list(pch=16,cex=.5)),auto.key=list(space="right",title="Distance to \n Coast (km)"))+
  layer(panel.abline(lm(y~x),col="red"))+
  layer(panel.text(500,50,bquote(beta==.(round(coefficients(lm(y~x))[2],4)))))

useOuterStrips(xyplot(value~dem|variable+lulc,groups=cut(DTCkm,breaks=c(0,300,1000)),
                      melt(s@data[s$lulc!="Snow",c("DTCkm","dem","lulc",months)],id.vars=c("DTCkm","dem","lulc")),pch=16,cex=.5,
       main="Month-by-month scatterplots of Elevation and LST, grouped by LULC and colored by distance to coast",
       sub="One point per pixel, per month",ylab="LST",
       par.settings = list(superpose.symbol = list(pch=16,cex=.5)),auto.key=list(space="right",title="Distance to \n Coast (km)")))+
  layer(panel.abline(lm(y~x),col="red"))



##########################################
### Model output

### show fitted values
spplot(s,zcol="demb",col.regions=terrain.colors(5),cex=.5)+layer(sp.lines(roi,lwd=1.2,col="black"))+
  layer(sp.points(s2,col="blue",pch=13,cex=2,lwd=2))+
  layer(sp.points(st2,col="black",lwd=1.5))


## look at correlation of LULC variables
lulct=s@data[,colnames(s@data)[colnames(s@data)%in%unique(s$lulc)]]
lulcc=cor(lulct)[order(cor(lulct)[1,]),order(cor(lulct)[1,])]
round(cor(lulct),2)
#print(xtable(round(cor(lulct),2),include.row.names=F))
plotcorr(lulcc,col=colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white","#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")[5*lulcc+6],
         main="Correlation Matrix")

t1=as.data.frame(table(s$lulc));colnames(t1)=c("class","samplefreq")
t1$sampleprop=t1$samplefreq/sum(t1$samplefreq)
t2=as.data.frame(table(st2$lulc))
t1$stationfreq=0
t1$stationfreq[t1$class%in%t2$Var1]=t2$Freq[match(t2$Var1,t1$class[t1$class%in%t2$Var1])]
t1$stationprop=t1$stationfreq/sum(t1$stationfreq)
print(xtable(t1[,c(1,3,5)]),include.rownames=F)

### two plots of model parameters by month to show the effects of LULC and topography on LST
## Spatial
xyplot(Q50~month|parm,data=ms1[ms1$type=="Spatial",],panel=function(x,y,subscripts){
  tms1=ms1[ms1$type=="Spatial",][subscripts,]
  panel.segments(tms1$month,tms1$Q2.5,tms1$month,tms1$Q97.5,groups=tms1$parm,subscripts=1:nrow(tms1))
  panel.xyplot(tms1$month,tms1$Q50,pch=16,type="l")
  panel.abline(h=0,lty="dashed",col="grey")
},auto.key=list(space="right"),par.settings = list(superpose.symbol = list(pch=16,cex=.5,col=1:15),superpose.line=list(col=1:15)),
       subscripts=T,scales=list(y=list(relation="free",rot=90)),ylab="Parameter Coefficient (slope) with 95% Credible Intervals",xlab="Month",
       main="Spatial Parameters",
       sub="")

## LULC
xyplot(Q50~month,groups=parm,data=ms1[ms1$type=="LULC",],panel=function(x,y,subscripts){
  tms1=ms1[ms1$type=="LULC",][subscripts,]
  sig=ifelse(tms1$Q2.5<0&tms1$Q97.5<0|tms1$Q2.5>0&tms1$Q97.5>0,"red","black")
  panel.segments(tms1$month,tms1$Q2.5,tms1$month,tms1$Q97.5,groups=tms1$parm,subscripts=1:nrow(tms1))
  panel.xyplot(tms1$month,tms1$Q50,groups=tms1$parm,pch=16,type="l",subscripts=1:nrow(tms1))
  panel.abline(h=0,lty="dashed",col="grey")
},auto.key=list(space="right"),par.settings = list(superpose.symbol = list(pch=16,cex=.5,col=1:15),superpose.line=list(col=1:15)),
       subscripts=T,scales=list(y=list(relation="free",rot=90)),ylab="Parameter Coefficient (slope) with 95% Credible Intervals",xlab="Month",
       main="Effects of LULC on LST",
       sub="Coefficients are unstandardized and represent the change in LST expected with a 1% increase in that class from 100% Forest")

## Topography
xyplot(Q50~month|parm,data=ms1[ms1$type=="Topography",],panel=function(x,y,subscripts){
  tms1=ms1[ms1$type=="Topography",][subscripts,]
  panel.segments(tms1$month,tms1$Q2.5,tms1$month,tms1$Q97.5,subscripts=1:nrow(tms1))
  panel.xyplot(tms1$month,tms1$Q50,groups=tms1$parm,pch=16,type="l",subscripts=1:nrow(tms1))
  panel.abline(h=0,lty="dashed",col="grey")
},auto.key=list(space="right"),par.settings = list(superpose.symbol = list(pch=16,cex=.5,col=1:15),superpose.line=list(col=1:15)),
       subscripts=T,scales=list(y=list(relation="free",rot=90)),ylab="Parameter Coefficient (slope) with 95% Credible Intervals",xlab="Month",
       main="Effects of Topography on LST",
       sub="Coefficients are unstandardized. Intercept is degrees C, eastwest and northsouth range \n from -1 (90 degree slope) to 0 (flat) to 1 (90 degree slope), and dem is m \n logDTCkm is in log(km), and y (lat) is m")

### Capture same pattern using only station data?


dev.off()

 shrinkpdf<-function(pdf,maxsize=1,suffix="_small",verbose=T){
   require(multicore)
   wd=getwd()
   td=paste(tempdir(),"/pdf",sep="")
   if(!file.exists(td)) dir.create(td)
   if(verbose) print("Performing initial compression")
   setwd(td)
   if(verbose) print("Splitting the PDF to parallelize the processing")
   system(paste("pdftk ",wd,"/",pdf," burst",sep=""))
   mclapply(list.files(td,pattern="pdf$"),function(f){
     ## loop through all pages, perform compression with with ps2pdf
     if(verbose) print("Performing initial compression")
       system(paste("ps2pdf -dUseFlateCompression=true ",td,"/",f," ",td,"/compressed_",f,sep=""))
        file.rename(paste(td,"/compressed_",f,sep=""),paste(td,"/",f,sep=""))
     ## get sysmte size
        size=file.info(paste(td,"/",f,sep=""))$size*0.000001 #get sizes of individual pages
        toobig=size>=maxsize
        if(verbose&toobig)  print(paste("Resizing ",basename(paste(td,"/",f,sep="")),sep=""))
        system(paste("gs -dBATCH -dTextAlphaBits=4 -dNOPAUSE -r300 -q -sDEVICE=png16m -sOutputFile=- -q ",paste(td,"/",f,sep="")," | convert -background transparent -quality 100 -density 300 - ",f,sep=""))
      })
        if(verbose) print("Compiling the final pdf")
        setwd(wd)
        system(paste("gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=",strsplit(pdf,".",fixed=T)[[1]][1],suffix,".pdf ",td,"/*.pdf",sep=""))
        file.remove(list.files(td,full=T))
       if(verbose) print("Finished!!")
}

shrinkpdf("output/lst_lulc.pdf")

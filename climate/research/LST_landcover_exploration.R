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


X11.options(type="X11")

ncores=20  #number of threads to use


### copy lulc data to litoria
setwd("data/lulc")
system("scp atlas:/home/parmentier/data_Oregon_stations/W_Layer* .")


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
#d2$value=d2$value/10 #convert to mm


## load topographical data
topo=brick(as.list(list.files("data/topography",pattern="rst$",full=T)))
topo=calc(topo,function(x) ifelse(x<0,NA,x))
names(topo)=c("aspect","dem","slope")
colnames(topo@data@values)=c("aspect","dem","slope")
projection(topo)=projs
NS=sin(subset(topo,subset="slope")*pi/180)*cos(subset(topo,subset="aspect")*pi/180);names(NS)="northsouth"
EW=sin(subset(topo,subset="slope")*pi/180)*sin(subset(topo,subset="aspect")*pi/180);names(EW)="eastwest"
slope=sin(subset(topo,subset="slope")*pi/180);names(slope)="slope"
topo2=stack(subset(topo,"dem"),EW,NS,slope)

## create binned elevation for stratified sampling
demc=quantile(subset(topo,subset="dem"))
demb=calc(subset(topo,subset="dem"),function(x) as.numeric(cut(x,breaks=demc)))
names(demb)="demb"
                                        #topo=brick(list(topo,demb))


### load the lulc data as a brick
lulc=brick(as.list(list.files("data/lulc",pattern="rst$",full=T)))
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

### load the LST data
lst=brick(as.list(list.files("data/lst",pattern="rescaled.rst$",full=T)[c(4:12,1:3)]))
lst=lst-273.15
colnames(lst@data@values)=format(as.Date(paste("2000-",as.numeric(gsub("[a-z]|[A-Z]|[_]|83","",layerNames(lst))),"-15",sep="")),"%b")
layerNames(lst)=format(as.Date(paste("2000-",as.numeric(gsub("[a-z]|[A-Z]|[_]|83","",layerNames(lst))),"-15",sep="")),"%b")
projection(lst)=projs


######################################
## compare LULC with station data
stlulc=extract(lulc,st2) #overlay stations and LULC values
st2$lulc=do.call(c,lapply(apply(stlulc,1,function(x) which.max(x)),function(x) ifelse(is.null(names(x)),NA,names(x))))


### generate sample of points to speed processing
n=10000/length(unique(demb))
n2=30  #number of knots
s=sampleStratified(demb,size=n,sp=T)
#s=spsample(as(topo,"SpatialGrid"),n=n,type="regular")
s2=spsample(as(topo,"SpatialGrid"),n=n2,type="regular")

         
s=SpatialPointsDataFrame(s,data=cbind.data.frame(extract(topo,s),extract(topo2,s),extract(lulc,s),extract(lst,s)))
### drop areas with no LST data
#s=s[!is.na(s$Aug),]
s=s[apply(s@data,1,function(x) all(!is.na(x))),]

###  add majority rules lulc for exploration
s$lulc=apply(s@data[,colnames(s@data)%in%lulct$class],1,function(x) (colnames(s@data)[colnames(s@data)%in%lulct$class])[which.max(x)])


### spatial regression to fit lulc coefficients

niter=1250
mclapply(months,function(m){
  print(paste("###################################   Starting month ",m))
  f1=formula(paste(m,"~Shrub+Grass+Crop+Mosaic+Urban+Barren+Snow+Wetland+dem+eastwest+northsouth",sep=""))
  m1=spLM(f1,data=s@data,coords=coordinates(s),knots=coordinates(s2),
    starting=list("phi"=0.6,"sigma.sq"=1, "tau.sq"=1),
    sp.tuning=list("phi"=0.01, "sigma.sq"=0.05, "tau.sq"=0.05),
    priors=list("phi.Unif"=c(0.3, 3), "sigma.sq.IG"=c(2, 1), "tau.sq.IG"=c(2, 1)),
    cov.model="exponential",
    n.samples=niter, verbose=TRUE, n.report=100)
  assign(paste("mod_",m,sep=""),m1)
  save(list=paste("mod_",m,sep=""),file=paste("output/mod_",m,".Rdata",sep=""))
})


m1.s=mcmc(t(m1$p.samples),start=round(niter/4),thin=1)
bwplot(value~X2,melt(as.matrix(m1$p.samples)[,!colnames(m1$p.samples)%in%c("(Intercept)","sigma.sq","tau.sq","phi","northsouth","dem","eastwest")]))
densityplot(m1$p.samples)

### load oregon boundary for comparison
roi=spTransform(as(readOGR("data/regions/Test_sites/Oregon.shp","Oregon"),"SpatialLines"),projs)


bgyr=colorRampPalette(c("blue","green","yellow","red"))
pdf("output/lst_lulc.pdf",width=11,height=8.5)

### Summary plots of covariates
## LULC
at=seq(0.1,100,length=100)
levelplot(lulc,at=at,col.regions=bgyr(length(at)),
          main="Land Cover Classes",sub="Sub-pixel %")+
  layer(sp.lines(roi, lwd=1.2, col='black'))

## TOPO
at=unique(quantile(as.matrix(subset(topo2,c("eastwest","northsouth","slope"))),seq(0,1,length=100),na.rm=T))
levelplot(subset(topo2,c("eastwest","northsouth","slope")),at=at,col.regions=bgyr(length(at)),
          main="Topographic Variables",sub="")+
  layer(sp.lines(roi, lwd=1.2, col='black'))

#LST
at=quantile(as.matrix(lst),seq(0,1,length=100),na.rm=T)
levelplot(lst,at=at,col.regions=bgyr(length(at)),
                      main="MOD11A1 Mean Monthly LST")+
  layer(sp.lines(roi, lwd=1.2, col='black'))


### show fitted values
spplot(s,zcol="dem")+layer(sp.lines(roi,lwd=1.2,col="black"))+layer(sp.points(s2,col="blue",pch=13,cex=2))

histogram(~value|variable,data=melt(s@data[,colnames(s@data)%in%lulct$class]))

bwplot(lulc~value|variable,data=melt(s@data,id.vars="lulc",measure.vars=layerNames(lst)),horizontal=T)


xyplot(value~dem|variable,groups=lulc,melt(s@data[,c("dem","lulc",months)],id.vars=c("dem","lulc")),pch=16,cex=.5,
       main="Month-by-month scatterplots of Elevation and LST, grouped by LULC",
       sub="One point per (subsetted) pixel, per month"
       par.settings = list(superpose.symbol = list(pch=16,cex=.5)),auto.key=list(space="right",title="LULC"))+
  layer(panel.abline(lm(y~x),col="red"))+
  layer(panel.text(500,50,bquote(beta==.(round(coefficients(lm(y~x))[2],4)))))






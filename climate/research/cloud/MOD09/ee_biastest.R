#### Evaluate the feasibility of correcting for orbital bias in cloud cover
library(raster)
library(rasterVis)
library(doMC)
library(multicore)
library(foreach)
library(rgdal)
registerDoMC(4)
library(plyr)
library(mgcv)
library(sampling)

setwd("~/acrobates/adamw/projects/cloud")


## set raster options
rasterOptions(maxmemory=1e8) 

## get CF data
##  Get list of available files
#df=data.frame(path=list.files("data/mod09cloud",pattern="*.tif$",full=T,recur=T),stringsAsFactors=F)
#df[,c("month")]=do.call(rbind,strsplit(basename(df$path),"_|[.]|-"))[,c(6)]
#df$date=as.Date(paste(2013,"_",df$month,"_15",sep=""),"%Y_%m_%d")
#df=df[order(df$date),]

#d=stack(df$path,bands=1)
#names(d)=df$month

reg=extent(c(-1434564.00523,1784892.95369, 564861.173869, 1880991.3772))
reg=extent(c(-2187230.72881, 4017838.07688,  -339907.592509, 3589340.63805))  #all sahara
reg=extent(c(-12020769.9608, 10201058.231,  -3682105.25271, 3649806.08382))  #large equitorial

#reg=extent(c(-151270.082307, 557321.958761, 1246351.8356, 1733123.75947))

d=stack("data/mcd09/2014113_combined_20002013_MOD09_1-img-0000000000-0000000000.tif")


names(d)=c("cf","cfsd","nobs","pobs")
#cd=coordinates(d)
#cd$x=as.integer(cd[,"x"]); cd$y=as.integer(cd[,"y"])
#cdr=rasterFromXYZ(cbind(cd,cd))
#names(cdr)
#d=stack(d,cdr)
#obs=crop(raster("data/mcd09/2014113_combined_20002013_MOD09_1-img-0000000000-0000000000.tif",band=4),reg)
#levelplot(stack(obs,d))

#plot(obs,d)
 
#pts=sampleStratified(d[["pobs"]], size=10000, exp=10, na.rm=TRUE, xy=FALSE, ext=NULL, sp=FALSE)
pts=sampleRandom(d, size=100000, na.rm=TRUE, xy=T, ext=NULL, sp=T)
pts=pts[pts$nobs>0,]  #drop data from missing tiles

#dt=data.frame(cf=as.numeric(values(d[["cf"]])),obs=as.numeric(values(d[["pobs"]])))
#dt[,c("x","y")]=coordinates(d)
#dt=dt[dt$obs>0,]  #drop data from missing tiles

#s=1:nrow(dt)
#s=sample(1:nrow(dt),10000)
#s=strata(dt, stratanames=dt$obs, size=1000)

lm1=bam(cf~s(x,y)+pobs+nobs,data=pts@data)
summary(lm1)
beta1=coef(lm1)["pobs"]; beta1
beta2=coef(lm1)["nobs"]; beta2

#d2=d
#d2[["pobs"]]=100

#pred1=raster::predict(d,lm1,file=paste("data/bias/pred1.tif",sep=""),format="GTiff",dataType="INT1U",overwrite=T,NAflag=255)#lm1=bam(as.vector(d[["cf"]]) ~ as.vector(d[["pobs"]]))
#pred2=raster::predict(d2,lm1,file=paste("data/bias/pred2.tif",sep=""),format="GTiff",dataType="INT1U",overwrite=T,NAflag=255)#lm1=bam(as.vector(d[["cf"]]) ~ as.vector(d[["pobs"]]))
#pred3=overlay(pred1,pred2,function(x,y) x-y,file="data/bias/pred3.tif",overwrite=T)
#biasc=overlay(d[["cf"]], pred3,fun=sum,file="data/bias/biasc.tif",overwrite=T)


getbias=function(cf,cfsd,nobs,pobs) return((100-pobs)*beta1+(4-nobs)*beta2)
getbias(cf=c(50,40,30),c(1,1,1),c(3,4,5),c(82,87,87))
bias=overlay(d,fun=getbias, unstack=TRUE,file=paste("data/bias/bias.tif",sep=""),format="GTiff",dataType="INT1U",overwrite=T,NAflag=255)
dc=overlay(d[["cf"]],bias,fun=function(x,y) x+y,
    file=paste("data/bias/biasc.tif",sep=""),
    format="GTiff",dataType="INT1U",overwrite=T,NAflag=255) #,options=c("COMPRESS=LZW","ZLEVEL=9"))
#levelplot(stack(d,dc))


library(multicore)

Mode <- function(x) {
      ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
  }
#rasterOptions(maxmemory)#=50GB) 
## set up processing chunks
nrw=nrow(d)
nby=20
nrwg=seq(1,nrw,by=nby)
writeLines(paste("Processing ",length(nrwg)," groups and",nrw,"lines"))

## Parallel loop to conduct moving window analysis and quantify pixels with significant shifts across pp or lulc boundaries
output=mclapply(nrwg,function(ti){
      ## Extract focal areas
      ngb=c(51,101)
      vals_cf=getValuesFocal(d,ngb=ngb,row=ti,nrows=nby)
      vals_obs=getValuesFocal(obs,ngb=ngb,row=ti,nrows=nby)
      ## extract bias
      bias=raster(matrix(do.call(rbind,lapply(1:nrow(vals_cf),function(i) {
#          if(i%in%round(seq(1,nrow(vals_cf),len=100))) print(i)
          tobs=vals_obs[i,]  #vector of indices
          tval=vals_cf[i,]    # vector of values
          lm1=lm(tval~tobs,na.rm=T)
          dif=round(predict(lm1)-predict(lm1,newdata=data.frame(tobs=median(tobs,na.rm=T))))
          return(dif)  
            })),nrow=nby,ncol=ncol(d),byrow=T))     # turn it back into a raster
        ## update raster and write it
        extent(bias)=extent(d[ti:(ti+nby-1),1:ncol(d),drop=F])
        projection(bias)=projection(d)
        NAvalue(bias) <- 255
        writeRaster(bias,file=paste("data/bias/tiles/bias_",ti,".tif",sep=""),
                                  format="GTiff",dataType="INT1U",overwrite=T,NAflag=255) #,options=c("COMPRESS=LZW","ZLEVEL=9")
    print(ti)
  }
)

#levelplot(d)
#plot(obs)

  system("gdalbuildvrt -srcnodata 255 -vrtnodata 255 data/bias/bias.vrt `find data/bias/tiles -name 'bias*tif'` ")
  system("gdalwarp -srcnodata 255 `find data/bias/tiles -name 'bias*tif'` data/bias/bias_movingwindow.tif ")

n=5000
ps=SpatialPoints(cbind(lon=runif(n,-180,10),lat=runif(n,-30,30)))
ps=SpatialPoints(cbind(lon=runif(n,-10,10),lat=runif(n,-5,30)))

projection(ps)=projection(d)

psd=extract(d,ps,sp=F)[,12]
psd=cbind.data.frame(cf=psd,pobs=extract(obs,ps,sp=F))
psd$cf[psd$cf<0]=NA
ps2=SpatialPointsDataFrame(ps,data=psd)

writeOGR(ps2,"output","MODIS_ObservationValidation",driver="ESRI Shapefile")


ps=readOGR("output","MODIS_ObservationValidation")

xyplot(cf~pobs,data=ps@data[!is.na(ps$cf)&ps$pobs>50,])

keep=T
keep=ps$cf<12&ps$pobs>80
lm1=lm(cf~pobs,data=ps@data[keep,])
summary(lm1)


pred=predict(lm1,newdata=ps@data[keep,],type="response")

plot(cf~pobs,data=ps@data[keep,],xlim=c(78,100))
points(pred~ps$pobs[keep],col="red")



#### fiddle with binomial
n=11
p=seq(0,1,len=n)
obs=1:30

mat=expand.grid(p=p,obs=obs)
mat$obsh=apply(sapply(1:14,function(x) rbinom(nrow(mat),size=mat$obs,prob=mat$p)),1,mean)
mat$ph=mat$obsh/mat$obs
mat$bias=mat$ph-mat$p

####  Simulate clouds
n=10
p=seq(0,1,len=n)
obs=10:30

## true clouds
mat=expand.grid(p=p,obs=obs)

res=matrix(nrow=nrow(mat),ncol=1000)
for(r in 1:1000){
    clouds=sapply(1:30,function(x) rbinom(nrow(mat),size=1,prob=mat$p))
    for(i in 1:nrow(mat)){
        res[i,r]=
            mean(sample(clouds[i,],mat$obs[i]))
    }
    
}

mat$ph=apply(res,1,mean)
                                        #mat$ph=mat$obsh/mat$obs
mat$bias=mat$ph-mat$p


levelplot(bias~p*obs,data=mat,col.regions=rainbow(100,end=0.8),at=seq(min(mat$bias),max(mat$bias),len=n),ylab="Observations in a 30-day month",xlab="'True'  Cloud Frequency (%) ")

hist(mat$bias)
plot(mat$p~mat$ph)
summary(mat)

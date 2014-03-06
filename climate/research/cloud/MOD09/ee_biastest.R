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

#reg=extent(c(-1434564.00523,1784892.95369, 564861.173869, 1880991.3772))
#reg=extent(c(-2187230.72881, 4017838.07688,  -339907.592509, 3589340.63805))  #all sahara
#reg=extent(c(-12020769.9608, 10201058.231,  -3682105.25271, 3649806.08382))  #large equitorial


## set up processing chunks
nrw=nrow(mod)
nby=20
nrwg=seq(1,nrw,by=nby)
writeLines(paste("Processing ",length(nrwg)," groups and",nrw,"lines"))

## Parallel loop to conduct moving window analysis and quantify pixels with significant shifts across pp or lulc boundaries
output=mclapply(nrwg,function(ti){
    ## Extract focal areas
    nr=51
    nc=101
    ngb=c(nr,nc)
      vals_cf=getValuesFocal(mod[[c("cf","pobs")]],ngb=ngb,row=ti,nrows=nby)
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

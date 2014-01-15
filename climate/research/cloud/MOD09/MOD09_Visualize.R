## script to visualize cloud frequency data

setwd("~/acrobates/adamw/projects/cloud/")

library(rasterVis)

## read in global coasts for nice plotting
library(maptools)

data(wrld_simpl)
coast <- unionSpatialPolygons(wrld_simpl, rep("land",nrow(wrld_simpl)), threshold=5)
coast=as(coast,"SpatialLines")
#coast=spTransform(coast,CRS(projection(mod35)))


#### Evaluate MOD35 Cloud data
mc=brick("~/acrobates/adamw/projects/cloud/data/mod09.nc",varname="CF")
NAvalue(mc)=-1

cols=colorRampPalette(c("#000000","#00FF00","#FF0000"))#"black","blue","red"))
for(i in 1:156){
png(paste("output/mod09_fullanimation_",i,".png",sep=""),width=2000,height=1000)
  print(i)
  r=mm[[i]]
  print(levelplot(r,col.regions=cols(100),at=seq(0,100,len=100),margin=F,maxpixels=1e6,ylim=c(-60,70),main=paste(names(mod09)[i])))+
    layer(sp.lines(coast))
dev.off()
}

#### Evaluate MOD35 Cloud data
mmc=brick("~/acrobates/adamw/projects/cloud/data/mod09_clim_mean.nc",varname="CF")
names(mmc)=month.name
NAvalue(mmc)=-1

cols=colorRampPalette(c("#000000","#00FF00","#FF0000"))#"black","blue","red"))
for(i in 1:12){
png(paste("output/mod09_animation_",i,".png",sep=""),width=2000,height=1000)
  print(i)
  r=mmc[[i]]
  print(levelplot(r,col.regions=cols(100),at=seq(0,100,len=100),margin=F,maxpixels=1e7,ylim=c(-60,70),
                  main=paste(month.name[i]),cex.main=3,scales=list(draw=F),cuts=99))+
    layer(sp.lines(coast))
dev.off()
}


## climatologies
mac=brick("~/acrobates/adamw/projects/cloud/data/cloud_mean.nc",varname="CF_annual")

pdf("output/mod09_climatology.pdf",width=11,height=8.5)
levelplot(mac,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e6)+
    layer(sp.lines(coast,lwd=.5,col="black"))
dev.off()


## Compare with worldclim and NPP
wc=stack(as.list(paste("/mnt/data/jetzlab/Data/environ/global/worldclim/prec_",1:12,".bil",sep="")))
wc_map=stack(as.list(paste("/mnt/data/jetzlab/Data/environ/global/worldclim/bio_12.bil",sep="")))
npp=raster("/mnt/data/jetzlab/Data/environ/global/MODIS/MOD17A3/MOD17A3_Science_NPP_mean_00_12.tif",sep="")


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
p_map=levelplot(crop(wc_map,regs[[r]]),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.75),xlab="",ylab="",main=names(regs)[r],useRaster=T)
p_mac=levelplot(crop(mac,regs[[r]]),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.75),useRaster=T)
#p_npp=levelplot(crop(npp,regs[[r]]),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,maxpixels=1e5,colorkey=list(space="bottom",height=.5),zscaleLog=T,useRaster=T)  #"NPP"=p_npp,
p3=c("WorldClim Mean Annual Precip (mm)"=p_map,"MOD09 Cloud Frequency (%)"=p_mac,x.same=T,y.same=T,merge.legends=T,layout=c(2,1))
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


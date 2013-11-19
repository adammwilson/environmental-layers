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
mod09=brick("~/acrobates/adamw/projects/cloud/data/mod09.nc",varname="CF")
NAvalue(mod09)=-1

cols=colorRampPalette(c("#000000","#00FF00","#FF0000"))#"black","blue","red"))
for(i in 1:156){
png(paste("output/mod09_fullanimation_",i,".png",sep=""),width=2000,height=1000)
  print(i)
  r=mod09[[i]]
  print(levelplot(r,col.regions=cols(100),at=seq(0,100,len=100),margin=F,maxpixels=1e6,ylim=c(-60,70),main=paste(names(mod09)[i])))+
    layer(sp.lines(coast))
dev.off()
}

#### Evaluate MOD35 Cloud data
mod09=brick("~/acrobates/adamw/projects/cloud/data/mod09_clim.nc",varname="CF")
NAvalue(mod09)=-1

cols=colorRampPalette(c("#000000","#00FF00","#FF0000"))#"black","blue","red"))
for(i in 1:12){
png(paste("output/mod09_animation_",i,".png",sep=""),width=2000,height=1000)
  print(i)
  r=mod09[[i]]
  print(levelplot(r,col.regions=cols(100),at=seq(0,100,len=100),margin=F,maxpixels=1e6,ylim=c(-60,70),main=paste(month.name[i])))+
    layer(sp.lines(coast))
dev.off()
}


## climatologies
mod09a=brick("~/acrobates/adamw/projects/cloud/data/mod09_clim2.nc",varname="CF")

pdf("output/mod09_climatology.pdf",width=11,height=8.5)
levelplot(mod09c,col.regions=cols(100),at=seq(0,100,len=100),margin=F,max.pixels=1e7)
dev.off()


## Compare with worldclim
wc=stack(as.list(paste("/mnt/data/jetzlab/Data/environ/global/worldclim/prec_",1:12,".bil",sep="")))
reg=extent(c(-83,-45,-5,13))
reg2=extent(c(-81,-70,-4,10))


wc_map=mean(crop(wc,reg))

twc_12=crop(wc[[12]],reg)
mod09_12=crop(mod09[[12]],reg)


p1=levelplot(wc_map,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,colorkey=list("top"))
p2=levelplot(mod09_12,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,colorkey=list("bottom"))

p3=c(p1,p2,x.same=T,y.same=T,merge.legends=F)
print(p3)

plot(mod09_12)


## reduced resolution
mod09_8km=aggregate(mod09_12,8)
mod09_1deg=aggregate(mod09_12,110)

pdf("output/mod09_resolution.pdf",width=11,height=8.5)
p1=levelplot(mod09_12,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e6)
p2=levelplot(mod09_8km,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e6)
p3=levelplot(mod09_1deg,col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e6)
print(c(p1,p2,p3))


p1=levelplot(crop(mod09_12,reg2),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e6)
p2=levelplot(crop(mod09_8km,reg2),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e6)
p3=levelplot(crop(mod09_1deg,reg2),col.regions=grey(seq(0,1,len=100)),cuts=99,margin=F,max.pixels=1e6)
print(c(p1,p2,p3),x.same=T,y.same=T)

dev.off()



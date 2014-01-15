## script to visualize cloud frequency data

setwd("~/acrobates/adamw/projects/cloud/")

library(rasterVis)

## read in global coasts for nice plotting
library(maptools)

#coast=spTransform(coast,CRS(projection(mod35)))
land=readShapePoly("/mnt/data/jetzlab/Data/environ/global/gshhg/GSHHS_shp/c/GSHHS_c_L1.shp",force_ring=TRUE)
projection(land)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
CP <- as(extent(-180, 180, -60, 84), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(land))
coast=as(land[land$area>50,],"SpatialLines")


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
mmc=brick("~/acrobates/adamw/projects/cloud/data/cloud_ymonmean.nc",varname="CF")
names(mmc)=month.name

cols=colorRampPalette(c("#000000","#00FF00","#FF0000"))#"black","blue","red"))
png("output/CF_Animation_%03d.png",width=5000,height=4000,res=600,pointsize=96,bg="white")
for(i in 1:12){
    print(i)
    r=mmc[[i]]
    print(levelplot(r,col.regions=cols(100),at=seq(1,100,len=100),margin=F,maxpixels=1e6,ylim=c(-60,73),
                    main=paste(month.name[i]),cex.main=3,scales=list(draw=F),cuts=99,ylab="",xlab="")+
                        layer(panel.polygon(x=c(-180,-180,180,180),y=c(-90,90,90,-90),col="black"),under=T)+
                        layer(sp.lines(coast,col="black"),under=F))
}
dev.off()



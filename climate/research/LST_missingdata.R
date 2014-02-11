### Short script to make a plot showing missing LST data in tiles from Alberto
library(rasterVis)

setwd("~/Downloads/nasa/")


f=data.frame(full=T,path=list.files(pattern="tif$"),stringsAsFactors=F)
f$month=as.numeric(do.call(rbind,strsplit(f$path,"_|[.]"))[,7])
f=f[order(f$month),]
f$mn=month.name[f$month]

d=stack(f$path)
names(d)=f$mn


colramp=colorRampPalette(c("blue","orange","red"))

png("Climatologies.png",width=1500,height=600,pointsize=22)
levelplot(d,col.regions=c("grey",colramp(99)),at=c(-0.5,0.5,seq(1,70,len=99)),main="Land Surface Temperature - Monthly Climatologies",sub="Tile H08v05 (California and Northern Mexico) \n Grey indicates missing data")
dev.off()

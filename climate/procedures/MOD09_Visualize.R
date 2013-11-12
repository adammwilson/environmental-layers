## script to visualize cloud frequency data

setwd("~/acrobates/adamw/projects/cloud/")

library(rasterVis)


#### Evaluate MOD35 Cloud data
mod09=brick("~/acrobates/adamw/projects/cloud/data/mod09.nc")
NAvalue(mod09)=-1

cols=colorRampPalette(c("black","blue","red"))

r=mod09[[10]]
levelplot(r,col.regions=cols(100),at=seq(0,100,len=100),margin=F,maxpixels=1e6)



## climatologies
mod09c=brick("~/acrobates/adamw/projects/cloud/data/mod09_clim.nc",varname="CF")
levelplot(mod09c,col.regions=cols(100),at=seq(0,100,len=100),margin=F,max.pixels=1e7)





## Figures associated with MOD35 Cloud Mask Exploration

setwd("~/acrobates/adamw/projects/MOD35C5")

library(raster);beginCluster(10)
library(rasterVis)
library(rgdal)
library(plotKML)
library(Cairo)
library(reshape)
library(rgeos)
library(splancs)


## add tags for distribution
## MOD35
tags=c("TIFFTAG_IMAGEDESCRIPTION='Collection 5 MOD35 Cloud Frequency for 2009 extracted from MOD09GA state_1km bits 0-1. The MOD35 bits encode four categories (with associated confidence that the pixel is actually clear): confidently clear (confidence > 0.99), probably clear (0.99 >= confidence > 0.95), probably cloudy (0.95 >= confidence > 0.66), and confidently cloudy (confidence <= 0.66).  Following the advice of the MODIS science team (Frey, 2010), we binned confidently clear and probably clear together as clear and the other two classes as cloudy.  The daily cloud mask time series were summarized to mean cloud frequency (CF) by calculating the proportion of cloudy days during 2009'",
  "TIFFTAG_DOCUMENTNAME='Collection 5 MOD35 Cloud Frequency'",
  "TIFFTAG_DATETIME='20090101'",
  "TIFFTAG_ARTIST='Adam M. Wilson (adam.wilson@yale.edu)'")
system(paste("/usr/local/src/gdal-1.10.0/swig/python/scripts/gdal_edit.py data/MOD35_2009.tif ",paste("-mo ",tags,sep="",collapse=" "),sep=""))
## MOD09
tags=c("TIFFTAG_IMAGEDESCRIPTION='Collection 5 MOD09 Cloud Frequency for 2009 extracted from MOD09GA \'PGE11\' internal cloud mask algorithm (embedded in MOD09GA \'state_1km\' bit 10. The daily cloud mask time series were summarized to mean cloud frequency (CF) by calculating the proportion of cloudy days during 2009'",
  "TIFFTAG_DOCUMENTNAME='Collection 5 MOD09 Cloud Frequency'",
  "TIFFTAG_DATETIME='20090101'",
  "TIFFTAG_ARTIST='Adam M. Wilson (adam.wilson@yale.edu)'")
system(paste("/usr/local/src/gdal-1.10.0/swig/python/scripts/gdal_edit.py data/MOD09_2009.tif ",paste("-mo ",tags,sep="",collapse=" "),sep=""))

## get % cloudy
mod09=raster("data/MOD09_2009.tif")
names(mod09)="C5MOD09CF"
NAvalue(mod09)=0

mod35c5=raster("data/MOD35_2009.tif")
names(mod35c5)="C5MOD35CF"
NAvalue(mod35c5)=0


## mod35C6 annual
if(!file.exists("data/MOD35C6_2009.tif")){
  system("/usr/local/gdal-1.10.0/bin/gdalbuildvrt  -a_srs '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs' -sd 1 -b 1 data/MOD35C6.vrt `find /home/adamw/acrobates/adamw/projects/interp/data/modis/mod35/summary/ -name '*h[1-9]*_mean.nc'` ")
  system("align.sh data/MOD35C6.vrt data/MOD09_2009.tif data/MOD35C6_2009.tif")
}

mod35c6=raster("data/MOD35C6_2009.tif")
names(mod35c6)="C6MOD35CF"
NAvalue(mod35c6)=255

## landcover
if(!file.exists("data/MCD12Q1_IGBP_2009_051_wgs84_1km.tif")){
  system(paste("/usr/local/gdal-1.10.0/bin/gdalwarp -tr 0.008983153 0.008983153 -r mode -ot Byte -co \"COMPRESS=LZW\"",
               " /mnt/data/jetzlab/Data/environ/global/MODIS/MCD12Q1/051/MCD12Q1_051_2009_wgs84.tif ",
               " -t_srs \"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs\" ",
               " -te -180.0044166 -60.0074610 180.0044166 90.0022083 ",
               "data/MCD12Q1_IGBP_2009_051_wgs84_1km.tif -overwrite ",sep=""))}
lulc=raster("data/MCD12Q1_IGBP_2009_051_wgs84_1km.tif")

  data(worldgrids_pal)  #load palette
  IGBP=data.frame(ID=0:16,col=worldgrids_pal$IGBP[-c(18,19)],
    lulc_levels2=c("Water","Forest","Forest","Forest","Forest","Forest","Shrublands","Shrublands","Savannas","Savannas","Grasslands","Permanent wetlands","Croplands","Urban and built-up","Cropland/Natural vegetation mosaic","Snow and ice","Barren or sparsely vegetated"),stringsAsFactors=F)
  IGBP$class=rownames(IGBP);rownames(IGBP)=1:nrow(IGBP)
  levels(lulc)=list(IGBP)
names(lulc)="MCD12Q1"

## make land mask
if(!file.exists("data/land.tif"))
  land=calc(lulc,function(x) ifelse(x==0,NA,1),file="data/land.tif",options=c("COMPRESS=LZW","ZLEVEL=9","PREDICTOR=2"),datatype="INT1U",overwrite=T)
land=raster("data/land.tif")

#####################################
### compare MOD43 and MOD17 products

## MOD17
if(!file.exists("data/MOD17.tif"))
system("align.sh ~/acrobates/adamw/projects/interp/data/modis/MOD17/MOD17A3_Science_NPP_mean_00_12.tif data/MOD09_2009.tif data/MOD17.tif")
mod17=raster("data/MOD17.tif",format="GTiff")
NAvalue(mod17)=65535
names(mod17)="MOD17_unscaled"

if(!file.exists("data/MOD17qc.tif"))
  system("align.sh ~/acrobates/adamw/projects/interp/data/modis/MOD17/MOD17A3_Science_NPP_Qc_mean_00_12.tif data/MOD09_2009.tif data/MOD17qc.tif")
mod17qc=raster("data/MOD17qc.tif",format="GTiff")
NAvalue(mod17qc)=255
names(mod17qc)="MOD17CF"

## MOD11 via earth engine
if(!file.exists("data/MOD11_2009.tif"))
  system("align.sh ~/acrobates/adamw/projects/interp/data/modis/mod11/2009/MOD11_LST_2009.tif data/MOD09_2009.tif data/MOD11_2009.tif")
mod11=raster("data/MOD11_2009.tif",format="GTiff")
names(mod11)="MOD11_unscaled"
NAvalue(mod11)=0
if(!file.exists("data/MOD11qc_2009.tif"))
  system("align.sh ~/acrobates/adamw/projects/interp/data/modis/mod11/2009/MOD11_Pmiss_2009.tif data/MOD09_2009.tif data/MOD11qc_2009.tif")
mod11qc=raster("data/MOD11qc_2009.tif",format="GTiff")
names(mod11qc)="MOD11CF"

### Processing path
if(!file.exists("data/MOD35pp.tif"))
system("align.sh data/C5MOD35_ProcessPath.tif data/MOD09_2009.tif data/MOD35pp.tif")
pp=raster("data/MOD35pp.tif")
NAvalue(pp)=255
names(pp)="MOD35pp"


###
n=100
at=seq(0,100,len=n)
cols=grey(seq(0,1,len=n))
cols=rainbow(n)
bgyr=colorRampPalette(c("blue","green","yellow","red"))
cols=bgyr(n)

### Transects
r1=Lines(list(
  Line(matrix(c(
                -61.688,4.098,
                -59.251,3.430
                ),ncol=2,byrow=T))),"Venezuela")
r2=Lines(list(
  Line(matrix(c(
                133.746,-31.834,
                134.226,-32.143
                ),ncol=2,byrow=T))),"Australia")
r3=Lines(list(
  Line(matrix(c(
                73.943,27.419,
                74.369,26.877
                ),ncol=2,byrow=T))),"India")
r4=Lines(list(
  Line(matrix(c(
                33.195,12.512,
                33.802,12.894
                ),ncol=2,byrow=T))),"Sudan")



trans=SpatialLines(list(r1,r2,r3,r4),CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))
### write out shapefiles of transects
writeOGR(SpatialLinesDataFrame(trans,data=data.frame(ID=names(trans)),match.ID=F),"output",layer="transects",driver="ESRI Shapefile",overwrite=T)

## buffer transects to get regional values 
transb=gBuffer(trans,byid=T,width=0.4)

## make polygons of bounding boxes
bb0 <- lapply(slot(transb, "polygons"), bbox)
bb1 <- lapply(bb0, bboxx)
# turn these into matrices using a helper function in splancs
bb2 <- lapply(bb1, function(x) rbind(x, x[1,]))
# close the matrix rings by appending the first coordinate
rn <- row.names(transb)
# get the IDs
bb3 <- vector(mode="list", length=length(bb2))
# make somewhere to keep the output
for (i in seq(along=bb3)) bb3[[i]] <- Polygons(list(Polygon(bb2[[i]])),
                   ID=rn[i])
# loop over the closed matrix rings, adding the IDs
bbs <- SpatialPolygons(bb3, proj4string=CRS(proj4string(transb)))

trd1=lapply(1:length(transb),function(x) {
  td=crop(mod11,transb[x])
  tdd=lapply(list(mod35c5,mod35c6,mod09,mod17,mod17qc,mod11,mod11qc,lulc,pp),function(l) resample(crop(l,transb[x]),td,method="ngb"))
  ## normalize MOD11 and MOD17
  for(j in which(do.call(c,lapply(tdd,function(i) names(i)))%in%c("MOD11_unscaled","MOD17_unscaled"))){
    trange=cellStats(tdd[[j]],range)
    tscaled=100*(tdd[[j]]-trange[1])/(trange[2]-trange[1])
    tscaled@history=list(range=trange)
    names(tscaled)=sub("_unscaled","",names(tdd[[j]]))
    tdd=c(tdd,tscaled)
  }
  return(brick(tdd))
})

## bind all subregions into single dataframe for plotting
trd=do.call(rbind.data.frame,lapply(1:length(trd1),function(i){
  d=as.data.frame(as.matrix(trd1[[i]]))
  d[,c("x","y")]=coordinates(trd1[[i]])
  d$trans=names(trans)[i]
  d=melt(d,id.vars=c("trans","x","y"))
  return(d)
}))

transd=do.call(rbind.data.frame,lapply(1:length(trans),function(l) {
  td=as.data.frame(extract(trd1[[l]],trans[l],along=T,cellnumbers=F)[[1]])
  td$loc=extract(trd1[[l]],trans[l],along=T,cellnumbers=T)[[1]][,1]
  td[,c("x","y")]=xyFromCell(trd1[[l]],td$loc)
  td$dist=spDistsN1(as.matrix(td[,c("x","y")]), as.matrix(td[1,c("x","y")]),longlat=T)
  td$transect=names(trans[l])
  td2=melt(td,id.vars=c("loc","x","y","dist","transect"))
  td2=td2[order(td2$variable,td2$dist),]
  # get per variable ranges to normalize
  tr=cast(melt.list(tapply(td2$value,td2$variable,function(x) data.frame(min=min(x,na.rm=T),max=max(x,na.rm=T)))),L1~variable)
  td2$min=tr$min[match(td2$variable,tr$L1)]
  td2$max=tr$max[match(td2$variable,tr$L1)]
  print(paste("Finished ",names(trans[l])))
  return(td2)}
  ))

transd$type=ifelse(grepl("MOD35|MOD09|CF",transd$variable),"CF","Data")


## comparison of % cloudy days
if(!file.exists("data/dif_c5_09.tif"))
  overlay(mod35c5,mod09,fun=function(x,y) {return(x-y)},file="data/dif_c5_09.tif",format="GTiff",options=c("COMPRESS=LZW","ZLEVEL=9"),overwrite=T)
dif_c5_09=raster("data/dif_c5_09.tif",format="GTiff")


##################################################################################
## Identify problematic areas with hard edges in cloud frequency
############################
library(multicore)

## set up processing chunks
nrw=nrow(mod35c5)
nby=10
nrwg=seq(1,nrw,by=nby)
writeLines(paste("Processing ",length(nrwg)," groups and",nrw,"lines"))

## Parallel loop to conduct moving window analysis and quantify pixels with significant shifts across pp or lulc boundaries
output=mclapply(nrwg,function(ti){
  ## Extract focal areas 
  ngb=5
  vals=getValuesFocal(mod35c5,ngb=ngb,row=ti,nrows=nby)
  vals_mod09=getValuesFocal(mod09,ngb=ngb,row=ti,nrows=nby)
  pp_ind=getValuesFocal(pp,ngb=ngb,row=ti,nrows=nby)
  lulc_ind=getValuesFocal(lulc,ngb=ngb,row=ti,nrows=nby)
  ## processing path
  pp_bias=raster(matrix(do.call(rbind,lapply(1:nrow(vals),function(i) {
    tind1=pp_ind[i,]  #vector of indices
    tval1=vals[i,]    # vector of values
    tind2=tind1[!is.na(tind1)&!is.na(tval1)]  #which classes exist without NAs?
    if(length(unique(tind2))<2) return(255)  #if only one class, return 255
    if(sort(table(tind2),dec=T)[2]<5) return(254) # if too few observations of class 2, return 254
#    return(round(kruskal.test(tval1,tind1)$p.value*100))         # if it works, return p.value*100
    m=mean(tval1,na.rm=T)
    dif=round(diff(range(tapply(tval1,tind1,mean)))/m*100)
    return(dif)         # if it works, return p.value*100
  })),nrow=nby,ncol=ncol(mod35c5),byrow=T))     # turn it back into a raster
  ## update raster and write it
  extent(pp_bias)=extent(mod35c5[ti:(ti+nby-1),1:ncol(mod35c5),drop=F])
  projection(pp_bias)=projection(mod35c5)
  NAvalue(pp_bias) <- 255
  writeRaster(pp_bias,file=paste("data/tiles/pp_bias_",ti,".tif",sep=""),
              format="GTiff",dataType="INT1U",overwrite=T,NAflag=255) #,options=c("COMPRESS=LZW","ZLEVEL=9")
  ## landcover
  lulc_bias=raster(matrix(do.call(rbind,lapply(1:nrow(vals),function(i) {
    tind1=lulc_ind[i,]  #vector of indices
    tval1=vals[i,]    # vector of values
    tind2=tind1[!is.na(tind1)&!is.na(tval1)]  #which classes exist without NAs?
    if(length(unique(tind2))<2) return(255)  #if only one class, return 255
    if(sort(table(tind2),dec=T)[2]<5) return(254) # if too few observations of class 2, return 254
#    return(round(kruskal.test(tval1,tind1)$p.value*100))         # if it works, get p.value*100
    m=mean(tval1,na.rm=T)
    dif=round(diff(range(tapply(tval1,tind1,mean)))/m*100)
    return(dif)         # if it works, return the normalized difference
  })),nrow=nby,ncol=ncol(mod35c5),byrow=T))     # turn it back into a raster
  ## udpate raster and write it
  extent(lulc_bias)=extent(mod35c5[ti:(ti+nby-1),1:ncol(mod35c5),drop=F])
  projection(lulc_bias)=projection(mod35c5)
  NAvalue(lulc_bias) <- 255
  writeRaster(lulc_bias,file=paste("data/tiles/lulc_bias_",ti,".tif",sep=""),
              format="GTiff",dataType="INT1U",overwrite=T,NAflag=255)#,options=c("COMPRESS=LZW","ZLEVEL=9")

    ## MOD09
  mod09_lulc_bias=raster(matrix(do.call(rbind,lapply(1:nrow(vals_mod09),function(i) {
    tind1=lulc_ind[i,]  #vector of indices
    tval1=vals_mod09[i,]    # vector of values
    tind2=tind1[!is.na(tind1)&!is.na(tval1)]  #which classes exist without NAs?
    if(length(unique(tind2))<2) return(255)  #if only one class, return 255
    if(sort(table(tind2),dec=T)[2]<5) return(254) # if too few observations of class 2, return 254
#    return(round(kruskal.test(tval1,tind1)$p.value*100))         # if it works, get p.value*100
    m=mean(tval1,na.rm=T)
    dif=round(diff(range(tapply(tval1,tind1,mean)))/m*100)
    return(dif)         # if it works, return normalized difference
  })),nrow=nby,ncol=ncol(mod35c5),byrow=T))     # turn it back into a raster
  ## udpate raster and write it
  extent(mod09_lulc_bias)=extent(mod09[ti:(ti+nby-1),1:ncol(mod09),drop=F])
  projection(mod09_lulc_bias)=projection(mod09)
  NAvalue(mod09_lulc_bias) <- 255
  writeRaster(mod09_lulc_bias,file=paste("data/tiles/mod09_lulc_bias_",ti,".tif",sep=""),
              format="GTiff",dataType="INT1U",overwrite=T,NAflag=255)#,options=c("COMPRESS=LZW","ZLEVEL=9")

  return(ti)
},mc.cores=15)


### check raster temporary files
showTmpFiles()
#removeTmpFiles(h=1)

## merge all the files back again
  system("gdalbuildvrt -srcnodata 255 -vrtnodata 255 data/lulc_bias.vrt `find data/tiles -name 'lulc_bias*tif'` ")
  system("gdalwarp -srcnodata 255 -dstnodata 255 -multi -r bilinear -co 'COMPRESS=LZW' -co 'ZLEVEL=9' data/lulc_bias.vrt data/lulc_bias.tif -r near")
#  system("align.sh data/lulc_bias.vrt data/MOD09_2009.tif data/lulc_bias.tif")

  system("gdalbuildvrt data/pp_bias.vrt `find data/tiles -name 'pp_bias*tif'` ")
  system("gdalwarp -srcnodata 255 -dstnodata 255 -multi -r bilinear -co 'COMPRESS=LZW' -co 'ZLEVEL=9' data/pp_bias.vrt data/pp_bias.tif -r near")
  system("align.sh -srcnodata 255 -dstnodata 255 -multi -r bilinear data/pp_bias.vrt data/MOD09_2009.tif data/pp_bias_align.tif &")

  system("gdalbuildvrt data/mod09_lulc_bias.vrt `find data/tiles -name 'mod09_lulc_bias*tif'` ")
  system("gdalwarp -srcnodata 255 -dstnodata 255 -multi -r bilinear -co 'COMPRESS=LZW' -co 'ZLEVEL=9' data/mod09_lulc_bias.vrt data/mod09_lulc_bias.tif -r near")

### read them back in
pp_bias=raster("data/pp_bias.tif")
names(pp_bias)="Processing Path"
NAvalue(pp_bias)=255
lulc_bias=raster("data/lulc_bias.tif")
names(lulc_bias)="Land Use Land Cover"
NAvalue(lulc_bias)=255

## read in WWF biome data to summarize by biome
if(!file.exists("data/teow/wwf_terr_ecos.shp"){
  system("wget -O data/teow.zip http://assets.worldwildlife.org/publications/15/files/original/official_teow.zip?1349272619")
  system("unzip data/teow.zip -d data/teow/")
   biome=readOGR("data/teow/wwf_terr_ecos.shp","wwf_terr_ecos")
   biome=biome[biome$BIOME<50,]
   biome2=gUnaryUnion(biome,id=biome$BIOME)
  ## create biome.csv using names in html file   
  biomeid=read.csv("data/teow/biome.csv",stringsAsFactors=F)
  biome2=SpatialPolygonsDataFrame(biome2,data=biomeid)
  writeOGR(biome2,"data/teow","biomes",driver="ESRI Shapefile")
}
   biome=readOGR("data/teow/biomes.shp","biomes")

biome2=extract(pp_bias,biome[biome$Biome==12,],df=T,fun=function(x) data.frame(mean=mean(x,na.rm=T),sd=sd(x,na.rm=T),prop=(sum(!is.na(x))/length(x))))

pat=c(seq(0,100,len=100),254)#seq(0,0.-5,len=2) #,seq(0.05,.1,len=50))
grayr2=colorRampPalette(c("grey","green","red"))#
#grayr2=colorRampPalette(grey(c(.75,.5,.25)))
levelplot(stack(pp_bias,lulc_bias),col.regions=c(grayr2(2)),at=pat,
          colorkey=F,margin=F,maxpixels=1e6)+layer(sp.lines(coast,lwd=.5))
levelplot(lulc_bias,col.regions=c(grayr2(100),"black"),at=pat,
          colorkey=T,margin=F,maxpixels=1e4)+layer(sp.lines(coast,lwd=.5))

histogram(lulc_bias)

cor(td1$MOD17,td1$C6MOD35,use="complete",method="spearman")
cor(td1$MOD17[td1$edgeb==1],td1$C5MOD35[td1$edgeb==1],use="complete",method="spearman")

bwplot(value~MOD35pp|variable,data=td1l[td1l$pedgeb==1,],horizontal=F)

crosstab(dif_c5_09,pp)
### Correlations
#trdw=cast(trd,trans+x+y~variable,value="value")
#cor(trdw$MOD17,trdw$C5MOD35,use="complete",method="spearman")

#Across all pixels in the four regions analyzed in Figure 3 there is a much larger correlation between mean NPP and the C5 MOD35 CF (Spearman’s ρ = -0.61, n=58,756) than the C6 MOD35 CF (ρ = 0.00, n=58,756) or MOD09 (ρ = -0.07, n=58,756) products.  
#by(trdw,trdw$trans,function(x) cor(as.data.frame(na.omit(x[,c("C5MOD35CF","C6MOD35CF","C5MOD09CF","MOD17","MOD11")])),use="complete",method="spearman"))


## table of correlations
#trdw_cor=as.data.frame(na.omit(trdw[,c("C5MOD35CF","C6MOD35CF","C5MOD09CF","MOD17","MOD11")]))
#nrow(trdw_cor)
#round(cor(trdw_cor,method="spearman"),2)


## set up some graphing parameters
at=seq(0,100,leng=100)
bgyr=colorRampPalette(c("purple","blue","green","yellow","orange","red","red"))
bgray=colorRampPalette(c("purple","blue","deepskyblue4"))
grayr=colorRampPalette(c("grey","red","darkred"))
bgrayr=colorRampPalette(c("darkblue","blue","grey","red","purple"))

cols=bgyr(100)

strip=strip.custom(par.strip.text=list(cex=.7),bg="transparent")

## global map
library(maptools)
coast=map2SpatialLines(map("world", interior=FALSE, plot=FALSE),proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

g1=levelplot(stack(mod35c5,mod35c6,mod09),xlab=" ",scales=list(x=list(draw=F),y=list(alternating=1)),
  col.regions=cols,at=at,cuts=length(at),maxpixels=1e6,
  colorkey=list(at=at))+
#  layer(sp.polygons(bbs,lwd=5,col="black"))+
  layer(sp.lines(coast,lwd=.5))+
  layer(sp.points(coordinates(bbs),col="black",cex=2,pch=13,lwd=2))

### Plot of differences between MOD09 adn MOD35 masks
#system("gdalinfo -stats /home/adamw/acrobates/adamw/projects/MOD35C5/data/dif_c5_09.tif")
## get quantiles for color bar of differences
#qs=unique(quantile(as.vector(as.matrix(dif_c5_09)),seq(0,1,len=100),na.rm=T))
#c(bgray(sum(qs<0)),grayr(sum(qs>=0)+1))
qs=seq(-80,80,len=100)
g2=levelplot(dif_c5_09,col.regions=bgrayr(100),cuts=100,at=qs,margin=F,ylab=" ",colorkey=list("right",at=qs),maxpixels=1e6)+
  layer(sp.points(coordinates(bbs),col="black",cex=2,pch=13,lwd=2))+
  #layer(sp.polygons(bbs,lwd=2))+
  layer(sp.lines(coast,lwd=.5))

g2$strip=strip.custom(var.name="Difference (C5MOD35-C5MOD09)",style=1,strip.names=T,strip.levels=F)  #update strip text
#g3=histogram(dif_c5_09,col="black",border=NA,scales=list(x=list(at=c(-50,0,50)),y=list(draw=F),cex=1))+layer(panel.abline(v=0,col="red",lwd=2))

### regional plots
p1=useOuterStrips(levelplot(value~x*y|variable+trans,data=trd[!trd$variable%in%c("MOD17_unscaled","MOD11_unscaled","MCD12Q1","MOD35pp"),],asp=1,scales=list(draw=F,rot=0,relation="free"),
                                       at=at,col.regions=cols,maxpixels=7e6,
                                       ylab="Latitude",xlab="Longitude"),strip.left=strip,strip = strip)+layer(sp.lines(trans,lwd=2))

p2=useOuterStrips(
  levelplot(value~x*y|variable+trans,data=trd[trd$variable%in%c("MCD12Q1"),],
            asp=1,scales=list(draw=F,rot=0,relation="free"),colorkey=F,
            at=c(-1,IGBP$ID),col.regions=IGBP$col,maxpixels=7e7,
            legend=list(
              right=list(fun=draw.key(list(columns=1,#title="MCD12Q1 \n IGBP Land \n Cover",
                           rectangles=list(col=IGBP$col,size=1),
                           text=list(as.character(IGBP$ID),at=IGBP$ID-.5))))),
            ylab="",xlab=" "),strip = strip,strip.left=F)+layer(sp.lines(trans,lwd=2))
p3=useOuterStrips(
  levelplot(value~x*y|variable+trans,data=trd[trd$variable%in%c("MOD35pp"),],
            asp=1,scales=list(draw=F,rot=0,relation="free"),colorkey=F,
            at=c(-1:4),col.regions=c("blue","cyan","tan","darkgreen"),maxpixels=7e7,
            legend=list(
              right=list(fun=draw.key(list(columns=1,#title="MOD35 \n Processing \n Path",
                           rectangles=list(col=c("blue","cyan","tan","darkgreen"),size=1),
                           text=list(c("Water","Coast","Desert","Land")))))),
            ylab="",xlab=" "),strip = strip,strip.left=F)+layer(sp.lines(trans,lwd=2))

## transects
p4=xyplot(value~dist|transect,groups=variable,type=c("smooth","p"),
       data=transd,panel=function(...,subscripts=subscripts) {
         td=transd[subscripts,]
         ## mod09
         imod09=td$variable=="C5MOD09CF"
         panel.xyplot(td$dist[imod09],td$value[imod09],type=c("p","smooth"),span=0.2,subscripts=1:sum(imod09),col="red",pch=16,cex=.25)
         ## mod35C5
         imod35=td$variable=="C5MOD35CF"
         panel.xyplot(td$dist[imod35],td$value[imod35],type=c("p","smooth"),span=0.09,subscripts=1:sum(imod35),col="blue",pch=16,cex=.25)
         ## mod35C6
         imod35c6=td$variable=="C6MOD35CF"
         panel.xyplot(td$dist[imod35c6],td$value[imod35c6],type=c("p","smooth"),span=0.09,subscripts=1:sum(imod35c6),col="black",pch=16,cex=.25)
         ## mod17
         imod17=td$variable=="MOD17"
         panel.xyplot(td$dist[imod17],100*((td$value[imod17]-td$min[imod17][1])/(td$max[imod17][1]-td$min[imod17][1])),
                      type=c("smooth"),span=0.09,subscripts=1:sum(imod17),col="darkgreen",lty=5,pch=1,cex=.25)
         imod17qc=td$variable=="MOD17CF"
         panel.xyplot(td$dist[imod17qc],td$value[imod17qc],type=c("p","smooth"),span=0.09,subscripts=1:sum(imod17qc),col="darkgreen",pch=16,cex=.25)
         ## mod11
         imod11=td$variable=="MOD11"
         panel.xyplot(td$dist[imod11],100*((td$value[imod11]-td$min[imod11][1])/(td$max[imod11][1]-td$min[imod11][1])),
                      type=c("smooth"),span=0.09,subscripts=1:sum(imod17),col="orange",lty="dashed",pch=1,cex=.25)
         imod11qc=td$variable=="MOD11CF"
         qcspan=ifelse(td$transect[1]=="Australia",0.2,0.05)
         panel.xyplot(td$dist[imod11qc],td$value[imod11qc],type=c("p","smooth"),npoints=100,span=qcspan,subscripts=1:sum(imod11qc),col="orange",pch=16,cex=.25)
         ## land
         path=td[td$variable=="MOD35pp",]
         panel.segments(path$dist,-10,c(path$dist[-1],max(path$dist,na.rm=T)),-10,col=c("blue","cyan","tan","darkgreen")[path$value+1],subscripts=1:nrow(path),lwd=10,type="l")
         land=td[td$variable=="MCD12Q1",]
         panel.segments(land$dist,-20,c(land$dist[-1],max(land$dist,na.rm=T)),-20,col=IGBP$col[land$value+1],subscripts=1:nrow(land),lwd=10,type="l")
        },subscripts=T,par.settings = list(grid.pars = list(lineend = "butt")),
       scales=list(
         x=list(alternating=1,relation="free"),#, lim=c(0,70)),
         y=list(at=c(-20,-10,seq(0,100,len=5)),
           labels=c("MCD12Q1 IGBP","MOD35 path",seq(0,100,len=5)),
           lim=c(-25,100)),
         alternating=F),
       xlab="Distance Along Transect (km)", ylab="% Missing Data / % of Maximum Value",
       legend=list(
         bottom=list(fun=draw.key(list( rep=FALSE,columns=1,title=" ",
                      lines=list(type=c("b","b","b","b","b","l","b","l"),pch=16,cex=.5,
                        lty=c(0,1,1,1,1,5,1,5),
                        col=c("transparent","red","blue","black","darkgreen","darkgreen","orange","orange")),
                       text=list(
                         c("MODIS Products","C5 MOD09 % Cloudy","C5 MOD35 % Cloudy","C6 MOD35 % Cloudy","MOD17 % Missing","MOD17 (scaled)","MOD11 % Missing","MOD11 (scaled)")),
                       rectangles=list(border=NA,col=c(NA,"tan","darkgreen")),
                       text=list(c("C5 MOD35 Processing Path","Desert","Land")),
                       rectangles=list(border=NA,col=c(NA,IGBP$col[sort(unique(transd$value[transd$variable=="MCD12Q1"]+1))])),
                       text=list(c("MCD12Q1 IGBP Land Cover",IGBP$class[sort(unique(transd$value[transd$variable=="MCD12Q1"]+1))])))))),
  strip = strip,strip.left=F)
#print(p4)


CairoPDF("output/mod35compare.pdf",width=11,height=7)
#CairoPNG("output/mod35compare_%d.png",units="in", width=11,height=8.5,pointsize=4000,dpi=1200,antialias="subpixel")
### Global Comparison
print(g1,position=c(0,.35,1,1),more=T)
print(g2,position=c(0,0,1,0.415),more=F)
#print(g3,position=c(0.31,0.06,.42,0.27),more=F)
         
### MOD35 Desert Processing path
levelplot(pp,asp=1,scales=list(draw=T,rot=0),maxpixels=1e6,cuts=3,
          at=(0:3)+.5,col.regions=c("blue","cyan","tan","darkgreen"),margin=F,
          colorkey=list(space="top",title="MOD35 Processing Path",labels=list(labels=c("Water","Coast","Desert","Land"),at=0:3),height=.25))+
  layer(sp.points(coordinates(bbs),col="black",cex=2,pch=13,lwd=2))+
  layer(sp.lines(coast,lwd=.5))

### levelplot of regions
print(p1,position=c(0,0,.62,1),more=T)
print(p2,position=c(0.6,0.21,0.78,0.79),more=T)
print(p3,position=c(0.76,0.21,1,0.79))
### profile plots
print(p4)
dev.off()

### summary stats for paper
td=cast(transect+loc+dist~variable,value="value",data=transd)
td2=melt.data.frame(td,id.vars=c("transect","dist","loc","MOD35pp","MCD12Q1"))

## function to prettyprint mean/sd's
msd= function(x) paste(round(mean(x,na.rm=T),1),"% ±",round(sd(x,na.rm=T),1),sep="")

cast(td2,transect+variable~MOD35pp,value="value",fun=msd)
cast(td2,transect+variable~MOD35pp+MCD12Q1,value="value",fun=msd)
cast(td2,transect+variable~.,value="value",fun=msd)

cast(td2,transect+variable~.,value="value",fun=msd)

cast(td2,variable~MOD35pp,value="value",fun=msd)
cast(td2,variable~.,value="value",fun=msd)

td[td$transect=="Venezuela",]


#### export KML
library(plotKML)

kml_open("output/modiscloud.kml")

readAll(mod35c5)

kml_layer.Raster(mod35c5,
     plot.legend = TRUE,raster_name="Collection 5 MOD35 2009 Cloud Frequency",
    z.lim = c(0,100),colour_scale = get("colour_scale_numeric", envir = plotKML.opts),
#    home_url = get("home_url", envir = plotKML.opts),
#    metadata = NULL, html.table = NULL,
    altitudeMode = "clampToGround", balloon = FALSE
)

system(paste("gdal_translate -of KMLSUPEROVERLAY ",mod35c5@file@name," output/mod35c5.kmz -co FORMAT=JPEG"))

logo = "http://static.tumblr.com/t0afs9f/KWTm94tpm/yale_logo.png"
kml_screen(image.file = logo, position = "UL", sname = "YALE logo",size=c(.1,.1))
kml_close("modiscloud.kml")
kml_compress("modiscloud.kml",files=c(paste(month.name,".png",sep=""),"obj_legend.png"),zip="/usr/bin/zip")

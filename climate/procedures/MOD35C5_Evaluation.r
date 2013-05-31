## Figures associated with MOD35 Cloud Mask Exploration

setwd("~/acrobates/adamw/projects/MOD35C5")

library(raster);beginCluster(10)
library(rasterVis)
library(rgdal)
library(plotKML)
library(Cairo)
library(reshape)

## get % cloudy
mod09=raster("data/MOD09_2009.tif")
names(mod09)="MOD09_cloud"

mod35c5=raster("data/MOD35_2009.tif")
mod35c5=crop(mod35c5,mod09)
names(mod35c5)="MOD35C5_cloud"

mod35c6=raster("")
 
## landcover
if(!file.exists("data/MCD12Q1_IGBP_2005_v51_1km_wgs84.tif")){
  system(paste("gdalwarp -r near -ot Byte -co \"COMPRESS=LZW\"",
               " ~/acrobatesroot/jetzlab/Data/environ/global/landcover/MODIS/MCD12Q1_IGBP_2005_v51.tif ",
               " -t_srs \"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs\" ",
               tempdir(),"/MCD12Q1_IGBP_2005_v51_wgs84.tif -overwrite ",sep=""))
  lulc=raster(paste(tempdir(),"/MCD12Q1_IGBP_2005_v51_wgs84.tif",sep=""))
  ## aggregate to 1km resolution
  lulc2=aggregate(lulc,2,fun=function(x,na.rm=T) {
    x=na.omit(x)
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  },file=paste(tempdir(),"/1km.tif",sep=""))
  writeRaster(lulc2,"data/MCD12Q1_IGBP_2005_v51_1km_wgs84.tif",options=c("COMPRESS=LZW","ZLEVEL=9","PREDICTOR=2"),datatype="INT1U",overwrite=T)
}
lulc=raster("data/MCD12Q1_IGBP_2005_v51_1km_wgs84.tif")
#  lulc=ratify(lulc)
  data(worldgrids_pal)  #load palette
  IGBP=data.frame(ID=0:16,col=worldgrids_pal$IGBP[-c(18,19)],
    lulc_levels2=c("Water","Forest","Forest","Forest","Forest","Forest","Shrublands","Shrublands","Savannas","Savannas","Grasslands","Permanent wetlands","Croplands","Urban and built-up","Cropland/Natural vegetation mosaic","Snow and ice","Barren or sparsely vegetated"),stringsAsFactors=F)
  IGBP$class=rownames(IGBP);rownames(IGBP)=1:nrow(IGBP)
  levels(lulc)=list(IGBP)
extent(lulc)=alignExtent(lulc,mod09)
names(lulc)="MCD12Q1"

## make land mask
land=calc(lulc,function(x) ifelse(x==0,NA,1),file="data/land.tif",options=c("COMPRESS=LZW","ZLEVEL=9","PREDICTOR=2"),datatype="INT1U",overwrite=T)
land=raster("data/land.tif")

#####################################
### compare MOD43 and MOD17 products

## MOD17
mod17=raster("data/MOD17A3_Science_NPP_mean_00_12.tif",format="GTiff")
NAvalue(mod17)=65535
#extent(mod17)=alignExtent(mod17,mod09)
mod17=crop(mod17,mod09)
names(mod17)="MOD17"

mod17qc=raster("data/MOD17A3_Science_NPP_Qc_mean_00_12.tif",format="GTiff")
NAvalue(mod17qc)=255
                                        #extent(mod17qc)=alignExtent(mod17qc,mod09)
mod17qc=crop(mod17qc,mod09)
names(mod17qc)="MOD17qc"

## MOD11 via earth engine
mod11=raster("data/MOD11_2009.tif",format="GTiff")
names(mod11)="MOD11"
mod11qc=raster("data/MOD11_Pmiss_2009.tif",format="GTiff")
names(mod11qc)="MOD11qc"

## MOD43 via earth engine
mod43=raster("data/mod43_2009.tif",format="GTiff")
mod43qc=raster("data/mod43_2009.tif",format="GTiff")


### Create some summary objects for plotting
#difm=v6m-v5m
#v5v6compare=stack(v5m,v6m,difm)
#names(v5v6compare)=c("Collection 5","Collection 6","Difference (C6-C5)")

### Processing path
pp=raster("data/MOD35_ProcessPath.tif")
extent(pp)=alignExtent(pp,mod09)
pp=crop(pp,mod09)

## Summary plot of mod17 and mod43
modprod=stack(mod35c5,mod09,pp,lulc)#,mod43,mod43qc)
names(modprod)=c("MOD17","MOD17qc")#,"MOD43","MOD43qc")


## comparison of % cloudy days
dif=mod35c5-mod09
hist(dif,maxsamp=1000000)

## draw lulc-stratified random sample of mod35-mod09 differences 
samp=sampleStratified(lulc, 1000, exp=10)
save(samp,file="LULC_StratifiedSample_10000.Rdata")

mean(dif[samp],na.rm=T)

Stats(dif,function(x) c(mean=mean(x),sd=sd(x)))


###

n=100
at=seq(0,100,len=n)
cols=grey(seq(0,1,len=n))
cols=rainbow(n)
bgyr=colorRampPalette(c("blue","green","yellow","red"))
cols=bgyr(n)

#levelplot(lulcf,margin=F,layers="LULC")

CairoPDF("output/mod35compare.pdf",width=11,height=8.5)
#CairoPNG("output/mod35compare_%d.png",units="in", width=11,height=8.5,pointsize=4000,dpi=1200,antialias="subpixel")

### Transects
r1=Lines(list(
  Line(matrix(c(
                -61.183,1.165,
                -60.881,0.825
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
                -5.164,42.270,
                -4.948,42.162
                ),ncol=2,byrow=T))),"Spain")

r5=Lines(list(
  Line(matrix(c(
                24.170,-17.769,
                24.616,-18.084
                ),ncol=2,byrow=T))),"Africa")


trans=SpatialLines(list(r1,r2,r3,r4,r5),CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

transd=lapply(list(mod35c5,mod09,mod17,mod17qc,mod11qc,lulc,pp),function(l) {
  td=extract(l,trans,along=T,cellnumbers=F)
  names(td)=names(trans)                                        #  colnames(td)=c("value","transect")
  cells=extract(l,trans,along=T,cellnumbers=T)
  cells2=lapply(cells,function(x) xyFromCell(l,x[,1]))
  dists=lapply(cells2,function(x) spDistsN1(x,x[1,],longlat=T))
  td2=do.call(rbind.data.frame,lapply(1:length(td),function(i) cbind.data.frame(value=td[[i]],cells2[[i]],dist=dists[[i]],transect=names(td)[i])))
  td2$prod=names(l)
  td2$loc=rownames(td2)
  td2=td2[order(td2$dist),]
  print(paste("Finished ",names(l)))
  return(td2)}
  )
transdl=melt(transd,id.vars=c("prod","transect","loc","x","y","dist"))
transd$loc=as.numeric(transd$loc)
transdl$type=ifelse(grepl("MOD35|MOD09|qc",transdl$prod),"QC","Data")
  
nppid=transdl$prod=="MOD17"

xyplot(value~dist|transect,groups=prod,type=c("smooth","p"),
       data=transdl,panel=function(...,subscripts=subscripts) {
         td=transdl[subscripts,]
         ## mod09
         imod09=td$prod=="MOD09_cloud"
         panel.xyplot(td$dist[imod09],td$value[imod09],type=c("p","smooth"),span=0.2,subscripts=1:sum(imod09),col="red",pch=16,cex=.5)
         ## mod35C5
         imod35=td$prod=="MOD35C5_cloud"
         panel.xyplot(td$dist[imod35],td$value[imod35],type=c("p","smooth"),span=0.09,subscripts=1:sum(imod35),col="blue",pch=16,cex=.5)
         ## mod17
         imod17=td$prod=="MOD17"
         panel.xyplot(td$dist[imod17],100*td$value[imod17]/max(td$value[imod17]),
                      type=c("smooth"),span=0.09,subscripts=1:sum(imod17),col="darkgreen",lty="dashed",pch=1,cex=.5)
         imod17qc=td$prod=="MOD17qc"
         panel.xyplot(td$dist[imod17qc],td$value[imod17qc],type=c("p","smooth"),span=0.09,subscripts=1:sum(imod17qc),col="darkgreen",pch=16,cex=.5)
         ## mod11
#         imod11=td$prod=="MOD11"
#         panel.xyplot(td$dist[imod17],100*td$value[imod17]/max(td$value[imod17]),
#                      type=c("smooth"),span=0.09,subscripts=1:sum(imod17),col="darkgreen",lty="dashed",pch=1,cex=.5)
         imod11qc=td$prod=="MOD11qc"
         panel.xyplot(td$dist[imod11qc],td$value[imod11qc],type=c("p","smooth"),span=0.09,subscripts=1:sum(imod11qc),col="maroon",pch=16,cex=.5)
         ## means
         means=td$prod%in%c("","MOD17qc","MOD09_cloud","MOD35_cloud")
         ## land
         path=td[td$prod=="MOD35_ProcessPath",]
         panel.segments(path$dist,0,c(path$dist[-1],max(path$dist)),0,col=IGBP$col[path$value],subscripts=1:nrow(path),lwd=15,type="l")
         land=td[td$prod=="MCD12Q1",]
         panel.segments(land$dist,-5,c(land$dist[-1],max(land$dist)),-5,col=IGBP$col[land$value],subscripts=1:nrow(land),lwd=15,type="l")
       },subscripts=T,par.settings = list(grid.pars = list(lineend = "butt")),
       scales=list(
         x=list(alternating=1), #lim=c(0,50),
         y=list(at=c(-5,0,seq(20,100,len=5)),
           labels=c("IGBP","MOD35",seq(20,100,len=5)),
           lim=c(-10,100))),
       xlab="Distance Along Transect (km)", 
       key=list(space="right",lines=list(col=c("red","blue","darkgreen","maroon")),text=list(c("MOD09 % Cloudy","MOD35 % Cloudy","MOD17 % Missing","MOD11 % Missing"),lwd=1,col=c("red","blue","darkgreen","maroon"))))

### levelplot of regions


c(levelplot(mod35c5,margin=F),levelplot(mod09,margin=F),levelplot(mod11qc),levelplot(mod17qc),x.same = T, y.same = T)

levelplot(modprod)



### LANDCOVER
levelplot(lulcf,col.regions=levels(lulcf)[[1]]$col,
          scales=list(cex=2),
          colorkey=list(space="right",at=0:16,labels=list(at=seq(0.5,16.5,by=1),labels=levels(lulcf)[[1]]$class,cex=2)),margin=F)


levelplot(mcompare,col.regions=cols,at=at,margin=F,sub="Frequency of MOD35 Clouds in March")
#levelplot(dif,col.regions=bgyr(20),margin=F)
levelplot(mdiff,col.regions=bgyr(100),at=seq(mdiff@data@min,mdiff@data@max,len=100),margin=F)


boxplot(as.matrix(subset(dif,subset=1))~forest,varwidth=T,notch=T);abline(h=0)


levelplot(modprod,main="Missing Data (%) in MOD17 (NPP) and MOD43 (BRDF Reflectance)",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at)




levelplot(modprod,main="Missing Data (%) in MOD17 (NPP) and MOD43 (BRDF Reflectance)",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          xlim=c(-7300000,-6670000),ylim=c(0,600000))

levelplot(v5m,main="Missing Data (%) in MOD17 (NPP) and MOD43 (BRDF Reflectance)",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          xlim=c(-7200000,-6670000),ylim=c(0,400000),margin=F)


levelplot(subset(v5v6compare,1:2),main="Proportion Cloudy Days (%) in Collection 5 and 6 MOD35",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          margin=F)

levelplot(subset(v5v6compare,1:2),main="Proportion Cloudy Days (%) in Collection 5 and 6 MOD35",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          xlim=c(-7200000,-6670000),ylim=c(0,400000),margin=F)

levelplot(subset(v5v6compare,1:2),main="Proportion Cloudy Days (%) in Collection 5 and 6 MOD35",
          sub="Tile H11v08 (Venezuela)",col.regions=cols,at=at,
          xlim=c(-7500000,-7200000),ylim=c(700000,1000000),margin=F)


dev.off()

### smoothing plots
## explore smoothed version
td=subset(v6,m)
## build weight matrix
s=3
w=matrix(1/(s*s),nrow=s,ncol=s)
#w[s-1,s-1]=4/12; w
td2=focal(td,w=w)
td3=stack(td,td2)

levelplot(td3,col.regions=cols,at=at,margin=F)

dev.off()
plot(stack(difm,lulc))

### ROI
tile_ll=projectExtent(v6, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

62,59
0,3



#### export KML timeseries
library(plotKML)
tile="h11v08"
file=paste("summary/MOD35_",tile,".nc",sep="")
system(paste("gdalwarp -overwrite -multi -ot INT16 -r cubicspline -srcnodata 255 -dstnodata 255 -s_srs '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs' -t_srs 'EPSG:4326' NETCDF:",file,":PCloud  MOD35_",tile,".tif",sep=""))

v6sp=brick(paste("MOD35_",tile,".tif",sep=""))
v6sp=readAll(v6sp)

## wasn't working with line below, perhaps Z should just be text? not date?
v6sp=setZ(v6sp,as.Date(paste("2011-",1:12,"-15",sep="")))
names(v6sp)=month.name

kml_open("output/mod35.kml")


kml_layer.RasterBrick(v6sp,
     plot.legend = TRUE, dtime = "", tz = "GMT",
    z.lim = c(0,100),colour_scale = get("colour_scale_numeric", envir = plotKML.opts))
#    home_url = get("home_url", envir = plotKML.opts),
#    metadata = NULL, html.table = NULL,
#    altitudeMode = "clampToGround", balloon = FALSE,
)

logo = "http://static.tumblr.com/t0afs9f/KWTm94tpm/yale_logo.png"
kml_screen(image.file = logo, position = "UL", sname = "YALE logo",size=c(.1,.1))
kml_close("mod35.kml")
kml_compress("mod35.kml",files=c(paste(month.name,".png",sep=""),"obj_legend.png"),zip="/usr/bin/zip")

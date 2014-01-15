###################################################################################
###  R code to aquire and process MOD06_L2 cloud data from the MODIS platform


## connect to server of choice
#system("ssh litoria")
#R

library(sp)
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

X11.options(type="Xlib")
ncores=20  #number of threads to use

setwd("/home/adamw/personal/projects/interp")
setwd("/home/adamw/acrobates/projects/interp")

roi=readOGR("data/regions/Test_sites/Oregon.shp","Oregon")
roi=spTransform(roi,CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))

### Downloading data from LAADSWEB
# subset by geographic area of interest
# subset: 40-47, -115--125

## download data from ftp site.  Unfortunately the selection has to be selected using the website and orders downloaded via ftp.

system("wget -r --retr-symlinks ftp://ladsweb.nascom.nasa.gov/orders/500676499/ -P /home/adamw/acrobates/projects/interp/data/modis/MOD06_L2_hdf")


gdir="output/"
datadir="data/modis/MOD06_L2_hdf"
outdir="data/modis/MOD06_L2_nc"
  
fs=data.frame(
  path=list.files(datadir,full=T,recursive=T,pattern="hdf"),
  file=basename(list.files(datadir,full=F,recursive=T,pattern="hdf")))
fs$date=as.Date(substr(fs$file,11,17),"%Y%j")
fs$time=substr(fs$file,19,22)
fs$datetime=as.POSIXct(strptime(paste(substr(fs$file,11,17),substr(fs$file,19,22)), '%Y%j %H%M'))
fs$path=as.character(fs$path)
fs$file=as.character(fs$file)

## output ROI
#get bounding box of region in m
ge=SpatialPoints(data.frame(lon=c(-125,-115),lat=c(40,47)))
projection(ge)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
ge2=spTransform(ge, CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))


## vars
vars=paste(c(
  "Cloud_Effective_Radius",
  "Cloud_Effective_Radius_Uncertainty",
  "Cloud_Optical_Thickness",
  "Cloud_Optical_Thickness_Uncertainty",
  "Cloud_Water_Path",
  "Cloud_Water_Path_Uncertainty",
  "Cloud_Phase_Optical_Properties",
  "Cloud_Multi_Layer_Flag",
  "Cloud_Mask_1km",
  "Quality_Assurance_1km"))

### Installation of hegtool
## needed 32-bit libraries and java for program to install correctly

#system(paste("ncl_convert2nc ",f," -i ",hdfdir," -o ",outdir," -v ",vars," -nc4c -l -cl 1 -B ",sep=""))
#system(paste("h4toh5 ",hdfdir,"/",f," ",outdir,"/",f,sep=""))
# system(paste("hegtool -h ",fs$path[1],sep=""))


#### Function to generate hegtool parameter file for multi-band HDF-EOS file
swath2grid=function(i=1,files,vars=vars,outdir,upleft="47 -125",lowright="41 -115"){
  file=fs$path[i]
  tempfile=paste(tempdir(),"/",fs$file[i],sep="")
  outfile=sub("hdf$","nc",paste(outdir,"/",fs$file[i],sep=""))
  date=fs$date[1]
  origin=as.POSIXct("1970-01-01 00:00:00",tz="GMT")
### First write the parameter file (careful, heg is very finicky!)
  hdr=paste("NUM_RUNS = ",length(vars),"|MULTI_BAND_HDFEOS:",length(vars),sep="")
  grp=paste("
BEGIN
INPUT_FILENAME=",file,"
OBJECT_NAME=mod06
FIELD_NAME=",vars,"|
BAND_NUMBER = 1
OUTPUT_PIXEL_SIZE_X=1000
OUTPUT_PIXEL_SIZE_Y=1000
SPATIAL_SUBSET_UL_CORNER = ( ",upleft," )
SPATIAL_SUBSET_LR_CORNER = ( ",lowright," )
RESAMPLING_TYPE = NN
OUTPUT_PROJECTION_TYPE = SIN
ELLIPSOID_CODE = WGS84
OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0  )
OUTPUT_TYPE = HDFEOS
OUTPUT_FILENAME = ",tempfile,"
END


",sep="")
  ## write it to a file
  cat(c(hdr,grp)    , file=paste(tempdir(),"/",basename(file),"_MODparms.txt",sep=""))
  ## now run the swath2grid tool  - must be run as root (argh!)!
  if(file.exists(tempfile)) file.remove(tempfile)
  log=system(paste("sudo MRTDATADIR=\"/usr/local/heg/data\" ",
    "PGSHOME=/usr/local/heg/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/bin/swtif -p ",
    paste(tempdir(),"/",basename(file),"_MODparms.txt -d",sep=""),sep=""),intern=T)
#  system(paste("h5dump -H ",tempdir(),"/",basename(file),sep=""))

#    log=system(paste("swtif -p ",paste(tempdir(),"/",basename(file),"_MODparms.txt",sep=""),sep=""),intern=T)
  ## convert it to netcdf and clean up
  system(paste("NCARG_ROOT=\"/usr/local/ncl_ncarg-6.0.0\" ncl_convert2nc ",
               basename(tempfile)," -i ",tempdir()," -o ",tempdir()," -B ",sep=""))
  ncfile1=paste(tempdir(),"/",sub("hdf","nc",basename(file)),sep="")
    ## add time variable
  print("Adding time dimension")
  system(paste("ncecat -O -u time ",ncfile1,outfile))
  system(paste("ncap2 -s \'time[time]=",
               as.numeric(difftime(fs$datetime[i],origin,units="mins")),"\'  ",outfile,sep=""))

 ######################################################################################
  print("Updating netCDF dimensions")
  ## rename dimension variables
  system(paste("ncrename -d YDim_mod06,y -d XDim_mod06,x ",outfile))
  system(paste("ncap2 -s \'x[x]=0;y[y]=0\' ",outfile))
  system(paste("ncap2 -s \'lat[x,y]=0;lon[x,y]=0\' ",outfile))
  system(paste("ncap2 -s \'sinusoidal=0\' ",outfile))
  
  nc=nc_open(outfile,write=T)
### Get corner coordinates and convert to cell centers
  ncd=system(paste("ncdump -h ",outfile),intern=T)
  UL= as.numeric(do.call(c,strsplit(gsub("[a-z]|[A-Z]|[\\]|[\t]|[\"]|[=]|[(]|[)]|","",
    ncd[grep("UpperLeft",ncd)]),",")))+c(500,-500)
  LR= as.numeric(do.call(c,strsplit(gsub("[a-z]|[A-Z]|[\\]|[\t]|[\"]|[=]|[(]|[)]|","",
    ncd[grep("LowerRight",ncd)]),",")))+c(-500,500)
  
  xvar=seq(UL[1],LR[1],by=1000)
  yvar=seq(UL[2],LR[2],by=-1000)
  ncvar_put(nc,"x",vals=xvar)
  ncvar_put(nc,"y",vals=yvar)

  
  ## add lat-lon grid
  grid=expand.grid(x=xvar,y=yvar)
  coordinates(grid)=c("x","y")
  projection(grid)=CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0")
  grid2=spTransform(grid,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  grid=SpatialPointsDataFrame(grid,data=cbind.data.frame(coordinates(grid),coordinates(grid2)))
  gridded(grid)=T
  fullgrid(grid)=T
  colnames(grid@data)=c("x","y","lon","lat")

  xlon=as.matrix(grid["lon"])
  ylat=as.matrix(grid["lat"])

  ncvar_put(nc,"lon",vals=xlon)
  ncvar_put(nc,"lat",vals=ylat)

  nc_close(nc)

  ## update attributes
  for(v in c(vars[!grepl("Mask|Quality",vars)],paste(vars[grepl("Mask|Quality",vars)],"_0",sep=""))) {
    print(v)
    system(paste("ncatted -a coordinates,",v,",o,c,\"lat lon\" ",outfile,sep=""))
    system(paste("ncatted -a grid_mapping,",v,",o,c,\"sinusoidal\" ",outfile,sep=""))
  }
  
  system(paste("ncatted -a units,time,o,c,\"Minutes since ",origin,"\" ",outfile))
  system(paste("ncatted -a long_name,time,o,c,\"time\"",outfile))

  system(paste("ncatted -a units,y,o,c,\"m\" ",outfile))
  system(paste("ncatted -a long_name,y,o,c,\"y coordinate of projection\" ",outfile))
  system(paste("ncatted -a standard_name,y,o,c,\"projection_y_coordinate\" ",outfile))

  system(paste("ncatted -a units,x,o,c,\"m\" ",outfile))
  system(paste("ncatted -a long_name,x,o,c,\"x coordinate of projection\" ",outfile))
  system(paste("ncatted -a standard_name,x,o,c,\"projection_x_coordinate\" ",outfile))

  ## grid attributes
  system(paste("ncatted -a units,lat,o,c,\"degrees_north\" ",outfile))
  system(paste("ncatted -a long_name,lat,o,c,\"latitude coordinate\" ",outfile))
  system(paste("ncatted -a standard_name,lat,o,c,\"latitude\" ",outfile))

  system(paste("ncatted -a units,lon,o,c,\"degrees_east\" ",outfile))
  system(paste("ncatted -a long_name,lon,o,c,\"longitude coordinate\" ",outfile))
  system(paste("ncatted -a standard_name,lon,o,c,\"longitude\" ",outfile))

   system(paste("ncatted -a grid_mapping_name,sinusoidal,o,c,\"sinusoidal\" ",outfile))
  system(paste("ncatted -a standard_parallel,sinusoidal,o,c,\"0\" ",outfile))
  system(paste("ncatted -a longitude_of_central_meridian,sinusoidal,o,c,\"0\" ",outfile))
  system(paste("ncatted -a latitude_of_central_meridian,sinusoidal,o,c,\"0\" ",outfile))

  system(paste("cdo griddes ",outfile," > grid.txt"))
  system(paste("cdo mergegrid",outfile,
               "data/modis/MOD06_L2_nc/MOD06_L2.A2006001.2025.051.2010304104117.gscs_000500676714.nc ",
               "data/modis/MOD06_L2_nc/test.nc",sep=" "))
  
  print(paste("Finished ", file))
}



i=100


#### Run the gridding procedure

for(fi in 1:nrow(fs))
swath2grid(fi,vars=vars,files=fs,outdir=outdir,upleft="47 -125",lowright="40 -115")

### get grid from file the covers the region
system(paste("cdo griddes",paste(list.files(outdir,full=T)[1],collapse=" ")," > data/modis/MOD06L2_grid.txt"))


#### Merge the files
system(paste("cdo mergetime ",paste("-remapnn,data/modis/MOD06L2_grid.txt",list.files(outdir,full=T),collapse=" "),"
data/modis/MOD06L2.nc"))

#get bounding box of region in m
ge=SpatialPoints(data.frame(lon=c(-125,-115),lat=c(40,47)))
projection(ge)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
ge2=spTransform(ge, CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))

system(paste("ncks -d x,-10674232,-8746440 -d y,4429529,5207247 ", paste(list.files(outdir,full=T)[1],collapse=" "),"data/modis/test.nc")) 



cat(paste("
               gridtype=curvilinear
               gridsize=
               xsize=2157
               ysize=1037

               "),file="data/modis/grid.txt")

#system(paste("cdo -f grb  ",list.files(outdir,full=T)[1]," data/modis/test.grb"))

fs2=list.files(outdir,full=T)
d=raster(fs2[3],varname=vars[3])
projection(d)=CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0")
plot(d)




### subset to particular variables and drop uncertainties
f=fs[grep(vars[3],fs$file),]
f=f[!grepl("Uncertainty",f$file),]

getextent=function(file=f$file[1]) {
ext=extent(raster(file))
data.frame(file=file,xmin=attr(ext,"xmin"),xmax=attr(ext,"xmax"),ymin=attr(ext,"ymin"),ymax=attr(ext,"ymax"))
}

### some extents don't line up, find which ones...
ext=do.call(rbind.data.frame,mclapply(f$path,getextent))
f[which(ext$xmax!=ext$xmax[1]),]
f[which(ext$ymin!=ext$ymin[1]),]

### get  extent of first image
ext1=extent(raster(f$path[1]))
### generate brick of all images
d=brick(lapply(f$path, function(i) {print(i) crop(raster(i),ext1)}))


## rescale data and identify NAs
NAvalue(d)=-9999
d=d*0.009999999776482582

plot(d)

## calculate overall mean
dm=mean(d,na.rm=T)

## plot it
X11.options(type="Xlib")

pdf("output/MOD06_mean.pdf",width=1000,height=600)
plot(d)
plot(roi,add=T)
dev.off()













#### load the functions
source("code/GHCN_functions.r")





#################################################
#### Download Data
dir.create("data/lst")
ftpsite="atlas.nceas.ucsb.edu:/home/parmentier/data_Oregon_stations/"
system(paste("scp -r wilson@",ftpsite,"mean_month* data/lst ",sep=""))

lst=brick(as.list(list.files("data/lst/",pattern=".*rescaled[.]rst",full=T)[c(4:12,1:3)]))
projection(lst)=CRS("+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
lst=lst-272.15


#################################################
### Load PRISM
prism=brick("data/prism/prism_tmax.nc")
prism=projectRaster(prism,lst)

prism2=as(prism,"SpatialGridDataFrame");colnames(prism2@data)=paste("X",1:12,sep="")
prism2$source="prism"
prism2@data[c("lon","lat")]=coordinates(prism2)

lst2=as(lst,"SpatialGridDataFrame");colnames(lst2@data)=paste("X",1:12,sep="")
lst2$source="modis"
lst2@data[c("lon","lat")]=coordinates(lst2)

d=rbind.data.frame(melt(prism2@data,id.vars=c("source","lat","lon")),melt(lst2@data,id.vars=c("source","lat","lon")))
d$source=as.factor(d$source)
colnames(d)[grep("variable",colnames(d))]="month"
levels(d$month)=month.name
d=d[!is.na(d$value),]

d2=cast(d,lat+lon+month~source); gc()
d2$modis[d2$modis<(-50)]=NA




plot(subset(lst,1),col=rev(heat.colors(20)))



#### Explore prism-LST relationship (and land cover!?!)

png(width=1024,height=768,file="LSTvsPRISM.png")
xyplot(modis~prism|month,data=d2,panel=function(x,y,subscripts){
  panel.xyplot(x,y,cex=.2,pch=16,col="black")
  lm1=lm(y~x)
  panel.abline(lm1)
  panel.abline(0,1,col="red")
  panel.text(-0,40,paste("R2=",round(summary(lm1)$r.squared,2)))
},ylab="MODIS Daytime LST (C)",xlab="PRISM Maximum Temperature (C)",
sub="Red line is y=x, black line is regression")
dev.off()


### Bunch of junk below here!


#### Perspective plot
open3d()


### load data to show some points
load("stroi.Rdata")
load("data/ghcn/roi_ghcn.Rdata")
d2=d[d$date=="2010-01-01"&d$variable=="tmax",]

r=extent(212836,234693,320614,367250)
z=as.matrix(raster(crop(lst,r),layer=1))

x <- 1:nrow(z) 
y <- 1:ncol(z) 


ncol=50
colorlut <- heat.colors(ncol,alpha=0) # height color lookup table
#col <- colorlut[ z-min(z)+1 ] # assign colors to heights for each point
col <- colorlut[trunc((z/max(z))*(ncol-1))+1] # assign colors to heights for each point


rgl.surface(x, y, z, color=col, alpha=0.75, back="lines")
rgl.points()

#### 2d example
n=100
xlim=c(-2,3)
x=seq(xlim[1],xlim[2],length=n)
e=rnorm(n,0,.5)

### climate function
yclimate<-function(x) x^4-x^3-7*x^2+1.2*x+15+(1.2*sin(x*.5))+(1*cos(x*5))
### daily weather function
yweather<-function(x) yclimate(x)+2.5*x+.3*x^3

y1=yclimate(x)
y2=yweather(x)

#points
s=c(-1.5,-.2,1.4,2.5)
s1=yclimate(s)
s2=yweather(s)

png(width=1024,height=768,file="anomalyapproach_%d.png",pointsize=28)
for(i in 1:6){
par(mfrow=c(2,1),mar=c(0,5,4,1))
plot(y2~x,type="l",xaxt="n",xlab="Space",ylab="Temperature",las=1,ylim=c(-9,22),
     yaxp=c(0,20,1),col="transparent")
if(i<5) mtext("Space",1)
## points
if(i>=1) {
  points(s,s2,pch=16,col="blue")
  text(0.1,11,"Station data",col="blue")
}
if(i>=2) {
  lines(y2~x,lwd=2)
  points(s,s2,pch=16,col="blue") #put points back on top
  text(xlim[1]+.5,-3,"Weather")
}
if(i>=3) {
  lines(y1~x,col="darkgreen",lwd=2)
  text(xlim[1]+.5,10,"Climate",col="darkgreen")
}
if(i>=4) segments(s,s2,s,s1,col="blue",lwd=2)
### anomalies
if(i>=5) {
par(mar=c(4,5,1,1))
plot(I(y2-y1)~x,type="l",xaxt="n",xlab="Space",
     ylab="Temperature Anomaly \n (Weather-Climate)",las=1,ylim=c(-9,20),
     yaxp=c(0,20,1),col="transparent")
## points
points(s,s2-s1,pch=16,col="blue")
abline(h=0,col="grey",lwd=2)
segments(s,0,s,s2-s1,col="blue",lwd=2)
}
if(i>=6) lines(I(y2-y1)~x,lwd=2)
}
dev.off()



##########################################################
#### Identify region of interest

### develop in region of interest spatial polygon
roi=readOGR("data/boundaries/statesp020.shp","statesp020")
proj4string(roi)=CRS("+proj=longlat +datum=WGS84")
roi=roi[roi$STATE=="Oregon",]
## buffer region of interest to include surrounding stations (in km)
roib=bufferroi(roi,distance=100)


############################
####  Extract MOD35 C6 processing path

setwd("~/acrobates/adamw/projects/interp/data/modis/mod35")
library(multicore)
library(raster)
library(spgrass6)
library(rgeos)
##download 3 days of modis swath data:

url="ftp://ladsweb.nascom.nasa.gov/allData/51/MOD35_L2/2012/"
dir.create("swath")

system(paste("wget -S --recursive --no-parent --no-directories -N -P swath --accept \"hdf\" --accept \"002|003|004\" ",url))


### make global raster that aligns with MODLAND tiles
## get MODLAND tile to serve as base
#system("wget http://e4ftl01.cr.usgs.gov/MOLT/MOD13A3.005/2000.02.01/MOD13A3.A2000032.h00v08.005.2006271174446.hdf")
#t=raster(paste("HDF4_EOS:EOS_GRID:\"",getwd(),"/MOD13A3.A2000032.h00v08.005.2006271174446.hdf\":MOD_Grid_monthly_1km_VI:1 km monthly NDVI",sep=""))
t=raster(paste("../MOD17/MOD17A3_Science_NPP_mean_00_12.tif",sep=""))
projection(t)

## make global extent
pmodis="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

glb=t
#values(glb)=NA
glb=extend(glb,extent(-180,180,-90,90))

#glb=raster(glb,crs="+proj=longlat +datum=WGS84",nrows=42500,ncols=85000)
#extent(glb)=alignExtent(projectRaster(glb,crs=projection(t),over=T),t)
#res(glb)=c(926.6254,926.6264)
#projection(glb)=pmodis

## confirm extent
#projectExtent(glb,crs="+proj=longlat +datum=WGS84")


#### Grid and mosaic the swath data

stitch="sudo MRTDATADIR=\"/usr/local/heg/2.12/data\" PGSHOME=/usr/local/heg/2.12/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/2.12/bin/swtif"
stitch="/usr/local/heg/2.12/bin/swtif"

stitch="sudo MRTDATADIR=\"/usr/local/heg/2.11/data\" PGSHOME=/usr/local/heg/2.11/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/2.11/bin/swtif"
files=paste(getwd(),"/",list.files("swath",pattern="hdf$",full=T),sep="")

## vars to process
vars=as.data.frame(matrix(c(
  "Cloud_Mask",           "CM",   "NN",    1,
#  "Sensor_Azimuth",       "ZA",   "CUBIC", 1,
  "Sensor_Zenith",        "SZ",   "CUBIC", 1),
  byrow=T,ncol=4,dimnames=list(1:2,c("variable","varid","method","band"))),stringsAsFactors=F)

## global bounding box
   gbb=cbind(c(-180,-180,180,180,-180),c(-90,90,90,-90,-90))
   gpp = SpatialPolygons(list(Polygons(list(Polygon(gbb)),1)))
   proj4string(gpp)=projection(glb)

outdir="~/acrobates/adamw/projects/interp/data/modis/mod35/gridded/"

swtif<-function(file,var){
  outfile=paste(tempdir(),"/",var$varid,"_",basename(file),sep="")  #gridded path
   ## First write the parameter file (careful, heg is very finicky!)
   hdr=paste("NUM_RUNS = 1")
   grp=paste("
BEGIN
INPUT_FILENAME=",file,"
OBJECT_NAME=mod35
FIELD_NAME=",var$variable,"|
BAND_NUMBER = ",var$band,"
OUTPUT_PIXEL_SIZE_X=0.008333333
OUTPUT_PIXEL_SIZE_Y=0.008333333
SPATIAL_SUBSET_UL_CORNER = ( ",bbox(gpp)[2,2]," ",bbox(gpp)[1,1]," )
SPATIAL_SUBSET_LR_CORNER = ( ",bbox(gpp)[2,1]," ",bbox(gpp)[1,2]," )
RESAMPLING_TYPE =",var$method,"
OUTPUT_PROJECTION_TYPE = GEO
OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# projection parameters from http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=sn_gctp
ELLIPSOID_CODE = WGS84
OUTPUT_TYPE = HDFEOS
OUTPUT_FILENAME= ",outfile,"
END

",sep="")
   ## write it to a file
   cat( c(hdr,grp)   , file=paste(tempdir(),"/",basename(file),"_MODparms.txt",sep=""))
   ## now run the swath2grid tool
   ## write the gridded file
   print(paste("Starting",file))
   system(paste("",stitch," -p ",tempdir(),"/",basename(file),"_MODparms.txt -d -log /dev/null ",sep=""),intern=F)#,ignore.stderr=F)
   print(paste("Finished processing variable",var$variable,"from ",basename(file),"to",outfile))
}  

getpath<- function(file){  
   setwd(tempdir())
   bfile=sub(".hdf","",basename(file))
   tempfile2_path=paste(tempdir(),"/",bfile,".tif",sep="")  #gridded/masked/processed path
   outfile=paste(outdir,"/",bfile,".tif",sep="")  #final file
   if(file.exists(outfile)) return(c(file,0))
   ppc=gpp
#######
## run swtif for each band
   lapply(1:nrow(vars),function(i) swtif(file,vars[i,]))
####### import to R for processing
  
   if(!file.exists(paste(tempdir(),"/CM_",basename(file),sep=""))) {
     file.remove(list.files(tempdir(),pattern=bfile,full=T))
     return(c(file,0))
   }
   ## convert to land path
   d=raster(paste(tempdir(),"/CM_",basename(file),sep=""))
   sz=raster(paste(tempdir(),"/SZ_",basename(file),sep=""))
   NAvalue(sz)=-9999
   getlc=function(x,y) {ifelse(y==0|y>6000,NA,((x%/%2^6) %% 2^2))}
   path=  overlay(d,sz,fun=getlc,filename=tempfile2_path,options=c("COMPRESS=LZW", "LEVEL=9","PREDICTOR=2"),datatype="INT1U",overwrite=T)
### warp them to align all pixels
   system(paste("gdalwarp -overwrite -srcnodata 255 -dstnodata 255 -tap -tr 0.008333333 0.008333333 -co COMPRESS=LZW -co ZLEVEL=9 -co PREDICTOR=2 -s_srs \"",projection(t),"\" ",tempfile2_path," ",outfile,sep=""))
   ## delete temporary files
   file.remove(list.files(tempdir(),pattern=bfile,full=T))
   return(c(file,1))
 }


### run it
mclapply(files,getpath,mc.cores=10)

## check gdal can read all of them
gfiles=list.files(outdir,pattern="tif$",full=T)
length(gfiles)

check=do.call(rbind,mclapply(gfiles,function(file){
    gd=system(paste("gdalinfo ",file,sep=""),intern=T)
    if(any(grepl("Corner",gd))) return(1)
    else return(0)
}))

table(check)

file.remove(gfiles[check==0])

## use new gdal
system(paste("/usr/local/gdal-1.10.0/bin/gdalwarp -wm 900 -overwrite -co COMPRESS=LZW -co PREDICTOR=2 -multi -r mode ",outdir,"/*.tif MOD35_path_gdalwarp.tif",sep=""))


###  Merge them into a geotiff
    system(paste("gdal_merge.py -v -init 255 -n 255 -o ",outdir,"/../MOD35_ProcessPath_gdalmerge2.tif -co \"ZLEVEL=9\" -co \"COMPRESS=LZW\" -co \"PREDICTOR=2\" `ls -d -1 ",outdir,"/*.tif --sort=size `",sep=""))

#  origin(raster(gfiles[5]))
  
  ## try with pktools
  ## global
system(paste("pkmosaic -co COMPRESS=LZW -co PREDICTOR=2 ",paste("-i",list.files("gridded",full=T,pattern="tif$"),collapse=" ")," -o MOD35_path_pkmosaic_mode.tif  -m 6 -v -t 255 -t 0 &"))
#bb="-ulx -180 -uly 90 -lrx 180 -lry -90"
#bb="-ulx -180 -uly 90 -lrx 170 -lry 80"
bb="-ulx -72 -uly 11 -lrx -59 -lry -1"


#expand.grid(x=seq(-180,170,by=10),y=seq(-90,80))
gf2=  grep("2012009[.]03",gfiles,value=T)
system(paste("pkmosaic ",bb," -co COMPRESS=LZW -co PREDICTOR=2 ",paste("-i",gf2,collapse=" ")," -o h11v08_path_pkmosaic.tif -ot Byte -m 7 -v -t 255"))

                                        #  bounding box?  

###########
### Use GRASS to import all the tifs and calculat the mode
## make temporary working directory
  tf=paste(tempdir(),"/grass", Sys.getpid(),"/", sep="")  #temporar
  if(!file.exists(tf)) dir.create(tf)
  
  ## set up temporary grass instance for this PID
  gisBase="/usr/lib/grass64"
  print(paste("Set up temporary grass session in",tf))
  initGRASS(gisBase=gisBase,gisDbase=tf,SG=as(glb,"SpatialGridDataFrame"),override=T,location="mod35",mapset="PERMANENT",home=tf,pid=Sys.getpid())
  system(paste("g.proj -c proj4=\"",projection(glb),"\"",sep=""),ignore.stdout=T,ignore.stderr=T)

## read in NPP grid to serve as grid
  execGRASS("r.in.gdal",input=t@file@name,output="grid")
  system("g.region rast=grid n=90 s=-90 save=roi --overwrite",ignore.stdout=F,ignore.stderr=F)

## get files already imported - only important if more tifs were written after an initial import
imported=basename(gfiles)%in%system("g.mlist type=rast pattern=MOD*",intern=T)
table(imported)

## read in all tifs
  for(f in gfiles[!imported])  execGRASS("r.in.gdal",input=f,output=basename(f),flags="o")

## calculate mode
execGRASS("r.series",input=paste(system("g.mlist type=rast pattern=MOD*",intern=T)[1:1000],sep="",collapse=","),output="MOD35_path",method="mode",range=c(1,5),flags=c("verbose","overwrite"))
## add colors
execGRASS("r.colors",map="MOD35_path",rules="MOD35_path_grasscolors.txt")
## write to disk
execGRASS("r.out.gdal",input="MOD35_path",output=paste(getwd(),"/MOD35_ProcessPath_C5.tif",sep=""),type="Byte",createopt="COMPRESS=LZW,LEVEL=9,PREDICTOR=2")

### delete the temporary files 
  unlink_.gislock()
  system(paste("rm -frR ",tf,sep=""))

#########################


cols=c("blue","lightblue","tan","green")



## connect to raster to extract land-cover bit
library(raster)

d=raster("CM.tif")
getlc=function(x) {(x/2^6) %% 2^2}

calc(d,fun=getlc,filename="CM_LC.tif")


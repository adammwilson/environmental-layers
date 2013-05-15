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

stitch="sudo MRTDATADIR=\"/usr/local/heg/data\" PGSHOME=/usr/local/heg/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/bin/swtif"

files=paste(getwd(),"/",list.files("swath",pattern="hdf$",full=T),sep="")

## vars to process
vars=as.data.frame(matrix(c(
  "Cloud_Mask",              "CM",
  "Sensor_Azimuth",       "ZA",
  "Sensor_Zenith",        "SZ"),
  byrow=T,ncol=2,dimnames=list(1:3,c("variable","varid"))),stringsAsFactors=F)

## global bounding box
   gbb=cbind(c(-180,-180,180,180,-180),c(-85,85,85,-85,-85))
   gpp = SpatialPolygons(list(Polygons(list(Polygon(gbb)),1)))
   proj4string(gpp)=projection(glb)


getpath<- function(file){  
   setwd(tempdir())
   bfile=sub(".hdf","",basename(file))
   tempfile_path=paste(tempdir(),"/path_",basename(file),sep="")  #gridded path
   tempfile_sz=paste(tempdir(),"/sz_",basename(file),sep="")  # gridded sensor zenith
   tempfile2_path=paste(tempdir(),"/",bfile,".tif",sep="")  #gridded/masked/processed path
   outfile=paste("~/acrobates/adamw/projects/interp/data/modis/mod35/gridded/",bfile,".tif",sep="")  #final file
   if(file.exists(outfile)) return(c(file,0))
   ## get bounding coordinates
   glat=as.numeric(do.call(c,strsplit(sub("GRINGPOINTLATITUDE=","",system(paste("gdalinfo ",file," | grep GRINGPOINTLATITUDE"),intern=T)),split=",")))
   glon=as.numeric(do.call(c,strsplit(sub("GRINGPOINTLONGITUDE=","",system(paste("gdalinfo ",file," | grep GRINGPOINTLONGITUDE"),intern=T)),split=",")))
   bb=cbind(c(glon,glon[1]),c(glat,glat[1]))
   pp = SpatialPolygons(list(Polygons(list(Polygon(bb)),1)))
   proj4string(pp)=projection(glb)
   ppc=gIntersection(pp,gpp)
   ppc=gBuffer(ppc,width=0.3)  #buffer a little to remove gaps between images
   ## First write the parameter file (careful, heg is very finicky!)
   hdr=paste("NUM_RUNS = ",length(vars$varid),"|MULTI_BAND_HDFEOS:",length(vars$varid),sep="")
   grp=paste("
BEGIN
INPUT_FILENAME=",file,"
OBJECT_NAME=mod35
FIELD_NAME=",vars$variable,"|
BAND_NUMBER = ",1:length(vars$varid),"
OUTPUT_PIXEL_SIZE_X=0.008333333
OUTPUT_PIXEL_SIZE_Y=0.008333333
SPATIAL_SUBSET_UL_CORNER = ( ",bbox(ppc)[2,2]," ",bbox(ppc)[1,1]," )
SPATIAL_SUBSET_LR_CORNER = ( ",bbox(ppc)[2,1]," ",bbox(ppc)[1,2]," )
OUTPUT_OBJECT_NAME = mod35|
RESAMPLING_TYPE =NN
OUTPUT_PROJECTION_TYPE = GEO
OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# projection parameters from http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=sn_gctp
ELLIPSOID_CODE = WGS84
OUTPUT_TYPE = HDFEOS
OUTPUT_FILENAME= ",tempfile_path,"
END

",sep="")

   ## if any remnants from previous runs remain, delete them
   if(length(list.files(tempdir(),pattern=bfile)>0))
     file.remove(list.files(tempdir(),pattern=bfile,full=T))
   ## write it to a file
   cat( c(hdr,grp)   , file=paste(tempdir(),"/",basename(file),"_MODparms.txt",sep=""))
   ## now run the swath2grid tool
   ## write the gridded file
   print(paste("Starting",file))
   system(paste("(",stitch," -p ",tempdir(),"/",basename(file),"_MODparms.txt -d)",sep=""),intern=F)#,ignore.stderr=F)
##############  Now run the 5km summary
   ## First write the parameter file (careful, heg is very finicky!)
   hdr=paste("NUM_RUNS = ",nrow(vars[-1,]),"|MULTI_BAND_HDFEOS:",nrow(vars[-1,]),sep="")
   grp=paste("
BEGIN
INPUT_FILENAME=",file,"
OBJECT_NAME=mod35
FIELD_NAME=",vars$variable[-1],"|
BAND_NUMBER = ",1,"
OUTPUT_PIXEL_SIZE_X=0.008333333
#0.0416666
OUTPUT_PIXEL_SIZE_Y=0.008333333
#0.0416666
SPATIAL_SUBSET_UL_CORNER = ( ",bbox(ppc)[2,2]," ",bbox(ppc)[1,1]," )
SPATIAL_SUBSET_LR_CORNER = ( ",bbox(ppc)[2,1]," ",bbox(ppc)[1,2]," )
#OUTPUT_OBJECT_NAME = mod35|
RESAMPLING_TYPE =NN
OUTPUT_PROJECTION_TYPE = GEO
OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
#OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# projection parameters from http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=sn_gctp
ELLIPSOID_CODE = WGS84
OUTPUT_TYPE = HDFEOS
OUTPUT_FILENAME= ",tempfile_sz,"
END

",sep="")

   ## write it to a file
   cat( c(hdr,grp)   , file=paste(tempdir(),"/",basename(file),"_MODparms_angle.txt",sep=""))
   
   ## now run the swath2grid tool
   ## write the gridded file
   print(paste("Starting",file))
   system(paste("(",stitch," -p ",tempdir(),"/",basename(file),"_MODparms_angle.txt -d)",sep=""),intern=F,ignore.stderr=F)
####### import to R for processing
   if(!file.exists(tempfile_path)) {
     file.remove(list.files(tempdir(),pattern=bfile,full=T))
     return(c(file,0))
   }
   ## convert to land path
   d=raster(paste("HDF4_EOS:EOS_GRID:\"",tempfile_path,"\":mod35:Cloud_Mask_0",sep=""))
   sz=raster(paste("HDF4_EOS:EOS_GRID:\"",tempfile_sz,"\":mod35:Sensor_Zenith",sep=""))
   NAvalue(sz)=0
   ## resample sensor angles to 1km grid and mask paths with angles >=30
#   sz2=resample(sz,d,method="ngb",file=sub("hdf","tif",tempfile_sz),overwrite=T)
   getlc=function(x,y) {ifelse(y==0|y>40,NA,((x%/%2^6) %% 2^2))}
   path=  overlay(d,sz,fun=getlc,filename=tempfile2_path,options=c("COMPRESS=LZW", "LEVEL=9","PREDICTOR=2"),datatype="INT1U",overwrite=T)
### warp them to align all pixels
   system(paste("gdalwarp -overwrite -srcnodata 255 -dstnodata 255 -tap -tr 0.008333333 0.008333333 -co COMPRESS=LZW -co ZLEVEL=9 -co PREDICTOR=2 -s_srs \"",projection(t),"\" ",tempfile2_path," ",outfile,sep=""))
   ## delete temporary files
   file.remove(list.files(tempdir(),pattern=bfile,full=T))
   return(c(file,1))
 }


## establish sudo priveleges to run swtif
system("sudo ls"); mclapply(files,getpath,mc.cores=10)

## check gdal can read all of them
gfiles=list.files("gridded",pattern="tif$",full=T)
length(gfiles)

check=do.call(rbind,mclapply(gfiles,function(file){
    gd=system(paste("gdalinfo ",file,sep=""),intern=T)
    if(any(grepl("Corner",gd))) return(1)
    else return(0)
}))

table(check)

file.remove(gfiles[check==0])

## use new gdal
system(paste("/usr/local/gdal-1.10.0/bin/gdalwarp -wm 900 -overwrite -co COMPRESS=LZW -co PREDICTOR=2 -multi -r mode gridded/*.tif MOD35_path_gdalwarp.tif"))


#  origin(raster(gfiles[5]))
  
  ## try with pktools
  ## global
system(paste("pkmosaic -co COMPRESS=LZW -co PREDICTOR=2 ",paste("-i",list.files("gridded",full=T,pattern="tif$"),collapse=" ")," -o MOD35_path_pkmosaic_max.tif  -m 2 -v -t 255 -t 0 &"))
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


  ###  Merge them into a geotiff
    system(paste("gdal_merge.py -v -n 255 -o MOD35_ProcessPath.tif -co \"COMPRESS=LZW\" -co \"PREDICTOR=2\" `ls -d -1 gridded/*.tif`",sep=""))


## connect to raster to extract land-cover bit
library(raster)

d=raster("CM.tif")
getlc=function(x) {(x/2^6) %% 2^2}

calc(d,fun=getlc,filename="CM_LC.tif")


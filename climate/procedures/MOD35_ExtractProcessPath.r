############################
####  Extract MOD35 C6 processing path

setwd("~/acrobates/adamw/projects/interp/data/modis/mod35")
library(multicore)
library(raster)
library(spgrass6)

##download 3 days of modis swath data:

url="ftp://ladsweb.nascom.nasa.gov/allData/51/MOD35_L2/2012/"
dir.create("swath")

system(paste("wget -S --recursive --no-parent --no-directories -N -P swath --accept \"hdf\" --accept \"002|003|004\" ",url))


### make global raster that aligns with MODLAND tiles
## get MODLAND tile to serve as base
#system("wget http://e4ftl01.cr.usgs.gov/MOLT/MOD13A3.005/2000.02.01/MOD13A3.A2000032.h00v08.005.2006271174446.hdf")
#t=raster(paste("HDF4_EOS:EOS_GRID:\"",getwd(),"/MOD13A3.A2000032.h00v08.005.2006271174446.hdf\":MOD_Grid_monthly_1km_VI:1 km monthly NDVI",sep=""))
t=raster(paste("../MOD17/Npp_1km_C5.1_mean_00_to_06.tif",sep=""))

## make global extent
pmodis="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

glb=t
values(glb)=NA
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
#  "Quality_Assurance",       "QA"),
  byrow=T,ncol=2,dimnames=list(1:2,c("variable","varid"))),stringsAsFactors=F)

## establish sudo priveleges to run swtif
system("sudo ls")

mclapply(files,function(file){
  
  tempfile=paste(tempdir(),"/",basename(file),sep="")
#  tempfile2=paste("gridded/",sub("hdf","tif",basename(tempfile)),sep="")
  tempfile2=paste(tempdir(),"/",sub("hdf","tif",basename(tempfile)),sep="")

  outfile=paste("gridded2/",sub("hdf","tif",basename(tempfile)),sep="")

  if(file.exists(outfile)) return(c(file,0))
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
SPATIAL_SUBSET_UL_CORNER = ( 90 -180 )
SPATIAL_SUBSET_LR_CORNER = ( -90 180 )
OUTPUT_OBJECT_NAME = mod35|
RESAMPLING_TYPE =NN
OUTPUT_PROJECTION_TYPE = GEO
OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# projection parameters from http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=sn_gctp
ELLIPSOID_CODE = WGS84
OUTPUT_TYPE = HDFEOS
OUTPUT_FILENAME= ",tempfile,"
END

",sep="")

  ## if any remnants from previous runs remain, delete them
   if(length(list.files(tempdir(),pattern=basename(file)))>0)
    file.remove(list.files(tempdir(),pattern=basename(file),full=T))
  ## write it to a file
 cat( c(hdr,grp)   , file=paste(tempdir(),"/",basename(file),"_MODparms.txt",sep=""))

  ## now run the swath2grid tool
  ## write the gridded file
  print(paste("Starting",file))
  system(paste("(",stitch," -p ",tempdir(),"/",basename(file),"_MODparms.txt -d ; echo $$)",sep=""),intern=T,ignore.stderr=F)
  ## convert to land path
  d=raster(paste("HDF4_EOS:EOS_GRID:\"",tempfile,"\":mod35:Cloud_Mask_0",sep=""))
#  za=raster(paste("HDF4_EOS:EOS_GRID:\"",tempfile,"\":mod35:Quality_Assurance_1",sep=""))
  ## add projection information - ellipsoid should be wgs84
  projection(d)=projection(glb)
  extent(d)=alignExtent(d,extent(glb))
                                        #  d=readAll(d)
  getlc=function(x) {(x%/%2^6) %% 2^2}
  calc(d,fun=getlc,filename=outfile,options=c("COMPRESS=LZW", "LEVEL=9","PREDICTOR=2"),datatype="INT1U",overwrite=T)

  ### warp it to align all pixels
  system(paste("gdalwarp -overwrite -tap -tr 0.008333333 0.008333333 -co COMPRESS=LZW -co LEVEL=9 -co PREDICTOR=2 -s_srs \"",projection(glb),"\" ",tempfile2," ",outfile,sep=""))

#  getqa=function(x) {(x%/%2^1) %% 2^3}
#  za=calc(za,fun=getqa)#,filename=outfile,options=c("COMPRESS=LZW", "LEVEL=9","PREDICTOR=2"),datatype="INT1U",overwrite=T)
  ## resample to line up with glb so we can use mosaic later
## delete temporary files
  file.remove(tempfile,tempfile2)
  return(c(file,1))
})

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

#  origin(raster(gfiles[5]))
  
  
  system(paste("gdalinfo ",gfiles[1]," | grep Origin"))
  system(paste("gdalinfo ",gfiles[2]," | grep Origin"))
  system(paste("gdalinfo ",gfiles[3]," | grep Origin"))

system(paste("gdalwarp -tap -tr 0.008333333 0.008333333 -s_srs \"",projection(glb),"\" ",gfiles[1]," test1.tif"))
system(paste("gdalwarp -tap -tr 0.008333333 0.008333333 -s_srs \"",projection(glb),"\" ",gfiles[2]," test2.tif"))
  system(paste("gdalinfo test1.tif | grep Origin"))
  system(paste("gdalinfo test2.tif | grep Origin"))
system(paste("pkmosaic ",paste("-i",gfiles[1:2],collapse=" ")," -o MOD35_path2.tif  -m 6 -v -t 255"))
system(paste("pkmosaic ",paste("-i",c("test1.tif","test2.tif"),collapse=" ")," -o MOD35_path2.tif  -m 6 -v -max 10 -ot Byte"))


  ## try with pktools
  ## global
system(paste("pkmosaic -co COMPRESS=LZW -co PREDICTOR=2 ",paste("-i",list.files("gridded",full=T,pattern="tif$"),collapse=" ")," -o MOD35_path_pkmosaic.tif  -m 6 -v -t 255 -t 0 &"))
#bb="-ulx -180 -uly 90 -lrx 180 -lry -90"
#bb="-ulx -180 -uly 90 -lrx 170 -lry 80"
bb="-ulx -72 -uly 11 -lrx -59 -lry -1"


#expand.grid(x=seq(-180,170,by=10),y=seq(-90,80))
  
system(paste("pkmosaic ",bb," -co COMPRESS=LZW -co PREDICTOR=2 ",paste("-i",list.files("gridded",full=T,pattern="tif$"),collapse=" ")," -o h11v08_path_pkmosaic.tif  -m 6 -v -t 255 -t 0 &"))

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


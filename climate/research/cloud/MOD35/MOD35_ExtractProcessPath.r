############################
####  Extract MOD35 C6 processing path

setwd("~/acrobates/adamw/projects/interp/data/modis/mod35/processpath")
library(multicore)
library(raster)
library(spgrass6)
library(rgeos)

#### Set up command for running swtif to grid and mosaic the swath data
stitch=paste("sudo MRTDATADIR=\"/usr/local/heg/2.12/data\" PGSHOME=/usr/local/heg/2.12/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/2.12b/bin/swtif")

## Link to MOD35 Swath data
url="ftp://ladsweb.nascom.nasa.gov/allData/51/MOD35_L2/2012/"
dir.create("swath")

##download 3 days of modis swath data:
getdata=F
if(getdata)
  system(paste("wget -S --recursive --no-parent --no-directories -N -P swath --accept \"hdf\" --accept \"002|003|004\" ",url))

### make global raster that aligns with MOD17 NPP raster
t=raster(paste("../../MOD17/MOD17A3_Science_NPP_mean_00_12.tif",sep=""))
projection(t)

## make global extent
glb=t
glb=extend(glb,extent(-180,180,-90,90))

### list of swath files
files=paste(getwd(),"/",list.files("swath",pattern="hdf$",full=T),sep="")[1:5000]

## vars to process
vars=as.data.frame(matrix(c(
  "Cloud_Mask",           "CM",   "NN",    1,
  "Sensor_Zenith",        "SZ",   "CUBIC", 1),
  byrow=T,ncol=4,dimnames=list(1:2,c("variable","varid","method","band"))),stringsAsFactors=F)

## global bounding box
   gbb=cbind(c(-180,-180,180,180,-180),c(-90,90,90,-90,-90))
   gpp = SpatialPolygons(list(Polygons(list(Polygon(gbb)),1)))
   proj4string(gpp)=projection(glb)

outdir="/gridded/"

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
   system(paste("sudo ",stitch," -p ",tempdir(),"/",basename(file),"_MODparms.txt -d -log /dev/null -tmpLatLondir ",tempdir(),sep=""),intern=F)#,ignore.stderr=F)
   print(paste("Finished processing variable",var$variable,"from ",basename(file),"to",outfile))
}  

getpath<- function(file){  
   setwd(tempdir())
   bfile=sub(".hdf","",basename(file))
   tempfile2_path=paste(tempdir(),"/",bfile,".tif",sep="")  #gridded/masked/processed path
   outfile=paste(outdir,"/",bfile,".tif",sep="")  #final file
   if(file.exists(outfile)) return(paste(file," already finished"))
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
   getlc=function(x,y) {ifelse(y<0|y>6000,NA,((x%/%2^6) %% 2^2))}
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

## report on any bad files
table(check)

## remove any fail the check
file.remove(gfiles[check==0])
gfiles=gfiles[check==1]


###########
### Use GRASS to import all the tifs and calculate the mode
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
  for(f in gfiles[!imported])  {
    print(f)
    execGRASS("r.external",input=f,output=basename(f),flags=c("overwrite"))
  }

## calculate mode  in chunks.  This first bins several individual swaths together into more or less complete global coverages taking the mode of each chunk
nbreaks=100
bins=cut(1:5000,nbreaks)
ts=system("g.mlist type=rast pattern=MOD*.tif",intern=T)  #files to process

for(i in 1:nbreaks)  #loop over breaks
execGRASS("r.series",input=paste(ts[bins==levels(bins)[i]],sep="",collapse=","),output=paste("path",i,sep="_"),method="mode",range=c(0,5),flags=c("verbose","overwrite"),Sys_wait=T)

##  Get mode of each chunk
execGRASS("r.series",input=paste(system("g.mlist type=rast pattern=path*",intern=T),sep="",collapse=","),output="MOD35_path",method="mode",range=c(0,5),flags=c("verbose","overwrite"))

## fill in missing data (due to gridding artifacts) very near poles with water (north) and land (south)
system("r.mapcalc \"MOD35_patha=if(isnull(MOD35_path)&y()>-84.31,0,MOD35_path)\"")
system("r.mapcalc \"MOD35_pathb=if(isnull(MOD35_patha)&y()<-84.31,3,MOD35_patha)\"")

## add colors
execGRASS("r.colors",map="MOD35_pathb",rules="MOD35_path_grasscolors.txt")

## write to disk
execGRASS("r.out.gdal",input="MOD35_pathb",output=paste(getwd(),"/C5MOD35_ProcessPath.tif",sep=""),type="Byte",createopt="COMPRESS=LZW,LEVEL=9,PREDICTOR=2")

## update metadata
tags=c("TIFFTAG_IMAGEDESCRIPTION='Collection 5 MOD35 Processing Path (0=Water,1=Coast,2=Desert,3=Land)'",
  "TIFFTAG_DOCUMENTNAME='Collection 5 MOD35 Processing Path'",
  "TIFFTAG_DATETIME='20130901'",
  "TIFFTAG_ARTIST='Adam M. Wilson (adam.wilson@yale.edu)'")
system(paste("/usr/local/src/gdal-1.10.0/swig/python/scripts/gdal_edit.py ",getwd(),"/C5MOD35_ProcessPath.tif ",paste("-mo ",tags,sep="",collapse=" "),sep=""))

### delete the temporary files 
  unlink_.gislock()
  system(paste("rm -frR ",tf,sep=""))

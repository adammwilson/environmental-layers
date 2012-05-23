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
library(spgrass6)

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
outdir="data/modis/MOD06_L2_hdf2"
  
fs=data.frame(
  path=list.files(datadir,full=T,recursive=T,pattern="hdf"),
  file=basename(list.files(datadir,full=F,recursive=T,pattern="hdf")))
fs$date=as.Date(substr(fs$file,11,17),"%Y%j")
fs$month=format(fs$date,"%m")
fs$year=format(fs$date,"%Y")
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
vars=as.data.frame(matrix(c(
  "Cloud_Effective_Radius","CER",
  "Cloud_Effective_Radius_Uncertainty","CERU",
  "Cloud_Optical_Thickness","COT",
  "Cloud_Optical_Thickness_Uncertainty","COTU",
  "Cloud_Water_Path","CWP",
  "Cloud_Water_Path_Uncertainty","CWPU",
  "Cloud_Phase_Optical_Properties","CPOP",
  "Cloud_Multi_Layer_Flag","CMLF",
  "Cloud_Mask_1km","CM1",
  "Quality_Assurance_1km","QA"),
  byrow=T,ncol=2,dimnames=list(1:10,c("variable","varid"))))


### Installation of hegtool
## needed 32-bit libraries and java for program to install correctly

# system(paste("hegtool -h ",fs$path[1],sep=""))


#### Function to generate hegtool parameter file for multi-band HDF-EOS file
swath2grid=function(i=1,files,vars=vars,outdir,upleft="47 -125",lowright="41 -115"){
  file=fs$path[i]
  print(paste("Starting file",basename(file)))
  outfile=paste(outdir,"/",fs$file[i],sep="")
#  date=fs$date[1]
#  origin=as.POSIXct("1970-01-01 00:00:00",tz="GMT")
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
#RESAMPLING_TYPE =",ifelse(grepl("Flag|Mask|Quality",vars$variable),"NN","BICUBIC"),"
RESAMPLING_TYPE =NN
OUTPUT_PROJECTION_TYPE = SIN
ELLIPSOID_CODE = WGS84
OUTPUT_PROJECTION_PARAMETERS = ( 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0  )
OUTPUT_TYPE = HDFEOS
OUTPUT_FILENAME = ",outfile,"
END


",sep="")
  ## if any remnants from previous runs remain, delete them
  if(length(list.files(tempdir(),pattern=basename(file)))>0)
    file.remove(list.files(tempdir(),pattern=basename(file),full=T))
  ## write it to a file
  cat(c(hdr,grp)    , file=paste(tempdir(),"/",basename(file),"_MODparms.txt",sep=""))
  ## now run the swath2grid tool  - must be run as root (argh!)!
  ## write the tiff file
  log=system(paste("sudo MRTDATADIR=/usr/local/heg/data ",
    "PGSHOME=/usr/local/heg/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/bin/swtif -p ",
    paste(tempdir(),"/",basename(file),"_MODparms.txt -d",sep=""),sep=""),intern=T)
      print(paste("Finished ", file))
}


### update fs with completed files
fs$complete=fs$file%in%list.files(outdir,pattern="hdf$")
table(fs$complete)

#### Run the gridding procedure

system("sudo ls")

mclapply(which(!fs$complete),function(fi){
  swath2grid(fi,vars=vars$variable,files=fs,
             outdir=outdir,
             upleft="47 -125",lowright="40 -115")},
         mc.cores=24)


##############################################################
### Import to GRASS for processing

fs$grass=paste(fs$month,fs$year,fs$file,sep="_")
td=readGDAL(paste("HDF4_EOS:EOS_GRID:\"",outdir,"/",fs$file[1],"\":mod06:Cloud_Mask_1km_0",sep=""))

## fucntion to convert binary to decimal to assist in identifying correct values
b2d=function(x) sum(x * 2^(rev(seq_along(x)) - 1)) #http://tolstoy.newcastle.edu.au/R/e2/help/07/02/10596.html
## for example:
b2d(c(T,T))

### create (or connect to) grass location
gisDbase="/media/data/grassdata"
gisLocation="oregon"
gisMapset="mod06"

initGRASS(gisBase="/usr/lib/grass64",gisDbase=gisDbase,SG=td,override=T,
          location=gisLocation,mapset=gisMapset)


## update projection (for some reason the datum doesn't get identified from the HDF files
#cat("name: Sinusoidal (Sanson-Flamsteed)
#proj: sinu
#datum: wgs84
#ellps: wgs84
#lon_0: 0
#x_0: 0
#y_0: 0
#towgs84: 0,0,0,0,0,0,0
#no_defs: defined",file=paste(gisDbase,"/",gisLocation,"/PERMANENT/PROJ_INFO",sep=""))
## update PROJ_UNITS
#cat("unit: Meter
#units: Meters
#meters: 1",file=paste(gisDbase,"/",gisLocation,"/PERMANENT/PROJ_UNITS",sep=""))
#system("g.proj -d")

i=1
file=paste(outdir,"/",fs$file[1],sep="")


loadcloud<-function(file){
  ## Cloud Mask
   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Mask_1km_0",sep=""),
            output="CM1",flags=c("overwrite","o"))
   system("g.region rast=CM1")

   ## extract cloudy and 'confidently clear' pixels
   system(paste("r.mapcalc <<EOF
                CM_cloud=  ((CM1 / 2^0) % 2) == 1  &&  ((CM1 / 2^1) % 2^2) == 0
                CM_clear=  ((CM1 / 2^0) % 2) == 1  &&  ((CM1 / 2^1) % 2^2) == 3
EOF"))
   
   ## QA
   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Quality_Assurance_1km_0",sep=""),
             output="QA",flags=c("overwrite","o"))
   ## QA_CER
   system(paste("r.mapcalc <<EOF
                 QA_COT=   ((QA / 2^0) % 2^1 )==1
                 QA_COT2=  ((QA / 2^1) % 2^2 )==3
                 QA_COT3=  ((QA / 2^3) % 2^2 )==0
                 QA_CER=   ((QA / 2^5) % 2^1 )==1
                 QA_CER2=  ((QA / 2^6) % 2^2 )==3
EOF")) 
#                 QA_CWP=   ((QA / 2^8) % 2^1 )==1
#                 QA_CWP2=  ((QA / 2^9) % 2^2 )==3

   ## Optical Thickness
   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Optical_Thickness",sep=""),
            output="COT",
            title="cloud_effective_radius",
            flags=c("overwrite","o")) ; print("")
   execGRASS("r.null",map="COT",setnull="-9999")
   ## keep only positive COT values where quality is 'useful' and 'very good' & scale to real units
   system(paste("r.mapcalc \"COT=if(QA_COT&&QA_COT2&&QA_COT3&&COT>=0,COT*0.009999999776482582,null())\""))   
   ## set COT to 0 in clear-sky pixels
   system(paste("r.mapcalc \"COT2=if(CM_clear,COT,0)\""))   
   
   ## Effective radius
   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Effective_Radius",sep=""),
            output="CER",
            title="cloud_effective_radius",
            flags=c("overwrite","o")) ; print("")
   execGRASS("r.null",map="CER",setnull="-9999")
   ## keep only positive CER values where quality is 'useful' and 'very good' & scale to real units
   system(paste("r.mapcalc \"CER=if(QA_CER&&QA_CER2&&CER>=0,CER*0.009999999776482582,null())\""))   

   ## Cloud Water Path
#   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Water_Path",sep=""),
#            output="CWP",title="cloud_water_path",
#            flags=c("overwrite","o")) ; print("")
#   execGRASS("r.null",map="CWP",setnull="-9999")
#   ## keep only positive CWP values where quality is 'useful' and 'very good' & scale to real units
#   system(paste("r.mapcalc \"CWP=if(QA_CWP&&QA_CWP2,CWP,null())\""))   

   
 } ;

## unlock the grass database
unlink_.gislock()



  d=readRAST6("COT")
  d$CER=readRAST6("CER")$CER
plot(COT~CER,data=d@data)
summary(lm(COT~CER,data=d@data))




# read in data as single spatialgrid
ms=c("01","02","03","04","05","06","07","08","09","10","11","12")
for (m in ms){
  d=readRAST6(fs$grass[1])
  projection(d)=projection(td)
  d@data=as.data.frame(do.call(cbind,mclapply(which(fs$month==m),function(i){
    print(fs$date[i])
    readRAST6(fs$grass[i])@data[,1]
    })))
  d=brick(d)
  gc()
  assign(paste("m",m,sep="_"),d)
}

save(paste("m",m,sep="_"),file="output/MOD06.Rdata")

## replace missings with 0 (because they mean no clouds)
db2=d
db2[is.na(db2)]=0


md=mean(db,na.rm=T)
mn=sum(!is.na(db))


md2=mean(db2,na.rm=T)


# Histogram equalization stretch
eqstretch<-function(img){
  ecdf<-ecdf(getValues(img))
  return(calc(img,fun=function(x) ecdf(x)*255))
}

ncol=100
plot(md,col=rainbow(ncol),breaks=quantile(as.matrix(md),seq(0,1,len=ncol-1),na.rm=T))

str(d)
plot(brick(d))







#################################################################
### start grass to process the files

#get bounding box of region in m
ge=SpatialPoints(data.frame(lon=c(-125,-115),lat=c(40,47)))
projection(ge)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
ge2=spTransform(ge, CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0")); ge2



system("grass -text /media/data/grassdata/oregon/mod06")

### import variables one at a time
basedir=/home/adamw/acrobates/projects/interp/data/modis/MOD06_L2_tif

## parse the file names
fs=`ls data/modis/MOD06_L2_tif | grep tif$`
echo `echo $fs | wc -w` files to process

## example file
f=MOD06_L2.A2000062.1830.051.2010273075045.gscs_000500676719.tif


for f in $fs
do
year=`echo $f |cut -c 11-14`
day=`echo $f |cut -c 15-17 |sed 's/^0*//'`
time=`echo $f |cut -c 19-22`
month=$(date -d "`date +%Y`-01-01 +$(( ${day} - 1 ))days" +%m)
ofile=$month\_$year\_cloud_effective_radius_$f
r.in.gdal --quiet -e input=$basedir/$f output=$ofile band=1 title=cloud_effective_radius --overwrite
r.mapcalc "$ofile=if($ofile,$ofile,-9999,-9999)"
r.null --q map="$ofile" setnull=-9999
r.mapcalc "$ofile=$ofile*0.009999999776482582"
r.colors --quiet -ne map=$ofile color=precipitation
echo Finished $f
done

## generate monthly means
m02=`g.mlist type=rast pattern="02*"`
m02n=`echo $m02 | wc -w`

m02p=`printf '%q+' $m02 | sed 's/\(.*\)./\1/'`
r.mapcalc "m02=($m02p)/$m02n"

#  g.mremove -f rast=02*



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


### set options for Raster Package
setOptions(progress="text",timer=T)

X11.options(type="Xlib")
ncores=20  #number of threads to use

setwd("/home/adamw/personal/projects/interp")
setwd("/home/adamw/acrobates/projects/interp")

## read in shapefile as Region of Interest (roi)
roi=readOGR("data/regions/Test_sites/Oregon.shp","Oregon")
roi=spTransform(roi,CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))

### use MODIS tile as ROI instead
modt=readOGR("/home/adamw/acrobates/Global/modis_sinusoidal","modis_sinusoidal_grid_world",)
tiles=c("H9V4")
roi=modt[modt$HV%in%tiles,]

## Bounding box of region in lat/lon
roi_ll=spTransform(roi,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
roi_bb=bbox(roi_ll)
     
### Downloading data from LAADSWEB
# subset by geographic area of interest

## download data from ftp site.  Unfortunately the selection has to be selected using the website and orders downloaded via ftp.

#system("wget -r --retr-symlinks ftp://ladsweb.nascom.nasa.gov/orders/500676499/ -P /home/adamw/acrobates/projects/interp/data/modis/MOD06_L2_hdf")

## specify some working directories
gdir="output/"
datadir="data/modis/MOD06_L2_hdf"
outdir="data/modis/MOD06_L2_hdf2"
tifdir="/media/data/MOD06_L2_tif"
summarydatadir="data/modis/MOD06_climatologies"


fs=data.frame(
  path=list.files(datadir,full=T,recursive=T,pattern="hdf"),
  file=basename(list.files(datadir,full=F,recursive=T,pattern="hdf")))
fs$date=as.Date(substr(fs$file,11,17),"%Y%j")
fs$month=format(fs$date,"%m")
fs$year=format(fs$date,"%Y")
fs$time=substr(fs$file,19,22)
fs$datetime=as.POSIXct(strptime(paste(substr(fs$file,11,17),substr(fs$file,19,22)), '%Y%j %H%M'))
fs$dateid=format(fs$date,"%Y%m%d")
fs$path=as.character(fs$path)
fs$file=as.character(fs$file)

## output ROI
#get bounding box of region in m
#ge=SpatialPoints(data.frame(lon=c(-125,-115),lat=c(40,47)))
#projection(ge)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#ge2=spTransform(ge, CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))

## vars
vars=as.data.frame(matrix(c(
  "Cloud_Effective_Radius",              "CER",
  "Cloud_Effective_Radius_Uncertainty",  "CERU",
  "Cloud_Optical_Thickness",             "COT",
  "Cloud_Optical_Thickness_Uncertainty", "COTU",
  "Cloud_Water_Path",                    "CWP",
  "Cloud_Water_Path_Uncertainty",        "CWPU",
  "Cloud_Phase_Optical_Properties",      "CPOP",
  "Cloud_Multi_Layer_Flag",              "CMLF",
  "Cloud_Mask_1km",                      "CM1",
  "Quality_Assurance_1km",               "QA"),
  byrow=T,ncol=2,dimnames=list(1:10,c("variable","varid"))),stringsAsFactors=F)


### Installation of hegtool
## needed 32-bit libraries and java for program to install correctly

# system(paste("hegtool -h ",fs$path[1],sep=""))


#### Function to generate hegtool parameter file for multi-band HDF-EOS file
swath2grid=function(i=1,files,vars,outdir,upleft,lowright){
  file=fs$path[i]
  print(paste("Starting file",basename(file)))
  outfile=paste(outdir,"/",fs$file[i],sep="")
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
#RESAMPLING_TYPE =",ifelse(grepl("Flag|Mask|Quality",vars),"NN","CUBIC"),"
RESAMPLING_TYPE =NN
OUTPUT_PROJECTION_TYPE = UTM
UTM_ZONE = 10
# OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 86400.0 0.0 1.0 0.0 0.0 0.0 0.0 )
# projection parameters from http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=is_gctp
ELLIPSOID_CODE = WGS84
OUTPUT_TYPE = HDFEOS
OUTPUT_FILENAME = ",outfile,"
END

",sep="")
  
  ## if any remnants from previous runs remain, delete them
  if(length(list.files(tempdir(),pattern=basename(file)))>0)
    file.remove(list.files(tempdir(),pattern=basename(file),full=T))
  ## write it to a file
  cat(c(hdr,grp)    , file=paste(tempdir(),"/",basename(file),"_MODparms.txt",sep=""))
  ## now run the swath2grid tool
  ## write the tiff file
  log=system(paste("sudo MRTDATADIR=/usr/local/heg/data ",
    "PGSHOME=/usr/local/heg/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/bin/swtif -p ",
    paste(tempdir(),"/",basename(file),"_MODparms.txt -d",sep=""),sep=""),intern=T)
      print(paste("Finished ", file))
}
 

### update fs with completed files
fs$complete=fs$file%in%list.files(outdir,pattern="hdf$")
table(fs$complete)

## must be run as root to access the data directory!  argh!
## run sudo once here to enter password before continuing
system('sudo ls')

#### Run the gridding procedure
mclapply(which(!fs$complete),function(fi){
  swath2grid(fi,vars=vars$variable,files=fs,
             outdir=outdir,
             upleft=paste(roi_bb[2,2],roi_bb[1,1]),
             lowright=paste(roi_bb[2,1],roi_bb[1,2]))},
         mc.cores=24)


##############################################################
### Import to GRASS for processing

#fs$grass=paste(fs$month,fs$year,fs$file,sep="_")
td=readGDAL(paste("HDF4_EOS:EOS_GRID:\"",outdir,"/",fs$file[1],"\":mod06:Cloud_Mask_1km_0",sep=""))
#projection(td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs +datum=WGS84 +ellps=WGS84 "
projection(td)="+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## fucntion to convert binary to decimal to assist in identifying correct values
b2d=function(x) sum(x * 2^(rev(seq_along(x)) - 1)) #http://tolstoy.newcastle.edu.au/R/e2/help/07/02/10596.html
## for example:
b2d(c(T,T))

### create (or connect to) grass location
gisDbase="/media/data/grassdata"
gisLocation="oregon"
gisMapset="mod06"
## set Grass to overwrite
Sys.setenv(GRASS_OVERWRITE=1)
Sys.setenv(DEBUG=0)

## temporary objects to test function below
 i=1
file=paste(outdir,"/",fs$file[1],sep="")
date=as.Date("2000-05-23")


### Function to extract various SDSs from a single gridded HDF file and use QA data to throw out 'bad' observations
loadcloud<-function(date,fs){
### set up unique grass session
  tf=paste(tempdir(),"/grass", Sys.getpid(),"/", sep="")
 
  ## set up tempfile for this PID
  initGRASS(gisBase="/usr/lib/grass64",gisDbase=tf,SG=td,override=T,location="mod06",mapset="PERMANENT",home=tf,pid=Sys.getpid())
#  system(paste("g.proj -c proj4=\"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +datum=WGS84 +units=m +no_defs\"",sep=""))
    system(paste("g.proj -c proj4=\"+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs\"",sep=""))

  ## Define region by importing one raster.
  execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",outdir,"/",fs$file[1],"\":mod06:Cloud_Mask_1km_0",sep=""),
            output="modisgrid",flags=c("quiet","overwrite","o"))
  system("g.region rast=modisgrid save=roi --overwrite")
  system("g.region roi")
  system("g.region -p")

  ## Identify which files to process
  tfs=fs$file[fs$date==date]
  nfs=length(tfs)

  ### print some summary info
  print(date)
  ## loop through scenes and process QA flags
  for(i in 1:nfs){
     file=paste(outdir,"/",tfs[i],sep="")
     ## Cloud Mask
     execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Mask_1km_0",sep=""),
              output=paste("CM1_",i,sep=""),flags=c("overwrite","o")) ; print("")
    ## extract cloudy and 'confidently clear' pixels
    system(paste("r.mapcalc <<EOF
                CM_cloud_",i," =  ((CM1_",i," / 2^0) % 2) == 1  &&  ((CM1_",i," / 2^1) % 2^2) == 0 
                CM_clear_",i," =  ((CM1_",i," / 2^0) % 2) == 1  &&  ((CM1_",i," / 2^1) % 2^2) == 3 
EOF",sep=""))

    ## QA
    execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Quality_Assurance_1km_0",sep=""),
             output=paste("QA_",i,sep=""),flags=c("overwrite","o")) ; print("")
   ## QA_CER
   system(paste("r.mapcalc <<EOF
                 QA_COT_",i,"=   ((QA_",i," / 2^0) % 2^1 )==1
                 QA_COT2_",i,"=  ((QA_",i," / 2^1) % 2^2 )==3
                 QA_COT3_",i,"=  ((QA_",i," / 2^3) % 2^2 )==0
                 QA_CER_",i,"=   ((QA_",i," / 2^5) % 2^1 )==1
                 QA_CER2_",i,"=  ((QA_",i," / 2^6) % 2^2 )==3
EOF",sep="")) 
#                 QA_CWP_",i,"=   ((QA_",i," / 2^8) % 2^1 )==1
#                 QA_CWP2_",i,"=  ((QA_",i," / 2^9) % 2^2 )==3

   ## Optical Thickness
   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Optical_Thickness",sep=""),
            output=paste("COT_",i,sep=""),
            title="cloud_effective_radius",
            flags=c("overwrite","o")) ; print("")
   execGRASS("r.null",map=paste("COT_",i,sep=""),setnull="-9999")
   ## keep only positive COT values where quality is 'useful' and 'very good' & scale to real units
   system(paste("r.mapcalc \"COT_",i,"=if(QA_COT_",i,"&&QA_COT2_",i,"&&QA_COT3_",i,"&&COT_",i,">=0,COT_",i,"*0.009999999776482582,null())\"",sep=""))   
   ## set COT to 0 in clear-sky pixels
   system(paste("r.mapcalc \"COT2_",i,"=if(CM_clear_",i,"==0,COT_",i,",0)\"",sep=""))   
   
   ## Effective radius ##
   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Effective_Radius",sep=""),
            output=paste("CER_",i,sep=""),
            title="cloud_effective_radius",
            flags=c("overwrite","o")) ; print("")
   execGRASS("r.null",map=paste("CER_",i,sep=""),setnull="-9999")
   ## keep only positive CER values where quality is 'useful' and 'very good' & scale to real units
   system(paste("r.mapcalc \"CER_",i,"=if(QA_CER_",i,"&&QA_CER2_",i,"&&CER_",i,">=0,CER_",i,"*0.009999999776482582,null())\"",sep=""))   
   ## set CER to 0 in clear-sky pixels
   system(paste("r.mapcalc \"CER2_",i,"=if(CM_clear_",i,"==0,CER_",i,",0)\"",sep=""))   

   ## Cloud Water Path
#   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Water_Path",sep=""),
#            output=paste("CWP_",i,sep=""),title="cloud_water_path",
#            flags=c("overwrite","o")) ; print("")
#   execGRASS("r.null",map=paste("CWP_",i,sep=""),setnull="-9999")
   ## keep only positive CER values where quality is 'useful' and 'very good' & scale to real units
#   system(paste("r.mapcalc \"CWP_",i,"=if(QA_CWP_",i,"&&QA_CWP2_",i,"&&CWP_",i,">=0,CWP_",i,"*0.009999999776482582,null())\"",sep=""))   
   ## set CER to 0 in clear-sky pixels
#   system(paste("r.mapcalc \"CWP2_",i,"=if(CM_clear_",i,"==0,CWP_",i,",0)\"",sep=""))   
     
 } #end loop through sub daily files

#### Now generate daily averages (or maximum in case of cloud flag)
  
  system(paste("r.mapcalc <<EOF
         COT_denom=",paste("!isnull(COT2_",1:nfs,")",sep="",collapse="+"),"
         COT_numer=",paste("if(isnull(COT2_",1:nfs,"),0,COT2_",1:nfs,")",sep="",collapse="+"),"
         COT_daily=COT_numer/COT_denom
         CER_denom=",paste("!isnull(CER2_",1:nfs,")",sep="",collapse="+"),"
         CER_numer=",paste("if(isnull(CER2_",1:nfs,"),0,CER2_",1:nfs,")",sep="",collapse="+"),"
         CER_daily=CER_numer/CER_denom
         CLD_daily=max(",paste("if(isnull(CM_cloud_",1:nfs,"),0,CM_cloud_",1:nfs,")",sep="",collapse=","),") 
EOF",sep=""))

  #### Write the files to a geotiff
  execGRASS("r.out.gdal",input="CER_daily",output=paste(tifdir,"/CER_",format(date,"%Y%m%d"),".tif",sep=""),nodata=-999,flags=c("quiet"))
  execGRASS("r.out.gdal",input="COT_daily",output=paste(tifdir,"/COT_",format(date,"%Y%m%d"),".tif",sep=""),nodata=-999,flags=c("quiet"))
  execGRASS("r.out.gdal",input="CLD_daily",output=paste(tifdir,"/CLD_",format(date,"%Y%m%d"),".tif",sep=""),nodata=99,flags=c("quiet"))

### delete the temporary files 
  unlink_.gislock()
  system("/usr/lib/grass64/etc/clean_temp")
 system(paste("rm -R ",tf,sep=""))
### print update
  print(paste(" ###################################################################               Finished ",date,"
################################################################"))
}


###########################################
### Now run it

tdates=sort(unique(fs$date))
done=tdates%in%as.Date(substr(list.files(tifdir),5,12),"%Y%m%d")
table(done)
tdates=tdates[!done]

mclapply(tdates,function(date) loadcloud(date,fs=fs))

 

#######################################################################################33
###  Produce the monthly averages

## get list of daily files
fs2=data.frame(
  path=list.files(tifdir,full=T,recursive=T,pattern="tif$"),
  file=basename(list.files(tifdir,full=F,recursive=T,pattern="tif$")))
fs2$type=substr(fs2$file,1,3)
fs2$date=as.Date(substr(fs2$file,5,12),"%Y%m%d")
fs2$month=format(fs2$date,"%m")
fs2$year=format(fs2$date,"%Y")
fs2$path=as.character(fs2$path)
fs2$file=as.character(fs2$file)


# Define type/month products
vs=expand.grid(type=unique(fs2$type),month=c("01","02","03","04","05","06","07","08","09","10","11","12"))

## identify which have been completed
done.vs=unique(do.call(rbind,strsplit(list.files(summarydatadir),"_|[.]"))[,c(1,3)])
done.vs=paste(vs$type,vs$month,sep="_")%in%paste(done.vs[,1],done.vs[,2],sep="_")
table(done.vs)
vs=vs[!done.vs,]

#tfs=fs2$path[which(fs2$month==vs$month[i]&fs2$type==vs$type[i])]
#do.call(c,mclapply(2:length(tfs),function(f) #identical(extent(raster(tfs[f-1])),extent(raster(tfs[f])))))
#raster(tfs[23])

## process the monthly summaries using the raster package
mclapply(1:nrow(vs),function(i){
  print(paste("Loading ",vs$type[i]," for month ",vs$month[i]))
  td=stack(fs2$path[which(fs2$month==vs$month[i]&fs2$type==vs$type[i])])
  print(paste("Processing Metric ",vs$type[i]," for month ",vs$month[i]))
  calc(td,mean,na.rm=T,
       filename=paste(summarydatadir,"/",vs$type[i],"_mean_",vs$month[i],".tif",sep=""),
       format="GTiff",overwrite=T)
  calc(td,sd,na.rm=T,
       filename=paste(summarydatadir,"/",vs$type[i],"_sd_",vs$month[i],".tif",sep=""),
       format="GTiff",overwrite=T)
  if(vs$type[i]%in%c("CER")) {
    ## Calculate number of days with effective radius > 20um, found to be linked to precipitating clouds
    ## (Kobayashi 2007)
    calc(td,function(x) mean(ifelse(x<20,0,1),na.rm=T),
      filename=paste(summarydatadir,"/",vs$type[i],"_P20um_",vs$month[i],".tif",sep=""),
      format="GTiff",overwrite=T)
#    td2[td2<20]=0
#    td2[td2>=20]=1
#    calc(td2,mean,na.rm=T,
#         filename=paste(summarydatadir,"/",vs$type[i],"_P20um_",vs$month[i],".tif",sep=""),
#         format="GTiff",overwrite=T)
  }  
  print(paste("Processing missing data for ",vs$type[i]," for month ",vs$month[i]))
  calc(td,function(i)
       sum(!is.na(i)),filename=paste(summarydatadir,"/",vs$type[i],"_count_",vs$month[i],".tif",sep=""),
       format="GTiff",overwrite=T)
  calc(td,function(i) sum(ifelse(i==0,0,1)),
       filename=paste(summarydatadir,"/",vs$type[i],"_clear_",vs$month[i],".tif",sep=""),format="GTiff",overwrite=T)
  gc()
}
)


vs[c(10:18,22:24,34:36),]

#### reproject to Oregon state plane for easy comparison with Benoit's data
dest=raster("data/regions/oregon/lulc/W_Layer1_ClippedTo_OR83M.rst")  # choose one to match (projection, resolution, extent)
projection(dest)=CRS("+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs") #define projection
tifs=list.files(summarydatadir,pattern="*.tif$",full=T) #get list of files to process
mclapply(tifs,function(f) projectRaster(raster(f),dest,filename=paste(dirname(f),"/OR03M_",basename(f),sep=""),overwrite=T))  # warp them




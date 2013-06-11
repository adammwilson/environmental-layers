###################################################################################
###  R code to aquire and process MOD35_L2 cloud data from the MODIS platform


# Redirect all warnings to stderr()
#options(warn = -1)
#write("2) write() to stderr", stderr())
#write("2) write() to stdout", stdout())
#warning("2) warning()")


## import commandline arguments
library(getopt)
## load libraries
require(reshape)
require(geosphere)
require(raster)
require(rgdal)
require(spgrass6)
require(RSQLite)

## get options
opta <- getopt(matrix(c(
                        'date', 'd', 1, 'character',
                        'tile', 't', 1, 'character',
                        'verbose','v',1,'logical',
                        'help', 'h', 0, 'logical'
                        ), ncol=4, byrow=TRUE))
if ( !is.null(opta$help) )
  {
       prg <- commandArgs()[1];
          cat(paste("Usage: ", prg,  " --date | -d <file> :: The date to process\n", sep=""));
          q(status=1);
     }


## default date and tile to play with  (will be overwritten below when running in batch)
#date="20090129"
#tile="h11v08"
platform="pleiades" 
verbose=T

## now update using options if given
date=opta$date  
tile=opta$tile 
verbose=opta$verbose  #print out extensive information for debugging?
## get year and doy from date
year=format(as.Date(date,"%Y%m%d"),"%Y")
doy=format(as.Date(date,"%Y%m%d"),"%j")

if(platform=="pleiades"){
  ## location of MOD35 files
  datadir=paste("/nobackupp4/datapool/modis/MOD35_L2.006/",year,"/",doy,"/",sep="")
  ## path to some executables
  ncopath="/nasa/sles11/nco/4.0.8/gcc/mpt/bin/"
  swtifpath="/nobackupp1/awilso10/software/heg/bin/swtif"
  ## path to swath database
  db="/nobackupp4/pvotava/DB/export/swath_geo.sql.sqlite3.db"
  ## specify working directory
  outdir=paste("/nobackupp1/awilso10/mod35/daily/",tile,"/",sep="")  #directory for separate daily files
  basedir="/nobackupp1/awilso10/mod35/" #directory to hold files temporarily before transferring to lou
  setwd(tempdir())
  ## grass database
  gisBase="/u/armichae/pr/grass-6.4.2/"
  ## path to MOD11A1 file for this tile to align grid/extent
  gridfile=list.files("/nobackupp4/datapool/modis/MOD11A2.005/2009.01.01",pattern=paste(tile,".*[.]hdf$",sep=""),recursive=T,full=T)[1]
  td=readGDAL(paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_8Day_1km_LST:LST_Day_1km",sep=""))
  projection(td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs +datum=WGS84 +ellps=WGS84 "
}

if(platform=="litoria"){  #if running on local server, use different paths
  ## specify working directory
  setwd("~/acrobates/adamw/projects/interp")
  outdir=paste("daily/",tile,"/",sep="")  #directory for separate daily files
  basedir=outdir
  gisBase="/usr/lib/grass64"
   ## location of MOD06 files
  datadir="~/acrobates/adamw/projects/interp/data/modis/mod35"
  ## path to some executables
  ncopath=""
  swtifpath="sudo MRTDATADIR=\"/usr/local/heg/data\" PGSHOME=/usr/local/heg/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/bin/swtif"
  ## path to swath database
  db="~/acrobates/adamw/projects/interp/data/modis/mod06/swath_geo.sql.sqlite3.db"
  ## get grid file
  td=raster(paste("~/acrobates/adamw/projects/interp/data/modis/mod06/summary/MOD06_",tile,".nc",sep=""),varname="CER")
  projection(td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs +datum=WGS84 +ellps=WGS84 "
}


### print some status messages
if(verbose) print(paste("Processing tile",tile," for date",date))

## load tile information and get bounding box
load(file="/nobackupp1/awilso10/mod35/modlandTiles.Rdata")
tile_bb=tb[tb$tile==tile,] ## identify tile of interest

## get bounds of swath to keep and feed into grass when generating tile
## expand a little (0.5 deg) to ensure that there is no clipping of pixels on the edges
## tile will later be aligned with MODLAND tile so the extra will eventually be trimmed
upleft=paste(min(90,tile_bb$lat_max+0.5),max(-180,tile_bb$lon_min-0.5)) #northwest corner
lowright=paste(max(-90,tile_bb$lat_min-0.5),min(180,tile_bb$lon_max+0.5)) #southeast corner

## vars to process
vars=as.data.frame(matrix(c(
  "Cloud_Mask",              "CM",
  "Quality_Assurance",       "QA"),
  byrow=T,ncol=2,dimnames=list(1:2,c("variable","varid"))),stringsAsFactors=F)

## vector of variables expected to be in final netcdf file.  If these are not present, the file will be deleted at the end.
finalvars=c("CMday","CMnight")


#####################################################
##find swaths in region from sqlite database for the specified date/tile
if(verbose) print("Accessing swath ID's from database")
con=dbConnect("SQLite", dbname = db)
fs=dbGetQuery(con,paste("SELECT * from swath_geo6
            WHERE east>=",tile_bb$lon_min," AND
                  west<=",tile_bb$lon_max," AND
                  north>=",tile_bb$lat_min," AND
                  south<=",tile_bb$lat_max," AND
                  year==",format(as.Date(date,"%Y%m%d"),"%Y")," AND
                  day==",as.numeric(format(as.Date(date,"%Y%m%d"),"%j"))
  ))
con=dbDisconnect(con)
fs$id=substr(fs$id,7,19)
## find the swaths on disk (using datadir)
swaths=list.files(datadir,pattern=paste(fs$id,collapse="|"),recursive=T,full=T)

if(verbose) print(paste(nrow(fs)," swath IDs recieved from database and ",length(swaths)," found on disk"))


############################################################################
############################################################################
### Use the HEG tool to grid all available swath data for this date-tile
for(file in swaths){
  ## Function to generate hegtool parameter file for multi-band HDF-EOS file
  print(paste("Starting file",basename(file)))
  outfile=paste(tempdir(),"/",basename(file),sep="")
  ## First write the parameter file (careful, heg is very finicky!)
  hdr=paste("NUM_RUNS = ",length(vars$varid),"|MULTI_BAND_HDFEOS:",length(vars$varid),sep="")
  grp=paste("
BEGIN
INPUT_FILENAME=",file,"
OBJECT_NAME=mod35
FIELD_NAME=",vars$variable,"|
BAND_NUMBER = 1
OUTPUT_PIXEL_SIZE_X=926.6
OUTPUT_PIXEL_SIZE_Y=926.6
# MODIS 1km Resolution
SPATIAL_SUBSET_UL_CORNER = ( ",upleft," )
SPATIAL_SUBSET_LR_CORNER = ( ",lowright," )
#RESAMPLING_TYPE =",ifelse(grepl("Flag|Mask|Quality",vars),"NN","CUBIC"),"
RESAMPLING_TYPE =NN
OUTPUT_PROJECTION_TYPE = SIN
OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# projection parameters from http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=sn_gctp
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
  ## write the gridded file
  system(paste("(",swtifpath," -p ",tempdir(),"/",basename(file),"_MODparms.txt -d)",sep=""),intern=F,ignore.stderr=F)
  print(paste("Finished gridding ", file," for tile ",tile))
}  #end looping over swaths

########################
## confirm at least one file for this date is present.  If not, quit.
outfiles=paste(tempdir(),"/",basename(swaths),sep="")
if(!any(file.exists(outfiles))) {
  print(paste("########################################   No gridded files for region exist for tile",tile," on date",date))
  q("no",status=0)
}

#####################################################
## Process the gridded files to align exactly with MODLAND tile and produce a daily summary of multiple swaths
  
## function to convert binary to decimal to assist in identifying correct values
## this is helpful when defining QA handling below, but isn't used in processing
## b2d=function(x) sum(x * 2^(rev(seq_along(x)) - 1)) #http://tolstoy.newcastle.edu.au/R/e2/help/07/02/10596.html
## for example:
## b2d(c(T,T))

  ## set Grass to overwrite
  Sys.setenv(GRASS_OVERWRITE=1)
  Sys.setenv(DEBUG=1)
  Sys.setenv(GRASS_GUI="txt")

### Extract various SDSs from a single gridded HDF file and use QA data to throw out 'bad' observations
## make temporary working directory
  tf=paste(tempdir(),"/grass", Sys.getpid(),"/", sep="")  #temporar
  if(!file.exists(tf)) dir.create(tf)
  ## create output directory if needed
  ## Identify output file
  ncfile=paste(outdir,"MOD35_",tile,"_",date,".nc",sep="")  #this is the 'final' daily output file
  if(!file.exists(dirname(ncfile))) dir.create(dirname(ncfile),recursive=T)
 
  ## set up temporary grass instance for this PID
  if(verbose) print(paste("Set up temporary grass session in",tf))
  initGRASS(gisBase=gisBase,gisDbase=tf,SG=as(td,"SpatialGridDataFrame"),override=T,location="mod06",mapset="PERMANENT",home=tf,pid=Sys.getpid())
  system(paste("g.proj -c proj4=\"",projection(td),"\"",sep=""),ignore.stdout=T,ignore.stderr=T)

  ## Define region by importing one MOD11A1 raster.
  print("Import one MOD11A1 raster to define grid")
  if(platform=="pleiades") {
    execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_8Day_1km_LST:LST_Day_1km",sep=""),
              output="modisgrid",flags=c("quiet","overwrite","o"))
    system("g.region rast=modisgrid save=roi --overwrite",ignore.stdout=F,ignore.stderr=F)
  }

if(platform=="litoria"){
  execGRASS("r.in.gdal",input=paste("NETCDF:\"/home/adamw/acrobates/adamw/projects/interp/data/modis/mod06/summary/MOD06_",tile,".nc\":CER",sep=""),
            output="modisgrid",flags=c("overwrite","o"))
  system("g.region rast=modisgrid.1 save=roi --overwrite",ignore.stdout=F,ignore.stderr=F)
}
## Identify which files to process
tfs=basename(swaths)
## drop swaths that did not produce an output file (typically due to not overlapping the ROI)
tfs=tfs[tfs%in%list.files(tempdir())]
#tfs=list.files(tempdir(),pattern="temp.*hdf")
nfs=length(tfs)
if(verbose) print(paste(nfs,"swaths available for processing"))

## loop through scenes and process QA flags
  for(i in 1:nfs){
     file=paste(tempdir(),"/",tfs[i],sep="")
     ## Cloud Mask
     execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod35:Cloud_Mask_0",sep=""),
              output=paste("CM1_",i,sep=""),flags=c("overwrite","o")) ; print("")
    ## QA      ## extract first bit to keep only "useful" values of cloud mask
     execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod35:Quality_Assurance_0",sep=""),
             output=paste("QA_",i,sep=""),flags=c("overwrite","o")) ; print("")
     ## extract first two bits of cloud mask
    system(paste("r.mapcalc <<EOF
                QA_useful_",i," =  if((QA_",i," / 2^0) % 2==1,1,0)
                CM_cloud_",i," =  if((CM1_",i," / 2^0) % 2==1,(CM1_",i," / 2^1) % 2^2,-9999)
                CM_dayflag_",i," =  if((CM1_",i," / 2^3) % 2==1,1,0)
                CMday_",i," = if(QA_useful_",i,"==1&CM_dayflag_",i,"==1,CM_cloud_",i,",-9999)
                CMnight_",i," = if(QA_useful_",i,"==1&CM_dayflag_",i,"==0,CM_cloud_",i,",-9999)
EOF",sep=""))
                                        #Pclear1_",i," = if(CM_cloud_",i,"==0,0,if(CM_cloud_",i,"==1,66,if(CM_cloud_",i,"==2,95,if(CM_cloud_",i,"==3,99,-9999))))
                                        #CM_path_",i," =   ((CM1_",i," / 2^6) % 2^2) 

#     execGRASS("r.null",map=paste("Pclearday_",i,sep=""),setnull="-9999")
#     execGRASS("r.null",map=paste("Pclearnight_",i,sep=""),setnull="-9999")

     execGRASS("r.null",map=paste("CMday_",i,sep=""),setnull="-9999")
     execGRASS("r.null",map=paste("CMnight_",i,sep=""),setnull="-9999")
   
    
 } #end loop through sub daily files

#### Now generate daily minimum p(clear)
  system(paste("r.mapcalc <<EOF
         CMday_daily=int((min(",paste("if(isnull(CMday_",1:nfs,"),9999,CMday_",1:nfs,")",sep="",collapse=","),"))) 
         CMnight_daily=int((min(",paste("if(isnull(CMnight_",1:nfs,"),9999,CMnight_",1:nfs,")",sep="",collapse=","),"))) 
EOF",sep=""))


## reset null values
execGRASS("r.null",map="CMday_daily",setnull="9999")
execGRASS("r.null",map="CMnight_daily",setnull="9999")

  ### Write the files to a netcdf file
  ## create image group to facilitate export as multiband netcdf
    execGRASS("i.group",group="mod35",input=c("CMday_daily","CMnight_daily")) ; print("")
   
  if(file.exists(ncfile)) file.remove(ncfile)  #if it exists already, delete it
  execGRASS("r.out.gdal",input="mod35",output=ncfile,type="Byte",nodata=255,flags=c("quiet"),
#      createopt=c("FORMAT=NC4","ZLEVEL=5","COMPRESS=DEFLATE","WRITE_GDAL_TAGS=YES","WRITE_LONLAT=NO"),format="netCDF")  #for compressed netcdf
      createopt=c("FORMAT=NC","WRITE_GDAL_TAGS=YES","WRITE_LONLAT=NO"),format="netCDF")

  system(paste(ncopath,"ncecat -O -u time ",ncfile," ",ncfile,sep=""))
## create temporary nc file with time information to append to MOD06 data
  cat(paste("
    netcdf time {
      dimensions:
        time = 1 ;
      variables:
        int time(time) ;
      time:units = \"days since 2000-01-01 00:00:00\" ;
      time:calendar = \"gregorian\";
      time:long_name = \"time of observation\"; 
    data:
      time=",as.integer(as.Date(date,"%Y%m%d")-as.Date("2000-01-01")),";
    }"),file=paste(tempdir(),"/time.cdl",sep=""))
system(paste("ncgen -o ",tempdir(),"/time.nc ",tempdir(),"/time.cdl",sep=""))
system(paste(ncopath,"ncks -A ",tempdir(),"/time.nc ",ncfile,sep=""))
## add other attributes
  system(paste(ncopath,"ncrename -v Band1,CMday -v Band2,CMnight ",ncfile,sep=""))
  system(paste(ncopath,"ncatted ",
" -a units,CMday,o,c,\"Cloud Flag (0-3)\" ",
" -a missing_value,CMday,o,b,255 ",
" -a _FillValue,CMday,o,b,255 ",
" -a valid_range,CMday,o,b,\"0,3\" ",
" -a long_name,CMday,o,c,\"Cloud Flag from 'day' pixels\" ",
" -a units,CMnight,o,c,\"Cloud Flag (0-3)\" ",
" -a missing_value,CMnight,o,b,255 ",
" -a _FillValue,CMnight,o,b,255 ",
" -a valid_range,CMnight,o,b,\"0,3\" ",
" -a long_name,CMnight,o,c,\"Cloud Flag from 'night' pixels\" ",
ncfile,sep=""))
#system(paste(ncopath,"ncatted -a sourcecode,global,o,c,",script," ",ncfile,sep=""))
   

## Confirm that the file has the correct attributes, otherwise delete it
ntime=as.numeric(system(paste("cdo -s ntime ",ncfile),intern=T))
## confirm it has all 'final variables as specified above"
fvar=all(finalvars%in%strsplit(system(paste("cdo -s showvar ",ncfile),intern=T)," ")[[1]])

  if(ntime!=1|!fvar) {
      print(paste("FILE ERROR:  tile ",tile," and date ",date," was not outputted correctly, deleting... "))
      file.remove(ncfile)
    }

############  copy files to lou
#if(platform=="pleiades"){
#  archivedir=paste("MOD35/",outdir,"/",sep="")  #directory to create on lou
#  system(paste("ssh -q bridge2 \"ssh -q lou mkdir -p ",archivedir,"\"",sep=""))
#  system(paste("ssh -q bridge2 \"scp -q ",ncfile," lou:",archivedir,"\"",sep=""))
#  file.remove(ncfile)
#  file.remove(paste(ncfile,".aux.xml",sep=""))
#}

  
### delete the temporary files 
#  unlink_.gislock()
#  system(paste("rm -frR ",tempdir(),sep=""))


  ## print out some info
print(paste(" ###################################################################               Finished ",date,
"################################################################"))

## delete old files
#system("cleartemp")

## quit
q("no",status=0)

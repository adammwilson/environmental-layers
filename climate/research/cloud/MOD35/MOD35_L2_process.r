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
                        'profile','p',0,'logical',
                        'help', 'h', 0, 'logical'
                        ), ncol=4, byrow=TRUE))
if ( !is.null(opta$help) )
  {
       prg <- commandArgs()[1];
          cat(paste("Usage: ", prg,  " --date | -d <file> :: The date to process\n", sep=""));
          q(status=1);
     }

testing=F
platform="pleiades" 

## record profiling information if requested
if(opta$profile)  Rprof("/nobackupp1/awilso10/mod35/log/profile.out")

## default date and tile to play with  (will be overwritten below when running in batch)
if(testing){
  date="20090129"
  tile="h11v08"
  tile="h17v00"
  verbose=T
}

## now update using options if given
if(!testing){
  date=opta$date  
  tile=opta$tile 
  verbose=opta$verbose  #print out extensive information for debugging?
}
## get year and doy from date
year=format(as.Date(date,"%Y%m%d"),"%Y")
doy=format(as.Date(date,"%Y%m%d"),"%j")

if(platform=="pleiades"){
  ## location of MOD35 files
  datadir=paste("/nobackupp4/datapool/modis/MOD35_L2.006/",year,"/",doy,"/",sep="")
  ## path to some executables
  ncopath="/nasa/sles11/nco/4.0.8/gcc/mpt/bin/"
  swtifpath="/nobackupp1/awilso10/software/heg/bin/swtif_2.12"
#  swtifpath="/nobackupp4/pvotava/software/heg/2.12/bin/swtif"
  ## path to swath database
  db="/nobackupp4/pvotava/DB/export/swath_geo.sql.sqlite3.db"
  ## specify working directory
  outdir=paste("/nobackupp1/awilso10/mod35/daily/",tile,"/",sep="")  #directory for separate daily files
  basedir="/nobackupp1/awilso10/mod35/" #directory to hold files temporarily before transferring to lou
  setwd(tempdir())
  ## grass database
  gisBase="/u/armichae/pr/grass-6.4.2/"
  ## path to MOD11A1 file for this tile to align grid/extent
  gridfile=list.files("/nobackupp1/awilso10/mod35/MODTILES/",pattern=tile,full=T)[1]
  td=raster(gridfile)
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
if(verbose) writeLines(paste("STATUS: Beginning ",tile,date))

## load tile information and get bounding box
load(file="/nobackupp1/awilso10/mod35/modlandTiles.Rdata")
tile_bb=tb[tb$tile==tile,] ## identify tile of interest

## get bounds of swath to keep and feed into grass when generating tile
## expand a little (0.5 deg) to ensure that there is no clipping of pixels on the edges
## tile will later be aligned with MODLAND tile so the extra will eventually be trimmed
upleft=paste(min(90,tile_bb$lat_max+0.5),max(-180,tile_bb$lon_min-0.5)) #northwest corner
lowright=paste(max(-90,tile_bb$lat_min-0.5),min(180,tile_bb$lon_max+0.5)) #southeast corner

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

### print some status messages
if(verbose) writeLines(paste("STATUS:swaths tile:",tile,"date:",date,"swathIDs:",nrow(fs)," swathsOnDisk:",length(swaths)))


## define function that grids swaths
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
OUTPUT_PIXEL_SIZE_X=926.6
OUTPUT_PIXEL_SIZE_Y=926.6
# MODIS 1km Resolution
SPATIAL_SUBSET_UL_CORNER = ( ",upleft," )
SPATIAL_SUBSET_LR_CORNER = ( ",lowright," )
RESAMPLING_TYPE =",var$method,"
OUTPUT_PROJECTION_TYPE = SIN
OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# projection parameters from http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=sn_gctp
ELLIPSOID_CODE = WGS84
OUTPUT_TYPE = HDFEOS
OUTPUT_FILENAME = ",outfile,"
END
",sep="")
  ## write it to a file
  cat(c(hdr,grp)    , file=paste(tempdir(),"/",basename(file),"_MODparms.txt",sep=""))
  ## now run the swath2grid tool
  ## write the gridded file
  system(paste(swtifpath," -p ",tempdir(),"/",basename(file),"_MODparms.txt -d  -tmpLatLondir ",tempdir(),sep=""),intern=F,ignore.stderr=F)
   print(paste("Finished processing variable",var$variable,"from ",basename(file),"to",outfile))
}

  ## vars to grid
vars=as.data.frame(matrix(c(
  "Cloud_Mask",              "CM",       "NN",    1,
  "Quality_Assurance",       "QA",       "NN",    1,
  "Solar_Zenith",            "SolZen",   "NN", 1,
  "Sensor_Zenith",           "SenZen",   "CUBIC", 1
  ),
  byrow=T,ncol=4,dimnames=list(1:4,c("variable","varid","method","band"))),stringsAsFactors=F)


############################################################################
############################################################################
### Use the HEG tool to grid all available swath data for this date-tile
for(file in swaths){
  print(paste("Starting file",basename(file)))
  ## run swtif for each band
  lapply(1:nrow(vars),function(i) swtif(file,vars[i,]))
}  #end looping over swaths


#############################################################################
## check for zero dimension in HDFs
## occasionlly swtif will output a hdf with a resolution of 0.  Not sure why, but drop them here.
CMcheck=list.files(pattern="CM_.*hdf$")  #list of files to check
CM_0=do.call(c,lapply(CMcheck, function(f) any(res(raster(f))==0)))
keep=sub("CM_","",CMcheck[!CM_0])
if(length(keep)<length(CMcheck)){writeLines(paste("Warning (Resolution of zero): ",paste(sub("CM_","",CMcheck)[!sub("CM_","",CMcheck)%in%keep],collapse=",")," from ",tile," for ",date))}
outfiles=list.files(tempdir(),full=T,pattern=paste(keep,"$",sep="",collapse="|"))
if(length(outfiles)==0) {
  print(paste("########################################   No gridded files for region exist for tile",tile," on date",date))
  q("no",status=0)
}

## confirm at least one file for this date is present.  If not, quit.
#outfiles=list.files(tempdir(),full=T,pattern=paste(basename(swaths),"$",sep="",collapse="|"))
#if(!any(file.exists(outfiles))) {
#  print(paste("########################################   No gridded files for region exist for tile",tile," on date",date))
#  q("no",status=0)
#}


plot=F
if(plot){
i=1
system(paste("gdalinfo ",outfiles[19]))
d=lapply(outfiles,function(r) raster(r))
summary(d[[6]])
}
#system(paste("scp ",outfiles[1]," adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/tmp/",sep=""))

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
  initGRASS(gisBase=gisBase,gisDbase=tf,SG=as(td,"SpatialGridDataFrame"),override=T,location="mod35",mapset="PERMANENT",home=tf,pid=Sys.getpid())
  system(paste("g.proj -c proj4=\"",projection(td),"\"",sep=""),ignore.stdout=T,ignore.stderr=T)

  ## Define region by importing one MOD11A1 raster.
  print("Import one MOD11A1 raster to define grid")
  if(platform=="pleiades") {
    execGRASS("r.in.gdal",input=td@file@name,output="modisgrid",flags=c("quiet","overwrite","o"))
    system("g.region rast=modisgrid save=roi --overwrite",ignore.stdout=F,ignore.stderr=F)
  }

if(platform=="litoria"){
  execGRASS("r.in.gdal",input=paste("NETCDF:\"/home/adamw/acrobates/adamw/projects/interp/data/modis/mod06/summary/MOD06_",tile,".nc\":CER",sep=""),
            output="modisgrid",flags=c("overwrite","o"))
  system("g.region rast=modisgrid.1 save=roi --overwrite",ignore.stdout=F,ignore.stderr=F)
}

## Identify which files to process
tfs=unique(sub("CM_|QA_|SenZen_|SolZen_","",basename(outfiles)))
#tfs=list.files(tempdir(),pattern="temp.*hdf")
nfs=length(tfs)
if(verbose) print(paste(nfs,"swaths available for processing"))

## loop through scenes and process QA flags
  for(i in 1:nfs){
    bfile=tfs[i]
     ## Read in the data from the HDFs
     ## Cloud Mask
     GDALinfo(paste("CM_",bfile,sep=""),returnStats=F,silent=T)
     execGRASS("r.in.gdal",input=paste("CM_",bfile,sep=""),
              output=paste("CM1_",i,sep=""),flags=c("overwrite","o")) ; print("")
     ## QA      ## extract first bit to keep only "useful" values of cloud mask
     execGRASS("r.in.gdal",input=paste("QA_",bfile,sep=""),
             output=paste("QA_",i,sep=""),flags=c("overwrite","o")) ; print("")
     ## Sensor Zenith      ## extract first bit to keep only "low angle" observations
     execGRASS("r.in.gdal",input=paste("SenZen_",bfile,sep=""),
             output=paste("SZ_",i,sep=""),flags=c("overwrite","o")) ; print("")
     ## Solar Zenith      ## extract first bit to keep only "low angle" observations
     execGRASS("r.in.gdal",input=paste("SolZen_",bfile,sep=""),
             output=paste("SoZ_",i,sep=""),flags=c("overwrite","o")) ; print("")
     ## produce the summaries
     system(paste("r.mapcalc <<EOF
                CM_fill_",i," =  if(isnull(CM1_",i,"),1,0)
                QA_useful_",i," =  if((QA_",i," / 2^0) % 2==1,1,0)
                SZ_low_",i," =  if(SZ_",i,"<6000,1,0)
                SoZ_low_",i," =  if(SoZ_",i,"<8500,1,0)
                CM_dayflag_",i," =  if((CM1_",i," / 2^3) % 2==1,1,0)
                CM_cloud_",i," =  if((CM1_",i," / 2^0) % 2==1,(CM1_",i," / 2^1) % 2^2,null())
                SZday_",i," = if(CM_dayflag_",i,"==1,SZ_",i,",null())
                SZnight_",i," = if(CM_dayflag_",i,"==0,SZ_",i,",null())
                CMday_",i," = if(SoZ_low_",i,"==1&SZ_low_",i,"==1&QA_useful_",i,"==1&CM_dayflag_",i,"==1,CM_cloud_",i,",null())
                CMnight_",i," = if(SZ_low_",i,"==1&QA_useful_",i,"==1&CM_dayflag_",i,"==0,CM_cloud_",i,",null())
EOF",sep=""))

#     CM_dayflag_",i," =  if((CM1_",i," / 2^3) % 2==1,1,0)
#     CM_dscore_",i," =  if((CM_dayflag_",i,"==0|isnull(CM1_",i,")),0,if(QA_useful_",i,"==0,1,if(SZ_",i,">=6000,2,if(SoZ_",i,">=8500,3,4))))
#     CM_nscore_",i," =  if((CM_dayflag_",i,"==1|isnull(CM1_",i,")),0,if(QA_useful_",i,"==0,1,if(SZ_",i,">=6000,2,4)))

     drawplot=F
     if(drawplot){
       d2=stack(
#         raster(readRAST6(paste("QA_useful_",i,sep=""))),
         raster(readRAST6(paste("CM1_",i,sep=""))),
         raster(readRAST6(paste("CM_cloud_",i,sep=""))),
         raster(readRAST6(paste("CM_dayflag_",i,sep=""))),
         raster(readRAST6(paste("CMday_",i,sep=""))),
         raster(readRAST6(paste("CMnight_",i,sep=""))),
#         raster(readRAST6(paste("CM_fill_",i,sep=""))),
#         raster(readRAST6(paste("SoZ_",i,sep=""))),
         raster(readRAST6(paste("SZ_",i,sep="")))
         )
       plot(d2,add=F)
     }
       
     
 } #end loop through sub daily files

## select lowest view angle
## use r.series to find minimum
system(paste("r.series input=",paste("SZnight_",1:nfs,sep="",collapse=",")," output=SZnight_min method=min_raster",sep=""))
system(paste("r.series input=",paste("SZday_",1:nfs,sep="",collapse=",")," output=SZday_min method=min_raster",sep=""))
## select cloud observation with lowest sensor zenith for day and night
system(
paste("r.mapcalc <<EOF
              CMday_daily=",paste(paste("if((SZday_min+1)==",1:nfs,",CMday_",1:nfs,",",sep="",collapse=" "),"null()",paste(rep(")",times=nfs),sep="",collapse="")),"
              CMnight_daily=",paste(paste("if((SZnight_min+1)==",1:nfs,",CMnight_",1:nfs,",",sep="",collapse=" "),"null()",paste(rep(")",times=nfs),sep="",collapse=""))
))

    execGRASS("r.null",map="CMday_daily",setnull="255") ; print("")
    execGRASS("r.null",map="CMnight_daily",setnull="255") ; print("")

if(plot){
  ps=1:nfs
  ps=c(10,11,13,14)
  sz1=brick(lapply(ps,function(i) raster(readRAST6(paste("SZday_",i,sep="")))))
  d=brick(lapply(ps,function(i) raster(readRAST6(paste("CMday_",i,sep="")))))
  d2=brick(list(raster(readRAST6("SZday_min")),raster(readRAST6("SZnight_min")),raster(readRAST6("CMday_daily")),raster(readRAST6("CMnight_daily"))))
  library(rasterVis)
  levelplot(sz1,col.regions=rainbow(100),at=seq(min(sz1@data@min),max(sz1@data@max),len=100))
  levelplot(d)
  levelplot(d2)
}


  ### Write the files to a netcdf file
  ## create image group to facilitate export as multiband netcdf
    execGRASS("i.group",group="mod35",input=c("CMday_daily","CMnight_daily"),flags=c("quiet")) ; print("")

if(file.exists(ncfile)) file.remove(ncfile)  #if it exists already, delete it
  execGRASS("r.out.gdal",input="mod35",output=ncfile,type="Byte",nodata=255,flags=c("verbose"),
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
## need to delete _FillValue becuase r.out.gdal incorrectly calls zero values missing if there are no other missing values in the raster.
## so need to delete then re-add.  If you just change the value, ncatted will change the values in the raster in addition to the attribute.
  system(paste(ncopath,"ncrename -v Band1,CMday -v Band2,CMnight ",ncfile,sep=""))
  system(paste(ncopath,"ncatted ",
" -a units,CMday,o,c,\"Cloud Flag (0-3)\" ",
" -a missing_value,CMday,o,b,255 ",
" -a _FillValue,CMday,d,, ", 
" -a valid_range,CMday,o,b,\"0,3\" ",
" -a long_name,CMday,o,c,\"Cloud Flag from day pixels\" ",
" -a units,CMnight,o,c,\"Cloud Flag (0-3)\" ",
" -a missing_value,CMnight,o,b,255 ",
" -a _FillValue,CMnight,d,, ",
" -a valid_range,CMnight,o,b,\"0,3\" ",
" -a long_name,CMnight,o,c,\"Cloud Flag from night pixels\" ",
ncfile,sep=""))
## add the fillvalue attribute back (without changing the actual values)
system(paste(ncopath,"ncatted -a _FillValue,CMday,o,b,255 ",ncfile,sep=""))
system(paste(ncopath,"ncatted -a _FillValue,CMnight,o,b,255 ",ncfile,sep=""))
   

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


### print some status messages
if(verbose) writeLines(paste("STATUS:end tile:",tile,"date:",date,"swathIDs:",nrow(fs)," swathsOnDisk:",length(swaths),"fileExists:",file.exists(ncfile)))

## turn off the profiler
if(opta$profile)  Rprof(NULL)


## quit
q("no",status=0)

###################################################################################
###  R code to aquire and process MOD06_L2 cloud data from the MODIS platform


# Redirect all warnings to stderr()
#options(warn = -1)
#write("2) write() to stderr", stderr())
#write("2) write() to stdout", stdout())
#warning("2) warning()")


## import commandline arguments
library(getopt)
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


## default date and tile to play with
date="20030301"
tile="h11v08"
platform="pleiades" 
verbose=T

## now update using options if given
date=opta$date  
tile=opta$tile 
verbose=opta$verbose  #print out extensive information for debugging?
outdir=paste("daily/",tile,"/",sep="")  #directory for separate daily files


if(platform="pleiades"){
  ## location of MOD06 files
  datadir="/nobackupp4/datapool/modis/MOD06_L2.005/"
  ## path to some executables
  ncopath="/nasa/sles11/nco/4.0.8/gcc/mpt/bin/"
  swtifpath="/nobackupp1/awilso10/software/heg/bin/swtif"
  ## path to swath database
  db="/nobackupp4/pvotava/DB/export/swath_geo.sql.sqlite3.db"
  ## specify working directory
  setwd("/nobackupp1/awilso10/mod06")
  gisBase="/u/armichae/pr/grass-6.4.2/"
  ## path to MOD11A1 file for this tile to align grid/extent
  gridfile=list.files("/nobackupp4/datapool/modis/MOD11A1.005/2006.01.27",pattern=paste(tile,".*[.]hdf$",sep=""),recursive=T,full=T)[1]
  td=readGDAL(paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_Daily_1km_LST:Night_view_angl",sep=""))
  projection(td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs +datum=WGS84 +ellps=WGS84 "
}

if(platform="litoria"){  #if running on local server, use different paths
  ## location of MOD06 files
  datadir="/nobackupp4/datapool/modis/MOD06_L2.005/"
  ## path to some executables
  ncopath="/nasa/sles11/nco/4.0.8/gcc/mpt/bin/"
  swtifpath="sudo MRTDATADIR=\"/usr/local/heg/data\" PGSHOME=/usr/local/heg/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/bin/swtif"
  ## path to swath database
  db="/nobackupp4/pvotava/DB/export/swath_geo.sql.sqlite3.db"
  ## specify working directory
  setwd("/nobackupp1/awilso10/mod06")
  gisBase="/usr/lib/grass64"
  ## get grid file
  td=raster(paste("~/acrobates/projects/interp/data/modis/mod06/summary/MOD06_",tile,".nc",sep=""),varname="CER")
  projection(td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs +datum=WGS84 +ellps=WGS84 "
}


### print some status messages
if(verbose) print(paste("Processing tile",tile," for date",date))

## load libraries
require(reshape)
require(geosphere)
require(raster)
require(rgdal)
require(spgrass6)
require(RSQLite)


## load tile information and get bounding box
load(file="modlandTiles.Rdata")
tile_bb=tb[tb$tile==tile,] ## identify tile of interest
upleft=paste(tile_bb$lat_max,tile_bb$lon_min) #northwest corner
lowright=paste(tile_bb$lat_min,tile_bb$lon_max) #southeast corner


## vars to process
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

## vector of variables expected to be in final netcdf file.  If these are not present, the file will be deleted at the end.
finalvars=c("CER","COT","CLD")


#####################################################
##find swaths in region from sqlite database for the specified date/tile
if(verbose) print("Accessing swath ID's from database")
con=dbConnect("SQLite", dbname = db)
fs=dbGetQuery(con,paste("SELECT * from swath_geo
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
OBJECT_NAME=mod06
FIELD_NAME=",vars$variable,"|
BAND_NUMBER = 1
OUTPUT_PIXEL_SIZE_X=1000
OUTPUT_PIXEL_SIZE_Y=1000
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
  log=system(paste("(",swtifpath," -p ",tempdir(),"/",basename(file),"_MODparms.txt -d ; echo $$)",sep=""),intern=T,ignore.stderr=T)
  ## clean up temporary files in working directory
#  file.remove(list.files(pattern=
#              paste("filetable.temp_",
#              as.numeric(log[length(log)]):(as.numeric(log[length(log)])+3),sep="",collapse="|")))  #Look for files with PID within 3 of parent process
  if(verbose) print(log)
  print(paste("Finished gridding ", file))
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
  
## Identify output file
  ncfile=paste(outdir,"/MOD06_",tile,"_",date,".nc",sep="")  #this is the 'final' daily output file

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
  if(!file.exists(dirname(ncfile))) dir.create(dirname(ncfile))
  
  ## set up temporary grass instance for this PID
  if(verbose) print(paste("Set up temporary grass session in",tf))
  initGRASS(gisBase=gisBase,gisDbase=tf,SG=td,override=T,location="mod06",mapset="PERMANENT",home=tf,pid=Sys.getpid())
  system(paste("g.proj -c proj4=\"",projection(td),"\"",sep=""),ignore.stdout=T,ignore.stderr=T)

  ## Define region by importing one MOD11A1 raster.
  print("Import one MOD11A1 raster to define grid")
  if(platform=="pleiades") execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_Daily_1km_LST:Night_view_angl",sep=""),
            output="modisgrid",flags=c("quiet","overwrite","o"))
   if(platform=="litoria") execGRASS("r.in.gdal",input=paste("NETCDF:\"/home/adamw/acrobates/projects/interp/data/modis/mod06/summary/MOD06_",tile,".nc\":CER",sep=""),output="modisgrid",flags=c("quiet","overwrite","o"))
  
system("g.region rast=modisgrid save=roi --overwrite",ignore.stdout=T,ignore.stderr=T)

## Identify which files to process
tfs=basename(swaths)
## drop swaths that did not produce an output file (typically due to not overlapping the ROI)
tfs=tfs[tfs%in%list.files(tempdir())]
nfs=length(tfs)
if(verbose) print(nfs,"swaths available for processing")

## loop through scenes and process QA flags
  for(i in 1:nfs){
     file=paste(tempdir(),"/",tfs[i],sep="")
     ## Cloud Mask
     execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Mask_1km_0",sep=""),
              output=paste("CM1_",i,sep=""),flags=c("overwrite","o")) ; print("")
    ## extract cloudy and 'probably/confidently clear' pixels
    system(paste("r.mapcalc <<EOF
                CM_cloud_",i," =  ((CM1_",i," / 2^0) % 2) == 1  &&  ((CM1_",i," / 2^1) % 2^2) == 0 
                CM_clear_",i," =  ((CM1_",i," / 2^0) % 2) == 1  &&  ((CM1_",i," / 2^1) % 2^2) >  2 
EOF",sep=""))

     ## QA
     execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Quality_Assurance_1km_0",sep=""),
             output=paste("QA_",i,sep=""),flags=c("overwrite","o")) ; print("")
   ## QA_CER
   system(paste("r.mapcalc <<EOF
                 QA_COT_",i,"=   ((QA_",i," / 2^0) % 2^1 )==1
                 QA_COT2_",i,"=  ((QA_",i," / 2^1) % 2^2 )>=2
                 QA_COT3_",i,"=  ((QA_",i," / 2^3) % 2^2 )==0
                 QA_CER_",i,"=   ((QA_",i," / 2^5) % 2^1 )==1
                 QA_CER2_",i,"=  ((QA_",i," / 2^6) % 2^2 )>=2
EOF",sep="")) 
#                 QA_CWP_",i,"=   ((QA_",i," / 2^8) % 2^1 )==1
#                 QA_CWP2_",i,"=  ((QA_",i," / 2^9) % 2^2 )==3

   ## Optical Thickness
   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Optical_Thickness",sep=""),
            output=paste("COT_",i,sep=""),
            title="cloud_effective_radius",
            flags=c("overwrite","o")) ; print("")
   execGRASS("r.null",map=paste("COT_",i,sep=""),setnull="-9999")
   ## keep only positive COT values where quality is 'useful' and '>= good' & scale to real units
   system(paste("r.mapcalc \"COT_",i,"=if(QA_COT_",i,"&&QA_COT2_",i,"&&QA_COT3_",i,"&&COT_",i,">=0,COT_",i,",null())\"",sep=""))   
   ## set COT to 0 in clear-sky pixels
   system(paste("r.mapcalc \"COT2_",i,"=if(CM_clear_",i,"==0,COT_",i,",0)\"",sep=""))   
   
   ## Effective radius ##
   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Effective_Radius",sep=""),
            output=paste("CER_",i,sep=""),
            title="cloud_effective_radius",
            flags=c("overwrite","o")) ; print("")
   execGRASS("r.null",map=paste("CER_",i,sep=""),setnull="-9999")
   ## keep only positive CER values where quality is 'useful' and '>= good' & scale to real units
   system(paste("r.mapcalc \"CER_",i,"=if(QA_CER_",i,"&&QA_CER2_",i,"&&CER_",i,">=0,CER_",i,",null())\"",sep=""))   
   ## set CER to 0 in clear-sky pixels
   system(paste("r.mapcalc \"CER2_",i,"=if(CM_clear_",i,"==0,CER_",i,",0)\"",sep=""))   

   ## Cloud Water Path
#   execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",file,"\":mod06:Cloud_Water_Path",sep=""),
#            output=paste("CWP_",i,sep=""),title="cloud_water_path",
#            flags=c("overwrite","o")) ; print("")
#   execGRASS("r.null",map=paste("CWP_",i,sep=""),setnull="-9999")
   ## keep only positive CER values where quality is 'useful' and 'very good' & scale to real units
#   system(paste("r.mapcalc \"CWP_",i,"=if(QA_CWP_",i,"&&QA_CWP2_",i,"&&CWP_",i,">=0,CWP_",i,",null())\"",sep=""))   
   ## set CER to 0 in clear-sky pixels
#   system(paste("r.mapcalc \"CWP2_",i,"=if(CM_clear_",i,"==0,CWP_",i,",0)\"",sep=""))   
     
 } #end loop through sub daily files

#### Now generate daily averages (or maximum in case of cloud flag)
  
  system(paste("r.mapcalc <<EOF
         COT_denom=",paste("!isnull(COT2_",1:nfs,")",sep="",collapse="+"),"
         COT_numer=",paste("if(isnull(COT2_",1:nfs,"),0,COT2_",1:nfs,")",sep="",collapse="+"),"
         COT_daily=int(COT_numer/COT_denom)
         CER_denom=",paste("!isnull(CER2_",1:nfs,")",sep="",collapse="+"),"
         CER_numer=",paste("if(isnull(CER2_",1:nfs,"),0,CER2_",1:nfs,")",sep="",collapse="+"),"
         CER_daily=int(CER_numer/CER_denom)
         CLD_daily=int((max(",paste("if(isnull(CM_cloud_",1:nfs,"),0,CM_cloud_",1:nfs,")",sep="",collapse=","),"))*100) 
EOF",sep=""))


  ### Write the files to a netcdf file
  ## create image group to facilitate export as multiband netcdf
    execGRASS("i.group",group="mod06",input=c("CER_daily","COT_daily","CLD_daily")) ; print("")
   
  if(file.exists(ncfile)) file.remove(ncfile)  #if it exists already, delete it
  execGRASS("r.out.gdal",input="mod06",output=ncfile,type="Int16",nodata=-32768,flags=c("quiet"),
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
  system(paste(ncopath,"ncrename -v Band1,CER -v Band2,COT -v Band3,CLD ",ncfile,sep=""))
  system(paste(ncopath,"ncatted -a scale_factor,CER,o,d,0.01 -a units,CER,o,c,\"micron\" -a missing_value,CER,o,d,-32768 -a long_name,CER,o,c,\"Cloud Particle Effective Radius\" ",ncfile,sep=""))
  system(paste(ncopath,"ncatted -a scale_factor,COT,o,d,0.01 -a units,COT,o,c,\"none\" -a missing_value,COT,o,d,-32768 -a long_name,COT,o,c,\"Cloud Optical Thickness\" ",ncfile,sep=""))
  system(paste(ncopath,"ncatted -a scale_factor,CLD,o,d,0.01 -a units,CLD,o,c,\"none\" -a missing_value,CLD,o,d,-32768 -a long_name,CLD,o,c,\"Cloud Mask\" ",ncfile,sep=""))
   
  
### delete the temporary files 
  unlink_.gislock()
  system(paste("rm -frR ",tf,sep=""))


## Confirm that the file has the correct attributes, otherwise delete it
ntime=as.numeric(system(paste("cdo -s ntime ",ncfile),intern=T))
## confirm it has all 'final variables as specified above"
fvar=all(finalvars%in%strsplit(system(paste("cdo -s showvar ",ncfile),intern=T)," ")[[1]])

  if(ntime!=1|!fvar) {
      print(paste("FILE ERROR:  tile ",tile," and date ",date," was not outputted correctly, deleting... "))
      file.remove(ncfile)
    }
    
  ## print out some info
print(paste(" ###################################################################               Finished ",date,
"################################################################"))

## delete old files
system("cleartemp")

## quit
q("no",status=0)

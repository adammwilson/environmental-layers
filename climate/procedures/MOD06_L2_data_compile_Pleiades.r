###################################################################################
###  R code to aquire and process MOD06_L2 cloud data from the MODIS platform


##First read in the arguments listed at the command line
args=(commandArgs(TRUE))
##args is now a list of character vectors
## First check to see if arguments are passed.
## if no args were given, print a warning and stop
if(length(args)==0) {stop("No parameters supplied, you must pass parameters")}

## Then cycle through each element of the list and evaluate the expressions.
eval(parse(text=args))
## now there is an i that corresponds to the row in the notdone object that will be processed.

## load libraries
require(sp)
require(rgdal)
require(reshape)
require(ncdf4)
require(geosphere)
require(raster)
require(spgrass6)

## specify some working directories
setwd("/nobackupp1/awilso10/mod06")
tempdir=tempdir()  # to hold intermediate files
outdir="2_daily"   # final daily product
tile="h11v08"   #can move this to submit script if needed
## read in list of all files
load("allfiles.Rdata")
load("notdone.Rdata")
load("vars.Rdata")

date=notdone[i]
#file=fs$path[fs$dateid==date][1]

## print out some info
print(paste("Processing date ",date," for tile",tile))

## identify tile of interest
tile_bb=tb[tb$tile==tile,]

#### Function to generate hegtool parameter file for multi-band HDF-EOS file
swath2grid=function(file,vars,upleft,lowright){
  print(paste("Starting file",basename(file)))
  outfile=paste(tempdir(),"/",basename(file),sep="")
### First write the parameter file (careful, heg is very finicky!)
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
#UTM_ZONE = 10
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
  ## write the tiff file
  log=system(paste("/nobackupp4/pvotava/software/heg/bin/swtif -p ",paste(tempdir(),"/",basename(file),"_MODparms.txt -d",sep=""),sep=""),intern=T)
  ## clean up temporary files in working directory
#  file.remove(paste("filetable.temp_",pid,sep=""))
  print(log)
  print(paste("Finished ", file))
}

#### Run the gridding procedure
lapply(fs$path[fs$dateid==date],swath2grid,vars=vars,upleft=paste(tile_bb$lat_max,tile_bb$lon_min),lowright=paste(tile_bb$lat_min,tile_bb$lon_max))
#upleft=paste(roi_bb[2,2],roi_bb[1,1]),lowright=paste(roi_bb[2,1],roi_bb[1,2]))


##############################################################
### Import to GRASS for processing

#fs$grass=paste(fs$month,fs$year,fs$file,sep="_")
#td=readGDAL(paste("HDF4_EOS:EOS_GRID:\"",outdir,"/",fs$file[1],"\":mod06:Cloud_Mask_1km_0",sep=""))

gridfile=list.files("/nobackupp4/datapool/modis/MOD11A1.005/2006.01.27/",pattern=paste(tile,".*hdf$",sep=""),full=T)
td=readGDAL(paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_Daily_1km_LST:Night_view_angl",sep=""))
projection(td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs +datum=WGS84 +ellps=WGS84 "

## fucntion to convert binary to decimal to assist in identifying correct values
b2d=function(x) sum(x * 2^(rev(seq_along(x)) - 1)) #http://tolstoy.newcastle.edu.au/R/e2/help/07/02/10596.html
## for example:
b2d(c(T,T))

### create (or connect to) grass location
gisLocation="tmp"
gisMapset="mod06"
## set Grass to overwrite
Sys.setenv(GRASS_OVERWRITE=1)
Sys.setenv(DEBUG=1)
Sys.setenv(GRASS_GUI="txt")

## temporary objects to test function below
# i=1
#file=paste(outdir,"/",fs$file[1],sep="")
#date=as.Date("2000-05-23")

#.GRASS_CACHE=spgrass6:::.GRASS_CACHE

### Function to extract various SDSs from a single gridded HDF file and use QA data to throw out 'bad' observations
loadcloud<-function(date,fs){
### set up unique grass session
  tf=paste(tempdir(),"/grass", Sys.getpid(),"/", sep="")
 
  ## set up temporary grass instance for this PID
  initGRASS(gisBase="/nobackupp1/awilso10/software/grass-6.4.3svn",gisDbase=tf,SG=td,override=T,location="mod06",mapset="PERMANENT",home=tf,pid=Sys.getpid())
  system(paste("g.proj -c proj4=\"",projection(td),"\"",sep=""))

  ## Define region by importing one MOD11A1 raster.
  execGRASS("r.in.gdal",input=paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_Daily_1km_LST:Night_view_angl",sep=""),
            output="modisgrid",flags=c("quiet","overwrite","o"))
  system("g.region rast=modisgrid save=roi --overwrite")

  ## Identify which files to process
  tfs=fs$file[fs$dateid==date]
  nfs=length(tfs)

  ## loop through scenes and process QA flags
  for(i in 1:nfs){
     file=paste(tempdir,"/",tfs[i],sep="")
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
  execGRASS("r.out.gdal",input="CER_daily",output=paste(outdir,"/CER_",date,".tif",sep=""),nodata=-999,flags=c("quiet"))
  execGRASS("r.out.gdal",input="COT_daily",output=paste(outdir,"/COT_",date,".tif",sep=""),nodata=-999,flags=c("quiet"))
  execGRASS("r.out.gdal",input="CLD_daily",output=paste(outdir,"/CLD_",date,".tif",sep=""),nodata=99,flags=c("quiet"))

### delete the temporary files 
  unlink_.gislock()
  system("/nobackupp1/awilso10/software/grass-6.4.3svn/etc/clean_temp")
  system(paste("rm -R ",tf,sep=""))
### print update
}


###########################################
### Now run it

lapply(date,function(date) loadcloud(date,fs=fs))



  print(paste(" ###################################################################               Finished ",date,"
################################################################"))


## quit R
q("no")
 


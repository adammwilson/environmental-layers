### Process a folder of daily MOD35 HDF files to produce a climatology

## import commandline arguments
library(getopt)
## get options
opta <- getopt(matrix(c(
                        'tile', 't', 1, 'character',
                        'verbose','v',1,'logical'
                        ), ncol=4, byrow=TRUE))

tile=opta$tile #tile="h11v08"
verbose=opta$verbose  #print out extensive information for debugging?

### directory containing daily files
outdir=paste("daily/",tile,"/",sep="")  #directory for separate daily files

### directory to hold climatology
outdir2="summary" #directory for combined daily files and summarized files
if(!file.exists(outdir2)) dir.create(outdir2)

### path to NCO
ncopath="/nasa/sles11/nco/4.0.8/gcc/mpt/bin/"

### Vector of variables that must be in file or they will be deleted.
###  Formated as output from system(paste("cdo -s showvar ",fdly$path[i]),intern=T)
#finalvars=" CER COT CLD"


################################################################################
## Get list of all daily files
if(verbose) print("Checking daily output in preparation for generating climatology")

 fdly=data.frame(path=list.files(outdir,pattern="nc$",full=T),stringsAsFactors=F)
  fdly$file=basename(fdly$path)
  fdly$dateid=substr(fdly$file,14,21)
  fdly$date=as.Date(substr(fdly$file,14,21),"%Y%m%d")
  fdly$month=format(fdly$date,"%m")
  fdly$year=format(fdly$date,"%Y")
nrow(fdly)

## check validity (via npar and ntime) of nc files
#for(i in 1:nrow(fdly)){
#  fdly$ntime[i]<-as.numeric(system(paste("cdo -s ntime ",fdly$path[i]),intern=T))
#  fdly$npar[i]<-as.numeric(system(paste("cdo -s npar ",fdly$path[i]),intern=T))
#  fdly$fyear[i]<-as.numeric(system(paste("cdo -s showyear ",fdly$path[i]),intern=T))
#  fdly$fmonth[i]<-as.numeric(system(paste("cdo -s showmon ",fdly$path[i]),intern=T))
#  fdly$fvar[i]<-system(paste("cdo -s showvar ",fdly$path[i]),intern=T)
#  print(paste(i," out of ",nrow(fdly)," for year ",  fdly$fyear[i]))
#}

## print some summaries
if(verbose) print("Summary of available daily files")
print(table(fdly$year))
print(table(fdly$month))
#print(table(fdly$fvar))

## Identify which files failed test
#fdly$drop=is.na(fdly$npar)|fdly$fvar!=finalvars

## delete files that fail check?
delete=F
if(delete) {
  print(paste(sum(fdly$drop),"files will be deleted"))
  file.remove(as.character(fdly$path[fdly$drop]))
}
## remove dropped files from list
#fdly=fdly[!fdly$drop,]

#################################################################################
## Combine the year-by-year files into a single daily file in the summary directory (for archiving)
if(verbose) print("Merging daily files into single file output")

## create temporary directory to put intermediate files (will be deleted when R quits)
tsdir=paste(tempdir(),"/summary",sep="")
if(!file.exists(tsdir)) dir.create(tsdir,recursive=T)

## merge all daily files to create a single file with all dates
system(paste(ncopath,"ncrcat -O ",outdir,"/*nc ",outdir2,"/MOD35_",tile,"_daily.nc",sep=""))
 
## Update attributes
system(paste(ncopath,"ncatted ",
" -a units,time,o,c,\"days since 2000-1-1 0:0:0\" ",
" -a title,global,o,c,\"MODIS Cloud Product (MOD35) Daily Timeseries\" ",
" -a institution,global,o,c,\"Yale University\" ",
" -a source,global,o,c,\"MODIS Cloud Mask (MOD35)\" ",
" -a comment,global,o,c,\"Compiled by Adam M. Wilson (adam.wilson@yale.edu)\" ",
outdir2,"/MOD35_",tile,"_daily.nc",sep=""))

### produce a monthly timeseries?
#system(paste("cdo -O monmean ",outdir2,"/MOD35_",tile,"_daily.nc ",tsdir,"/MOD35_",tile,"_monmean.nc",sep=""))

#############################
##  Generate the Climatologies
if(verbose) print("Generate monthly climatologies")

myear=as.integer(max(fdly$year))  #this year will be used in all dates of monthly climatologies (and day will = 15)

## Monthly means
if(verbose) print("Calculating the monthly means")
system(paste("cdo -O sorttimestamp -setyear,",myear," -setday,15 -mulc,-1 -subc,100 -ymonmean ",outdir2,"/MOD35_",tile,"_daily.nc ",tsdir,"/MOD35_",tile,"_ymonmean.nc",sep=""),wait=T)
system(paste(ncopath,"ncrename -v PClear,PCloud ",tsdir,"/MOD35_",tile,"_ymonmean.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,PCloud,o,c,\"Mean Probability of Cloud\" ",
tsdir,"/MOD35_",tile,"_ymonmean.nc",sep=""))

## Monthly standard deviation
if(verbose) print("Calculating the monthly SD")
system(paste("cdo -O sorttimestamp -setyear,",myear," -setday,15 -ymonstd  ",outdir2,"/MOD35_",tile,"_daily.nc ",tsdir,"/MOD35_",tile,"_ymonstd.nc",sep=""))
system(paste(ncopath,"ncrename -v PClear,PCloud_sd ",tsdir,"/MOD35_",tile,"_ymonstd.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,PCloud_sd,o,c,\"Standard Deviation of p(cloud)\" ",
tsdir,"/MOD35_",tile,"_ymonstd.nc",sep=""))

## frequency of cloud days p(clear<90%)  
if(verbose) print("Calculating the proportion of cloudy and probably cloudy days")
system(paste("cdo -O  sorttimestamp -setyear,",myear," -setday,15 -ymonmean  -mulc,100 -lec,90 -selvar,PClear ",outdir2,"/MOD35_",tile,"_daily.nc ",tsdir,"/MOD35_",tile,"_ymoncld01.nc",sep=""))
system(paste(ncopath,"ncrename -v PClear,CF ",tsdir,"/MOD35_",tile,"_ymoncld01.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CF,o,c,\"Cloud Frequency: Proportion of Days with probability of clear < 90%\" ",
" -a units,CF,o,c,\"Proportion (%)\" ",
tsdir,"/MOD35_",tile,"_ymoncld01.nc",sep=""))

## number of observations
if(verbose) print("Calculating the number of missing variables")
system(paste("cdo -O sorttimestamp  -setyear,",myear," -setday,15 -nint -ymonmean -mulc,100  -eqc,9999 -setmisstoc,9999   -selvar,CLD ",outdir2,"/MOD35_",tile,"_daily.nc ",tsdir,"/MOD35_",tile,"_ymonmiss.nc",sep=""))
system(paste(ncopath,"ncrename -v PClear,CF_pmiss ",tsdir,"/MOD35_",tile,"_ymonmiss.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CF_pmiss,o,c,\"Proportion of Days with missing data for CF\" ",
" -a units,CF_pmiss,o,c,\"Proportion (%)\" ",
tsdir,"/MOD35_",tile,"_ymonmiss.nc",sep=""))

## TODO: fix projection information so GDAL can read it correctly.
## clean up variables?

## append variables to a single file
if(verbose) print("Append all monthly climatologies into a single file")
system(paste(ncopath,"ncks -O ",tsdir,"/MOD35_",tile,"_ymonmean.nc  ",tsdir,"/MOD35_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD35_",tile,"_ymonstd.nc  ",tsdir,"/MOD35_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD35_",tile,"_ymoncld01.nc  ",tsdir,"/MOD35_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD35_",tile,"_ymonmiss.nc  ",tsdir,"/MOD35_",tile,"_ymon.nc",sep=""))

## append sinusoidal grid from one of input files as CDO doesn't transfer all attributes
if(verbose) print("Clean up file (update attributes, flip latitudes, add grid description")

## invert latitude so it plays nicely with gdal
system(paste(ncopath,"ncpdq -O -a -y ",tsdir,"/MOD35_",tile,"_ymon.nc ",outdir2,"/MOD35_",tile,".nc",sep=""))

## proj string taken from GDAL-written MODIS tile 
projstring="PROJCS[\"Sinusoidal (Sanson-Flamsteed)\",GEOGCS[\"wgs84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS_1984\",6378137,298.257223563],TOWGS84[0,0,0,0,0,0,0]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]],PROJECTION[\"Sinusoidal\"],PARAMETER[\"longitude_of_center\",0],PARAMETER[\"false_easting\",0],PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]"

## update attributes
system(paste(ncopath,"ncatted ",
" -a false_easting,sinusoidal,o,d,0. ",
" -a false_northing,sinusoidal,o,d,0. ",
" -a longitude_of_central_meridian,sinusoidal,o,d,0. ",
" -a longitude_of_prime_meridian,sinusoidal,o,d,0. ",
" -a semi_major_axis,sinusoidal,o,d,6378137. ",
" -a inverse_flattening,sinusoidal,o,d,298.257223563 ",
" -a spatial_ref,sinusoidal,o,c,",projstring,
" -a GeoTransform,sinusoidal,o,c,\"-7783653.638366 926.6254331391661 0 1111950.519767 0 -926.6254331391667\" ",
" -a units,time,o,c,\"days since 2000-1-1 0:0:0\" ",
" -a title,global,o,c,\"MODIS Cloud Product (MOD35) Climatology\" ",
" -a tile,global,o,c,\"",tile,"\" ",
" -a institution,global,o,c,\"Yale University\" ",
" -a source,global,o,c,\"MODIS Cloud Product (MOD35) Collection 6\" ",
" -a comment,global,o,c,\"Compiled by Adam M. Wilson (adam.wilson@yale.edu)\" ",
outdir2,"/MOD35_",tile,".nc",sep=""))


print(paste("###############################  Processed ",nrow(fdly),"days for tile:",tile," ###################################################"))
print("Years:")
print(table(fdly$fyear))
print("Months:")
print(table(fdly$fmonth))
 
## quit R
q("no")
 

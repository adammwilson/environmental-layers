### Process a folder of daily MOD35 HDF files to produce a climatology

.libPaths("/pleiades/u/awilso10/R/x86_64-unknown-linux-gnu-library/2.15/")

## import commandline arguments
library(getopt,lib="/pleiades/u/awilso10/R/x86_64-unknown-linux-gnu-library/2.15/")
## get options
opta <- getopt(matrix(c(
                        'tile', 't', 1, 'character',
                        'verbose','v',1,'logical'
                        ), ncol=4, byrow=TRUE))

tile=opta$tile #tile="h00v08"
verbose=opta$verbose  #print out extensive information for debugging?

## set working directory
setwd("/nobackupp1/awilso10/mod35")
#setwd("/u/awilso10/MOD35")

### directory containing daily files
outdir=paste("daily/",tile,"/",sep="")  #directory for separate daily files

### directory to hold climatology
outdir2="summary" #directory for combined daily files and summarized files
if(!file.exists(outdir2)) dir.create(outdir2)

### path to NCO
ncopath="/nasa/sles11/nco/4.0.8/gcc/mpt/bin/"

################################################################################
## Get list of all daily files
if(verbose) print(paste("Checking daily output in preparation for generating climatology:",tile))

 fdly=data.frame(path=list.files(outdir,pattern="nc$",full=T),stringsAsFactors=F)
  fdly$file=basename(fdly$path)
  fdly$dateid=substr(fdly$file,14,21)
  fdly$date=as.Date(substr(fdly$file,14,21),"%Y%m%d") #23,30 for alltiles
  fdly$month=format(fdly$date,"%m")
  fdly$year=format(fdly$date,"%Y")
nrow(fdly)

## print some summaries
if(verbose) print(paste("Summary of available daily files:",tile))
print(table(fdly$year))
print(table(fdly$month))
#print(table(fdly$fvar))

#################################################################################
## Combine the year-by-year files into a single daily file in the summary directory (for archiving)
if(verbose) print(paste("Merging daily files into single file output:",tile))

## create temporary directory to put intermediate files (will be deleted when R quits)
tsdir=paste(tempdir(),"/summary",sep="")
if(!file.exists(tsdir)) dir.create(tsdir,recursive=T)

## merge all daily files to create a single file with all dates
system(paste(ncopath,"ncrcat -O ",outdir,"/*nc ",outdir2,"/MOD35_",tile,"_daily.nc",sep=""))
#system(paste(ncopath,"ncrcat -O ",outdir,"/*alltests*nc ",outdir2,"/MOD35_",tile,"_daily_alltiles.nc",sep=""))
system(paste("ncdump -h ",outdir2,"/MOD35_",tile,"_daily.nc",sep=""))
 
## Update attributes
system(paste(ncopath,"ncatted ",
" -a title,global,o,c,\"MODIS Cloud Product (MOD35) Summaries\" ",
" -a institution,global,o,c,\"Yale University\" ",
" -a source,global,o,c,\"MODIS Collection 6 Cloud Mask (MOD35)\" ",
" -a comment,global,o,c,\"Compiled by Adam M. Wilson (adam.wilson@yale.edu)\" ",
outdir2,"/MOD35_",tile,"_daily.nc",sep=""))

## report on daily file:
#ncfile=paste(outdir2,"/MOD35_",tile,"_daily.nc",sep="")
system(paste("ncdump -h ",outdir2,"/MOD35_",tile,"_daily.nc | head -20 ",sep=""))
system(paste("cdo showyear ",outdir2,"/MOD35_",tile,"_daily.nc",sep=""))
system(paste("cdo showmon ",outdir2,"/MOD35_",tile,"_daily.nc",sep=""))
#system(paste("cdo showdate ",outdir2,"/MOD35_",tile,"_daily.nc",sep=""))


### produce a monthly timeseries?
#system(paste("cdo -O monmean ",outdir2,"/MOD35_",tile,"_daily.nc ",tsdir,"/MOD35_",tile,"_monmean.nc",sep=""))

#############################
##  Generate the Climatologies
if(verbose) print(paste("Generate monthly climatologies: ",tile))

myear=as.integer(max(fdly$year))  #this year will be used in all dates of monthly climatologies (and day will = 15)

## Overall Means
if(verbose) print(paste("Calculating the overall mean:",tile))
system(paste("cdo -O -b I8 -v sorttimestamp -setyear,",myear," -setmon,1 -setday,1 -mulc,100 -timmean -lec,1 ",outdir2,"/MOD35_",tile,"_daily.nc ",outdir2,"/MOD35_",tile,"_mean.nc",sep=""),wait=T)
system(paste(ncopath,"ncrename -v CMday,CFday -v CMnight,CFnight ",outdir2,"/MOD35_",tile,"_mean.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CFday,o,c,\"Daytime Cloud Frequency\" ",
" -a missing_value,CFday,o,b,255 ",
" -a _FillValue,CFday,d,b,255 ",
" -a long_name,CFnight,o,c,\"Nighttime Cloud Frequency\" ",
" -a missing_value,CFnight,o,b,255 ",
" -a _FillValue,CFnight,d,b,255 ",
outdir2,"/MOD35_",tile,"_mean.nc",sep=""))

## Monthly means
if(verbose) print(paste("Calculating the monthly means:",tile))
system(paste("cdo -O -b I8 sorttimestamp -setyear,",myear," -setday,15 -mulc,100  -ymonmean -lec,1 ",outdir2,"/MOD35_",tile,"_daily.nc ",tsdir,"/MOD35_",tile,"_ymonmean.nc",sep=""),wait=T)
system(paste(ncopath,"ncrename -v CMday,CFday -v CMnight,CFnight ",tsdir,"/MOD35_",tile,"_ymonmean.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CFday,o,c,\"Daytime Cloud Frequency\" ",
" -a units,CFday,o,c,\"Proportion (%)\" ",
" -a missing_value,CFday,o,b,255 ",
" -a _FillValue,CFday,d,b,255 ",
" -a long_name,CFnight,o,c,\"Nighttime Cloud Frequency\" ",
" -a units,CFnight,o,c,\"Proportion (%)\" ",
" -a missing_value,CFnight,o,b,255 ",
" -a _FillValue,CFnight,d,b,255 ",
tsdir,"/MOD35_",tile,"_ymonmean.nc",sep=""))


## Monthly Mean
#months=c("01","02","03","04","05","06","07","08","09","10","11","12")
#  month="02"

#system(paste("cdo -O sorttimestamp -setyear,",myear," -mulc,-1 -subc,100 -ydrunmean,30 ",outdir2,"/MOD35_",tile,"_daily.nc ",outdir2,"/MOD35_",tile,"_ydrunmean30.nc &",sep=""),wait=T)
#system(paste("scp summary/MOD35_",tile,".nc adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod35/",sep=""))
#system(paste("ncdump -h ",tsdir,"/MOD35_",tile,"_ymonmean.nc ",sep=""))

## Monthly standard deviation
if(verbose) print(paste("Calculating the monthly SD:",tile))
system(paste("cdo -O -b I8 sorttimestamp -setyear,",myear," -setday,15 -ymonstd -mulc,100 -monmean -lec,1 ",
    outdir2,"/MOD35_",tile,"_daily.nc ",
    tsdir,"/MOD35_",tile,"_ymonstd.nc",sep=""))
system(paste(ncopath,"ncrename -v CMday,CFday_sd -v CMnight,CFnight_sd ",tsdir,"/MOD35_",tile,"_ymonstd.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CFnight_sd,o,c,\"Standard Deviation of monthly nighttime cloud frequency\" ",
" -a long_name,CFday_sd,o,c,\"Standard Deviation of monthly daytime cloud frequency\" ",
tsdir,"/MOD35_",tile,"_ymonstd.nc",sep=""))

## frequency of cloud days p(clear<90%)  
#if(verbose) print(paste("Calculating the proportion of cloudy and probably cloudy days:",tile))
#system(paste("cdo -O -b I8 sorttimestamp -setyear,",myear," -setday,15 -ymonmean  -mulc,100 -lec,90 -selvar,PClear ",outdir2,"/MOD35_",tile,"_daily.nc ",tsdir,"/MOD35_",tile,"_ymoncld01.nc",sep=""))
#system(paste(ncopath,"ncrename -v PClear,CF ",tsdir,"/MOD35_",tile,"_ymoncld01.nc",sep=""))
#system(paste(ncopath,"ncatted ",
#" -a long_name,CF,o,c,\"Cloud Frequency: Proportion of Days with probability of clear < 90%\" ",
#" -a units,CF,o,c,\"Proportion (%)\" ",
#tsdir,"/MOD35_",tile,"_ymoncld01.nc",sep=""))

## number of observations
if(verbose) print(paste("Calculating the number of missing variables:",tile))
system(paste("cdo -O -b I8 sorttimestamp  -setyear,",myear," -setday,15 -ymonmean -mulc,100  -eqc,9999 -setmisstoc,9999 ",outdir2,"/MOD35_",tile,"_daily.nc ",tsdir,"/MOD35_",tile,"_ymonmiss.nc",sep=""))
system(paste(ncopath,"ncrename -v CMday,CFday_pmiss -v CMnight,CFnight_pmiss ",tsdir,"/MOD35_",tile,"_ymonmiss.nc",sep=""))
system(paste(ncopath,"ncatted ",
             " -a long_name,CFday_pmiss,o,c,\"Proportion of Days with missing data\" ",
             " -a units,CFday_pmiss,o,c,\"Proportion (%)\" ",
             " -a long_name,CFnight_pmiss,o,c,\"Proportion of Days with missing data\" ",
             " -a units,CFnight_pmiss,o,c,\"Proportion (%)\" ",
             tsdir,"/MOD35_",tile,"_ymonmiss.nc",sep=""))

## TODO: fix projection information so GDAL can read it correctly.
## clean up variables?

## append variables to a single file
if(verbose) print(paste("Append all monthly climatologies into a single file:",tile))
system(paste(ncopath,"ncks -O ",tsdir,"/MOD35_",tile,"_ymonmean.nc  ",tsdir,"/MOD35_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD35_",tile,"_ymonstd.nc  ",tsdir,"/MOD35_",tile,"_ymon.nc",sep=""))
#system(paste(ncopath,"ncks -A ",tsdir,"/MOD35_",tile,"_ymoncld01.nc  ",tsdir,"/MOD35_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD35_",tile,"_ymonmiss.nc  ",tsdir,"/MOD35_",tile,"_ymon.nc",sep=""))

## append sinusoidal grid from one of input files as CDO doesn't transfer all attributes
if(verbose) print(paste("Clean up file (update attributes, flip latitudes, add grid description:",tile))

#system(paste(ncopath,"ncea -d time,0,1 -v sinusoidal ",list.files(outdir,full=T,pattern="[.]nc$")[1],"  ",tsdir,"/sinusoidal.nc",sep=""))
#system(paste(ncopath,"ncks -A -d time,0,1 -v sinusoidal ",list.files(outdir,full=T,pattern="[.]nc$")[1],"  ",tsdir,"/MOD35_",tile,"_ymon.nc",sep=""))

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
 

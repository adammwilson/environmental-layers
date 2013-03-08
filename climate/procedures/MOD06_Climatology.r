### Process a folder of daily MOD06 HDF files to produce a climatology

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
finalvars=" CER COT CLD"


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
system(paste(ncopath,"ncrcat -O ",outdir,"/*nc ",outdir2,"/MOD06_",tile,"_daily.nc",sep=""))

## Update attributes
system(paste(ncopath,"ncatted ",
" -a units,time,o,c,\"days since 2000-1-1 0:0:0\" ",
" -a title,global,o,c,\"MODIS Cloud Product (MOD06) Daily Timeseries\" ",
" -a institution,global,o,c,\"Yale University\" ",
" -a source,global,o,c,\"MODIS Cloud Product (MOD06)\" ",
" -a comment,global,o,c,\"Compiled by Adam M. Wilson (adam.wilson@yale.edu)\" ",
outdir2,"/MOD06_",tile,"_daily.nc",sep=""))

### produce a monthly timeseries?
#system(paste("cdo -O monmean ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_monmean.nc",sep=""))

#############################
##  Generate the Climatologies
if(verbose) print("Generate monthly climatologies")

myear=as.integer(max(fdly$year))  #this year will be used in all dates of monthly climatologies (and day will = 15)

## Monthly means
if(verbose) print("Calculating the monthly means")
system(paste("cdo -O sorttimestamp -setyear,",myear," -setday,15 -ymonmean ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymonmean.nc",sep=""),wait=T)

## Monthly standard deviation
if(verbose) print("Calculating the monthly SD")
system(paste("cdo -O sorttimestamp -setyear,",myear," -setday,15 -ymonstd ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymonstd.nc",sep=""))
system(paste(ncopath,"ncrename -v CER,CER_sd -v CLD,CLD_sd -v COT,COT_sd ",tsdir,"/MOD06_",tile,"_ymonstd.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CER_sd,o,c,\"Cloud Particle Effective Radius (standard deviation of daily observations)\" ",
" -a long_name,CLD_sd,o,c,\"Cloud Mask (standard deviation of daily observations)\" ",
" -a long_name,COT_sd,o,c,\"Cloud Optical Thickness (standard deviation of daily observations)\" ",
tsdir,"/MOD06_",tile,"_ymonstd.nc",sep=""))

## cer > 20
if(verbose) print("Calculating the proportion of days with CER > 20")
system(paste("cdo -O  sorttimestamp -setyear,",myear," -setday,15 -ymonmean -gtc,20 -selvar,CER ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymoncer20.nc",sep=""))
system(paste(ncopath,"ncrename -v CER,CER20 ",tsdir,"/MOD06_",tile,"_ymoncer20.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CER20,o,c,\"Proportion of Days with Cloud Particle Effective Radius > 20um\" ",
" -a units,CER20,o,c,\"Proportion\" ",
tsdir,"/MOD06_",tile,"_ymoncer20.nc",sep=""))

## cld == 0
if(verbose) print("Calculating the proportion of cloudy days")
system(paste("cdo -O  sorttimestamp -setyear,",myear," -setday,15 -nint -mulc,100 -ymonmean -eqc,0 -setctomiss,1 -selvar,CLD2 ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymoncld0.nc",sep=""))
system(paste(ncopath,"ncrename -v CLD2,CLD0 ",tsdir,"/MOD06_",tile,"_ymoncld0.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CLD0,o,c,\"Proportion of Days with Cloud Mask == 0\" ",
" -a units,CLD0,o,c,\"Proportion\" ",
tsdir,"/MOD06_",tile,"_ymoncld0.nc",sep=""))

## cld == 0|1
if(verbose) print("Calculating the proportion of cloudy days")
system(paste("cdo -O  sorttimestamp -setyear,",myear," -setday,15 -nint -mulc,100 -ymonmean -lec,1 -selvar,CLD2 ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymoncld01.nc",sep=""))
system(paste(ncopath,"ncrename -v CLD2,CLD01 ",tsdir,"/MOD06_",tile,"_ymoncld01.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CLD01,o,c,\"Proportion of Days with Cloud Mask == 0|1\" ",
" -a units,CLD01,o,c,\"Proportion\" ",
tsdir,"/MOD06_",tile,"_ymoncld01.nc",sep=""))

## cld == 1
if(verbose) print("Calculating the proportion of uncertain days")
system(paste("cdo -O  sorttimestamp -setyear,",myear," -setday,15 -nint -mulc,100 -ymonmean -eqc,1 -selvar,CLD2 ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymoncld1.nc",sep=""))
system(paste(ncopath,"ncrename -v CLD2,CLD1 ",tsdir,"/MOD06_",tile,"_ymoncld1.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CLD1,o,c,\"Proportion of Days with Cloud Mask == 1 (uncertain)\" ",
" -a units,CLD1,o,c,\"Proportion\" ",
tsdir,"/MOD06_",tile,"_ymoncld1.nc",sep=""))


## cld >= 2 (setting cld==01 to missing because 'uncertain')
if(verbose) print("Calculating the proportion of clear days")
system(paste("cdo -O  sorttimestamp -setyear,",myear," -setday,15 -nint -mulc,100 -ymonmean -gtc,1 -setctomiss,1 -selvar,CLD2 ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymoncld2.nc",sep=""))
system(paste(ncopath,"ncrename -v CLD2,CLD23 ",tsdir,"/MOD06_",tile,"_ymoncld2.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CLD23,o,c,\"Proportion of Days with Cloud Mask >= 2 (Probably Clear or Certainly Clear)\" ",
" -a units,CLD23,o,c,\"Proportion\" ",
tsdir,"/MOD06_",tile,"_ymoncld2.nc",sep=""))

## cld >= 1
if(verbose) print("Calculating the proportion of clear days")
system(paste("cdo -O  sorttimestamp -setyear,",myear," -setday,15 -nint -mulc,100 -ymonmean -gec,1 -selvar,CLD2 ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymoncld13.nc",sep=""))
system(paste(ncopath,"ncrename -v CLD2,CLD13 ",tsdir,"/MOD06_",tile,"_ymoncld13.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CLD13,o,c,\"Proportion of Days with Cloud Mask >= 1\" ",
" -a units,CLD13,o,c,\"Proportion\" ",
tsdir,"/MOD06_",tile,"_ymoncld13.nc",sep=""))

## number of observations
if(verbose) print("Calculating the number of missing variables")
system(paste("cdo -O sorttimestamp  -setyear,",myear," -setday,15 -nint -mulc,100 -ymonmean -eqc,9999 -setmisstoc,9999   -selvar,CER,CLD ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymonmiss.nc",sep=""))
system(paste(ncopath,"ncrename -v CER,CER_pmiss -v CLD,CLD_pmiss ",tsdir,"/MOD06_",tile,"_ymonmiss.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CER_pmiss,o,c,\"Proportion of Days with missing data for CER\" ",
" -a long_name,CLD_pmiss,o,c,\"Proportion of Days with missing data for CLD\" ",
" -a scale_factor,CER_pmiss,o,d,0.01 ",
" -a units,CER_pmiss,o,c,\"Proportion\" ",
" -a scale_factor,CLD_pmiss,o,d,0.01 ",
" -a units,CLD_pmiss,o,c,\"Proportion\" ",
tsdir,"/MOD06_",tile,"_ymonmiss.nc",sep=""))

## append variables to a single file
if(verbose) print("Append all monthly climatologies into a single file")
system(paste(ncopath,"ncks -O ",tsdir,"/MOD06_",tile,"_ymonmean.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD06_",tile,"_ymonstd.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD06_",tile,"_ymoncer20.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD06_",tile,"_ymoncld0.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD06_",tile,"_ymoncld1.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD06_",tile,"_ymoncld2.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD06_",tile,"_ymonmiss.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))

## append sinusoidal grid from one of input files as CDO doesn't transfer all attributes
if(verbose) print("Clean up file (update attributes, flip latitudes, add grid description")

#system(paste(ncopath,"ncea -d time,0,1 -v sinusoidal ",list.files(outdir,full=T,pattern="[.]nc$")[1],"  ",tsdir,"/sinusoidal.nc",sep=""))
#system(paste(ncopath,"ncks -A -d time,0,1 -v sinusoidal ",list.files(outdir,full=T,pattern="[.]nc$")[1],"  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))

## invert latitude so it plays nicely with gdal
system(paste(ncopath,"ncpdq -O -a -y ",tsdir,"/MOD06_",tile,"_ymon.nc ",outdir2,"/MOD06_",tile,".nc",sep=""))

## update attributes
system(paste(ncopath,"ncatted ",
#" -a standard_parallel,sinusoidal,o,c,\"0\" ",
#" -a longitude_of_central_meridian,sinusoidal,o,c,\"0\" ",
#" -a latitude_of_central_meridian,sinusoidal,o,c,\"0\" ",
" -a units,time,o,c,\"days since 2000-1-1 0:0:0\" ",
" -a title,global,o,c,\"MODIS Cloud Product (MOD06) Climatology\" ",
" -a institution,global,o,c,\"Yale University\" ",
" -a source,global,o,c,\"MODIS Cloud Product (MOD06)\" ",
" -a comment,global,o,c,\"Compiled by Adam M. Wilson (adam.wilson@yale.edu)\" ",
outdir2,"/MOD06_",tile,".nc",sep=""))


print(paste("###############################  Processed ",nrow(fdly),"days for tile:",tile," ###################################################"))
print("Years:")
print(table(fdly$fyear))
print("Months:")
print(table(fdly$fmonth))

## quit R
q("no")
 

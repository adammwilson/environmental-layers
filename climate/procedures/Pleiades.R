#### Script to facilitate processing of MOD06 data

setwd("/nobackupp1/awilso10/mod06")

library(rgdal)
library(raster)

## get MODLAND tile information
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
save(tb,file="modlandTiles.Rdata")

## delete temporary log file that can grow to GB
system("rm /nobackupp1/awilso10/software/heg/TOOLKIT_MTD/runtime/LogStatus")

tile="h11v08"  # Venezuela
tile="h11v07"  # Venezuela coast
tile="h09v04"  # Oregon

outdir="2_daily"  #directory for separate daily files
outdir=paste("daily/",tile,"/",sep="")  #directory for separate daily files
       if(!file.exists(outdir)) dir.create(outdir)
outdir2="summary" #directory for combined daily files and summarized files
       if(!file.exists(outdir2)) dir.create(outdir2)

  ## load a MOD11A1 file to define grid
gridfile=list.files("/nobackupp4/datapool/modis/MOD11A1.005/2006.01.27/",pattern=paste(tile,".*hdf$",sep=""),full=T)[1]
  td=readGDAL(paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_Daily_1km_LST:Night_view_angl",sep=""))
  projection(td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs +datum=WGS84 +ellps=WGS84 "


### get list of files to process
datadir="/nobackupp4/datapool/modis/MOD06_L2.005/"
datadir="/nobackupp1/awilso10/mod06/data"   #for data downloaded from 

fs=data.frame(path=list.files(datadir,full=T,recursive=T,pattern="hdf"),stringsAsFactors=F)
fs$file=basename(fs$path)
fs$date=as.Date(substr(fs$file,11,17),"%Y%j")
fs$month=format(fs$date,"%m")
fs$year=format(fs$date,"%Y")
fs$time=substr(fs$file,19,22)
fs$datetime=as.POSIXct(strptime(paste(substr(fs$file,11,17),substr(fs$file,19,22)), '%Y%j %H%M'))
fs$dateid=format(fs$date,"%Y%m%d")
fs$path=as.character(fs$path)
fs$file=as.character(fs$file)

## get all unique dates
alldates=unique(fs$dateid)


#### Generate submission file
## identify which have been completed
#fdone=system(paste("ssh -q lou2 \"ls ",pdir," | grep \"nc$\"\"",sep=""),intern=T)
fdone=list.files(outdir,pattern="nc$")
done=alldates%in%substr(fdone,14,21)

if(exists("fdly")){  #update using table from below
  done[alldates%in%fdly$dateid[is.na(fdly$npar)]]=F
}

table(done)
notdone=alldates[!done]  #these are the dates that still need to be processed

script="/u/awilso10/environmental-layers/climate/procedures/MOD06_L2_process.r"

write.table(paste("--verbose ",script," --date ",notdone," --verbose T --tile \"",tile,"\"",sep=""),file=paste(tile,"_notdone.txt",sep=""),row.names=F,col.names=F,quote=F)

save(fs,alldates,gridfile,td,file="allfiles.Rdata")

### qsub script
cat(paste("
#PBS -S /bin/bash
#PBS -l select=40:ncpus=8:mpiprocs=8
##PBS -l select=2:ncpus=4:mpiprocs=4
#PBS -l walltime=2:00:00
#PBS -j n
#PBS -m be
#PBS -N mod06
#PBS -q devel
#PBS -V

CORES=320
HDIR=/u/armichae/pr/
  source $HDIR/etc/environ.sh
  source /u/awilso10/.bashrc
IDIR=/nobackupp1/awilso10/mod06/
##WORKLIST=$HDIR/var/run/pxrRgrs/work.txt
WORKLIST=$IDIR/",tile,"_notdone.txt
EXE=Rscript
LOGSTDOUT=$IDIR/log/log.stdout
LOGSTDERR=$IDIR/log/log.stderr
mpiexec -np $CORES pxargs -a $WORKLIST -p $EXE -v -v -v --work-analyze 1> $LOGSTDOUT 2> $LOGSTDERR
",sep=""),file=paste(tile,"_mod06_qsub",sep=""))


### Check the file
system(paste("cat ",tile,"_mod06_qsub",sep=""))
#system("cat ~/environmental-layers/climate/procedures/MOD06_L2_process.r")

## check queue status
system("/u/scicon/tools/bin/node_stats.sh")
system("/u/scicon/tools/bin/qtop.pl 492352")

## Submit it (and keep the pid)!
system(paste("qsub ",tile,"_mod06_qsub",sep=""))

## work in interactive mode
# system("qsub -I -l walltime=2:00:00 -lselect=2:ncpus=16:model=san -q devel")
# mpirun -np 1 -r ssh R --no-save

## check progress
system("qstat -u awilso10")
system(paste("/u/scicon/tools/bin/qps ",568835))
system(paste("qstat -t -x",pid))


####################################

  ## move to permanent storage on lou?
#  system(paste("scp -r ",ncfile," ",pdir,sep=""))
file.remove(
list.files(pattern="filetable[.]temp|GetAttrtemp|core[.]|MCFWrite[.]temp")
)

################################################################################
## now generate the climatologies
fdly=data.frame(
  path=list.files(outdir,pattern="nc$",full=T),
  file=list.files(outdir,pattern="nc$"))
  fdly$dateid=substr(fdly$file,14,21)
  fdly$date=as.Date(substr(fdly$file,14,21),"%Y%m%d")
  fdly$month=format(fdly$date,"%m")
  fdly$year=format(fdly$date,"%Y")
nrow(fdly)

## check validity (via npar and ntime) of nc files
for(i in 1:nrow(fdly)){
  fdly$ntime[i]<-as.numeric(system(paste("cdo -s ntime ",fdly$path[i]),intern=T))
  fdly$npar[i]<-as.numeric(system(paste("cdo -s npar ",fdly$path[i]),intern=T))
  fdly$fyear[i]<-as.numeric(system(paste("cdo -s showyear ",fdly$path[i]),intern=T))
  fdly$fmonth[i]<-as.numeric(system(paste("cdo -s showmon ",fdly$path[i]),intern=T))
  fdly$fvar[i]<-system(paste("cdo -s showvar ",fdly$path[i]),intern=T)
  print(paste(i," out of ",nrow(fdly)," for year ",  fdly$fyear[i]))
}

### table of problematic files
table(is.na(fdly$npar))
table(fdly$fmonth)
table(fdly$fvar)

fdly[is.na(fdly$npar),]

## delete files that fail check?
delete=T
if(delete) {
  drop=is.na(fdly$npar)|fdly$fvar!=" CER COT CLD"
  file.remove(as.character(fdly$path[drop]))
  fdly=fdly[!drop,]
}

## Combine all days within years into a single file (can't mergetime all days at once because this opens too many files)
ncopath="/nasa/sles11/nco/4.0.8/gcc/mpt/bin/"

library(multicore)

## create temporary directory to put intermediate files (will be deleted when R quits)
tsdir=paste(tempdir(),"/summary",sep="")
dir.create(tsdir)

## create a file of daily scenes for each year
#mclapply(unique(fdly$year),function(y){
#  system(paste("cdo -O mergetime ",paste(fdly$path[!is.na(fdly$npar)&fdly$year==y],collapse=" ")," ",tsdir,"/MOD06_",tile,"_",y,"_daily.nc",sep=""))
#  print(paste("Finished merging daily files for year",y))
#},mc.cores=5)


## merge all daily files to create a single file with all dates
  system(paste(ncopath,"ncrcat -O ",outdir,"/*nc ",outdir2,"/MOD06_",tile,"_daily.nc",sep=""))

##############################
## Combine the year-by-year files into a single daily file in the summary directory (for archiving)
#system(paste("cdo -O mergetime ",paste(list.files(tsdir,full=T,pattern="daily[.]nc$"),collapse=" ")," ",outdir2,"/MOD06_",tile,"_daily.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a units,time,o,c,\"days since 2000-1-1 0:0:0\" ",
" -a title,global,o,c,\"MODIS Cloud Product (MOD06) Daily Timeseries\" ",
" -a institution,global,o,c,\"Yale University\" ",
" -a source,global,o,c,\"MODIS Cloud Product (MOD06)\" ",
" -a comment,global,o,c,\"Compiled by Adam M. Wilson (adam.wilson@yale.edu)\" ",
outdir2,"/MOD06_",tile,"_daily.nc",sep=""))

#system(paste("cdo -O monmean ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_monmean.nc",sep=""))

#############################
##  Generate the Climatologies
myear=as.integer(max(fdly$year))  #this year will be used in all dates of monthly climatologies (and day will = 15)

## Monthly means
system(paste("cdo -O sorttimestamp -setyear,",myear," -setday,15 -ymonmean ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymonmean.nc",sep=""))

## Monthly standard deviation
system(paste("cdo -O sorttimestamp -setyear,",myear," -setday,15 -ymonstd ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymonstd.nc",sep=""))
system(paste(ncopath,"ncrename -v CER,CER_sd -v CLD,CLD_sd -v COT,COT_sd ",tsdir,"/MOD06_",tile,"_ymonstd.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CER_sd,o,c,\"Cloud Particle Effective Radius (standard deviation of daily observations)\" ",
" -a long_name,CLD_sd,o,c,\"Cloud Mask (standard deviation of daily observations)\" ",
" -a long_name,COT_sd,o,c,\"Cloud Optical Thickness (standard deviation of daily observations)\" ",
tsdir,"/MOD06_",tile,"_ymonstd.nc",sep=""))

## cer > 20
system(paste("cdo -O  sorttimestamp -setyear,",myear," -setday,15 -ymonmean -gtc,20 -selvar,CER ",outdir2,"/MOD06_",tile,"_daily.nc ",tsdir,"/MOD06_",tile,"_ymoncer20.nc",sep=""))
system(paste(ncopath,"ncrename -v CER,CER20 ",tsdir,"/MOD06_",tile,"_ymoncer20.nc",sep=""))
system(paste(ncopath,"ncatted ",
" -a long_name,CER20,o,c,\"Proportion of Days with Cloud Particle Effective Radius > 20um\" ",
" -a units,CER20,o,c,\"Proportion\" ",
tsdir,"/MOD06_",tile,"_ymoncer20.nc",sep=""))

## number of observations
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
system(paste(ncopath,"ncks -O ",tsdir,"/MOD06_",tile,"_ymonmean.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD06_",tile,"_ymonstd.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD06_",tile,"_ymoncer20.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))
system(paste(ncopath,"ncks -A ",tsdir,"/MOD06_",tile,"_ymonmiss.nc  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))

## append sinusoidal grid from one of input files as CDO doesn't transfer all attributes
system(paste(ncopath,"ncks -A -v sinusoidal ",list.files(tsdir,full=T,pattern="daily[.]nc$")[1],"  ",tsdir,"/MOD06_",tile,"_ymon.nc",sep=""))

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


print("Finished!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
## quit R
#q("no")
 

#################################################################

### copy the files back to Yale
system(paste("scp ",outdir2,"/MOD06_",tile,".nc adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod06/summary",sep=""))

system("scp  /tmp/Rtmp6I6tFn/MOD06_L2.A2000061.1615.051.2010273184629.hdf adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/Venezuela")
system("scp 2_daily/MOD06_20000410.nc adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/Venezuela")







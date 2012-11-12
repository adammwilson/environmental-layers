#### Script to facilitate processing of MOD06 data

setwd("/nobackupp1/awilso10/mod06")
library(rgdal)
library(raster)

## get MODLAND tile information
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
save(tb,file="modlandTiles.Rdata")

outdir="2_daily"  #directory for separate daily files
outdir2="3_summary" #directory for combined daily files and summarized files

  ## load a MOD11A1 file to define grid
gridfile=list.files("/nobackupp4/datapool/modis/MOD11A1.005/2006.01.27/",pattern=paste(tile,".*hdf$",sep=""),full=T)[1]
  td=readGDAL(paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_Daily_1km_LST:Night_view_angl",sep=""))
  projection(td)="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs +datum=WGS84 +ellps=WGS84 "


### get list of files to process
datadir="/nobackupp4/datapool/modis/MOD06_L2.005/"

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
done=alldates%in%substr(list.files(outdir),7,14)
table(done)
notdone=alldates[!done]  #these are the dates that still need to be processed

tile="h11v08"   #can move this to submit script if needed
script="/u/awilso10/environmental-layers/climate/procedures/MOD06_L2_process.r"
#write.table(paste("--verbose ",script," date=",notdone," tile=\"",tile,"\"",sep=""),file="notdone.txt",row.names=F,col.names=F,quote=F)
#write.table(paste("--verbose ",script," date=",notdone[1:30],sep=""),file="notdone.txt",row.names=F,col.names=F,quote=F)
write.table(notdone[1:30],file="notdone.txt",row.names=F,col.names=F,quote=F)

save(fs,alldates,gridfile,td,file="allfiles.Rdata")

## run script
cat(paste("
#! /bin/bash
source ~/moduleload
source ~/.bashrc
Rscript --verbose --vanilla /u/awilso10/environmental-layers/climate/procedures/MOD06_L2_process.r date=$1
#Rscript --verbose --vanilla rtest
",sep=""),file="MOD06_process2")
system("chmod +x MOD06_process2")

cat(paste("
library(rgdal)
GDALinfo
",sep=""),file="rtest")


## Submission script

cat(paste("
#PBS -S /bin/bash
#PBS -l select=2:ncpus=16:model=san
###PBS -l select=4:ncpus=8:model=neh
##PBS -l select=1:ncpus=12:model=wes
####### old: select=48:ncpus=8:mpiprocs=8:model=neh
#PBS -l walltime=2:00:00
#PBS -j oe
#PBS -m e
#PBS -V
#PBS -q devel
#PBS -o log/log_^array_index^
#PBS -o log/log_DataCompile.log
#PBS -M adam.wilson@yale.edu
#PBS -N MOD06

## cd to working directory
cd /nobackupp1/awilso10/mod06

## set some memory limits
#  ulimit -d 1500000 -m 1500000 -v 1500000  #limit memory usage
  source /usr/local/lib/global.profile
  source /u/awilso10/.bashrc
  source /u/awilso10/moduleload
## export a few important variables
  export NNODES=32
  export R_LIBS=\"/u/awilso10/R/x86_64-unknown-linux-gnu-library/2.15/\"
## Run the script!
## current version not parallelizing across nodes!
#  TMPDIR=$TMPDIR Rscript --verbose --vanilla /u/awilso10/environmental-layers/climate/procedures/MOD06_L2_process.r date=20000403

WORKLIST=notdone.txt
#EXE=\"Rscript\"
EXE="./MOD06_process2"
LOG=log/log_DataCompile.log
MQUEUE=/nobackupp4/pvotava/software/share/mqueue-eg/mqueue/mqueue

TMPDIR=$TMPDIR mpiexec -np $NNODES $MQUEUE -l $WORKLIST -p $EXE -v -v -v --random-starts 2-4 --work-analyze #> $LOG
exit 0
",sep=""),file="MOD06_process")

### Check the file
system("cat MOD06_process")
#system("cat ~/environmental-layers/climate/procedures/MOD06_L2_process.r")

## check queue status
system("/u/scicon/tools/bin/node_stats.sh")
system("/u/scicon/tools/bin/qtop.pl 492352")

## Submit it (and keep the pid)!
system("qsub MOD06_process")
system("/u/scicon/tools/bin/pdsh_gdb -j 493281 -d tmp -s -u awilso10")

## work in interactive mode
# system("qsub -I -l walltime=2:00:00 -lselect=2:ncpus=16:model=san -q devel")
# mpirun -np 1 -r ssh R --no-save

## check progress
system("qstat -u awilso10")
system(paste("/u/scicon/tools/bin/qps ",pid))
system(paste("qstat -t -x",pid))

system("qstat devel ") 
#system("qstat | grep awilso10") 

####################################


################################################################################
## now generate the climatologies
fdly=data.frame(
  path=list.files(outdir,pattern="nc$",full=T),
  file=list.files(outdir,pattern="nc$"))
fdly$date=as.Date(substr(fdly$file,7,14),"%Y%m%d")
fdly$month=format(fdly$date,"%m")
fdly$year=format(fdly$date,"%Y")

## check validity (via npar and ntime) of nc files
for(i in 1:nrow(fdly)){
  fdly$ntime[i]=as.numeric(system(paste("cdo  sinfo ",fdly$path[i]),intern=T))
  fdly$npar[i]=as.numeric(system(paste("cdo -s npar ",fdly$path[i]),intern=T))
  print(i)
}

## Combine all days within years into a single file (can't mergetime all days at once because this opens too many files)
tsdir=paste(tempdir(),"/summary",sep="")
dir.create(tsdir)
lapply(unique(fdly$year),function(y){
  system(paste("cdo -O mergetime ",paste(fdly$path[fdly$year==y],collapse=" ")," ",tsdir,"/MOD09_",tile,"_",y,"_daily.nc",sep=""))
  print(paste("Finished merging daily files for year",y))
})
## Combine the year-by-year files into a single daily file
system(paste("cdo -O mergetime ",paste(list.files(tsdir,full=T,pattern="daily[.]nc$"),collapse=" ")," ",outdir2,"/MOD09_",tile,"_daily.nc",sep=""))

system(paste("cdo -O monmean ",outdir2,"/MOD09_",tile,"_daily.nc ",outdir2,"/",tile,"_monmean.nc",sep=""))
system(paste("cdo -O ymonmean ",outdir2,"/MOD09_",tile,"_daily.nc ",outdir2,"/",tile,"_ymonmean.nc",sep=""))
system(paste("cdo -O ymonstd ",outdir2,"/MOD09_",tile,"_daily.nc ",outdir2,"/",tile,"_ymonstd.nc",sep=""))

print("Finished!   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
## quit R
q("no")
 

#################################################################

### copy the files back to Yale
list.files("2_daily")
system("scp 2_daily/* adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/Venezuela")

system("scp  /tmp/Rtmp6I6tFn/MOD06_L2.A2000061.1615.051.2010273184629.hdf adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/Venezuela")
system("scp 2_daily/MOD06_20000410.nc adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/Venezuela")


list.files(" /tmp/Rtmp6I6tFn")





#### Script to facilitate processing of MOD06 data

setwd("/nobackupp1/awilso10/mod06")

## get MODLAND tile information
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
save(tb,file="modlandTiles.Rdata")

### Submission script

cat(paste("
#PBS -S /bin/bash
#PBS -l select=1:ncpus=16:model=san
###PBS -l select=4:ncpus=8:model=neh
##PBS -l select=1:ncpus=12:model=wes
####### old: select=48:ncpus=8:mpiprocs=8:model=neh
#PBS -l walltime=2:00:00
#PBS -j oe
#PBS -m e
#PBS -V
####PBS -W group_list=s1007
#PBS -q devel
#PBS -o log/log_^array_index^
#PBS -o log/log_DataCompile
#PBS -M adam.wilson@yale.edu
#PBS -N MOD06

#source /usr/share/modules/init/bash

## cd to working directory
cd /nobackupp1/awilso10/mod06

## set some memory limits
#  ulimit -d 1500000 -m 1500000 -v 1500000  #limit memory usage
  source /u/awilso10/.bashrc
  source /u/awilso10/moduleload
  source /usr/local/lib/global.profile
## export a few important variables
  export NCORES=16  # use to limit mclapply() to set nubmer of cores, should be select*ncpus above
  export R_LIBS=\"/u/awilso10/R/x86_64-unknown-linux-gnu-library/2.15/\"
## load modules
  module load gcc hdf4 udunits R nco mpi-intel #mpi-sgi/mpt.2.06r6
## Run the script!
## current version not parallelizing across nodes!
  TMPDIR=$TMPDIR Rscript --verbose --vanilla /u/awilso10/environmental-layers/climate/procedures/MOD06_L2_process.r 
exit 0
exit 0

",sep=""),file="MOD06_process")

### Check the file
system("cat MOD06_process")
#system("cat ~/environmental-layers/climate/procedures/MOD06_L2_process.r")

## check queue status
system("/u/scicon/tools/bin/node_stats.sh")

## Submit it (and keep the pid)!
system("qsub MOD06_process")

## work in interactive mode
# system("qsub -I -l walltime=2:00:00 -lselect=2:ncpus=16:model=san -q devel")
# mpirun -np 1 -r ssh R --no-save

## check progress
system("qstat -u awilso10")
system(paste("/u/scicon/tools/bin/qps ",pid))
system(paste("qstat -t -x",pid))

system("qstat devel ") 
#system("qstat | grep awilso10") 


### copy the files back to Yale
list.files("2_daily")
system("scp 2_daily/* adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/Venezuela")

system("scp  /tmp/Rtmp6I6tFn/MOD06_L2.A2000061.1615.051.2010273184629.hdf adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/Venezuela")
system("scp 2_daily/MOD06_20000410.nc adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/Venezuela")


list.files(" /tmp/Rtmp6I6tFn")

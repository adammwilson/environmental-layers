#### Script to facilitate processing of MOD06 data

setwd("/nobackupp1/awilso10/mod06")

## get MODLAND tile information
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
save(tb,file="modlandTiles.Rdata")

### Submission script

cat(paste("
#PBS -S /bin/bash
#PBS -l select=64:ncpus=4:mpiprocs=4:model=wes
####old PBS -l select=64:ncpus=4:mpiprocs=4:model=wes
####### old: select=48:ncpus=8:mpiprocs=8:model=neh
#PBS -l walltime=10:00:00
#PBS -j oe
#PBS -m e
#PBS -V
####PBS -W group_list=s1007
###PBS -q devel
###PBS -o log/log_^array_index^
#PBS -o log/log_DataCompile
#PBS -M adam.wilson@yale.edu
#PBS -N MOD06

source /usr/share/modules/init/bash

## cd to working directory
cd /nobackupp1/awilso10/mod06

## set some memory limits
#  ulimit -d 1500000 -m 1500000 -v 1500000  #limit memory usage
  source /usr/local/lib/global.profile
  source /u/awilso10/.bashrc
## export a few important variables
  export PATH=$PATH:/nobackupp1/awilso10/bin:/nobackupp1/awilso10/software/bin
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/nobackupp1/awilso10/software/lib
  export MRTDATADIR=/nobackupp1/awilso10/software/heg/data
  export PGSHOME=/nobackupp1/awilso10/software/heg
  export MRTBINDIR=/nobackupp1/awilso10/software/TOOLKIT_MTD
  export R_LIBS=\"/u/awilso10/R/x86_64-unknown-linux-gnu-library/2.15/\"
  export TMPDIR=/nobackupp1/awilso10/mod06/tmp
## load modules
  module load gcc mpi-sgi/mpt.2.06r6 hdf4 udunits R
## Run the script!
  TMPDIR=$TMPDIR Rscript --verbose --vanilla /u/awilso10/environmental-layers/climate/procedures/MOD06_L2_process.r 
exit 0
",sep=""),file="MOD06_process")

### Check the file
system("cat MOD06_process")
#system("cat ~/environmental-layers/climate/procedures/MOD06_L2_process.r")

## Submit it (and keep the pid)!
pid=system("qsub MOD06_process",intern=T); pid; pid=strsplit(pid,split="[.]")[[1]][1]

#system("qsub MOD06_process")

## work in interactive mode
#system("qsub -I -lselect=1:ncpus=2:model=wes -q devel")

## check progress
system("qstat -u awilso10")
system(paste("qstat -t -x",pid))

system("qstat devel ") 
#system("qstat | grep awilso10") 


### copy the files back to Yale
system("scp 2_daily/* adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/")

system("scp  /tmp/Rtmp6I6tFn/MOD06_L2.A2000061.1615.051.2010273184629.hdf adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/")

list.files(" /tmp/Rtmp6I6tFn")

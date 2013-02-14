#### Script to facilitate processing of MOD06 data
 
  setwd("/nobackupp1/awilso10/mod06")

library(rgdal)
library(raster)
library(RSQLite)


verbose=T

## get MODLAND tile information
txbb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
save(tb,file="modlandTiles.Rdata")
load("modlandTiles.Rdata")

## delete temporary log file that can grow to GB
system("rm /nobackupp1/awilso10/software/heg/TOOLKIT_MTD/runtime/LogStatus")


tile="h11v08"  # Venezuela
#tile="h11v07"  # Venezuela coast
#tile="h09v04"  # Oregon
tile="h21v09"  #Kenya


### list of tiles to process
tiles=c("h11v08","h21v09","h08v04","h09v04","h08v05","h09v05","h20v11","h31v11")
tiles=tiles[c(5,7,8)]
tile_bb=tb[tb$tile%in%tiles,]

### get list of files to process
datadir="/nobackupp4/datapool/modis/MOD06_L2.005/"
#datadir="/nobackupp1/awilso10/mod06/data"   #for data downloaded from 

outdir="daily/" #paste("daily/",tile,sep="")

##find swaths in region from sqlite database for the specified date/tile
## path to swath database
db="/nobackupp4/pvotava/DB/export/swath_geo.sql.sqlite3.db"
con=dbConnect("SQLite", dbname = db)
fs=do.call(rbind.data.frame,lapply(1:nrow(tile_bb),function(i){
  d=dbGetQuery(con,paste("SELECT * from swath_geo
            WHERE east>=",tile_bb$lon_min[i]," AND
                  west<=",tile_bb$lon_max[i]," AND
                  north>=",tile_bb$lat_min[i]," AND
                  south<=",tile_bb$lat_max[i])
    )
  d$tile=tile_bb$tile[i]
  print(paste("Finished tile",tile_bb$tile[i]))
  return(d)
}))
  con=dbDisconnect(con)
  fs$id=substr(fs$id,7,19)

### Identify which swaths are available in the datapool
swaths=data.frame(path=list.files(datadir,pattern=paste("hdf$"),recursive=T,full=T),stringsAsFactors=F)  #all swaths in data pool
swaths$id=substr(basename(swaths$path),10,22)
fs$exists=fs$id%in%swaths$id 
fs$path=swaths$path[match(fs$id,swaths$id)]
  
if(verbose) print(paste("###############",nrow(fs)," swath IDs recieved from database"))


## get all unique dates
fs$dateid=format(as.Date(paste(fs$year,fs$day,sep=""),"%Y%j"),"%Y%m%d")
alldates=unique(fs$dateid[fs$exists])

#### Generate submission file
alldates=format(seq(as.Date("2000-03-01"),as.Date("2011-12-31"),1),"%Y%m%d")
proclist=expand.grid(date=alldates,tile=tiles)
proclist$year=substr(proclist$date,1,4)
  
## identify which have been completed
fdone=data.frame(path=list.files(outdir,pattern="nc$",recursive=T))
fdone$date=substr(basename(as.character(fdone$path)),14,21)
fdone$tile=substr(basename(as.character(fdone$path)),14,21)

## identify which date-tiles have already been run
proclist$done=paste(proclist$tile,proclist$date,sep="_")%in%substr(basename(as.character(fdone$path)),7,21)

### report on what has already been processed
print(paste("Overview of completed tile-days (",round(sum(proclist$done)/nrow(proclist),2),"%)"))
table(tile=proclist$tile[proclist$done],year=proclist$year[proclist$done])


#updatedone=F #update the "done" list using the 
#if(updatedone&exists("fdly")){  #update using table from below
#  done[alldates%in%fdly$dateid[fdly$drop]]=F
#}

## Identify which dates still need to be processed
## This vector will be used to tell mpiexec which days to include
#notdone=alldates[!done]  

script="/u/awilso10/environmental-layers/climate/procedures/MOD06_L2_process.r"
climatescript="/u/awilso10/environmental-layers/climate/procedures/MOD06_Climatology.r"

## write the table processed by mpiexec
write.table(paste("--verbose ",script," --date ",proclist$date[!proclist$done]," --verbose T --tile ",proclist$tile[!proclist$done],sep=""),
file=paste("notdone.txt",sep=""),row.names=F,col.names=F,quote=F)

### qsub script
cat(paste("
#PBS -S /bin/bash
#PBS -l select=50:ncpus=8:mpiprocs=8
##PBS -l select=50:ncpus=8:mpiprocs=8
##PBS -l select=2:ncpus=4:mpiprocs=4
#PBS -l walltime=5:00:00
#PBS -j n
#PBS -m be
#PBS -N mod06
#PBS -q normal
#PBS -V

CORES=400
HDIR=/u/armichae/pr/
  source $HDIR/etc/environ.sh
  source /u/awilso10/.bashrc
IDIR=/nobackupp1/awilso10/mod06/
##WORKLIST=$HDIR/var/run/pxrRgrs/work.txt
WORKLIST=$IDIR/notdone.txt
EXE=Rscript
LOGSTDOUT=$IDIR/log/swath_stdout
LOGSTDERR=$IDIR/log/swath_stderr
### use mpiexec to parallelize across days
mpiexec -np $CORES pxargs -a $WORKLIST -p $EXE -v -v -v --work-analyze 1> $LOGSTDOUT 2> $LOGSTDERR
",sep=""),file=paste("mod06_qsub",sep=""))


### Check the files
system(paste("cat mod06_qsub",sep=""))
system(paste("cat notdone.txt | head",sep=""))
system(paste("cat notdone.txt | wc -l ",sep=""))

## Submit it
system(paste("qsub mod06_qsub",sep=""))

#######################################################
### Now submit the script to generate the climatologies

tiles
ctiles=tiles[c(1,3)]  #subset to only some tiles (for example if some aren't finished yet)?
climatescript="/u/awilso10/environmental-layers/climate/procedures/MOD06_Climatology.r"

## write the table processed by mpiexec
write.table(paste("--verbose ",climatescript," --verbose T --tile ",ctiles,sep=""),
file=paste("notdone_climate.txt",sep=""),row.names=F,col.names=F,quote=F)

### qsub script
cat(paste("
#PBS -S /bin/bash
#PBS -l select=1:ncpus=8:mpiprocs=8
##PBS -l select=2:ncpus=4:mpiprocs=4
#PBS -l walltime=5:00:00
#PBS -j n
#PBS -m be
#PBS -N mod06_climate
#PBS -q normal
#PBS -V

CORES=8
HDIR=/u/armichae/pr/
  source $HDIR/etc/environ.sh
  source /u/awilso10/.bashrc
IDIR=/nobackupp1/awilso10/mod06/
##WORKLIST=$HDIR/var/run/pxrRgrs/work.txt
WORKLIST=$IDIR/notdone_climate.txt
EXE=Rscript
LOGSTDOUT=$IDIR/log/climatology_stdout
LOGSTDERR=$IDIR/log/climatology_stderr
### use mpiexec to parallelize across days
mpiexec -np $CORES pxargs -a $WORKLIST -p $EXE -v -v -v --work-analyze 1> $LOGSTDOUT 2> $LOGSTDERR
### Now process the climatologies
",sep=""),file=paste("mod06_climatology_qsub",sep=""))

## check files
system(paste("cat mod06_climatology_qsub",sep=""))        #qsub submission script
system(paste("cat notdone_climate.txt | head",sep=""))    #top of job file
system(paste("cat notdone_climate.txt | wc -l ",sep=""))  #number of jobs to be run

## Submit it
system(paste("qsub mod06_climatology_qsub",sep=""))

## check progress
system("qstat -u awilso10")

## start interactive job on compute node for debugging
# system("qsub -I -l walltime=2:00:00 -lselect=2:ncpus=16:model=san -q devel")


#################################################################
### copy the files back to Yale
summarydir="summary"

sumfiles=list.files("summary",pattern="^MOD06_.*[0-9][.]nc",full=T)

system(paste("scp ",paste(sumfiles,collapse=" ")," adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod06/summary",sep=""))

#system(paste("scp ",tsdir,"/MOD06_",tile,"*.nc adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod06/summary",sep=""))
#system(paste("scp ",paste(fs$path[40421:40422],collapse=" ")," adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod06/swaths",sep=""))






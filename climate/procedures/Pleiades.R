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
#tile="h11v07"  # Venezuela coast
#tile="h09v04"  # Oregon
tile="h21v09"  #Kenya

### get list of files to process
datadir="/nobackupp4/datapool/modis/MOD06_L2.005/"
#datadir="/nobackupp1/awilso10/mod06/data"   #for data downloaded from 

outdir=paste("daily/",tile,sep="")

##find swaths in region from sqlite database for the specified date/tile
## path to swath database
db="/nobackupp4/pvotava/DB/export/swath_geo.sql.sqlite3.db"
con=dbConnect("SQLite", dbname = db)
fs=dbGetQuery(con,paste("SELECT * from swath_geo
            WHERE east>=",tile_bb$lon_min," AND
                  west<=",tile_bb$lon_max," AND
                  north>=",tile_bb$lat_min," AND
                  south<=",tile_bb$lat_max)
    )
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
## identify which have been completed
fdone=list.files(outdir,pattern="nc$")
done=alldates%in%substr(fdone,14,21)

### report on what has already been processed
print(paste(table(done)[2]," out of",length(alldates),
      "(",round(100*table(done)[2]/length(alldates),1),
      "%) dates for tile",tile,
      "have been processed.  Breakdown by year of completed days:"))
print(table(substr(alldates[done],1,4)))

#updatedone=F #update the "done" list using the 
#if(updatedone&exists("fdly")){  #update using table from below
#  done[alldates%in%fdly$dateid[fdly$drop]]=F
#}

## Identify which dates still need to be processed
## This vector will be used to tell mpiexec which days to include
notdone=alldates[!done]  

script="/u/awilso10/environmental-layers/climate/procedures/MOD06_L2_process.r"
climatescript="/u/awilso10/environmental-layers/climate/procedures/MOD06_Climatology.r"

## write the table processed by mpiexec
write.table(paste("--verbose ",script," --date ",notdone," --verbose T --tile \"",tile,"\"",sep=""),file=paste(tile,"_notdone.txt",sep=""),row.names=F,col.names=F,quote=F)

### qsub script
cat(paste("
#PBS -S /bin/bash
#PBS -l select=50:ncpus=8:mpiprocs=8
##PBS -l select=2:ncpus=4:mpiprocs=4
#PBS -l walltime=2:00:00
#PBS -j n
#PBS -m be
#PBS -N mod06
#PBS -q devel
#PBS -V

CORES=400
HDIR=/u/armichae/pr/
  source $HDIR/etc/environ.sh
  source /u/awilso10/.bashrc
IDIR=/nobackupp1/awilso10/mod06/
##WORKLIST=$HDIR/var/run/pxrRgrs/work.txt
WORKLIST=$IDIR/",tile,"_notdone.txt
EXE=Rscript
LOGSTDOUT=$IDIR/log/",tile,"_stdout
LOGSTDERR=$IDIR/log/",tile,"_stderr
### use mpiexec to parallelize across days
mpiexec -np $CORES pxargs -a $WORKLIST -p $EXE -v -v -v --work-analyze 1> $LOGSTDOUT 2> $LOGSTDERR
### Now process the climatologies
Rscript --verbose ",climatescript," --verbose T --tile \"",tile,"\"
",sep=""),file=paste(tile,"_mod06_qsub",sep=""))


### Check the file
system(paste("cat ",tile,"_mod06_qsub",sep=""))

## Submit it (and keep the pid)!
system(paste("qsub ",tile,"_mod06_qsub",sep=""))

## work in interactive mode
# system("qsub -I -l walltime=2:00:00 -lselect=2:ncpus=16:model=san -q devel")
# mpirun -np 1 -r ssh R --no-save

## check progress
system("qstat -u awilso10")


#################################################################
### copy the files back to Yale
summarydir="summary"



system(paste("scp ",summarydir,"/MOD06_",tile,".nc adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod06/summary",sep=""))

system(paste("scp ",tsdir,"/MOD06_",tile,"*.nc adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod06/summary",sep=""))
system(paste("scp ",paste(fs$path[40421:40422],collapse=" ")," adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod06/swaths",sep=""))






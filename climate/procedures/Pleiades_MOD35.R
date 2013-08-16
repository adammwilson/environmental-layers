#### Script to facilitate processing of MOD06 data
### This script is meant to be run iteratively, rather than unsupervised. There are several steps that require manual checking (such as choosing the number of cores, etc.)

## working directory
setwd("/nobackupp1/awilso10/mod35")

## load libraries
library(rgdal)
library(raster)
library(RSQLite)

## flag to increase verbosity of output
verbose=T

## get MODLAND tile information
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
tb=tb[tb$lon_min!=-999,]
save(tb,file="modlandTiles.Rdata")
load("modlandTiles.Rdata")

## Choose some tiles to process
### list of tiles to process
tiles=c("h10v08","h11v08","h12v08","h10v07","h11v07","h12v07")  # South America
## or a northern block of tiles
tiles=apply(expand.grid(paste("h",11:17,sep=""),v=c("v00","v01","v02","v03","v04")),1,function(x) paste(x,collapse="",sep=""))
## subset to MODLAND tiles
alltiles=system("ls -r MODTILES/ | grep tif$ | cut -c1-6 | sort | uniq - ",intern=T)

## subset to tiles in global region (not outside global boundary in sinusoidal projection)
tiles=tiles[tiles%in%alltiles]

## subset tile corner matrix to tiles selected above
tile_bb=tb[tb$tile%in%tiles,]

### get list of files to process
datadir="/nobackupp4/datapool/modis/MOD35_L2.006/"

outdir="daily/" #paste("daily/",tile,sep="")

##find swaths in region from sqlite database for the specified date/tile
## this takes a while, about 30 minutes, so only rebuild if you need to update what's available...
rebuildswathtable=F
if(rebuildswathtable){
  ## path to swath database
  db="/nobackupp4/pvotava/DB/export/swath_geo.sql.sqlite3.db"
  con=dbConnect("SQLite", dbname = db)
  fs=do.call(rbind.data.frame,lapply(1:nrow(tile_bb),function(i){
    d=dbGetQuery(con,paste("SELECT * from swath_geo6
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

  ## Identify which swaths are available in the datapool
  swaths=data.frame(path=list.files(datadir,pattern=paste("hdf$"),recursive=T,full=T),stringsAsFactors=F)  #all swaths in data pool
  swaths$id=substr(basename(swaths$path),10,22)
  fs$exists=fs$id%in%swaths$id 
  fs$path=swaths$path[match(fs$id,swaths$id)]

  ## write tile-swath list to disk
  save(fs,swaths,file="swathtile.Rdata")
}

load("swathtile.Rdata")

if(verbose) print(paste("###############",nrow(fs)," swath IDs recieved from database"))

## get all unique dates
fs$dateid=format(as.Date(paste(fs$year,fs$day,sep=""),"%Y%j"),"%Y%m%d")
#alldates=unique(fs$dateid[fs$exists])

#### Generate submission file
startdate="2000-03-01"
stopdate="2011-12-31"
## just 2005-2010
startdate="2009-01-01"
stopdate="2009-12-31"

alldates=format(seq(as.Date(startdate),as.Date(stopdate),1),"%Y%m%d")

proclist=expand.grid(date=alldates,tile=tiles)
proclist$year=substr(proclist$date,1,4)

 ## identify tile-dates with no available swaths
avail=unique(cbind.data.frame(tile=fs$tile,date=fs$dateid)[fs$exists, ])
proclist$avail=paste(proclist$tile,proclist$date,sep="_")%in%paste(avail$tile,avail$date,sep="_")

## identify which have been completed
#fdone=data.frame(path=system("ssh lou 'find MOD35/daily -name \"*.nc\"' ",intern=T))
fdone=data.frame(path=list.files(outdir,pattern="nc$",recursive=T))
fdone$date=substr(basename(as.character(fdone$path)),14,21)
fdone$tile=substr(basename(as.character(fdone$path)),7,12)
proclist$done=paste(proclist$tile,proclist$date,sep="_")%in%substr(basename(as.character(fdone$path)),7,21)

### report on what has already been processed
print(paste(sum(!proclist$done)," out of ",nrow(proclist)," (",round(100*sum(!proclist$done)/nrow(proclist),2),"%) remain"))
stem(table(tile=proclist$tile[proclist$done],year=proclist$year[proclist$done]))
#table(tile=proclist$tile[proclist$done],year=proclist$year[proclist$done])
table(table(tile=proclist$tile[!proclist$done],year=proclist$year[!proclist$done]))

### explore tile counts
#x=table(tile=proclist$tile[proclist$done],year=proclist$year[proclist$done])
#x=x[order(rownames(x)),]

script="/u/awilso10/environmental-layers/climate/procedures/MOD35_L2_process.r"
 
## write the table processed by mpiexec
tp=T  # rerun everything
tp=((!proclist$done)&proclist$avail)  #date-tiles to process
table(Available=proclist$avail,Completed=proclist$done)
table(tp)

write.table(paste("--verbose ",script," --date ",proclist$date[tp]," --verbose T --tile ",proclist$tile[tp],sep=""),
file=paste("notdone.txt",sep=""),row.names=F,col.names=F,quote=F)

## try running it once for a single tile-date to get estimate of time/tile-day
test=F
if(test){
  i=2
  time1=system.time(system(paste("Rscript --verbose ",script," --date ",proclist$date[i]," --verbose T --tile ",proclist$tile[i],sep="")))
  hours=round(length(proclist$date[tp])*142/60/60)
  hours=round(length(proclist$date[tp])*time1[3]/60/60,1); hours
  hours/400
  print(paste("Based on runtime of previous command, it will take",hours," hours to process the full set"))
}


### qsub script
cat(paste("
#PBS -S /bin/bash
#PBS -l select=50:ncpus=8:mpiprocs=8
##PBS -l select=100:ncpus=8:mpiprocs=8
##PBS -l walltime=8:00:00
#PBS -l walltime=2:00:00
#PBS -j n
#PBS -m be
#PBS -N mod35
##PBS -q normal
#PBS -q devel
#PBS -V

#CORES=800
CORES=400

HDIR=/u/armichae/pr/
  source $HDIR/etc/environ.sh
  source /u/awilso10/environ.sh
  source /u/awilso10/.bashrc
IDIR=/nobackupp1/awilso10/mod35/
##WORKLIST=$HDIR/var/run/pxrRgrs/work.txt
WORKLIST=$IDIR/notdone.txt
EXE=Rscript
LOGSTDOUT=$IDIR/log/mod35_stdout
LOGSTDERR=$IDIR/log/mod35_stderr
### use mpiexec to parallelize across days
mpiexec -np $CORES pxargs -a $WORKLIST -p $EXE -v -v -v --work-analyze 1> $LOGSTDOUT 2> $LOGSTDERR
",sep=""),file=paste("mod35_qsub",sep=""))

### Check the files
system(paste("cat mod35_qsub",sep=""))
system(paste("cat notdone.txt | head",sep=""))
system(paste("cat notdone.txt | wc -l ",sep=""))


## Submit it
system(paste("qsub mod35_qsub",sep=""))

system("qstat -u awilso10")

#######################################################
### Now submit the script to generate the climatologies

## report 'mostly' finished tiles
## this relies on proclist above so be sure to update above before running
md=table(tile=proclist$tile[!proclist$done],year=proclist$year[!proclist$done])
mdt=names(md[md<10,])
tiles=mdt

tiles
ctiles=c("h10v08","h11v08","h12v08","h10v07","h11v07","h12v07")  # South America

ctiles=tiles#[c(1:3)]  #subset to only some tiles (for example if some aren't finished yet)?
climatescript="/pleiades/u/awilso10/environmental-layers/climate/procedures/MOD35_Climatology.r"

## check which tiles have been processed and are on lou with a filename "MOD35_[tile].nc"
cdone=data.frame(path="",tile="")  #use this if you want to re-run everything
cdone=data.frame(path=sapply(strsplit(basename(
                   system("ssh lou 'find MOD35/summary -name \"MOD35_h[0-9][0-9]v[0-9][0-9].nc\"' ",intern=T)),split="_"),function(x) x[2]))
cdone=data.frame(path=sapply(strsplit(basename(
                   system("find summary -name \"MOD35_h[0-9][0-9]v[0-9][0-9].nc\"",intern=T)),split="_"),function(x) x[2]))
cdone$tile=substr(basename(as.character(cdone$path)),1,6)
print(paste(length(ctiles[!ctiles%in%cdone$tile]),"Tiles still need to be processed"))

## write the table processed by mpiexec
write.table(paste("--verbose ",climatescript," --verbose T --tile ",ctiles[!ctiles%in%cdone$tile],sep=""),
file=paste("notdone_climate.txt",sep=""),row.names=F,col.names=F,quote=F)

## delay start until previous jobs have finished?
delay=T
## check running jobs to get JobID of job you want to wait for
system("qstat -u awilso10",intern=T)
## enter JobID here:
job="2031668.pbspl1.nas.nasa.gov"

### qsub script
cat(paste("
#PBS -S /bin/bash
#PBS -l select=4:ncpus=8:mem=94
#PBS -l walltime=2:00:00
#PBS -j n
#PBS -m be
#PBS -N mod35_climate
#PBS -q devel
##PBS -q normal
##PBS -q ldan
#PBS -V
",if(delay) paste("#PBS -W depend=afterany:",job,sep="")," 

CORES=32
HDIR=/u/armichae/pr/
  source $HDIR/etc/environ.sh
  source /pleiades/u/awilso10/environ.sh
  source /pleiades/u/awilso10/.bashrc
IDIR=/nobackupp1/awilso10/mod35/
##WORKLIST=$HDIR/var/run/pxrRgrs/work.txt
WORKLIST=$IDIR/notdone_climate.txt
EXE=Rscript
LOGSTDOUT=$IDIR/log/climatology_stdout
LOGSTDERR=$IDIR/log/climatology_stderr
### use mpiexec to parallelize across tiles
mpiexec -np $CORES pxargs -a $WORKLIST -p $EXE -v -v -v --work-analyze 1> $LOGSTDOUT 2> $LOGSTDERR
",sep=""),file=paste("mod35_climatology_qsub",sep=""))

## check files
system(paste("cat mod35_climatology_qsub",sep=""))        #qsub submission script
system(paste("cat notdone_climate.txt | head",sep=""))    #top of job file
system(paste("cat notdone_climate.txt | wc -l ",sep=""))  #number of jobs to be run

## Submit it
system(paste("qsub mod35_climatology_qsub",sep=""))

## check progress
system("qstat -u awilso10")

## start interactive job on compute node for debugging
# system("qsub -I -l walltime=2:00:00 -lselect=2:ncpus=16:model=san -q devel")


#################################################################
### copy the files back to Yale


system("ssh lou")
#scp `find MOD35/summary -name "MOD35_h[0-9][0-9]v[0-9][0-9].nc"` adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod35/summary/
system("rsync -cavv `find summary -name \"MOD35_h[0-9][0-9]v[0-9][0-9]_mean.nc\"` adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod35/summary/")
system("rsync -cavv `find summary -name \"MOD35_h[0-9][0-9]v[0-9][0-9].nc\"` adamw@acrobates.eeb.yale.edu:/data/personal/adamw/projects/interp/data/modis/mod35/summary/")


system("gdalbuildvrt MOD35C6_2009.vrt summary/*2009mean.nc ") 
system("gdal_translate -stats -co \"COMPRESS=LZW\" -of GTiff MOD35C6_2009.vrt MOD35C6_2009.tif ")              
system("scp MOD35C6_2009.tif adamw@acrobates.eeb.24.177.10.190:/Users/adamw/Downloads/")
exit



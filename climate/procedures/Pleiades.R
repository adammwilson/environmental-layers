#### Script to facilitate processing of MOD06 data


setwd("/nobackupp1/awilso10/mod06")

### get list of files to process
datadir="/nobackupp4/datapool/modis/MOD06_L2.005/"

fs=data.frame(
  path=list.files(datadir,full=T,recursive=T,pattern="hdf"),
  file=basename(list.files(datadir,full=F,recursive=T,pattern="hdf")))
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


## get MODLAND tile information
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
### use MODIS tile as ROI
#modt=readOGR("modgrid","modis_sinusoidal_grid_world",)
#modt@data[,colnames(tb)[3:6]]=tb[match(paste(modt$h,modt$v),paste(tb$ih,tb$iv)),3:6]
#write.csv(modt@data,file="modistile.csv")


## write it out
save(fs,tb,file="allfiles.Rdata")
#save(alldates,file="alldates.Rdata")

## identify which have been completed
outdir="2_daily"
done=alldates%in%substr(list.files(outdir),5,12)
table(done)
notdone=alldates[!done]

#notdone=alldates[1:4]

save(notdone,file="notdone.Rdata")


## vars
vars=as.data.frame(matrix(c(
  "Cloud_Effective_Radius",              "CER",
  "Cloud_Effective_Radius_Uncertainty",  "CERU",
  "Cloud_Optical_Thickness",             "COT",
  "Cloud_Optical_Thickness_Uncertainty", "COTU",
  "Cloud_Water_Path",                    "CWP",
  "Cloud_Water_Path_Uncertainty",        "CWPU",
  "Cloud_Phase_Optical_Properties",      "CPOP",
  "Cloud_Multi_Layer_Flag",              "CMLF",
  "Cloud_Mask_1km",                      "CM1",
  "Quality_Assurance_1km",               "QA"),
  byrow=T,ncol=2,dimnames=list(1:10,c("variable","varid"))),stringsAsFactors=F)
save(vars,file="vars.Rdata")


### Submission script

cat(paste("
#PBS -S /bin/sh
#PBS -J 700-899
###PBS -J 1-",length(notdone),"
#PBS -l walltime=0:10:00
#PBS -l ncpus=100
#PBS -j oe
#PBS -o log/log_^array_index^
#PBS -m e
#PBS -M adam.wilson@yale.edu
#PBS -N MOD06

## cd to working directory
cd /nobackupp1/awilso10/mod06

## set some memory limits
#  ulimit -d 1500000 -m 1500000 -v 1500000  #limit memory usage
  source /usr/local/lib/global.profile
## export a few important variables
  export PATH=$PATH:/nobackupp1/awilso10/bin:/nobackupp1/awilso10/software/bin
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/nobackupp1/awilso10/software/lib
  export MRTDATADIR=/nobackupp1/awilso10/software/heg/data
  export PGSHOME=/nobackupp1/awilso10/software/heg
  export MRTBINDIR=/nobackup1/awilso10/software/TOOLKIT_MTD
  export R_LIBS=\"/u/awilso10/R/x86_64-unknown-linux-gnu-library/2.15/\"
## load modules
  module load gcc mpi-sgi/mpt.2.06r6 hdf4 udunits R
## Run the script!
  Rscript --verbose --vanilla /u/awilso10/environmental-layers/climate/procedures/MOD06_L2_data_compile_Pleiades.r i=${PBS_ARRAY_INDEX}
rm -r $TMPDIR
exit 0
",sep=""),file="MOD06_process")

### Check the file
system("cat MOD06_process")
#system("chmod +x MOD06_process")

## Submit it!
#system("qsub -q devel MOD06_process")
system("qsub MOD06_process")

## check progress
system("qstat -u awilso10")
system("qstat -t 391843[]")
system("qstat -f 391843[2]")

#system("qstat devel ") 
#system("qstat | grep awilso10") 

                                        #print(paste(max(0,length(system("qstat",intern=T))-2)," processes running"))
# system("ssh c0-8.farm.caes.ucdavis.edu")
# system("qalter -p +1024 25964")  #decrease priority of job to run extraction below.
system("cat log/InterpScript.o55934.2")

## check log
system(paste("cat",list.files("log",pattern="InterpScript",full=T)[100]))
#system(paste("cat",list.files("log",pattern="InterpScript",full=T)[13]," | grep \"Temporary Directory\""))

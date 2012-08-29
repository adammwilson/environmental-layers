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

## write it out
save(fs,file="allfiles.Rdata")
save(alldates,file="alldates.Rdata")

notdonedates=alldates
save(notdonedates,file="notdonedates.Rdata")


## output ROI
#get bounding box of region in m
#ge=SpatialPoints(data.frame(lon=c(-125,-115),lat=c(40,47)))
#projection(ge)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#ge2=spTransform(ge, CRS(" +proj=sinu +lon_0=0 +x_0=0 +y_0=0"))

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
cat("
#PBS -S /bin/csh
#PBS -N cfd
# This example uses the Harpertown nodes
# User job can access ~7.6 GB of memory per Harpertown node.
# A memory intensive job that needs more than ~0.9 GB
# per process should use less than 8 cores per node
# to allow more memory per MPI process. This example
# asks for 64 nodes and 4 MPI processes per node.
# This request implies 64x4 = 256 MPI processes for the job.
#PBS -l select=64:ncpus=8:mpiprocs=4:model=har
#PBS -l walltime=4:00:00
#PBS -j oe
#PBS -W group_list=a0801
#PBS -m e

# Load some modules
module load gcc
module load hdf5
module load netcdf/4.1.3/gcc/mpt
module load mpi
module load tcl-tk/8.5.11
module load udunits/2.1.19
module load szip/2.1/gcc
module load R
module load git

# By default, PBS executes your job from your home directory.
# However, you can use the environment variable
# PBS_O_WORKDIR to change to the directory where
# you submitted your job.

cd $PBS_O_WORKDIR

# use of dplace to pin processes to processors may improve performance
# Here you request to pin processes to processors 2, 3, 6, 7 of each node.
# This helps for using the Harpertown nodes, but not for Nehalem-EP or
# Westmere-EP nodes

# The resource request of select=64 and mpiprocs=4 implies
# that you want to have 256 MPI processes in total.
# If this is correct, you can omit the -np 256 for mpiexec
# that you might have used before.

mpiexec dplace -s1 -c2,3,6,7 ./grinder < run_input > output

# It is a good practice to write stderr and stdout to a file (ex: output)
# Otherwise, they will be written to the PBS stderr and stdout in /PBS/spool,
# which has limited amount  of space. When /PBS/spool is filled up, any job
# that tries to write to /PBS/spool will die.

# -end of script-

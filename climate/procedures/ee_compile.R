###  Script to compile the monthly cloud data from earth engine into a netcdf file for further processing

setwd("~/acrobates/adamw/projects/cloud")
library(raster)
library(doMC)

registerDoMC(10)

tempdir="tmp"
if(!file.exists(tempdir)) dir.create(tempdir)

##  Get list of available files
df=data.frame(path=list.files("data/mod09",pattern="*.tif$",full=T),stringsAsFactors=F)
df[,c("region","year","month")]=do.call(rbind,strsplit(basename(df$path),"_|[.]"))[,c(2,3,4)]
df$date=as.Date(paste(df$year,"_",df$month,"_15",sep=""),"%Y_%m_%d")

table(df$year,df$month)
## drop some if not complete
df=df[df$year<=2006,]

## Loop over existing months to build composite netcdf files
foreach(date=unique(df$date)) %dopar% {
## get date
  print(date)
  ## Define output and check if it already exists
  ncfile=paste(tempdir,"/mod09_",date,".nc",sep="")
  if(file.exists(ncfile)) next
  ## merge regions to a new netcdf file
  system(paste("gdal_merge.py -o ",ncfile," -of netCDF -ot Byte -n 0 ",paste(df$path[df$date==date],collapse=" ")))
  system(paste("ncecat -O -u time ",ncfile," ",ncfile,sep=""))
## create temporary nc file with time information to append to MOD06 data
  cat(paste("
    netcdf time {
      dimensions:
        time = 1 ;
      variables:
        int time(time) ;
      time:units = \"days since 2000-01-01 00:00:00\" ;
      time:calendar = \"gregorian\";
      time:long_name = \"time of observation\"; 
    data:
      time=",as.integer(date-as.Date("2000-01-01")),";
    }"),file=paste(tempdir,"/",date,"_time.cdl",sep=""))
system(paste("ncgen -o ",tempdir,"/",date,"_time.nc ",tempdir,"/",date,"_time.cdl",sep=""))
system(paste("ncks -A ",tempdir,"/",date,"_time.nc ",ncfile,sep=""))
## add other attributes
  system(paste("ncrename -v Band1,CF ",ncfile,sep=""))
  system(paste("ncatted ",
" -a units,CF,o,c,\"Proportion Days Cloudy\" ",
" -a missing_value,CF,o,b,255 ",
" -a _FillValue,CF,d,, ", 
" -a valid_range,CF,o,b,\"0,100\" ",
" -a long_name,CF,o,c,\"Proportion cloudy days (%)\" ",
ncfile,sep=""))
## add the fillvalue attribute back (without changing the actual values)
system(paste("ncatted -a _FillValue,CF,o,b,255 ",ncfile,sep=""))

if(as.numeric(system(paste("cdo -s ntime ",ncfile),intern=T))<1) {
  print(paste(ncfile," has no time, deleting"))
  file.remove(ntime)
}
  print(paste(basename(ncfile)," Finished"))

}

### merge all the tiles to a single global composite
#system(paste("ncdump -h ",list.files(tempdir,pattern="mod09.*.nc$",full=T)[10]))
system(paste("cdo -O mergetime ",paste(list.files(tempdir,pattern="mod09.*.nc$",full=T),collapse=" ")," data/mod09.nc"))


### generate the monthly mean and sd
system(paste("cdo -O merge -ymonmean data/mod09.nc -chname,CF,CF_sd -ymonstd data/mod09.nc -chname,CF,CF_annual -timmean data/mod09.nc data/mod09_clim.nc"))



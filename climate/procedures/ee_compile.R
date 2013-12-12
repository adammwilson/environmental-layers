###  Script to compile the monthly cloud data from earth engine into a netcdf file for further processing

setwd("~/acrobates/adamw/projects/cloud")
library(raster)
library(doMC)

registerDoMC(4)
beginCluster(4)

tempdir="tmp"
if(!file.exists(tempdir)) dir.create(tempdir)

##  Get list of available files
df=data.frame(path=list.files("/mnt/data2/projects/cloud/mod09",pattern="*.tif$",full=T),stringsAsFactors=F)
df[,c("region","year","month")]=do.call(rbind,strsplit(basename(df$path),"_|[.]"))[,c(2,3,4)]
df$date=as.Date(paste(df$year,"_",df$month,"_15",sep=""),"%Y_%m_%d")

table(df$year,df$month)#,df$region)

## drop some if not complete
#df=df[df$year<=2009,]
rerun=T  # set to true to recalculate all dates even if file already exists

## Loop over existing months to build composite netcdf files
foreach(date=unique(df$date)) %dopar% {
## get date
  print(date)
  ## Define output and check if it already exists
  ncfile=paste(tempdir,"/mod09_",date,".nc",sep="")
  if(!rerun&file.exists(ncfile)) next
  ## merge regions to a new netcdf file
  system(paste("gdal_merge.py -o ",ncfile," -of netCDF -ot Byte ",paste(df$path[df$date==date],collapse=" ")))
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
" -a valid_range,CF,o,b,\"0,100\" ",
" -a long_name,CF,o,c,\"Proportion cloudy days (%)\" ",
ncfile,sep=""))
#" -a missing_value,CF,o,b,0 ",
#" -a _FillValue,CF,o,b,0 ", 
## add the fillvalue attribute back (without changing the actual values)
#system(paste("ncatted -a _FillValue,CF,o,b,255 ",ncfile,sep=""))

if(as.numeric(system(paste("cdo -s ntime ",ncfile),intern=T))<1) {
  print(paste(ncfile," has no time, deleting"))
  file.remove(ncfile)
}
  print(paste(basename(ncfile)," Finished"))

}





### merge all the tiles to a single global composite
#system(paste("ncdump -h ",list.files(tempdir,pattern="mod09.*.nc$",full=T)[10]))
system(paste("cdo -O mergetime ",paste(list.files(tempdir,pattern="mod09.*.nc$",full=T),collapse=" ")," data/mod09.nc"))


### generate the monthly mean and sd
#system(paste("cdo -P 10 -O merge -ymonmean data/mod09.nc -chname,CF,CF_sd -ymonstd data/mod09.nc data/mod09_clim.nc"))
system(paste("cdo  -O -ymonmean data/mod09.nc data/mod09_clim_mean.nc"))
system(paste("cdo  -O -chname,CF,CF_sd -ymonstd data/mod09.nc data/mod09_clim_sd.nc"))

#  Overall mean
system(paste("cdo -O  -chname,CF,CF_annual -timmean data/mod09.nc  data/mod09_clim_mac.nc"))

### Long term summaries
seasconc <- function(x,return.Pc=T,return.thetat=F) {
          #################################################################################################
          ## Precipitation Concentration function
          ## This function calculates Precipitation Concentration based on Markham's (1970) technique as described in Schulze (1997)
          ## South Africa Atlas of Agrohydology and Climatology - R E Schulze, M Maharaj, S D Lynch, B J Howe, and B Melvile-Thomson
          ## Pages 37-38
          #################################################################################################
          ## x is a vector of precipitation quantities - the mean for each factor in "months" will be taken,
          ## so it does not matter if the data are daily or monthly, as long as the "months" factor correctly
          ## identifies them into 12 monthly bins, collapse indicates whether the data are already summarized as monthly means.
          #################################################################################################
          theta=seq(30,360,30)*(pi/180)                                       # set up angles for each month & convert to radians
                  if(sum(is.na(x))==12) { return(cbind(Pc=NA,thetat=NA)) ; stop}
                  if(return.Pc) {
                              rt=sqrt(sum(x * cos(theta))^2 + sum(x * sin(theta))^2)    # the magnitude of the summation
                                        Pc=as.integer(round((rt/sum(x))*100))}
                  if(return.thetat){
                              s1=sum(x*sin(theta),na.rm=T); s2=sum(x*cos(theta),na.rm=T)
                                        if(s1>=0 & s2>=0)  {thetat=abs((180/pi)*(atan(sum(x*sin(theta),na.rm=T)/sum(x*cos(theta),na.rm=T))))}
                                        if(s1>0 & s2<0)  {thetat=180-abs((180/pi)*(atan(sum(x*sin(theta),na.rm=T)/sum(x*cos(theta),na.rm=T))))}
                                        if(s1<0 & s2<0)  {thetat=180+abs((180/pi)*(atan(sum(x*sin(theta),na.rm=T)/sum(x*cos(theta),na.rm=T))))}
                                        if(s1<0 & s2>0)  {thetat=360-abs((180/pi)*(atan(sum(x*sin(theta),na.rm=T)/sum(x*cos(theta),na.rm=T))))}
                             thetat=as.integer(round(thetat))
                            }
                  if(return.thetat&return.Pc) return(c(conc=Pc,theta=thetat))
                  if(return.Pc)          return(Pc)
                  if(return.thetat)  return(thetat)
        }


## read in monthly dataset
mod09=brick("data/mod09_clim_mean.nc",varname="CF")
plot(mod09[1])

mod09_seas=calc(mod09,seasconc,return.Pc=T,return.thetat=F,overwrite=T,filename="data/mod09_seas.nc",NAflag=255,datatype="INT1U")
mod09_seas2=calc(mod09,seasconc,return.Pc=F,return.thetat=T,overwrite=T,filename="data/mod09_seas_theta.nc",datatype="INT1U")

plot(mod09_seas)

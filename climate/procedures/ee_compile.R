###  Script to compile the monthly cloud data from earth engine into a netcdf file for further processing

setwd("~/acrobates/adamw/projects/cloud")
library(raster)
library(doMC)
library(multicore)
library(foreach)
#library(doMPI)
registerDoMC(10)
#beginCluster(4)

tempdir="tmp"
if(!file.exists(tempdir)) dir.create(tempdir)


## Load list of tiles
tiles=read.table("tile_lat_long_10d.txt",header=T)

### Build Tiles

## bin sizes
ybin=30
xbin=30

tiles=expand.grid(ulx=seq(-180,180-xbin,by=xbin),uly=seq(90,-90+ybin,by=-ybin))
tiles$h=factor(tiles$ulx,labels=paste("h",sprintf("%02d",1:length(unique(tiles$ulx))),sep=""))
tiles$v=factor(tiles$uly,labels=paste("v",sprintf("%02d",1:length(unique(tiles$uly))),sep=""))
tiles$tile=paste(tiles$h,tiles$v,sep="")
tiles$urx=tiles$ulx+xbin
tiles$ury=tiles$uly
tiles$lrx=tiles$ulx+xbin
tiles$lry=tiles$uly-ybin
tiles$llx=tiles$ulx
tiles$lly=tiles$uly-ybin
tiles$cy=(tiles$uly+tiles$lry)/2
tiles$cx=(tiles$ulx+tiles$urx)/2
tiles=tiles[,c("tile","h","v","ulx","uly","urx","ury","lrx","lry","llx","lly","cx","cy")]

jobs=expand.grid(tile=tiles$tile,year=2000:2012,month=1:12)
jobs[,c("ulx","uly","urx","ury","lrx","lry","llx","lly")]=tiles[match(jobs$tile,tiles$tile),c("ulx","uly","urx","ury","lrx","lry","llx","lly")]

## drop Janurary 2000 from list (pre-modis)
jobs=jobs[!(jobs$year==2000&jobs$month==1),]

#jobs=jobs[jobs$month==1,]


#jobs=jobs[jobs$month==7,]
## Run the python downloading script
#system("~/acrobates/adamw/projects/environmental-layers/climate/procedures/ee.MOD09.py -projwin -159 20 -154.5 18.5 -year 2001 -month 6 -region test")   
i=1
#testtiles=c("h02v07","h02v06","h02v08","h03v07","h03v06","h03v08")
#todo=which(jobs$tile%in%testtiles)
#todo=todo[1:3]
#todo=1
todo=1:nrow(jobs)

checkcomplete=T
if(checkcomplete&exists("df")){  #if desired (and "df" exists from below) drop complete date-tiles
todo=which(!paste(jobs$tile,jobs$year,jobs$month)%in%paste(df$region,df$year,df$month))
}

writeLines(paste("Tiling options will produce",nrow(tiles),"tiles and ",nrow(jobs),"tile-months.  Current todo list is ",length(todo)))

foreach(i=todo) %dopar%{
    system(paste("python ~/acrobates/adamw/projects/environmental-layers/climate/procedures/ee.MOD09.py -projwin ",
                      jobs$ulx[i]," ",jobs$uly[i]," ",jobs$urx[i]," ",jobs$ury[i]," ",jobs$lrx[i]," ",jobs$lry[i]," ",jobs$llx[i]," ",jobs$lly[i]," ",
                      "  -year ",jobs$year[i]," -month ",jobs$month[i]," -region ",jobs$tile[i],sep=""))
     }


##  Get list of available files
df=data.frame(path=list.files("/mnt/data2/projects/cloud/mod09",pattern="*.tif$",full=T,recur=T),stringsAsFactors=F)
df[,c("region","year","month")]=do.call(rbind,strsplit(basename(df$path),"_|[.]"))[,c(1,2,3)]
df$date=as.Date(paste(df$year,"_",df$month,"_15",sep=""),"%Y_%m_%d")

## add stats to test for missing data
addstats=F
if(addstats){
    df[,c("max","min","mean","sd")]=do.call(rbind.data.frame,mclapply(1:nrow(df),function(i) as.numeric(sub("^.*[=]","",grep("STATISTICS",system(paste("gdalinfo -stats",df$path[i]),inter=T),value=T)))))
    table(df$sd==0)
}

## subset to testtiles?
#df=df[df$region%in%testtiles,]
#df=df[df$month==1,]
table(df$year,df$month)

## drop some if not complete
df=df[df$month==1&df$year%in%c(2001:2002,2005),]
rerun=T  # set to true to recalculate all dates even if file already exists

## Loop over existing months to build composite netcdf files
foreach(date=unique(df$date)) %dopar% {
## get date
  print(date)
  ## Define output and check if it already exists
  tffile=paste(tempdir,"/mod09_",date,".tif",sep="")
  ncfile=paste(tempdir,"/mod09_",date,".nc",sep="")
  if(!rerun&file.exists(ncfile)) next
  ## merge regions to a new netcdf file
#  system(paste("gdal_merge.py -tap -init 5 -o ",ncfile," -n -32768.000 -of netCDF -ot Int16 ",paste(df$path[df$date==date],collapse=" ")))
  system(paste("gdal_merge.py -o ",tffile," -init -32768  -n -32768.000 -ot Int16 ",paste(df$path[df$date==date],collapse=" ")))
  system(paste("gdal_translate -of netCDF ",tffile," ",ncfile," -ot Int16 "))
  file.remove(tffile)
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
#               " -a valid_range,CF,o,b,\"0,100\" ",
               " -a scale_factor,CF,o,f,\"0.1\" ",
               " -a long_name,CF,o,c,\"Proportion cloudy days (%)\" ",
               ncfile,sep=""))

               ## add the fillvalue attribute back (without changing the actual values)
#system(paste("ncatted -a _FillValue,CF,o,b,-32768 ",ncfile,sep=""))

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
xosystem(paste("cdo  -O -ymonmean data/mod09.nc data/mod09_clim_mean.nc"))
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

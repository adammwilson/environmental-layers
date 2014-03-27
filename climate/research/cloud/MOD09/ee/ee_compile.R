###  Script to compile the monthly cloud data from earth engine into a netcdf file for further processing

library(rasterVis)
library(doMC)
library(multicore)
library(foreach)
library(mgcv)
library(RcppOctave)
registerDoMC(12)


## start raster cluster
#beginCluster(5)

setwd("~/acrobates/adamw/projects/cloud")

datadir="/mnt/data2/projects/cloud/"



##  Get list of available files
df=data.frame(path=list.files(paste(datadir,"mcd09ee",sep="/"),pattern="*.tif$",full=T,recur=T),stringsAsFactors=F)
df[,c("month","sensor")]=do.call(rbind,strsplit(basename(df$path),"_|[.]|-"))[,c(5,4)]
df$date=as.Date(paste(2013,"_",df$month,"_15",sep=""),"%Y_%m_%d")


## use ramdisk?
tmpfs="tmp/"#tempdir()


ramdisk=F
if(ramdisk) {
    system("sudo mkdir -p /mnt/ram")
    system("sudo mount -t ramfs -o size=30g ramfs /mnt/ram")
    system("sudo chmod a+w /mnt/ram")
    tmpfs="/mnt/ram"
}

rasterOptions(tmpdir=tmpfs,overwrite=T, format="GTiff",maxmemory=1e9)


rerun=T  # set to true to recalculate all dates even if file already exists


jobs=expand.grid(month=1:12,sensor=c("MOD09GA","MYD09GA"))
i=1

#jobs=jobs[jobs$sensor=="MYD09",]


## Loop over data to mosaic tifs, compress, and add metadata
    foreach(i=1:nrow(jobs)) %dopar% {
        ## get month
        m=jobs$month[i]
        date=df$date[df$month==m][1]
        print(date)
        ## get sensor
        s=jobs$sensor[i]
        s2=sub("GA","",s)

        ## Define output and check if it already exists
        tvrt=paste(tmpfs,"/",s2,"_",sprintf("%02d", m),".vrt",sep="")
        ttif1=paste(tmpfs,"/",s2,"_",sprintf("%02d", m),".tif",sep="")
        ttif2=paste(datadir,"/mcd09tif/",s2,"_",sprintf("%02d", m),".tif",sep="")

        ## check if output already exists
        if(!rerun&file.exists(ttif1)) return(NA)
        ## build VRT to merge tiles
        proj="'+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'"
        system(paste("gdalbuildvrt -b 1 -b 2 ",tvrt," ",paste(df$path[df$month==m&df$sensor==s],collapse=" ")))
        ## Merge to geotif in temporary directory
        ## specify sourc projection because it gts it slightly wrong by default
        ops=paste("-s_srs ",proj,"  -t_srs 'EPSG:4326' -multi -srcnodata 255 -ot Byte -srcnodata -128 -dstnodata 255 -r bilinear -te -180 -90 180 90 -tr 0.008333333333333 -0.008333333333333",
            "-co BIGTIFF=YES -ot Byte --config GDAL_CACHEMAX 500 -wm 500 -wo NUM_THREADS:10 -co COMPRESS=LZW -co PREDICTOR=2")
        system(paste("gdalwarp -overwrite ",ops," ",tvrt," ",ttif1))

        ## Compress file and add metadata tags
        ops2=paste("-ot Byte -co COMPRESS=LZW -co PREDICTOR=2 -stats")
        tags=c(paste("TIFFTAG_IMAGEDESCRIPTION='Monthly Cloud Frequency for 2000-2013 extracted from C5 MODIS ",s,"GA PGE11 internal cloud mask algorithm (embedded in state_1km bit 10).",
            "The daily cloud mask time series were summarized to mean cloud frequency (CF) by calculating the proportion of cloudy days. ",
            "Band Descriptions: 1) Mean Monthly Cloud Frequency 2) Standard Deviation of Mean Monthly Cloud'"),
              "TIFFTAG_DOCUMENTNAME='Collection 5 ",s," Cloud Frequency'",
              paste("TIFFTAG_DATETIME='2013",sprintf("%02d", m),"15'",sep=""),
              "TIFFTAG_ARTIST='Adam M. Wilson (adam.wilson@yale.edu)'")
        system(paste("gdal_translate  ",ops2," ",paste("-mo ",tags,sep="",collapse=" ")," ",ttif1," ",ttif2))

        ## delete temporary files
        file.remove(tvrt,ttif1)
        writeLines(paste("Month:",m," Sensor:",s," Finished"))
    }




       

## Create combined (MOD+MYD) uncorrected mean CF
#    foreach(i=1:12) %dopar% {
#        ## get files
#        f=list.files("/mnt/data2/projects/cloud/mcd09tif",pattern=paste(".*[O|Y].*_",i,"[.]tif$",sep=""),full=T)
#        ## Define output and check if it already exists
#        tmcd=paste("/mnt/data2/projects/cloud/mcd09tif/MCD09_",sprintf("%02d", i),".tif",sep="")
#        ## check if output already exists
#        ops=paste("-t_srs 'EPSG:4326' -multi -srcnodata 255 -dstnodata 255 -r bilinear -te -180 -90 180 90 -tr 0.008333333333333 -0.008333333333333",
#            "-co BIGTIFF=YES -ot Byte --config GDAL_CACHEMAX 500 -wm 500 -wo NUM_THREADS:10 -wo SOURCE_EXTRA=5")
#        system(paste("gdalwarp -overwrite -r average -co COMPRESS=LZW -co ZLEVEL=9  ",ops," ",paste(f,collapse=" ")," ",tmcd))
#        ## update metadata
#        tags=c(paste("TIFFTAG_IMAGEDESCRIPTION='Monthly Cloud Frequency for 2000-2013 extracted from C5 MODIS MOD09GA and MYD09GA PGE11 internal cloud mask algorithm (embedded in state_1km bit 10).",
#            "The daily cloud mask time series were summarized to mean cloud frequency (CF) by calculating the proportion of cloudy days. ",
#            "Band Descriptions: 1) Mean Monthly Cloud Frequency 2) Four Times the Standard Deviation of Mean Monthly Cloud Frequency",
#            " 3) Mean number of daily observations for each pixel 4) Proportion of days with at least one observation '"),
#            "TIFFTAG_DOCUMENTNAME='Collection 5 MCD09 Cloud Frequency'",
#            paste("TIFFTAG_DATETIME='2013",sprintf("%02d", i),"15'",sep=""),
#              "TIFFTAG_ARTIST='Adam M. Wilson (adam.wilson@yale.edu)'")
#        system(paste("/usr/local/src/gdal-1.10.0/swig/python/scripts/gdal_edit.py ",tmcd," ",paste("-mo ",tags,sep="",collapse=" "),sep=""))
#        writeLines(paste("Finished month",i))
#    }


### Perform bias correction
foreach(i=1:nrow(jobs)) %dopar% {
        ## get month
        m=jobs$month[i]
        date=df$date[df$month==m][1]
        print(date)

        ttif1=paste(datadir,"/mcd09tif/",s,"_",sprintf("%02d", m),".tif",sep="")
        ttif2=paste(tmpfs,"/",s,"_",m,"_wgs84.tif",sep="")
        ncfile=paste("data/mcd09nc/",s,"_",m,".nc",sep="")

        ## 
        mod=stack(ttif1)
        names(mod)=c("cf","cfsd","nobs","pobs")

        ## set up processing chunks
        nrw=nrow(mod)
        nby=20
        nrwg=seq(1,nrw,by=nby)
        writeLines(paste("Processing ",length(nrwg)," groups and",nrw,"lines"))

        output=mclapply(nrwg,function(ti){
            ## Extract focal areas
            nr=51
            nc=101
            ngb=c(nr,nc)
            vals_cf=getValuesFocal(mod[[c("cf","pobs")]],ngb=ngb,row=ti,nrows=nby)
            vals_obs=getValuesFocal(obs,ngb=ngb,row=ti,nrows=nby)
            ## extract bias
      bias=raster(matrix(do.call(rbind,lapply(1:nrow(vals_cf),function(i) {
#          if(i%in%round(seq(1,nrow(vals_cf),len=100))) print(i)
          tobs=vals_obs[i,]  #vector of indices
          tval=vals_cf[i,]    # vector of values
          lm1=lm(tval~tobs,na.rm=T)
          dif=round(predict(lm1)-predict(lm1,newdata=data.frame(tobs=median(tobs,na.rm=T))))
          return(dif)  
            })),nrow=nby,ncol=ncol(d),byrow=T))     # turn it back into a raster
        ## update raster and write it
        extent(bias)=extent(d[ti:(ti+nby-1),1:ncol(d),drop=F])
        projection(bias)=projection(d)
        NAvalue(bias) <- 255
        writeRaster(bias,file=paste("data/bias/tiles/bias_",ti,".tif",sep=""),
                                  format="GTiff",dataType="INT1U",overwrite=T,NAflag=255) #,options=c("COMPRESS=LZW","ZLEVEL=9")
    print(ti)
  }
)

        

        modpts=sampleRandom(cmod, size=10000, na.rm=TRUE, xy=T, sp=T)
        rm(cmod)  #remove temporary raster to save space
        modpts=modpts[modpts$nobs>0,]  #drop data from missing tiles
        ### fit popbs correctionmodel (accounting for spatial variation in cf)
        modlm1=bam(cf~s(x,y)+pobs,data=modpts@data)
        summary(modlm1)
        modbeta1=coef(modlm1)["pobs"]

        writeLines(paste(date,"       slope:",round(modbeta1,4)))

        ## mask no data regions (with less than 1 observation per day within that month)
        ## use model above to correct for orbital artifacts
        biasf=function(cf,cfsd,nobs,pobs) {
            ## drop data in areass with nobs<1
            drop=nobs<0|pobs<=50
            cf[drop]=NA
            cfsd[drop]=NA
            nobs[drop]=NA
            pobs[drop]=NA
            bias=round((100-pobs)*modbeta1)
            cfc=cf+bias
            return(c(cf=cf,cfsd=cfsd,bias=bias,cfc=cfc,nobs=nobs,pobs=pobs))}
        
#        treg=extent(c(-1434564.00523,1784892.95369, 564861.173869, 1880991.3772))
#        treg=extent(c(-2187230.72881, 4017838.07688,  -339907.592509, 3589340.63805))  #all sahara

        mod2=overlay(cmod,fun=biasf,unstack=TRUE,filename=ttif1,format="GTiff",
            dataType="INT1U",overwrite=T,NAflag=255, options=c("COMPRESS=LZW", "BIGTIFF=YES"))

        ## warp to wgs84
        ops=paste("-t_srs 'EPSG:4326' -multi -srcnodata 255 -dstnodata 255 -r bilinear -te -180 -90 180 90 -tr 0.008333333333333 -0.008333333333333",
            "-co BIGTIFF=YES -ot Byte --config GDAL_CACHEMAX 500 -wm 500 -wo NUM_THREADS:10 -wo SOURCE_EXTRA=5")
#        ops=paste("-t_srs 'EPSG:4326' -srcnodata 255 -dstnodata 255 -multi -r bilinear -tr 0.008333333333333 -0.008333333333333",
#            "-co BIGTIFF=YES -ot Byte --config GDAL_CACHEMAX 300000 -wm 300000 -wo NUM_THREADS:10 -wo SOURCE_EXTRA=5")
        system(paste("gdalwarp -overwrite ",ops," ",ttif1," ",ttif2))
        
        ##  convert to netcdf, subset to mean/sd bands
        trans_ops=paste(" -co COMPRESS=DEFLATE -a_nodata 255 -stats -co FORMAT=NC4 -co ZLEVEL=9 -b 4 -b 2")
        system(paste("gdal_translate -of netCDF ",trans_ops," ",ttif2," ",ncfile))
        ## file.remove(temptffile)
        system(paste("ncecat -O -u time ",ncfile," ",ncfile,sep=""))
        ## create temporary nc file with time information to append to CF data
        cat(paste("
    netcdf time {
      dimensions:
        time = 1 ;
      variables:
        int time(time) ;
      time:units = \"days since 2000-01-01 ",ifelse(s=="MOD09","10:30:00","13:30:00"),"\" ;
      time:calendar = \"gregorian\";
      time:long_name = \"time of observation\"; 
    data:
      time=",as.integer(date-as.Date("2000-01-01")),";
    }"),file=paste(tempdir(),"/",date,"_time.cdl",sep=""))
        system(paste("ncgen -o ",tempdir(),"/",date,"_time.nc ",tempdir(),"/",date,"_time.cdl",sep=""))
        ## add time dimension to ncfile and compress (deflate)
        system(paste("ncks --fl_fmt=netcdf4 -L 9 -A ",tempdir(),"/",date,"_time.nc ",ncfile,sep=""))
        ## add other attributes
        system(paste("ncrename -v Band1,CF ",ncfile,sep=""))
        system(paste("ncrename -v Band2,CFsd ",ncfile,sep=""))
        ## build correction factor explanation
        system(paste("ncatted ",
                     ## CF Mean
                     " -a units,CF,o,c,\"%\" ",
                     " -a valid_range,CF,o,ub,\"0,100\" ",
                                        #               " -a scale_factor,CF,o,b,\"0.1\" ",
                     " -a _FillValue,CF,o,ub,\"255\" ",
                     " -a missing_value,CF,o,ub,\"255\" ",
                     " -a long_name,CF,o,c,\"Cloud Frequency (%)\" ",
                     " -a correction_factor_description,CF,o,c,\"To account for variable observation frequency, CF in each pixel was adjusted by the proportion of days with at least one MODIS observation\" ",
                     " -a correction_factor,CF,o,f,\"",round(modbeta1,4),"\" ",
                     " -a NETCDF_VARNAME,CF,o,c,\"Cloud Frequency (%)\" ",
                     ## CF Standard Deviation
                     " -a units,CFsd,o,c,\"SD\" ",
                     " -a valid_range,CFsd,o,ub,\"0,200\" ",
                     " -a scale_factor,CFsd,o,f,\"0.25\" ",
                     " -a _FillValue,CFsd,o,ub,\"255\" ",
                     " -a missing_value,CFsd,o,ub,\"255\" ",
                     " -a long_name,CFsd,o,c,\"Cloud Frequency (%) Intra-month (2000-2013) Standard Deviation\" ",
                     " -a NETCDF_VARNAME,CFsd,o,c,\"Cloud Frequency (%) Intra-month (2000-2013) Standard Deviation\" ",
                     ## global
                     " -a title,global,o,c,\"Cloud Climatology from MOD09 Cloud Mask\" ",
                     " -a institution,global,o,c,\"Jetz Lab, EEB, Yale University, New Haven, CT\" ",
                     " -a source,global,o,c,\"Derived from MOD09GA Daily Data\" ",
                     " -a comment,global,o,c,\"Developed by Adam M. Wilson (adam.wilson@yale.edu / http://adamwilson.us)\" ",
                     ncfile,sep=""))

        ## add the fillvalue attribute back (without changing the actual values)
                                        #system(paste("ncatted -a _FillValue,CF,o,b,-32768 ",ncfile,sep=""))
        
        if(as.numeric(system(paste("cdo -s ntime ",ncfile),intern=T))<1) {
            print(paste(ncfile," has no time, deleting"))
            file.remove(ncfile)
        }
        print(paste(basename(ncfile)," Finished"))
        
        
    }

if(ramdisk) {
    ## unmount the ram disk
    system(paste("sudo umount ",tmpfs)
}


### merge all the tiles to a single global composite
#system(paste("ncdump -h ",list.files(tempdir,pattern="mod09.*.nc$",full=T)[10]))
file.remove("tmp/mod09_2000-01-15.nc")
system(paste("cdo -O  mergetime -setrtomiss,-32768,-1 ",paste(list.files(tempdir,pattern="mod09.*.nc$",full=T),collapse=" ")," data/cloud_monthly.nc"))

#  Overall mean
system(paste("cdo -O  timmean data/cloud_monthly.nc  data/cloud_mean.nc"))

### generate the monthly mean and sd
#system(paste("cdo -P 10 -O merge -ymonmean data/mod09.nc -chname,CF,CF_sd -ymonstd data/mod09.nc data/mod09_clim.nc"))
system(paste("cdo  -f nc4c -O -ymonmean data/cloud_monthly.nc data/cloud_ymonmean.nc"))


## Seasonal Means
system(paste("cdo  -f nc4c -O -yseasmean data/cloud_monthly.nc data/cloud_yseasmean.nc"))
system(paste("cdo  -f nc4c -O -yseasstd data/cloud_monthly.nc data/cloud_yseasstd.nc"))

## standard deviations, had to break to limit memory usage
system(paste("cdo  -f nc4c -z zip -O -chname,CF,CF_sd -ymonstd -selmon,1,2,3,4,5,6 data/cloud_monthly.nc data/cloud_ymonsd_1-6.nc"))
system(paste("cdo  -f nc4c -z zip -O -chname,CF,CF_sd -ymonstd -selmon,7,8,9,10,11,12 data/cloud_monthly.nc data/cloud_ymonsd_7-12.nc"))
system(paste("cdo  -f nc4c -z zip -O mergetime  data/cloud_ymonsd_1-6.nc  data/cloud_ymonsd_7-12.nc data/cloud_ymonstd.nc"))

system("cdo -f nc4c -z zip  timmin data/cloud_ymonmean.nc data/cloud_min.nc")
system("cdo -f nc4c -z zip  timmax data/cloud_ymonmean.nc data/cloud_max.nc")

## standard deviation of mean monthly values give intra-annual variability
system("cdo -f nc4c -z zip -chname,CF,CFsd -timstd data/cloud_ymonmean.nc data/cloud_std_intra.nc")
## mean of monthly standard deviations give inter-annual variability 
system("cdo -f nc4c -z zip -chname,CF,CFsd -timmean data/cloud_ymonstd.nc data/cloud_std_inter.nc")


# Regressions through time by season
s=c("DJF","MAM","JJA","SON")

system(paste("cdo  -f nc4c -O regres -selseas,",s[1]," data/cloud_monthly.nc data/slope_",s[1],".nc",sep=""))
system(paste("cdo  -f nc4c -O regres -selseas,",s[2]," data/cloud_monthly.nc data/slope_",s[2],".nc",sep=""))
system(paste("cdo  -f nc4c -O regres -selseas,",s[3]," data/cloud_monthly.nc data/slope_",s[3],".nc",sep=""))
system(paste("cdo  -f nc4c -O regres -selseas,",s[4]," data/cloud_monthly.nc data/slope_",s[4],".nc",sep=""))



## Daily animations
regs=list(
    Venezuela=extent(c(-69,-59,0,7)),
    Cascades=extent(c(-122.8,-118,44.9,47)),
    Hawaii=extent(c(-156.5,-154,18.75,20.5)),
    Boliva=extent(c(-71,-63,-20,-15)),
    CFR=extent(c(17.75,22.5,-34.8,-32.6)),
    Madagascar=extent(c(46,52,-17,-12))
    )

r=1

system(paste("cdo  -f nc4c -O inttime,2012-01-15,12:00:00,7day  -sellonlatbox,",
             paste(regs[[r]]@xmin,regs[[r]]@xmax,regs[[r]]@ymin,regs[[r]]@ymax,sep=","),
             "  data/cloud_monthly.nc data/daily_",names(regs[r]),".nc",sep=""))




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
mod09=brick("data/cloud_ymonmean.nc",varname="CF")
plot(mod09[1])

mod09_seas=calc(mod09,seasconc,return.Pc=T,return.thetat=F,overwrite=T,filename="data/mod09_seas.nc",NAflag=255,datatype="INT1U")
mod09_seas2=calc(mod09,seasconc,return.Pc=F,return.thetat=T,overwrite=T,filename="data/mod09_seas_theta.nc",datatype="INT1U")

plot(mod09_seas)

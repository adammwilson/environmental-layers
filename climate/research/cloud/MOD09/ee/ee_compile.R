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


### Download files from google drive
## This only works if google-cli is installed and has already been authenticated 
download=T
if(download) system(paste("google docs get 2014*_g3_* ",datadir,"/mcd09ee",sep=""))


##  Get list of available files
version="g3"  #which version of data from EE?
df=data.frame(path=list.files(paste(datadir,"mcd09ee",sep="/"),pattern=paste(".*",version,".*.tif$",sep=""),full=T,recur=T),stringsAsFactors=F)
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

## define month-sensors to process
jobs=unique(data.frame(month=df$month,sensor=df$sensor))

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
        system(paste("gdalbuildvrt -b 1 -b 2 -srcnodata -32768 ",tvrt," ",paste(df$path[df$month==m&df$sensor==s],collapse=" ")))
        ## Merge to geotif in temporary directory
        ## specify sourc projection because it gts it slightly wrong by default #-ot Int16 -dstnodata -32768
        ops=paste("-s_srs ",proj,"  -t_srs 'EPSG:4326' -multi -srcnodata -32768  -ot Int16 -dstnodata -32768 -r bilinear -te -180 -90 180 90 -tr 0.008333333333333 -0.008333333333333",
            "-co BIGTIFF=YES --config GDAL_CACHEMAX 500 -wm 500 -wo NUM_THREADS:10 -co COMPRESS=LZW -co PREDICTOR=2")
        system(paste("gdalwarp -overwrite ",ops," ",tvrt," ",ttif1))

        ## Compress file and add metadata tags
        ops2=paste("-ot Int16 -co COMPRESS=LZW -co PREDICTOR=2 -stats")
        tags=c(paste("TIFFTAG_IMAGEDESCRIPTION='Monthly Cloud Frequency for 2000-2013 extracted from C5 MODIS ",s,"GA PGE11 internal cloud mask algorithm (embedded in state_1km bit 10).",
            "The daily cloud mask time series were summarized to mean cloud frequency (CF) by calculating the proportion of cloudy days. ",
            "Band Descriptions: 1) Mean Monthly Cloud Frequency x 10000 2) Standard Deviation of Mean Monthly Cloud x 10000'"),
              "TIFFTAG_DOCUMENTNAME='Collection 5 ",s," Cloud Frequency'",
              paste("TIFFTAG_DATETIME='2013",sprintf("%02d", m),"15'",sep=""),
              "TIFFTAG_ARTIST='Adam M. Wilson (adam.wilson@yale.edu)'")
        system(paste("gdal_translate  ",ops2," ",paste("-mo ",tags,sep="",collapse=" ")," ",ttif1," ",ttif2))

        ## delete temporary files
        file.remove(tvrt,ttif1)
        writeLines(paste("Month:",m," Sensor:",s," Finished"))
    }




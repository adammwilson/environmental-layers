###  Script to compile the monthly cloud data from earth engine into a netcdf file for further processing

library(rasterVis)
library(doMC)
library(foreach)
library(RcppOctave)
library(rgdal)
registerDoMC(7)


# final output will be written to data directory here:
setwd("~/acrobates/adamw/projects/cloud")

# temporary files will be written here:
datadir="/mnt/data2/projects/cloud/"


## Specify path to VSNR souce code and add it to RcppOctave path
mpath="/home/adamw/acrobates/adamw/projects/environmental-layers/climate/research/cloud/MOD09/vsnr/"
.O$addpath(mpath)


#########################################
####  Bias correction functions

fgabor=function(d,theta=-15,x=200,y=5){
    thetaR=(theta*pi)/180
    cds=expand.grid(x=(1:nrow(d))-round(nrow(d)/2),y=(1:ncol(d))-round(ncol(d)/2))
    sigma_x=x
    sigma_y=y
    lambda=0
    tpsi=0
    x_theta=cds[,"x"]*cos(thetaR)+cds[,"y"]*sin(thetaR);
    y_theta=-cds[,"x"]*sin(thetaR)+cds[,"y"]*cos(thetaR);
    n=max(cds)
    gb= 1/(2*pi*sigma_x *sigma_y) * exp(-.5*(x_theta^2/sigma_x^2+y_theta^2/sigma_y^2))*cos(2*pi/n*lambda*x_theta+tpsi);
    gb2=1e-2*gb/max(gb); #Normalization
    psi=d
    values(psi)=matrix(gb2,ncol=ncol(d))
    return(psi)
}
  
     
vsnr=function(d,gabor,alpha=2,p=2,epsilon=0,prec=5e-3,maxit=100,C1=1,full=F){
    ## VSNR can't run with any missing values, set them to zero here then switch them back to NA later
    d2=as.matrix(d)
    d2[is.na(d2)]=0
    ## Process with VSNR
    dt=.CallOctave("VSNR",d2,epsilon,p,as.matrix(gabor),alpha,maxit+1,prec,C1,argout ="u",verbose=T);
    ## Create spatial objects from VSNR output
    dc=d
    values(dc)=as.numeric(t(dt$u))  # Make 'corrected' version of data
    ##  Set NA values in original data back to NA 
    dc[is.na(d)]=NA
    return(dc)
}

rmr=function(x){
    ## function to truly delete raster and temporary files associated with them
        if(class(x)=="RasterLayer"&grepl("^/tmp",x@file@name)&fromDisk(x)==T){
            file.remove(x@file@name,sub("grd","gri",x@file@name))
            rm(x)
    }
}

                    
######################################
## Run the correction functions
###  Subset equitorial region to correct orbital banding


### build table of tiles to process
extf=function(xmin=-180,xmax=180,ymin=-30,ymax=30,size=10,overlap=0.5){
    xmins=unique(sort(c(seq(xmin,xmax-size,by=size),seq(xmin+(overlap*size),xmax-size,by=size))))
    ymins=unique(sort(c(seq(ymin,ymax-size,by=size),seq(ymin+(overlap*size),ymax-size,by=size))))
    exts=expand.grid(xmin=xmins,ymin=ymins)
    exts$ymax=exts$ymin+size
    exts$xmax=exts$xmin+size
    return(exts)
}

df2=list.files(paste(datadir,"/mcd09tif",sep=""),full=T,pattern="[0-9][.]tif$")
i=1
ti=7

### Build the tiles
exts=extf(xmin=-180,xmax=180,ymin=-30,ymax=30,size=60,overlap=0)

## add an extra tile to account for regions of reduced data availability for each sensor
modexts=c(xmin=130,xmax=180,ymin=-50,ymax=0)
mydexts=c(xmin=-170,xmax=-140,ymin=-30,ymax=55)


## loop over sensor-months to create full grid of corrected values
for( i in 1:nrow(df2))){
    file=df2[i]
    outfile=paste(datadir,"/mcd09ctif/",basename(file),sep="")
#    outfile2=paste("data/mcd09tif/",basename(file),sep="")
#    outfile2b=paste("data/mcd09tif/",sub("[.]tif","_sd.tif",basename(file)),sep="")

## set sensor-specific parameters
    ## add extra region for correction depending on which sensor is being processed
    ## set angle of orbital artefacts to be corrected
    sensor=ifelse(grepl("MOD",file),"MOD","MYD")
    if(sensor=="MOD") {
        exts=rbind(exts,modexts)
        scanangle=-15
    }
    if(sensor=="MYD") {
        exts=rbind(exts,mydexts)
        scanangle=15
    }

    ## Process the tiles
    foreach(ti=1:nrow(exts)) %dopar% {
        textent=extent(exts$xmin[ti],exts$xmax[ti],exts$ymin[ti],exts$ymax[ti])
        ## extract the tile
        toutfile=paste("tmp/", sub(".tif","",basename(file)),"_",sprintf("%03d",ti),".tif",sep="")
        writeLines(paste("Starting: ",toutfile," tile:",ti," ( out of ",nrow(exts),")"))
        d=crop(raster(file),textent)
        ## acount for scale of data is 10000*CF
        d=d*.01
        ## skip null tiles - will only have this if tiles are quite small (<10 degrees)
        if(is.null(d@data@values)) return(NULL)
        ## make the gabor kernel
        ## this specifies the 'shape' of the noise we are trying to remove
        psi=fgabor(d,theta=scanangle,x=400,y=4) #3
#        psi=stack(lapply(scanangle,function(a) fgabor(d,theta=a,x=400,y=4))) #3

        ## run the correction function.  
        res=vsnr(d,gabor=psi,alpha=2,p=2,epsilon=1,prec=5e-6,maxit=50,C=1,full=F)
        ## write the file
        if(!is.null(outfile)) writeRaster(res*100,file=toutfile,overwrite=T,datatype='INT2S',options=c("COMPRESS=LZW", "PREDICTOR=2"),NAvalue=-32768)
        ## remove temporary files
        rmr(d);rmr(psi);rmr(res)
        print(paste("Finished Temporary File: ",toutfile))
    }
## create VRT of first band of the full image 
fvrt=sub("[.]tif","_cf.vrt",file)
system(paste("gdalbuildvrt -b 1 ",fvrt," ",file))
## mosaic the tiles with the original data (keeping the new data when available)
tfiles=paste(c(fvrt,list.files("tmp",pattern=paste(sub("[.]tif","",basename(outfile)),"_[0-9]*[.]tif",sep=""),full=T)),collapse=" ")
system(paste("gdal_merge.py -init -32768 -n -32768 -co COMPRESS=LZW -co PREDICTOR=2 -co BIGTIFF=yes  -o ",outfile," ",tfiles,sep="")) 
}


################################################################################
###  calculate monthly means of terra and aqua

# Create combined (MOD+MYD) uncorrected mean CF
    foreach(i=1:12) %dopar% {
        ## get files
        f=list.files(paste(datadir,"/mcd09ctif",sep=""),pattern=paste(".*[O|Y].*_",sprintf("%02d",i),"[.]tif$",sep=""),full=T)
        ## Define output and check if it already exists
        tmcd=paste(datadir,"/mcd09ctif/MCD09_",sprintf("%02d", i),".tif",sep="")
        ## check if output already exists
        ops=paste("-t_srs 'EPSG:4326' -multi -srcnodata -32768 -dstnodata -32768 -r bilinear -te -180 -90 180 90 -tr 0.008333333333333 -0.008333333333333",
            "-co BIGTIFF=YES  --config GDAL_CACHEMAX 500 -wm 500 -wo NUM_THREADS:10 -wo SOURCE_EXTRA=5")
        system(paste("gdalwarp -overwrite -r average -co COMPRESS=LZW -co ZLEVEL=9  ",ops," ",paste(f,collapse=" ")," ",tmcd))
        ## update metadata
        tags=c(paste("TIFFTAG_IMAGEDESCRIPTION='Monthly Cloud Frequency for 2000-2013 extracted from C5 MODIS MOD09GA and MYD09GA PGE11 internal cloud mask algorithm (embedded in state_1km bit 10).",
            "The daily cloud mask time series were summarized to mean cloud frequency (CF) by calculating the proportion of cloudy days. ",
            "Band Descriptions: 1) Mean Monthly Cloud Frequency 2) Four Times the Standard Deviation of Mean Monthly Cloud Frequency"'"),
            "TIFFTAG_DOCUMENTNAME='Collection 5 MCD09 Cloud Frequency'",
            paste("TIFFTAG_DATETIME='2013",sprintf("%02d", i),"15'",sep=""),
              "TIFFTAG_ARTIST='Adam M. Wilson (adam.wilson@yale.edu)'")
        system(paste("/usr/local/src/gdal-1.10.0/swig/python/scripts/gdal_edit.py ",tmcd," ",paste("-mo ",tags,sep="",collapse=" "),sep=""))
        writeLines(paste("Finished month",i))
    }




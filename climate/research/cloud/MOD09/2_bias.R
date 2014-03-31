###  Script to compile the monthly cloud data from earth engine into a netcdf file for further processing

library(rasterVis)
library(doMC)
library(foreach)
library(RcppOctave)
registerDoMC(12)


## start raster cluster
#beginCluster(5)

setwd("~/acrobates/adamw/projects/cloud")

datadir="/mnt/data2/projects/cloud/"

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
  
     
vsnr=function(d,gabor,alpha=10,p=2,epsilon=0,prec=5e-3,maxit=100,C1=1){
#    d=d*1.0
    ## Process with VSNR
    dt=.O$VSNR(as.matrix(d),epsilon,p,as.matrix(gabor),alpha,maxit+1,prec,1,argout = c("u", "Gap", "Primal","Dual","EstP","EstD"));
    ## Create spatial objects from VSNR output
    bias=d
    values(bias)=-as.numeric(t(convolve(dt$EstP,as.matrix(gabor))))
    #bias[abs(bias)<5]=0  # set all bias<0.5 to zero to avoit minor adjustment and added striping in data
    #bias=bias*10
#    dc=d-bias  # Make 'corrected' version of data
#    NAvalue(bias)=0  #set bias==0 to NA to facilate plotting, areas that are NA were not adjusted
#    res=stack(list(dc=dc,d=d,bias=bias)) #,EstP=EstP,EstD1=EstD1,EstD2=EstD2
    res=stack(list(bias=bias)) #,EstP=EstP,EstD1=EstD1,EstD2=EstD2
#    rm(dt,dc)
    rm(dt)
    return(res)
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

### Build the tiles
exts=extf(xmin=-180,xmax=180,ymin=-30,ymax=30,size=10,overlap=0)
#exts=extf(xmin=-10,xmax=20,ymin=0,ymax=30,size=10,overlap=0)
#exts=extf(xmin=-60,xmax=60,ymin=-30,ymax=30,size=10,overlap=0)


## loop over sensor-months to create full grid of corrected values
for( i in 1:length(df2)){
### Process the tiles
foreach(ti=1:nrow(exts)) %dopar% {
    textent=extent(exts$xmin[ti],exts$xmax[ti],exts$ymin[ti],exts$ymax[ti])
    ## extract the tile
    file=df2[i]
    outfile=paste("tmp/",sub(".tif","",basename(file)),"_",sprintf("%03d",ti),".tif",sep="")
    writeLines(paste("Starting: ",outfile," tile:",ti," ( out of ",nrow(exts),")"))
    d=crop(raster(file),textent)
    ## acount for scale of data is 10000*CF
    d=d*.01
    ## skip null tiles
    if(is.null(d@data@values)) return(NULL)
    ## make the gabor kernel
    psi=fgabor(d,theta=-15,x=400,y=4) #3
    ## run the correction
    res=vsnr(d,gabor=psi,alpha=2,p=2,epsilon=1,prec=5e-6,maxit=50,C=1)
    ## write the file
    if(!is.null(outfile)) writeRaster(res,file=outfile,overwrite=T,datatype='INT2S',options=c("COMPRESS=LZW", "PREDICTOR=2"))#,NAflag=-127)
    print(paste("Finished ",outfile))
}
}

## mosaic the tiles
system("ls tmp/test_[0-9]*[.]tif")
system("rm tmp/test2.tif; gdal_merge.py -co COMPRESS=LZW -co PREDICTOR=2 -co BIGTIFF=yes 'tmp/test_[0-9]*[.]tif' -o tmp/test2.tif")
#system("rm tmp/test2.tif; gdalwarp -multi -r average -dstnodata 255 -ot Byte -co COMPRESS=LZW -co PREDICTOR=2 `ls tmp/test_[0-9]*[.]tif` tmp/test2.tif")

res=brick("tmp/test2.tif")
names(res)=c("dc","d","bias")
NAvalue(res)=-32768

tplot=F
if(tplot){
    gcol=colorRampPalette(c("blue","yellow","red"))
    gcol=colorRampPalette(c("black","white"))
    levelplot(res[["bias"]],col.regions=gcol(100),cuts=99,margin=F,maxpixels=1e6)
    levelplot(stack(list(Original=res[["d"]],Corrected=res[["dc"]])),col.regions=gcol(100),cuts=99,maxpixels=1e6)
    levelplot(stack(list(Original=res[["d"]],Corrected=res[["dc"]])),col.regions=gcol(100),cuts=99,maxpixels=1e6,ylim=c(10,13),xlim=c(-7,-1))
}



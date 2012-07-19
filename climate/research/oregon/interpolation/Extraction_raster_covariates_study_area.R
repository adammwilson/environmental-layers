##################################   DATA PREPERATION-EXTRACTION   ###############################
############################ EXTRACT VALUES FROM COVARIATE RASTER STACK ##########################
#PURPOSE: General script to prepare the design matrix dataset for interpolation                  #
#This script extracts values from a list of raster files given some point locations.             #
#Note that this program assumes that:                                                            #
#1)Locations are provided in shape file format with the same reference system                    #  
#   than the raster images.                                                                      #
#2)The user has provided a list of raster images with new short names.                           #
#  The short names are used in in the new shape file being created (two columns space delimited).#
#3)The output names for the new shape file must be specified  and the new values                 #  
#  extracted are joined to the output table.                                                     #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 06/26/212                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#???--                                  #
###################################################################################################

###Loading R library and packages   
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                                          # GDAL wrapper for R, spatial utilities
library(raster)                                         # Raster package for image processing by Hijmans et al. 
library(reshape)

###Parameters and arguments

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"             #Path to all datasets on Atlas
path<-"/home/adamw/acrobates/projects/interp/data/regions/oregon"                                 #Path to all datasets on Atlas

setwd(path)                                                                   #Setting the working directory

infile1<-"station_monthly_PRCP_20120705.shp"                            #Weather station location with interp. var. (TMAX, TMIN or PRCP)

inlistf<-"covar.txt"                                           #Covariates as list of raster files and output names separated by space

                         #Name of raster files should come with extension
### Alternatively, create this list using script: ppt_build_covariatelist.r

## a complicating factor here is that we don't have write permission to other's folders, so may need to change things as follows for personal directory...
outfile<-'station_monthly_PRCP_covariates_20120705'                         #Name of the new shapefile created with covariates extracted at station locations
outpath=path
#inlistf<-paste(outpath,"/covar.txt",sep="")                                           #Covariates as list of raster files and output names separated by space

#######START OF THE SCRIPT #############

###Reading the station data
sdata<-readOGR(dirname(infile1),layer=sub(".shp","",basename(infile1)))       #Reading shape file using rgdal library
 
###Extracting the variables values from the raster files                                             
covarf<-read.table(paste(path,"/",inlistf,sep=""), stringsAsFactors=F)                  #Column 1 contains the names of raster files

covar<- stack(covarf$files)           #Creating a stack of raster images from the list of variables.

### Add slope/aspect transformations
ns=sin(subset(covar,grep("slope",covarf$files,ig=T))*pi/180)*cos(subset(covar,grep("aspect",covarf$files,ig=T))*pi/180)
ew=sin(subset(covar,grep("slope",covarf$files,ig=T))*pi/180)*sin(subset(covar,grep("aspect",covarf$files,ig=T))*pi/180)
covar=stack(covar,ns,ew)
layerNames(covar)<-c(covarf$var,"ns","ew")                                    #Assigning names to the raster layers

## set time attribute to distinguish static and temporal variables
covar=setZ(covar,z=c(sprintf("%02d",covarf$time),"00","00"),name="month")        #assign month indicators (and 00s for static)

## add projection information
projection(covar)=CRS(projection(sdata))

#Extracting values from the raster stack for every point location in coords data frame.
sdata@data=cbind.data.frame(sdata@data,extract(subset(covar,subset=which(getZ(covar)=="00")), sdata))                

### get unique station locations
sdata.u=sdata[!duplicated(sdata@data[,c("station","latitude","longitude")]),c("station","latitude","longitude")]
sdata.u@data[,c("x","y")]=coordinates(sdata.u)
sdata.u@data=cbind.data.frame(sdata.u@data,extract(subset(covar,subset=which(getZ(covar)!="00")), sdata.u))      #Extracting values from the raster stack for every point 
sdata.u=sdata.u@data #drop the spatial-ness

### reshape for easy merging
sdata.ul=melt(sdata.u,id.vars=c("station","latitude","longitude","x","y"))
sdata.ul[,c("metric","type","month")]=do.call(rbind.data.frame,strsplit(as.character(sdata.ul$variable),"_"))
sdata.ul$metric=paste(sdata.ul$metric,sdata.ul$type,sep="_")
sdata.ul2=cast(sdata.ul,station+month~metric)
## create a station_month unique id.  If month column exists (as in monthly data), use it, otherwise extract it from date column (as in daily data)
if(!is.null(sdata$month)) stmo=paste(sdata$station,sprintf("%02d",sdata$month),sep="_")
if(is.null(sdata$month)) stmo=paste(sdata$station,format(as.Date(sdata$date),"%m"),sep="_")

### add monthly data to sdata table by matching unique station_month ids.
sdata@data[,unique(sdata.ul$metric)]=sdata.ul2[
            match(stmo,paste(sdata.ul2$station,sprintf("%02d",sdata.ul2$month),sep="_")),
            unique(sdata.ul$metric)]

###################################################################
### save the data

## save the raster stack for prediction
writeRaster(covar,format="raster",filename=paste(path,"/covariates",sep=""),overwrite=T)
## save layer Zs as a separate csv file because they are lost when saving, argh!
write.table(getZ(covar),paste(path,"/covariates-Z.csv",sep=""),row.names=F,col.names=F,sep=",")

#Save a textfile and shape file of all the subset data
#write.table(as.data.frame(data_RST_SDF),paste(outpath,"/",outfile,".txt",sep=""), sep=",")
writeOGR(sdata, outpath, outfile, driver ="ESRI Shapefile") #Note that the layer name is the file name without extension


##### END OF SCRIPT ##########

##################    Data preparation for interpolation   #######################################
############################ Extraction of station data ##########################################
#This script perform queries on the Postgres database ghcn for stations matching the             #
#interpolation area. It requires the following inputs:                                           #
# 1)the text file ofGHCND  stations from NCDC matching the database version release              #
# 2)a shape file of the study area with geographic coordinates: lonlat WGS84                     #                                                     #       
# 3)a new coordinate system can be provided as an argument                                       #
# 4)the variable of interest: "TMAX","TMIN" or "PRCP"                                            #
#                                                                                                #
#The outputs are text files and a shape file of a time subset of the database                    #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 06/02/212                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--                                  #
##################################################################################################

###Loading R library and packages   
library(RPostgreSQL)
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                                          # GDAL wrapper for R, spatial utilities
library(rgeos)                                          # Polygon buffering and other vector operations
library(reshape)

### Parameters and arguments

db.name <- "ghcn"                #name of the Postgres database
var <- c("TMAX","TMIN","PRCP")                    #name of the variables to keep: TMIN, TMAX or PRCP
year_start<-"1970"               #starting year for the query (included)
year_end<-"2011"                 #end year for the query (excluded)

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations/"        #Jupiter LOCATION on EOS/Atlas
#path<-"H:/Data/IPLANT_project/data_Oregon_stations"                      #Jupiter Location on XANDERS

outpath=path                                                              # create different output path because we don't have write access to other's home dirs
setwd(path) 
out_prefix<-"stationarity"                                                 #User defined output prefix
buffer=100

#for Adam
outpath="/home/wilson/data/"


############ START OF THE SCRIPT #################

#####  Connect to Station database
drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname=db.name)#,options="statement_timeout = 1m")

##### STEP 1: Select station in the study area

infile1<- "ORWGS84_state_outline.shp"        #This is the shape file of outline of the study area.
filename<-sub(".shp","",infile1)             #Removing the extension from file.
interp_area <- readOGR(".",filename)
CRS_interp<-proj4string(interp_area)         #Storing the coordinate information: geographic coordinates longlat WGS84

#####  Buffer shapefile if desired
##     This is done to include stations from outside the region in the interpolation fitting process and reduce edge effects when stiching regions
if(buffer>0){  #only apply buffer if buffer >0
  interp_area=gUnionCascaded(interp_area)  #dissolve any subparts of roi (if there are islands, lakes, etc.)
  interp_areaC=gCentroid(interp_area)       # get centroid of region
  interp_areaB=spTransform(                # buffer roi (transform to azimuthal equidistant with centroid of region for most (?) accurate buffering, add buffer, then transform to WGS84)
    gBuffer(
      spTransform(interp_area,
                  CRS(paste("+proj=aeqd +lat_0=",interp_areaC@coords[2]," +lon_0=",interp_areaC@coords[1]," +ellps=WGS84 +datum=WGS84 +units=m +no_defs ",sep=""))),
      width=buffer*1000),                  # convert buffer (km) to meters
    CRS(CRS_interp))                       # reproject back to original projection
#  interp_area=interp_areaB                 # replace original region with buffered region
}

## get bounding box of study area
bbox=bbox(interp_areab)

### read in station location information from database
### use the bbox of the region to include only station in rectangular region to speed up overlay
dat_stat=dbGetQuery(db, paste("SELECT id,name,latitude,longitude
                  FROM stations
                  WHERE latitude>=",bbox[2,1]," AND latitude<=",bbox[2,2],"
                  AND longitude>=",bbox[1,1]," AND longitude<=",bbox[1,2],"
                  ;",sep=""))
coordinates(dat_stat)<-c("longitude","latitude")
proj4string(dat_stat)<-CRS_interp


# Spatial query to find relevant stations
inside <- !is.na(over(dat_stat, as(interp_areab, "SpatialPolygons")))  #Finding stations contained in the current interpolation area
stat_roi<-dat_stat[inside,]                                            #Finding stations contained in the current interpolation area
#stat_roi<-spTransform(stat_roi,CRS(new_proj))         # Project from WGS84 to new coord. system

#Quick visualization of station locations
plot(interp_area, axes =TRUE)
plot(stat_roi, pch=1, col="red", cex= 0.7, add=TRUE)
#legend("topleft", pch=1,col="red",bty="n",title= "Stations",cex=1.6)


#################################################################
### STEP 2: generate monthly means for climate-aided interpolation
##  Query to link station location information and observations
##  Concatenate date columns into single field for easy convert to date
##  Divide value by 10 to convert to degrees C and mm
##  Subset to years in year_start -> year_end
##  Drop missing values (-9999)
##  Drop observations that failed quality control (keep only qflag==NA)

### first extract average daily values by month.  
system.time(
           d<<-dbGetQuery(db,  # create dm object (data monthly)
                          paste("SELECT station,month,element,count30,value30,count10,value10,latitude,longitude,elevation
                                 FROM 
                                          (SELECT station,month,element,count(value) as count30,avg(value)/10.0 as value30,latitude,longitude,elevation
                                           FROM ghcn, stations
                                           WHERE station = id
                                           AND id IN ('",paste(stat_roi$id,collapse="','"),"')
                                           AND element IN ('",paste(var,collapse="','"),"')
                                           AND year>=",1970,"
                                           AND year<",2000,"
                                           AND value<>-9999
                                           AND qflag IS NULL
                                           GROUP BY station, month,latitude,longitude,elevation,element
                                           ) as a30
                                  INNER JOIN 
                                           (SELECT station,month,element,count(value) as count10,avg(value)/10.0 as value10
                                           FROM ghcn, stations
                                           WHERE station = id
                                           AND id IN ('",paste(stat_roi$id,collapse="','"),"')
                                           AND element IN ('",paste(var,collapse="','"),"')
                                           AND year>=",2000,"
                                           AND year<",2010,"
                                           AND value<>-9999
                                           AND qflag IS NULL
                                           GROUP BY station, month,element
                                           ) as a10
                                      USING (station,element,month)
                                 ;",sep=""))
            )  ### print used time in seconds  ~ 10 minutes

save(d,file=paste(outpath,"stationarity.Rdata"))

#####################################################################33
#### Explore it
load(paste(outpath,"stationarity.Rdata"))

## subset by # of observations?
thresh=.75 #threshold % to keep
d$keep=d$count30/900>thresh&d$count10/300>thresh
table(d$keep)

## create month factor
d$monthname=factor(d$month,labels=format(as.Date(paste(2000,1:12,1,sep="-")),"%B"),ordered=T)
### start PDF
pdf(paste(outpath,"ClimateStationarity.pdf",sep=""),width=11,height=8.5)

library(latticeExtra)
#combineLimits(useOuterStrips(xyplot(value10~value30|monthname+element,data=d[d$keep,],scales=list(relation="free",rot=0),cex=.5,pch=16,
#                      ylab="2000-2010 Mean Daily Value",xlab="1970-2000 Mean Daily Value",
#                      main="Comparison of Mean Daily Values",asp=1)))+
#  layer(panel.abline(0,1,col="red"))+
#  layer(panel.text(max(x),min(y),paste("R^2=",round(summary(lm(y~x))$r.squared,2)),cex=.5,pos=2))

for(v in unique(d$element)){
print(xyplot(value10~value30|monthname,data=d[d$keep&d$element==v,],scales=list(relation="free",rot=0),cex=.5,pch=16,
                      ylab="2000-2010 Mean Daily Value",xlab="1970-2000 Mean Daily Value",
                      main=paste("Comparison of Mean Daily Values for",v),asp=1)+
  layer(panel.abline(0,1,col="red"))+
  layer(panel.text(max(x),min(y),paste("R^2=",round(summary(lm(y~x))$r.squared,2)),cex=1,pos=2)))
}

## look at deviances
d$dif=d$value10-d$value30

trellis.par.set(superpose.symbol = list(col=c("blue","grey","green","red"),cex=.5,pch=16))

 for(v in unique(d$element)){
    print(xyplot(latitude~longitude|monthname,group=cut(dif,quantile(d$dif[d$keep&d$element==v],seq(0,1,len=5))),
       data=d[d$keep&d$element==v,],auto.key=list(space="right"),
                 main=paste("Current-Past anomolies for",v," (2000-2010 Daily Means Minus 1970-2000 Daily Means)"),
                 sub="Positive values indicate stations that were warmer/wetter in 2000-2010 than 1970-2000")+
          layer(sp.lines(as(interp_area,"SpatialLines"),col="black")))
}

dev.off()

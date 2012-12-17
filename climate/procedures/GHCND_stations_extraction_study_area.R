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
library(raster)

### Parameters and arguments

db.name <- "ghcn"                #name of the Postgres database
var <- "PRCP"                    #name of the variables to keep: TMIN, TMAX or PRCP
year_start<-"1970"               #starting year for the query (included)
year_end<-"2011"                 #end year for the query (excluded)
buffer=500                      #  size of buffer around shapefile to include in spatial query (in km), set to 0 to use no buffer
infile2<-"ghcnd-stations.txt"                             #This is the textfile of station locations from GHCND
new_proj<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
#tile="h11v08" 
tile="h09v04"

#out_prefix<-"20121212"                                                 #User defined output prefix

#for Adam
outpath="/home/wilson/data"
path="/home/wilson/data"
setwd(path) 


############ START OF THE SCRIPT #################

#####  Connect to Station database
drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname=db.name)#,options="statement_timeout = 1m")

##### STEP 1: Select station in the study area
## get MODLAND tile information
#tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
#tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")
#tile_bb=tb[tb$tile==tile,] ## identify tile of interest
#roi_ll=extent(tile_bb$lon_min,tile_bb$lon_max,tile_bb$lat_min,tile_bb$lat_max)

modgrid=readOGR("/home/wilson/data/modisgrid","modis_sinusoidal_grid_world")
roi=modgrid[modgrid$h==as.numeric(substr(tile,2,3))&modgrid$v==as.numeric(substr(tile,5,6)),]
CRS_interp="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
interp_area=spTransform(roi,CRS(CRS_interp))

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
  interp_area=interp_areaB                 # replace original region with buffered region
}

## get bounding box of study area
bbox=bbox(interp_area)

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
inside <- !is.na(over(dat_stat, as(interp_area, "SpatialPolygons")))  #Finding stations contained in the current interpolation area
stat_roi<-dat_stat[inside,]                                            #Finding stations contained in the current interpolation area
stat_roi<-spTransform(stat_roi,CRS(new_proj))         # Project from WGS84 to new coord. system

#Quick visualization of station locations
#plot(interp_area, axes =TRUE)
#plot(stat_roi, pch=1, col="red", cex= 0.7, add=TRUE)
#legend("topleft", pch=1,col="red",bty="n",title= "Stations",cex=1.6)

#### STEP 2: Connecting to the database and query for relevant data 

##  Query to link station location information and observations
##  Concatenate date columns into single field for easy convert to date
##  Divide value by 10 to convert to degrees C and mm
##  Subset to years in year_start -> year_end
##  Drop missing values (-9999)
##  Drop observations that failed quality control (keep only qflag==NA)
system.time(
  dd<<-dbGetQuery(db,  #save object dd (data daily)
                          paste("SELECT station,year||'-'||month||'-'||day AS date,value / 10.0 as value,latitude,longitude,elevation
                                 FROM ghcn, stations
                                 WHERE station = id
                                 AND id IN ('",paste(stat_roi$id,collapse="','"),"')
                                 AND element='",var,"'
                                 AND year>=",year_start,"
                                 AND year<",year_end,"
                                 AND value<>-9999
                                 AND qflag IS NULL
                                 ;",sep=""))
  )  ### print used time in seconds  - only taking ~30 seconds....

dd=dd[!is.na(dd$value),]  #drop any NA values

#################################################################
### STEP 3: generate monthly means for climate-aided interpolation

### first extract average daily values by year.  
system.time(
           dm<<-dbGetQuery(db,  # create dm object (data monthly)
                          paste("SELECT station,month,count(value) as count,avg(value)/10.0 as value,latitude,longitude,elevation
                                 FROM ghcn, stations
                                 WHERE station = id
                                 AND id IN ('",paste(stat_roi$id,collapse="','"),"')
                                 AND element='",var,"'
                                 AND year>=",year_start,"
                                 AND year<",year_end,"
                                 AND value<>-9999
                                 AND qflag IS NULL
                                 GROUP BY station, month,latitude,longitude,elevation
                                 ;",sep=""))
            )  ### print used time in seconds  - only taking ~30 seconds....

### drop months with fewer than 75% of the data observations
### is 75% the right number?
#dm=dm[dm$count>=(.75*10*30),]

## add the monthly data to the daily table
dd$month=as.numeric(as.character(format(as.Date(dd$date),"%m")))
dd$avgvalue=dm$value[match(paste(dd$station,dd$month),paste(dm$station,dm$month))]
dd$avgcount=dm$count[match(paste(dd$station,dd$month),paste(dm$station,dm$month))]

### Generate log-transformed ratio of daily:monthly ppt and add it to daily data
if(var=="PRCP"){
  dd$anom=log((1+dd$value)/(1+dd$avgvalue))
  dd$anom[dd$anom==Inf|dd$anom2==-Inf]=NA
}

### Generate temperature anomolies for each daily value
if(var!="PRCP"){
  dd$anom=dd$value-dd$avgvalue
}


###  Transform the subset data frame in a spatial data frame and reproject
dd_sp<-SpatialPointsDataFrame (dd[,c('longitude','latitude')],data=dd,proj=CRS(CRS_interp)) 
dd_sp<-spTransform(dd_sp,CRS(new_proj))         # Project from WGS84 to new coord. system

###  Transform the subset data frame in a spatial data frame and reproject
dm_sp<-SpatialPointsDataFrame (dm[,c('longitude','latitude')],data=dm,proj=CRS(CRS_interp)) 
dm_sp<-spTransform(dm_sp,CRS(new_proj))         # Project from WGS84 to new coord. system

################################################################
### STEP 4: Save results and outuput in textfile and a shape file

##  Save shapefile of station locations
writeOGR(stat_roi,outpath,paste("station_location_",tile,"_",var,sep=""),driver ="ESRI Shapefile")

## save shapefile of daily observations
writeOGR(dd_sp,outpath,paste("/station_daily_",tile,"_",var,sep=""), driver ="ESRI Shapefile") #Note that the layer name is the file name without extension

### write monthly data
writeOGR(dm_sp, outpath, paste("/station_monthly_",tile,"_",var,sep=""), driver ="ESRI Shapefile") #Note that the layer name is the file name without extension

##### END OF SCRIPT ##########

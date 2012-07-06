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

### Parameters and arguments

db.name <- "ghcn"                #name of the Postgres database
var <- "PRCP"                    #name of the variables to keep: TMIN, TMAX or PRCP
year_start<-"2010"               #starting year for the query (included)
year_end<-"2011"                 #end year for the query (excluded)
infile1<- "ORWGS84_state_outline.shp"                     #This is the shape file of outline of the study area.                
buffer=100                      #  size of buffer around shapefile to include in spatial query (in km), set to 0 to use no buffer
infile2<-"ghcnd-stations.txt"                             #This is the textfile of station locations from GHCND
new_proj<-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations/"        #Jupiter LOCATION on EOS/Atlas
#path<-"H:/Data/IPLANT_project/data_Oregon_stations"                      #Jupiter Location on XANDERS

outpath=path                                                              # create different output path because we don't have write access to other's home dirs
setwd(path) 
out_prefix<-"y2010_2010_OR_20110705"                                                 #User defined output prefix

#for Adam
#outpath="/home/wilson/data"
#out_prefix<-"y2010_OR_20110705"                                                 #User defined output prefix

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
stat_OR<-dat_stat[inside,]                                            #Finding stations contained in the current interpolation area

#Quick visualization of station locations
plot(interp_area, axes =TRUE)
plot(stat_OR, pch=1, col="red", cex= 0.7, add=TRUE)
#legend("topleft", pch=1,col="red",bty="n",title= "Stations",cex=1.6)

#### STEP 2: Connecting to the database and query for relevant data 

##  Query to link station location information and observations
##  Concatenate date columns into single field for easy convert to date
##  Divide value by 10 to convert to degrees C and mm
##  Subset to years in year_start -> year_end
##  Drop missing values (-9999)
##  Drop observations that failed quality control (keep only qflag==NA)
system.time(
  data_table<<-dbGetQuery(db,
                          paste("SELECT station,year||'-'||month||'-'||day AS date,value / 10.0 as value,latitude,longitude,elevation
                                 FROM ghcn, stations
                                 WHERE station = id
                                 AND id IN ('",paste(stat_OR$id,collapse="','"),"')
                                 AND element='",var,"'
                                 AND year>=",year_start,"
                                 AND year<",year_end,"
                                 AND value<>-9999
                                 AND qflag IS NULL
                                 ;",sep=""))
  )  ### print used time in seconds  - only taking ~30 seconds....

 
#Transform the subset data frame in a spatial data frame and reproject
data3<-data_table                               #Make a copy of the data frame
coords<- data3[c('longitude','latitude')]              #Define coordinates in a data frame
coordinates(data3)<-coords                      #Assign coordinates to the data frame
proj4string(data3)<-CRS_interp                  #Assign coordinates reference system in PROJ4 format
data_proj<-spTransform(data3,CRS(new_proj))     #Project from WGS84 to new coord. system

### STEP 3: Save results and outuput in textfile and a shape file

#Save a textfile of the locations of meteorological stations in the study area
write.table(as.data.frame(stat_OR), file=paste(outpath,"/","location_study_area_",out_prefix,".txt",sep=""),sep=",")

#Save a textfile and shape file of all the subset data
write.table(data_table, file= paste(outpath,"/","ghcn_data_",var,out_prefix,".txt",sep=""), sep=",")
#outfile<-paste(path,"ghcn_data_",var,out_prefix,sep="")   #Removing extension if it is present
outfile<-paste(outpath,"/ghcn_data_",var,out_prefix,sep="")         #Name of the file
writeOGR(data_proj, paste(outfile, "shp", sep="."), outfile, driver ="ESRI Shapefile") #Note that the layer name is the file name without extension

##### END OF SCRIPT ##########

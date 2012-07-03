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

### Parameters and arguments

db.name <- "ghcn"                #name of the Postgres database
var <- "TMAX"                    #name of the variables to keep: TMIN, TMAX or PRCP
year_start<-"2010"               #starting year for the query (included)
year_end<-"2011"                 #end year for the query (excluded)
infile1<- "ORWGS84_state_outline.shp"                     #This is the shape file of outline of the study area.                
infile2<-"ghcnd-stations.txt"                             #This is the textfile of station locations from GHCND
new_proj<-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations/"        #Jupiter LOCATION on EOS/Atlas
#path<-"H:/Data/IPLANT_project/data_Oregon_stations"                      #Jupiter Location on XANDERS
setwd(path) 
out_prefix<-"y2010_2010_OR_0626012"                                                 #User defined output prefix

### Functions used in the script

format_s <-function(s_ID){
  #Format station ID in a vector format/tuple that is used in a psql query.
  # Argument 1: vector of station ID
  # Return: character of station ID
  tx2<-s_ID
  tx2<-as.character(tx2)
  stat_list<-tx2
  temp<-shQuote(stat_list)
  t<-paste(temp, collapse= " ")
  t1<-gsub(" ", ",",t)
  sf_ID<-paste("(",t1,")",sep="") #vector containing the station ID to query
  return(sf_ID)
}

############ START OF THE SCRIPT #################

##### STEP 1: Select station in the study area

infile1<- "ORWGS84_state_outline.shp"        #This is the shape file of outline of the study area.
filename<-sub(".shp","",infile1)             #Removing the extension from file.
interp_area <- readOGR(".",filename)
CRS_interp<-proj4string(interp_area)         #Storing the coordinate information: geographic coordinates longlat WGS84

dat_stat <- read.fwf("ghcnd-stations.txt", widths = c(11,9,10,7,3,31,4,4,6),fill=TRUE)
colnames(dat_stat)<-c("STAT_ID","lat","lon","elev","state","name","GSNF","HCNF","WMOID")
coords<- dat_stat[,c('lon','lat')]
coordinates(dat_stat)<-coords
locs_coord<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")
#proj4string(dat_stat)<-locs_coord
proj4string(dat_stat)<-CRS_interp

# Spatial query to find relevant stations
inside <- !is.na(over(dat_stat, as(interp_area, "SpatialPolygons")))  #Finding stations contained in the current interpolation area
stat_OR<-dat_stat[inside,]                                            #Finding stations contained in the current interpolation area

#Quick visualization of station locations
plot(interp_area, axes =TRUE)
plot(stat_OR, pch=1, col="red", cex= 0.7, add=TRUE)
#legend("topleft", pch=1,col="red",bty="n",title= "Stations",cex=1.6)

#### STEP 2: Connecting to the database and query for relevant data 

drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname=db.name)

time1<-proc.time()    #Start stop watch
list_s<-format_s(stat_OR$STAT_ID)
data2<-dbGetQuery(db, paste("SELECT *
      FROM ghcn
      WHERE element=",shQuote(var),
      "AND year>=",year_start,
      "AND year<",year_end,
      "AND station IN ",list_s,";",sep=""))  #Selecting station using a SQL query
time_duration<-proc.time()-time1             #Time for the query may be long given the size of the database
time_minutes<-time_duration[3]/60
  
data_table<-merge(data2,stat_OR, by.x = "station", by.y = "STAT_ID")

#Transform the subset data frame in a spatial data frame and reproject
data3<-data_table                               #Make a copy of the data frame
coords<- data3[c('lon.1','lat.1')]              #Define coordinates in a data frame
coordinates(data3)<-coords                      #Assign coordinates to the data frame
proj4string(data3)<-CRS_interp                  #Assign coordinates reference system in PROJ4 format
data_proj<-spTransform(data3,CRS(new_proj))     #Project from WGS84 to new coord. system

### STEP 3: Save results and outuput in textfile and a shape file

#Save a textfile of the locations of meteorological stations in the study area
write.table(as.data.frame(stat_OR), file=paste(path,"/","location_study_area",out_prefix,".txt",sep=""),sep=",")

#Save a textfile and shape file of all the subset data
write.table(data_table, file= paste(path,"/","ghcn_data_",var,out_prefix,".txt",sep=""), sep=",")
#outfile<-paste(path,"ghcn_data_",var,out_prefix,sep="")   #Removing extension if it is present
outfile<-paste("ghcn_data_",var,out_prefix,sep="")         #Name of the file
writeOGR(data_proj, paste(outfile, "shp", sep="."), outfile, driver ="ESRI Shapefile") #Note that the layer name is the file name without extension

##### END OF SCRIPT ##########

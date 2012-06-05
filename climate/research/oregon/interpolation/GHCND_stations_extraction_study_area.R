##################    Data preparation for interpolation   #######################################
###########################  TWO-STAGE REGRESSION  ###############################################
#This script perform queries on the Postgres database ghcn for stations matching the             #
#interpolation area. It requires the text file of stations and a shape file of the study area.   #       
#Note that the projection for both GHCND and study area is lonlat WGS84.                         #
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

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations/"        #Jupiter LOCATION on EOS/Atlas
#path<-"H:/Data/IPLANT_project/data_Oregon_stations"                      #Jupiter Location on XANDERS
setwd(path) 
out_prefix<-"y2010_2010_OR_0602012"                                                 #User defined output prefix

### Functions

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

##### STEP 1: Select station in Oregon

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

z1<-stat_OR$STAT_ID[999:1002] # Selecting three station to check the query and processing time
l2<-format_s(z1)  

d1<-dbGetQuery(db, paste("SELECT *
      FROM ghcn
      WHERE element=",shQuote(var),
      "AND year>=",year_start,
      "AND year<",year_end,
      "AND station IN ",l2,";",sep=""))  #Selecting one station

time1<-proc.time()    #Start stop watch
list_s<-format_s(stat_OR$STAT_ID)
data<-dbGetQuery(db, paste("SELECT *
      FROM ghcn
      WHERE element=",shQuote(var),
      "AND year>=",year_start,
      "AND year<",year_end,
      "AND station IN ",list_s,";",sep=""))  #Selecting one station 
time_duration<-proc.time()-time1 #time for the query?

write.table(data, file= paste(path,"/","ghcn_data_",var,out_prefix,".txt",sep=""), sep=",")
#write.table(d1, file= paste(path,"/","ghcn_data_",var,out_prefix,".txt",sep=""), sep=",")
outfile<-file= paste(path,"/","ghcn_data_",var,out_prefix,".shp",sep="")   #Removing extension if it is present
#writeOGR(data_SDF,".", outfile, driver ="ESRI Shapefile")

##### END OF SCRIPT ##########

# data<-dbGetQuery(db, paste("SELECT *
#       FROM ghcn
#       WHERE element='TMAX'
#       AND station IN ",t1,";",sep=""))  #Selecting one station
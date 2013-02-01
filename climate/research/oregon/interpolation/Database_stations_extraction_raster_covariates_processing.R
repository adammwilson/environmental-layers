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
#DATE: 01/31/2013                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--     
#Comments and TODO
#-Add buffer option...
#-Add calculation of monthly mean...
##################################################################################################

###Loading R library and packages   

library(RPostgreSQL)
library(sp)                                           # Spatial pacakge with class definition by Bivand et al.
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                                          # GDAL wrapper for R, spatial utilities
library(rgeos)
library(rgdal)
library(raster)
library(rasterVis)

### Parameters and arguments

db.name <- "ghcn"                #name of the Postgres database
var <- "TMAX"                    #name of the variables to keep: TMIN, TMAX or PRCP
year_start<-"2010"               #starting year for the query (included)
year_end<-"2011"                 #end year for the query (excluded)
infile1<- "_venezuela_region__VE_01292013.shp"      #This is the shape file of outline of the study area. 
                                                    #It is projected alreaday
infile2<-"ghcnd-stations.txt"                             #This is the textfile of station locations from GHCND
new_proj<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
infile3<-"covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script

##Paths to inputs and output
in_path <- "/home/parmentier/Data/benoit_test"
in_path <- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data/"
out_path<- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data/"
ghcnd_path<- "/home/layers/data/climate/ghcn/v2.92-upd-2012052822"
setwd(in_path) 
out_suffix<-"y2010_2010_VE_01292013"                                                 #User defined output prefix
out_region_name<-"_venezuela_region"
#out_suffix<-"_VE_01292013"

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

############ BEGIN: START OF THE SCRIPT #################

##### STEP 1: Select station in the study area

filename<-sub(".shp","",infile1)             #Removing the extension from file.
interp_area <- readOGR(".",filename)
CRS_interp<-proj4string(interp_area)         #Storing the coordinate information: geographic coordinates longlat WGS84

dat_stat <- read.fwf(file.path(ghcnd_path,"ghcnd-stations.txt"), widths = c(11,9,10,7,3,31,4,4,6),fill=TRUE)
colnames(dat_stat)<-c("STAT_ID","lat","lon","elev","state","name","GSNF","HCNF","WMOID")
coords<- dat_stat[,c('lon','lat')]
coordinates(dat_stat)<-coords
locs_coord<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0")
proj4string(dat_stat)<-locs_coord
#proj4string(dat_stat)<-CRS_interp
dat_stat2<-spTransform(dat_stat,CRS(new_proj))         # Project from WGS84 to new coord. system

# Spatial query to find relevant stations
inside <- !is.na(over(dat_stat2, as(interp_area, "SpatialPolygons")))  #Finding stations contained in the current interpolation area
stat_reg<-dat_stat2[inside,]                                            #Finding stations contained in the current interpolation area

#Quick visualization of station locations
plot(interp_area, axes =TRUE)
plot(stat_reg, pch=1, col="red", cex= 0.7, add=TRUE)
#plot(data3,pch=1,col="blue",cex=3,add=TRUE)
#legend("topleft", pch=1,col="red",bty="n",title= "Stations",cex=1.6)
#only 357 station for Venezuela??

####
##Add buffer option? 
####

#### STEP 2: Connecting to the database and query for relevant data 

drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname=db.name)

time1<-proc.time()    #Start stop watch
list_s<-format_s(stat_reg$STAT_ID)
data2<-dbGetQuery(db, paste("SELECT *
      FROM ghcn
      WHERE element=",shQuote(var),
      "AND year>=",year_start,
      "AND year<",year_end,
      "AND station IN ",list_s,";",sep=""))  #Selecting station using a SQL query
time_duration<-proc.time()-time1             #Time for the query may be long given the size of the database
time_minutes<-time_duration[3]/60

###
#Add month query and averages here...
###

#data2 contains only 46 stations for Venezueal area??
data_table<-merge(data2,as.data.frame(stat_reg), by.x = "station", by.y = "STAT_ID")

#Transform the subset data frame in a spatial data frame and reproject
data_reg<-data_table                               #Make a copy of the data frame
coords<- data_reg[c('lon.1','lat.1')]              #Define coordinates in a data frame: clean up here!!
                                                   #Wrong label...it is in fact projected...
coordinates(data_reg)<-coords                      #Assign coordinates to the data frame
#proj4string(data3)<-locs_coord                  #Assign coordinates reference system in PROJ4 format
proj4string(data_reg)<-new_proj                 #Assign coordinates reference system in PROJ4 format
#data_proj<-spTransform(data3,CRS(new_proj))     #Project from WGS84 to new coord. system

plot(interp_area, axes =TRUE)
plot(stat_reg, pch=1, col="red", cex= 0.7, add=TRUE)
plot(data_reg,pch=2,col="blue",cex=2,add=TRUE)

##################################################################
### STEP 3: Save results and outuput in textfile and a shape file

#Save a textfile of the locations of meteorological stations in the study area
write.table(as.data.frame(stat_reg), file=file.path(in_path,paste(out_region_name,"_",
                                                          out_suffix,".txt",sep="")),sep=",")
#Save a textfile and shape file of all the subset data
#write.table(data_table,file= paste(path,"/","ghcn_data_",var,out_suffix,".txt",sep=""), sep=",")
#outfile<-paste(path,"ghcn_data_",var,out_prefix,sep="")   #Removing extension if it is present
outfile<-paste("ghcn_data_",var,out_suffix,sep="")         #Name of the file
#writeOGR(data_proj, paste(outfile, "shp", sep="."), outfile, driver ="ESRI Shapefile") #Note that the layer name is the file name without extension
writeOGR(data_reg,dsn= ".",layer= outfile, driver="ESRI Shapefile",overwrite_layer=TRUE)
outfile<-paste(out_region_name,"_",
               out_suffix,sep="")
writeOGR(stat_reg,dsn= ".",layer= outfile, driver="ESRI Shapefile",overwrite_layer=TRUE)

###################################################################
### STEP 4: Extract values at stations from covariates stack of raster images
#Eventually this step may be skipped if the covariates information is stored in the database...

#The names of covariates can be changed...
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")

covar_names<-c(rnames,lc_names,lst_names)

s_raster<-stack(infile3)                   #read in the data stack
names(s_raster)<-covar_names               #Assigning names to the raster layers: making sure it is included in the extraction
stat_val<- extract(s_raster, data_reg)        #Extracting values from the raster stack for every point location in coords data frame.

#create a shape file and data_frame with names

data_RST<-as.data.frame(stat_val)                                            #This creates a data frame with the values extracted
data_RST_SDF<-cbind(data_reg,data_RST)
coordinates(data_RST_SDF)<-coordinates(data_reg) #Transforming data_RST_SDF into a spatial point dataframe
CRS<-proj4string(data_reg)
proj4string(data_RST_SDF)<-CRS  #Need to assign coordinates...

#Creating a date column
date1<-ISOdate(data_RST_SDF$year,data_RST_SDF$month,data_RST_SDF$day) #Creating a date object from 3 separate column
date2<-gsub("-","",as.character(as.Date(date1)))
data_RST_SDF$date<-date2                                              #Date format (year,month,day) is the following: "20100627"

#write out a new shapefile (including .prj component)
outfile<-paste("covariates_ghcn_data_",var,out_suffix,sep="")         #Name of the file
writeOGR(data_RST_SDF,,dsn= ".",layer= outfile, driver="ESRI Shapefile",overwrite_layer=TRUE)

##### END OF SCRIPT ##########

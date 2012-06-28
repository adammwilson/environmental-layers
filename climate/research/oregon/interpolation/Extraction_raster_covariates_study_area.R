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

###Parameters and arguments

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"             #Path to all datasets on Atlas
setwd(path)                                                                   #Setting the working directory
infile1<-"ghcn_data_TMAXy2010_2010_OR_0626012.shp"                            #Weather station location with interp. var. (TMAX, TMIN or PRCP)
#inlistf<-"list_files_04252012.txt"                                           #Covariates as list of raster files and output names separated by space
                                                                              #Name of raster files should come with extension    
outfile<-'ghcn_or_tmax_covariates_06262012_OR83M.shp'                         #Name of the new shapefile created with covariates extracted at station locations

#######START OF THE SCRIPT #############

###Reading the station data
filename<-sub(".shp","",infile1)                                             #Removing the extension from file.
ghcn3<-readOGR(".", filename)                                                #Reading shape file using rgdal library
 
###Extracting the variables values from the raster files                                             

lines<-read.table(paste(path,"/",inlistf,sep=""), sep=" ")                  #Column 1 contains the names of raster files
inlistvar<-lines[,1]
inlistvar<-paste(path,"/",as.character(inlistvar),sep="")
covar_names<-as.character(lines[,2])                                         #Column two contains short names for covaraites

s_raster<- stack(inlistvar)                                                  #Creating a stack of raster images from the list of variables.
layerNames(s_raster)<-covar_names                                            #Assigning names to the raster layers
stat_val<- extract(s_raster, ghcn3)                                          #Extracting values from the raster stack for every point location in coords data frame.

#create a shape file and data_frame with names

data_RST<-as.data.frame(stat_val)                                            #This creates a data frame with the values extracted

data_RST_SDF<-cbind(ghcn3,data_RST)
coordinates(data_RST_SDF)<-coordinates(ghcn3) #Transforming data_RST_SDF into a spatial point dataframe
CRS<-proj4string(ghcn3)
proj4string(data_RST_SDF)<-CRS  #Need to assign coordinates...

#Creating a date column
date1<-ISOdate(data_RST_SDF$year,data_RST_SDF$month,data_RST_SDF$day) #Creating a date object from 3 separate column
date2<-gsub("-","",as.character(as.Date(date1)))
data_RST_SDF$date<-date2                                              #Date format (year,month,day) is the following: "20100627"

#write out a new shapefile (including .prj component)
outfile<-sub(".shp","",outfile)   #Removing extension if it is present

#Save a textfile and shape file of all the subset data
write.table(as.data.frame(data_RST_SDF),paste(outfile,".txt",sep=""), sep=",")
writeOGR(data_RST_SDF, paste(outfile, "shp", sep="."), outfile, driver ="ESRI Shapefile") #Note that the layer name is the file name without extension

##### END OF SCRIPT ##########
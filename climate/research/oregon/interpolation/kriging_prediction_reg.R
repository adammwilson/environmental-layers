##################    Interpolation of Tmax Using Kriging  #######################################
########################### Kriging and Cokriging   ###############################################
#This script interpolates station values for the Oregon case study using Kriging and Cokring.    #
#The script uses LST monthly averages as input variables and  loads the station data             # 
#from a shape file with projection information.                                                  #
#Note that this program:                                                                         #
#1)assumes that the shape file is in the current working.                                        # 
#2)relevant variables were extracted from raster images before performing the regressions        #
#  and stored shapefile                                                                          #
#This scripts predicts tmax using autokrige, gstat and LST derived from MOD11A1.                 #
#also included and assessed using the RMSE,MAE,ME and R2 from validation dataset.                #
#TThe dates must be provided as a textfile.                                                      #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 07/15/2012                                                                                #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#364--                                  #
##################################################################################################

###Loading R library and packages                                                      
#library(gtools)                                         # loading some useful tools 
library(mgcv)                                           # GAM package by Wood 2006 (version 2012)
library(sp)                                             # Spatial pacakge with class definition by Bivand et al. 2008
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al. 2012
library(rgdal)                                          # GDAL wrapper for R, spatial utilities (Keitt et al. 2012)
library(gstat)                                          # Kriging and co-kriging by Pebesma et al. 2004
library(automap)                                        # Automated Kriging based on gstat module by Hiemstra et al. 2008
library(spgwr)
library(gpclib)
library(maptools)
library(graphics)
library(parallel)                            # Urbanek S. and Ripley B., package for multi cores & parralel processing
library(raster)

###Parameters and arguments

infile1<- "ghcn_or_tmax_covariates_06262012_OR83M.shp"             #GHCN shapefile containing variables for modeling 2010                 
#infile2<-"list_10_dates_04212012.txt"                     #List of 10 dates for the regression
infile2<-"list_365_dates_04212012.txt"
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       
inlistf<-"list_files_05032012.txt"                        #Stack of images containing the Covariates

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_07152012"     #Jupiter LOCATION on Atlas for kriging
#path<-"H:/Data/IPLANT_project/data_Oregon_stations"                                 #Jupiter Location on XANDERS

setwd(path) 
prop<-0.3                                                                       #Proportion of testing retained for validation   
seed_number<- 100                                                               #Seed number for random sampling
models<-7                                                                       #Number of kriging model
out_prefix<-"_07202012_auto_krig1"                                              #User defined output prefix

source("krigingUK_function_07192012b.R")

###STEP 1 DATA PREPARATION AND PROCESSING#####

###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)

mean_LST<- readGDAL(infile5)                 #Reading the whole raster in memory. This provides a grid for kriging
proj4string(mean_LST)<-CRS                   #Assigning coordinate information to prediction grid.

##Extracting the variables values from the raster files                                             

lines<-read.table(paste(path,"/",inlistf,sep=""), sep=" ")                  #Column 1 contains the names of raster files
inlistvar<-lines[,1]
inlistvar<-paste(path,"/",as.character(inlistvar),sep="")
covar_names<-as.character(lines[,2])                                         #Column two contains short names for covaraites

s_raster<- stack(inlistvar)                                                  #Creating a stack of raster images from the list of variables.
layerNames(s_raster)<-covar_names                                            #Assigning names to the raster layers
projection(s_raster)<-CRS

#stat_val<- extract(s_raster, ghcn3)                                          #Extracting values from the raster stack for every point location in coords data frame.
pos<-match("ASPECT",layerNames(s_raster)) #Find column with name "value"
r1<-raster(s_raster,layer=pos)             #Select layer from stack
pos<-match("slope",layerNames(s_raster)) #Find column with name "value"
r2<-raster(s_raster,layer=pos)             #Select layer from stack
N<-cos(r1*pi/180)
E<-sin(r1*pi/180)
Nw<-sin(r2*pi/180)*cos(r1*pi/180)   #Adding a variable to the dataframe
Ew<-sin(r2*pi/180)*sin(r1*pi/180)   #Adding variable to the dataframe.
#r<-stack(N,E,Nw,Ew)
#rnames<-c("Northness","Eastness","Northness_w","Eastness_w")
#layerNames(r)<-rnames
#s_raster<-addLayer(s_raster, r)
#s_sgdf<-as(s_raster,"SpatialGridDataFrame") #Conversion to spatial grid data frame
xy<-coordinates(r1)  #get x and y projected coordinates...
xy_latlon<-project(xy, CRS, inv=TRUE) # find lat long for projected coordinats (or pixels...)
tmp<-raster(xy_latlon) #, ncol=ncol(r1), nrow=nrow(r1))
ncol(tmp)<-ncol(r1)
nrow(tmp)<-nrow(r1)
extent(tmp)<-extent(r1)
projection(tmp)<-CRS
tmp2<-tmp
values(tmp)<-xy_latlon[,1]
values(tmp2)<-xy_latlon[,2]

r<-stack(N,E,Nw,Ew,tmp,tmp2)
rnames<-c("Northness","Eastness","Northness_w","Eastness_w", "lon","lat")
layerNames(r)<-rnames
s_raster<-addLayer(s_raster, r)
rm(r)
### adding var
ghcn = transform(ghcn,Northness = cos(ASPECT*pi/180)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness = sin(ASPECT*pi/180))  #adding variable to the dataframe.
ghcn = transform(ghcn,Northness_w = sin(slope*pi/180)*cos(ASPECT*pi/180)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness_w = sin(slope*pi/180)*sin(ASPECT*pi/180))  #adding variable to the dataframe.

#Remove NA for LC and CANHEIGHT
ghcn$LC1[is.na(ghcn$LC1)]<-0
ghcn$LC3[is.na(ghcn$LC3)]<-0
ghcn$CANHEIGHT[is.na(ghcn$CANHEIGHT)]<-0

dates <-readLines(paste(path,"/",infile2, sep=""))
LST_dates <-readLines(paste(path,"/",infile3, sep=""))
#models <-readLines(paste(path,"/",infile4, sep=""))

#Model assessment: specific diagnostic/metrics for GAM
results_AIC<- matrix(1,1,models+3)  
results_GCV<- matrix(1,1,models+3)
results_DEV<- matrix(1,1,models+3)
#results_RMSE_f<- matrix(1,length(models)+3)

#Model assessment: general diagnostic/metrics 
results_RMSE <- matrix(1,1,models+3)
results_MAE <- matrix(1,1,models+3)
results_ME <- matrix(1,1,models+3)       #There are 8+1 models
results_R2 <- matrix(1,1,models+3)       #Coef. of determination for the validation dataset

results_RMSE_f<- matrix(1,1,models+3)    #RMSE fit, RMSE for the training dataset
results_MAE_f <- matrix(1,1,models+3)
#Screening for bad values: value is tmax in this case
#ghcn$value<-as.numeric(ghcn$value)
ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$value>-150 & ghcn$value<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

###CREATING SUBSETS BY INPUT DATES AND SAMPLING
set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, ghcn$date==as.numeric(d)))   #Producing a list of data frame, one data frame per date.
sampling<-vector("list",length(dates))

for(i in 1:length(dates)){
  n<-nrow(ghcn.subsets[[i]])
  ns<-n-round(n*prop)   #Create a sample from the data frame with 70% of the rows
  nv<-n-ns              #create a sample for validation with prop of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
  sampling[[i]]<-ind.training
}


kriging_mod<-mclapply(1:length(dates), runKriging, mc.cores = 8)#This is the end bracket from mclapply(...) statement

#for(i in 1:length(dates)){            # start of the for loop #1
#i<-3                                           #Date 10 is used to test kriging

## Plotting and saving diagnostic measures
accuracy_tab_fun<-function(i,f_list){
  tb<-f_list[[i]][[3]]
  return(tb)
}

tb<-kriging_mod[[1]][[3]][0,]  #empty data frame with metric table structure that can be used in rbinding...
tb_tmp<-kriging_mod #copy

for (i in 1:length(tb_tmp)){
  tmp<-tb_tmp[[i]][[3]]
  tb<-rbind(tb,tmp)
}
rm(tb_tmp)

for(i in 4:(models+3)){            # start of the for loop #1
  tb[,i]<-as.numeric(as.character(tb[,i]))  
}

tb_RMSE<-subset(tb, metric=="RMSE")
tb_MAE<-subset(tb,metric=="MAE")
tb_ME<-subset(tb,metric=="ME")
tb_R2<-subset(tb,metric=="R2")
tb_RMSE_f<-subset(tb, metric=="RMSE_f")
tb_MAE_f<-subset(tb,metric=="MAE_f")

tb_diagnostic1<-rbind(tb_RMSE,tb_MAE,tb_ME,tb_R2)
#tb_diagnostic2<-rbind(tb_,tb_MAE,tb_ME,tb_R2)

mean_RMSE<-sapply(tb_RMSE[,4:(models+3)],mean)
mean_MAE<-sapply(tb_MAE[,4:(models+3)],mean)
mean_R2<-sapply(tb_R2[,4:(models+3)],mean)
mean_ME<-sapply(tb_ME[,4:(models+3)],mean)
mean_MAE_f<-sapply(tb_MAE[,4:(models+3)],mean)
mean_RMSE_f<-sapply(tb_RMSE_f[,4:(models+3)],mean)

write.table(tb_diagnostic1, file= paste(path,"/","results2_fusion_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
write.table(tb, file= paste(path,"/","results2_fusion_Assessment_measure_all",out_prefix,".txt",sep=""), sep=",")
save(kriging_mod,file= paste(path,"/","results2_fusion_Assessment_measure_all",out_prefix,".RData",sep=""))

#### END OF SCRIPT #####
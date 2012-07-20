##################    CLIMATE INTERPOLATION FUSION METHOD   #######################################
############################ Merging LST and station data ##########################################
#This script interpolates tmax values using MODIS LST and GHCND station data                     #
#interpolation area. It requires the text file of stations and a shape file of the study area.   #       
#Note that the projection for both GHCND and study area is lonlat WGS84.                         #
#AUTHOR: Brian McGill                                                                            #
#DATE: 07/11/2012                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--                                  #
###################################################################################################

###Loading R library and packages                                                      
library(gtools)                                         # loading some useful tools 
library(mgcv)                                           # GAM package by Simon Wood
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                              # NCAR Spatial Interpolation methods such as kriging, splines
library(raster)                              # Hijmans et al. package for raster processing
library(parallel)                            # Urbanek S. and Ripley B., package for multi cores & parralel processing

### Parameters and argument

infile1<- "ghcn_or_tmax_covariates_06262012_OR83M.shp"             #GHCN shapefile containing variables for modeling 2010                 
#infile2<-"list_10_dates_04212012.txt"                     #List of 10 dates for the regression
#infile2<-"list_2_dates_04212012.txt"
infile2<-"list_365_dates_04212012.txt"
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       #Raster or grid for the locations of predictions
#infile6<-"lst_climatology.txt"
infile6<-"LST_files_monthly_climatology.txt"
#path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"
path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"
#path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_07152012"     #Jupiter LOCATION on Atlas for kriging"
#path<-"M:/Data/IPLANT_project/data_Oregon_stations"   #Locations on Atlas

#Station location of the study area
stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)
#GHCN Database for 1980-2010 for study area (OR) 
data3<-read.table(paste(path,"/","ghcn_data_TMAXy1980_2010_OR_0602012.txt",sep=""),sep=",", header=TRUE)

prop<-0.3                                                                           #Proportion of testing retained for validation   
#prop<-0.25
seed_number<- 100                                                                   #Seed number for random sampling
out_prefix<-"_07152012_10d_fusion17"                                                   #User defined output prefix
setwd(path)
bias_val<-0            #if value 1 then training data is used in the bias surface rather than the all monthly stations

#source("fusion_function_07192012.R")
source("fusion_function_07192012.R")
############ START OF THE SCRIPT ##################
#
#
### Step 0/Step 6 in Brian's code...preparing year 2010 data for modeling 
#


###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)

mean_LST<- readGDAL(infile5)                 #Reading the whole raster in memory. This provides a grid for kriging
proj4string(mean_LST)<-CRS                   #Assigning coordinate information to prediction grid.

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
models <-readLines(paste(path,"/",infile4, sep=""))

# #Model assessment: specific diagnostic/metrics for GAM
# results_AIC<- matrix(1,length(dates),length(models)+3)  
# results_GCV<- matrix(1,length(dates),length(models)+3)
# results_DEV<- matrix(1,length(dates),length(models)+3)
# results_RMSE_f<- matrix(1,length(dates),length(models)+3)
# 
# #Model assessment: general diagnostic/metrics 
# results_RMSE <- matrix(1,length(dates),length(models)+4)
# results_MAE <- matrix(1,length(dates),length(models)+4)
# results_ME <- matrix(1,length(dates),length(models)+4)       #There are 8+1 models
# results_R2 <- matrix(1,length(dates),length(models)+4)       #Coef. of determination for the validation dataset
# 
# results_RMSE_f<- matrix(1,length(dates),length(models)+4)    #RMSE fit, RMSE for the training dataset

#Model assessment: specific diagnostic/metrics for GAM
results_AIC<- matrix(1,1,length(models)+3)  
results_GCV<- matrix(1,1,length(models)+3)
results_DEV<- matrix(1,1,length(models)+3)
#results_RMSE_f<- matrix(1,length(models)+3)

#Model assessment: general diagnostic/metrics 
results_RMSE <- matrix(1,1,length(models)+4)
results_MAE <- matrix(1,1,length(models)+4)
results_ME <- matrix(1,1,length(models)+4)       #There are 8+1 models
results_R2 <- matrix(1,1,length(models)+4)       #Coef. of determination for the validation dataset

results_RMSE_f<- matrix(1,1,length(models)+4)    #RMSE fit, RMSE for the training dataset
results_MAE_f <- matrix(1,1,length(models)+4)

#Screening for bad values: value is tmax in this case
#ghcn$value<-as.numeric(ghcn$value)
ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$value>-150 & ghcn$value<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 or 365 subsets dataset based on dates
sampling<-vector("list",length(dates))

for(i in 1:length(dates)){
n<-nrow(ghcn.subsets[[i]])
ns<-n-round(n*prop)   #Create a sample from the data frame with 70% of the rows
nv<-n-ns              #create a sample for validation with prop of the rows
ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
sampling[[i]]<-ind.training
}

#Start loop here...

## looping through the dates...this is the main part of the code
#i=1 #for debugging
#j=1 #for debugging
#for(i in 1:length(dates)){     [[       # start of the for loop #1
#i=1


#mclapply(1:length(dates), runFusion, mc.cores = 8)#This is the end bracket from mclapply(...) statement

fusion_mod<-mclapply(1:length(dates), runFusion, mc.cores = 8)#This is the end bracket from mclapply(...) statement
#fusion_mod357<-mclapply(357:365,runFusion, mc.cores=8)# for debugging
#test<-runFusion(362) #date 362 has problems with GAM
#test<-mclapply(357,runFusion, mc.cores=1)# for debugging

## Plotting and saving diagnostic measures
accuracy_tab_fun<-function(i,f_list){
tb<-f_list[[i]][[3]]
return(tb)
}


tb<-fusion_mod[[1]][[3]][0,]  #empty data frame with metric table structure that can be used in rbinding...
tb_tmp<-fusion_mod #copy

for (i in 1:length(tb_tmp)){
  tmp<-tb_tmp[[i]][[3]]
  tb<-rbind(tb,tmp)
}
rm(tb_tmp)

for(i in 4:12){            # start of the for loop #1
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

mean_RMSE<-sapply(tb_RMSE[,4:12],mean)
mean_MAE<-sapply(tb_MAE[,4:12],mean)

#tb<-sapply(fusion_mod,accuracy_tab_fun)

write.table(tb_diagnostic1, file= paste(path,"/","results2_fusion_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
write.table(tb, file= paste(path,"/","results2_fusion_Assessment_measure_all",out_prefix,".txt",sep=""), sep=",")

#tb<-as.data.frame(tb_diagnostic1)

#write.table(tb_1, file= paste(path,"/","results2_fusion_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")

#write.table(tb_diagnostic2, file= paste(path,"/","results_fusion_Assessment_measure2",out_prefix,".txt",sep=""), sep=",")

#### END OF SCRIPT
##################    Interpolation of Tmax Using GWR     ########################################
########################### GWR WITH LST   #######################################################
#This script interpolates station values for the Oregon case study using Geographically Weighted. #
#The script uses LST monthly averages as input variables and  loads the station data              # 
#from a shape file with projection information.                                                   #
#Note that this program:                                                                          #
#1)assumes that the shape file is in the current working folder                                   # 
#2)relevant variables were extracted from raster images before performing the regressions         #
#  and stored shapefile                                                                           #
#3)covariate raster images are present in the current working folder                              #
#This scripts predicts tmax using autokrige, gstat and LST derived from MOD11A1.                  #
#also included and assessed using the RMSE,MAE,ME and R2 from validation dataset.                 #
#TThe dates must be provided as a textfile.                                                       #
#AUTHOR: Benoit Parmentier                                                                        #
#DATE: 08/15/2012                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#364--                                   #
###################################################################################################

###Loading R library and packages                                                      
#library(gtools)                                         # loading some useful tools 
library(mgcv)                                           # GAM package by Wood 2006 (version 2012)
library(sp)                                             # Spatial pacakge with class definition by Bivand et al. 2008
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al. 2012
library(rgdal)                                          # GDAL wrapper for R, spatial utilities (Keitt et al. 2012)
library(gstat)                                          # Kriging and co-kriging by Pebesma et al. 2004
library(automap)                                        # Automated Kriging based on gstat module by Hiemstra et al. 2008
library(spgwr)
library(maptools)
library(graphics)
library(parallel)                            # Urbanek S. and Ripley B., package for multi cores & parallel processing
library(raster)
library(rasterVis)
library(fields)                              # May be used later...

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
inlistf<-"list_files_05032012.txt"                        #Stack of images containing the Covariates

#path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_07192012_GAM"
path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_07152012_GWR"     #Jupiter LOCATION on Atlas for kriging

#Station location of the study area
#stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)
#GHCN Database for 1980-2010 for study area (OR) 
#data3<-read.table(paste(path,"/","ghcn_data_TMAXy1980_2010_OR_0602012.txt",sep=""),sep=",", header=TRUE) #Not needing at this stage...

nmodels<-8                                    #number of models running
y_var_name<-"dailyTmax"                       #variable value being modeled...("value" in the GHCND database)
predval<-1                                    # if set to 1, full interpolation raster produced for the study area
prederr<-0                                    # if set to 0, no uncertain error (e.g. standard error or kriging std dev) is produced
prop<-0.3                                     #Proportion of testing retained for validation   
#prop<-0.25
seed_number<- 100                             #Seed number for random sampling
out_prefix<-"_08152012_1d_gwr4"                                                   #User defined output prefix
setwd(path)

#source("fusion_function_07192012.R")
#source("KrigingUK_function_07262012.R")
source("GWR_function_08152012b.R")
############ START OF THE SCRIPT ##################

###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)

mean_LST<- readGDAL(infile5)                 #Reading the whole raster in memory. This provides a grid for kriging
proj4string(mean_LST)<-CRS                   #Assigning coordinate information to prediction grid.

ghcn <- transform(ghcn,Northness = cos(ASPECT*pi/180)) #Adding a variable to the dataframe
ghcn <- transform(ghcn,Eastness = sin(ASPECT*pi/180))  #adding variable to the dataframe.
ghcn <- transform(ghcn,Northness_w = sin(slope*pi/180)*cos(ASPECT*pi/180)) #Adding a variable to the dataframe
ghcn <- transform(ghcn,Eastness_w = sin(slope*pi/180)*sin(ASPECT*pi/180))  #adding variable to the dataframe.

#Remove NA for LC and CANHEIGHT
ghcn$LC1[is.na(ghcn$LC1)]<-0
ghcn$LC3[is.na(ghcn$LC3)]<-0
ghcn$CANHEIGHT[is.na(ghcn$CANHEIGHT)]<-0

dates <-readLines(paste(path,"/",infile2, sep=""))
LST_dates <-readLines(paste(path,"/",infile3, sep=""))
models <-readLines(paste(path,"/",infile4, sep=""))

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

pos<-match("LC1",layerNames(s_raster)) #Find column with name "value"
LC1<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC1[is.na(LC1)]<-0
pos<-match("LC3",layerNames(s_raster)) #Find column with name "value"
LC3<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC3[is.na(LC3)]<-0
pos<-match("CANHEIGHT",layerNames(s_raster)) #Find column with name "value"
CANHEIGHT<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
CANHEIGHT[is.na(CANHEIGHT)]<-0

xy<-coordinates(r1)  #get x and y projected coordinates...
xy_latlon<-project(xy, CRS, inv=TRUE) # find lat long for projected coordinats (or pixels...)
lon<-raster(xy_latlon) #Transform a matrix into a raster object ncol=ncol(r1), nrow=nrow(r1))
ncol(lon)<-ncol(r1)
nrow(lon)<-nrow(r1)
extent(lon)<-extent(r1)
projection(lon)<-CRS  #At this stage this is still an empty raster with 536 nrow and 745 ncell 
lat<-lon
values(lon)<-xy_latlon[,1]
values(lat)<-xy_latlon[,2]

r<-stack(N,E,Nw,Ew,lon,lat,LC1,LC3,CANHEIGHT)
rnames<-c("Northness","Eastness","Northness_w","Eastness_w", "lon","lat","LC1","LC3","CANHEIGHT")
layerNames(r)<-rnames
s_raster<-addLayer(s_raster, r)
#s_sgdf<-as(s_raster,"SpatialGridDataFrame") #Conversion to spatial grid data frame

####### Preparing LST stack of climatology...

#l=list.files(pattern="mean_month.*rescaled.rst")
l <-readLines(paste(path,"/",infile6, sep=""))
molst<-stack(l)  #Creating a raster stack...
#setwd(old)
molst<-molst-273.16  #K->C          #LST stack of monthly average...
idx <- seq(as.Date('2010-01-15'), as.Date('2010-12-15'), 'month')
molst <- setZ(molst, idx)
layerNames(molst) <- month.abb

####### CREATE A MASK FOR WATER AND 

pos<-match("LC10",layerNames(s_raster)) #Find column with the current month for instance mm12
LC10<-raster(s_raster,pos)

LC10_mask<-LC10
LC10_mask[is.na(LC10_mask)]<-0
LC10_mask[LC10==100]<-NA
LC10_mask[LC10_mask<100]<-1
LC10_mask[is.na(LC10_mask)]<-0
mask_land_NA<-LC10_mask
mask_land_NA[mask_land_NA==0]<-NA

breakpoints<-c(0,0.99)
colors<-c("black","red")
#plot(LC10_mask, breaks=breakpoints,col=colors)
#quartz()
plot(LC10_mask, col=colors)

# ELEV_SRTM[ELEV_SRTM==-9999]<-NA
# avl<-c(0,60000,1)
# rclmat<-matrix(avl,ncol=3,byrow=TRUE)
# ELEV_rc<-reclass(ELEV_SRTM,rclmat)
# ELEV_SRTM_m <-mask(ELEV_STRM,mask_land_NA)
# mask2<-ELEV_SRTM_m
# mask2[is.na(mask)]<-1
# s_rst_m <-mask(s_raster,mask_land_NA)   #This assigns NA to all values from LC1 that are NA in the mask_land_NA
# 
#######  Preparing tables for model assessment: specific diagnostic/metrics

#Model assessment: specific diagnostics/metrics
results_m1<- matrix(1,1,nmodels+3)    #Diagnostic metrics specific to the modeleling framework 
results_m2<- matrix(1,1,nmodels+3)
results_m3<- matrix(1,1,nmodels+3)
#results_RMSE_f<- matrix(1,length(models)+3)

#Model assessment: general diagnostic/metrics 
results_RMSE <- matrix(1,1,nmodels+3)
results_MAE <- matrix(1,1,nmodels+3)
results_ME <- matrix(1,1,nmodels+3)       #There are 8 models for kriging!!!
results_R2 <- matrix(1,1,nmodels+3)       #Coef. of determination for the validation dataset

results_RMSE_f<- matrix(1,1,nmodels+3)    #RMSE fit, RMSE for the training dataset
results_MAE_f <- matrix(1,1,nmodels+3)
results_R2_f <- matrix(1,1,nmodels+3)

######### Preparing daily values for training and testing

#Screening for bad values: value is tmax in this case
#ghcn$value<-as.numeric(ghcn$value)
ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$value>-150 & ghcn$value<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

##Sampling: training and testing sites...

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

######## Prediction for the range of dates

gwr_mod<-mclapply(1:length(dates), runGWR,mc.preschedule=FALSE,mc.cores = 8) #This is the end bracket from mclapply(...) statement
#fusion_mod357<-mclapply(357:365,runFusion, mc.cores=8)# for debugging
#test<-runKriging(1)
#test357<-mclapply(357:365,runFusion, mc.cores=8)# for debugging
#gwr_mod<-mclapply(1:1, runGWR,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement

#test<-mclapply(357,runFusion, mc.cores=1)# for debugging

tb<-gwr_mod[[1]][[3]][0,]  #empty data frame with metric table structure that can be used in rbinding...
tb_tmp<-gwr_mod #copy

for (i in 1:length(tb_tmp)){
  tmp<-tb_tmp[[i]][[3]]
  tb<-rbind(tb,tmp)
}
rm(tb_tmp)

for(i in 4:(nmodels+3)){            # start of the for loop #1
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

mean_RMSE<-sapply(tb_RMSE[,4:(nmodels+3)],mean)
mean_MAE<-sapply(tb_MAE[,4:(nmodels+3)],mean)
mean_R2<-sapply(tb_R2[,4:(nmodels+3)],mean)
mean_ME<-sapply(tb_ME[,4:(nmodels+3)],mean)
mean_MAE_f<-sapply(tb_MAE[,4:(nmodels+3)],mean)
mean_RMSE_f<-sapply(tb_RMSE_f[,4:(nmodels+3)],mean)

#Wrting metric results in textfile and model objects in .RData file
write.table(tb_diagnostic1, file= paste(path,"/","results2_gwr_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
write.table(tb, file= paste(path,"/","results2_gwr_Assessment_measure_all",out_prefix,".txt",sep=""), sep=",")
save(gwr_mod,file= paste(path,"/","results2_gwr_Assessment_measure_all",out_prefix,".RData",sep=""))

#### END OF SCRIPT
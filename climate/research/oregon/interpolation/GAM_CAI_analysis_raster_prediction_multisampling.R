######################################## METHOD COMPARISON #######################################
############################ Constant sampling for GAM CAI method #####################################
#This script interpolates tmax values using MODIS LST and GHCND station data                     #
#interpolation area. It requires the text file of stations and a shape file of the study area.   #       
#Note that the projection for both GHCND and study area is lonlat WGS84.                         #
#Method is assedsed using constant sampling with variation  of validation sample with different  #
#hold out proportions.                                                                           #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 10/25/2012                                                                                #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#491--                                  #
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
library(rasterVis)
library(parallel)                            # Urbanek S. and Ripley B., package for multi cores & parralel processing
library(reshape)

### Parameters and argument

infile1<- "ghcn_or_tmax_covariates_06262012_OR83M.shp"             #GHCN shapefile containing variables for modeling 2010                 
#infile2<-"list_10_dates_04212012.txt"                     #List of 10 dates for the regression
infile2<-"list_365_dates_04212012.txt"
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       #Raster or grid for the locations of predictions
#infile6<-"lst_climatology.txt"
infile6<-"LST_files_monthly_climatology.txt"
inlistf<-"list_files_05032012.txt"                        #Stack of images containing the Covariates

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_CAI" #Atlas location
setwd(path)

#Station location for the study area
stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)
#GHCN Database for 1980-2010 for study area (OR) 
data3<-read.table(paste(path,"/","ghcn_data_TMAXy1980_2010_OR_0602012.txt",sep=""),sep=",", header=TRUE)

nmodels<-8   #number of models running
y_var_name<-"dailyTmax"
climgam=1                                                     #if 1, then GAM is run on the climatology rather than the daily deviation surface...
predval<-1
prop<-0.3                                                     #Proportion of testing retained for validation   

seed_number<- 100                                             #Seed number for random sampling, if seed_number<0, no seed number is used..
#out_prefix<-"_365d_GAM_CAI2_const_10222012_"                  #User defined output prefix
out_prefix<-"_365d_GAM_CAI2_all_lstd_10262012"                #User defined output prefix

bias_val<-0            #if value 1 then daily training data is used in the bias surface rather than the all monthly stations (added on 07/11/2012)
bias_prediction<-1     #if value 1 then use GAM for the BIAS prediction otherwise GAM direct reprediction for y_var (daily tmax)
nb_sample<-1           #number of time random sampling must be repeated for every hold out proportion
prop_min<-0.3          #if prop_min=prop_max and step=0 then predicitons are done for the number of dates...
prop_max<-0.3
step<-0         
constant<-0            #if value 1 then use the same samples as date one for the all set of dates
#projection used in the interpolation of the study area
CRS_interp<-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84

source("GAM_CAI_function_multisampling_10252012.R")

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
ghcn$LC4[is.na(ghcn$LC4)]<-0
ghcn$LC6[is.na(ghcn$LC6)]<-0

#Use file.path for to construct pathfor independent os platform? !!!
dates<-readLines(file.path(path,infile2))
#dates <-readLines(paste(path,"/",infile2, sep=""))
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
LC1[is.na(LC1)]<-0                      #NA must be set to zero.
pos<-match("LC3",layerNames(s_raster)) #Find column with name "value"
LC3<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC3[is.na(LC3)]<-0

#Modification added to account for other land cover

pos<-match("LC4",layerNames(s_raster)) #Find column with name "value"
LC4<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC4[is.na(LC4)]<-0

pos<-match("LC6",layerNames(s_raster)) #Find column with name "value"
LC6<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC6[is.na(LC6)]<-0

LC_s<-stack(LC1,LC3,LC4,LC6)
layerNames(LC_s)<-c("LC1_forest","LC3_grass","LC4_crop","LC6_urban")
plot(LC_s)

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

r<-stack(N,E,Nw,Ew,lon,lat,LC1,LC3,LC4,LC6, CANHEIGHT)
rnames<-c("Northness","Eastness","Northness_w","Eastness_w", "lon","lat","LC1","LC3","LC4","LC6","CANHEIGHT")
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

######  Preparing tables for model assessment: specific diagnostic/metrics

#Model assessment: specific diagnostics/metrics
results_m1<- matrix(1,1,nmodels+3)  
results_m2<- matrix(1,1,nmodels+3)
results_m3<- matrix(1,1,nmodels+3)
#results_RMSE_f<- matrix(1,length(models)+3)

#Model assessment: general diagnostic/metrics 
results_RMSE <- matrix(1,1,nmodels+4)
results_MAE <- matrix(1,1,nmodels+4)
results_ME <- matrix(1,1,nmodels+4)       #There are 8+1 models
results_R2 <- matrix(1,1,nmodels+4)       #Coef. of determination for the validation dataset

results_RMSE_f<- matrix(1,1,nmodels+4)    #RMSE fit, RMSE for the training dataset
results_MAE_f <- matrix(1,1,nmodels+4)
results_R2_f<-matrix(1,1,nmodels+4)
######## Preparing monthly averages from the ProstGres database

# do this work outside of (before) this function
# to avoid making a copy of the data frame inside the function call
date1<-ISOdate(data3$year,data3$month,data3$day) #Creating a date object from 3 separate column
date2<-as.POSIXlt(as.Date(date1))
data3$date<-date2
d<-subset(data3,year>=2000 & mflag=="0" ) #Selecting dataset 2000-2010 with good quality: 193 stations
#May need some screeing??? i.e. range of temp and elevation...
d1<-aggregate(value~station+month, data=d, mean)  #Calculate monthly mean for every station in OR
id<-as.data.frame(unique(d1$station))     #Unique station in OR for year 2000-2010: 193 but 7 loss of monthly avg    

dst<-merge(d1, stat_loc, by.x="station", by.y="STAT_ID")   #Inner join all columns are retained

#This allows to change only one name of the data.frame
pos<-match("value",names(dst)) #Find column with name "value"
names(dst)[pos]<-c("TMax")
dst$TMax<-dst$TMax/10                #TMax is the average max temp for monthy data
#dstjan=dst[dst$month==9,]  #dst contains the monthly averages for tmax for every station over 2000-2010

#Extracting covariates from stack for the monthly dataset...
coords<- dst[c('lon','lat')]              #Define coordinates in a data frame
coordinates(dst)<-coords                      #Assign coordinates to the data frame
proj4string(dst)<-CRS_locs_WGS84                  #Assign coordinates reference system in PROJ4 format
dst_month<-spTransform(dst,CRS(CRS_interp))     #Project from WGS84 to new coord. system

stations_val<-extract(s_raster,dst_month)  #extraction of the infomration at station location
stations_val<-as.data.frame(stations_val)
dst_extract<-cbind(dst_month,stations_val)
dst<-dst_extract

######### Preparing daily values for training and testing

#Screening for bad values: value is tmax in this case
#ghcn$value<-as.numeric(ghcn$value)
ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$value>-150 & ghcn$value<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

##Sampling: training and testing sites...

if (seed_number>0) {
  set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...
}
nel<-length(dates)
dates_list<-vector("list",nel) #list of one row data.frame

prop_range<-(seq(from=prop_min,to=prop_max,by=step))*100
sn<-length(dates)*nb_sample*length(prop_range)

for(i in 1:length(dates)){
  d_tmp<-rep(dates[i],nb_sample*length(prop_range)) #repeating same date
  s_nb<-rep(1:nb_sample,length(prop_range))         #number of random sample per proportion
  prop_tmp<-sort(rep(prop_range, nb_sample))
  tab_run_tmp<-cbind(d_tmp,s_nb,prop_tmp)
  dates_list[[i]]<-tab_run_tmp
}

sampling_dat<-as.data.frame(do.call(rbind,dates_list))
names(sampling_dat)<-c("date","run_samp","prop")

for(i in 2:3){            # start of the for loop #1
  sampling_dat[,i]<-as.numeric(as.character(sampling_dat[,i]))  
}

sampling_dat$date<- as.character(sampling_dat[,1])
#ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 or 365 subsets dataset based on dates
ghcn.subsets <-lapply(as.character(sampling_dat$date), function(d) subset(ghcn, date==d)) #this creates a list of 10 or 365 subsets dataset based on dates

sampling<-vector("list",length(ghcn.subsets))

for(i in 1:length(ghcn.subsets)){
  n<-nrow(ghcn.subsets[[i]])
  prop<-(sampling_dat$prop[i])/100
  ns<-n-round(n*prop)   #Create a sample from the data frame with 70% of the rows
  nv<-n-ns              #create a sample for validation with prop of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
  sampling[[i]]<-ind.training
}

if (constant==1){
  sampled<-sampling[[1]]
  list_const_sampling<-vector("list",sn)
  for(i in 1:sn){
    list_const_sampling[[i]]<-sampled
  }
  sampling<-list_const_sampling  
}

######## Prediction for the range of dates

#Start loop here...

#gam_CAI_mod<-mclapply(1:length(dates), runGAMCAI,mc.preschedule=FALSE,mc.cores = 8) #This is the end bracket from mclapply(...) statement
gam_CAI_mod<-mclapply(1:length(ghcn.subsets), runGAMCAI,mc.preschedule=FALSE,mc.cores = 8) #This is the end bracket from mclapply(...) statement
#gam_CAI_mod<-mclapply(1:1, runGAMCAI,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement

tb<-gam_CAI_mod[[1]][[3]][0,]  #empty data frame with metric table structure that can be used in rbinding...
tb_tmp<-gam_CAI_mod #copy

for (i in 1:length(tb_tmp)){
  tmp<-tb_tmp[[i]][[3]]
  tb<-rbind(tb,tmp)
}
rm(tb_tmp)

for(i in 4:(nmodels+4)){            # start of the for loop #1
  tb[,i]<-as.numeric(as.character(tb[,i]))  
}

metrics<-as.character(unique(tb$metric))            #Name of accuracy metrics (RMSE,MAE etc.)
tb_metric_list<-vector("list",length(metrics))

for(i in 1:length(metrics)){            # Reorganizing information in terms of metrics 
  metric_name<-paste("tb_",metrics[i],sep="")
  tb_metric<-subset(tb, metric==metrics[i])
  tb_metric<-cbind(tb_metric,sampling_dat[,2:3])
  assign(metric_name,tb_metric)
  tb_metric_list[[i]]<-tb_metric
}

tb_diagnostic<-do.call(rbind,tb_metric_list)  #produce a data.frame from the list ...
tb_diagnostic[["prop"]]<-as.factor(tb_diagnostic[["prop"]])

t<-melt(tb_diagnostic,
        measure=c("mod1","mod2","mod3","mod4", "mod5", "mod6", "mod7", "mod8","mod9"), 
        id=c("dates","metric","prop"),
        na.rm=F)
avg_tb<-cast(t,metric+prop~variable,mean)
median_tb<-cast(t,metric+prop~variable,mean)
avg_tb[["prop"]]<-as.numeric(as.character(avg_tb[["prop"]]))
avg_RMSE<-subset(avg_tb,metric=="RMSE")

# Save before plotting
#sampling_obj<-list(sampling_dat=sampling_dat,training=sampling)
sampling_obj<-list(sampling_dat=sampling_dat,training=sampling, tb=tb_diagnostic)

write.table(avg_tb, file= paste(path,"/","results2_fusion_Assessment_measure_avg_",out_prefix,".txt",sep=""), sep=",")
write.table(median_tb, file= paste(path,"/","results2_fusion_Assessment_measure_median_",out_prefix,".txt",sep=""), sep=",")
write.table(tb_diagnostic, file= paste(path,"/","results2_fusion_Assessment_measure",out_prefix,".txt",sep=""), sep=",")
write.table(tb, file= paste(path,"/","results2_fusion_Assessment_measure_all",out_prefix,".txt",sep=""), sep=",")


# tb_RMSE<-subset(tb, metric=="RMSE")
# tb_MAE<-subset(tb,metric=="MAE")
# tb_ME<-subset(tb,metric=="ME")
# tb_R2<-subset(tb,metric=="R2")
# tb_RMSE_f<-subset(tb, metric=="RMSE_f")
# tb_MAE_f<-subset(tb,metric=="MAE_f")
# tb_R2_f<-subset(tb,metric=="R2_f")
# 
# tb_diagnostic1<-rbind(tb_RMSE,tb_MAE,tb_ME,tb_R2)
# #tb_diagnostic2<-rbind(tb_,tb_MAE,tb_ME,tb_R2)
# 
# mean_RMSE<-sapply(tb_RMSE[,4:(nmodels+4)],mean)
# mean_MAE<-sapply(tb_MAE[,4:(nmodels+4)],mean)
# mean_R2<-sapply(tb_R2[,4:(nmodels+4)],mean)
# mean_ME<-sapply(tb_ME[,4:(nmodels+4)],mean)
# mean_MAE_f<-sapply(tb_MAE[,4:(nmodels+4)],mean)
# mean_RMSE_f<-sapply(tb_RMSE_f[,4:(nmodels+4)],mean)
# 
# #Wrting metric results in textfile and model objects in .RData file
# write.table(tb_diagnostic1, file= paste(path,"/","results2_CAI_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
# write.table(tb, file= paste(path,"/","results2_CAI_Assessment_measure_all",out_prefix,".txt",sep=""), sep=",")
save(sampling_obj, file= paste(path,"/","results2_CAI_sampling_obj",out_prefix,".RData",sep=""))
save(gam_CAI_mod,file= paste(path,"/","results2_CAI_Assessment_measure_all",out_prefix,".RData",sep=""))

#### END OF SCRIPT
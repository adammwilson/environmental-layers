##################    MULTI SAMPLING GAM FUSION METHOD ASSESSMENT ####################################
############################ Merging LST and station data ##########################################
#This script interpolates tmax values using MODIS LST and GHCND station data                      
#interpolation area. It requires the text file of stations and a shape file of the study area.           
#Note that the projection for both GHCND and study area is lonlat WGS84.       
#Options to run this program are:
#1) Multisampling: vary the porportions of hold out and use random samples for each run
#2)Constant sampling: use the same sample over the runs
#3)over dates: run over for example 365 dates without mulitsampling
#4)use seed number: use seed if random samples must be repeatable
#5)GAM fusion: possibilty of running GAM+FUSION or GAM separately 
#AUTHOR: Benoit Parmentier                                                                        
#DATE: 02/06/2013                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--                                   
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
library(plotrix)
### Parameters and argument

infile1<- "ghcn_or_tmax_covariates_06262012_OR83M.shp"             #GHCN shapefile containing variables for modeling 2010                 
infile2<-"list_365_dates_04212012.txt"
#infile3<-"LST_dates_var_names.txt"                        #LST dates name
#infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
#infile5<-"mean_day244_rescaled.rst"                       #Raster or grid for the locations of predictions
#infile6<-"lst_climatology.txt"
#infile6<-"LST_files_monthly_climatology.txt"
#inlistf<-"list_files_05032012.txt"                        #Stack of images containing the Covariates

infile_monthly<-"monthly_covariates_ghcn_data_TMAXy2010_2010_VE_02062013.shp"
infile_daily<-"daily_covariates_ghcn_data_TMAXy2010_2010_VE_02062013.shp"
infile_locs<-"stations_venezuela_region_y2010_2010_VE_02062013.shp"
infile3<-"covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script

in_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data"
out_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data"
setwd(in_path)

nmodels<-9   #number of models running
y_var_name<-"dailyTmax"
predval<-1
prop<-0.3             #Proportion of testing retained for validation   
#prop<-0.25
seed_number<- 100  #if seed zero then no seed?                                                                 #Seed number for random sampling
out_prefix<-"_10d_GAM_fus5_all_lstd_020632013"                #User defined output prefix
#out_prefix<-"_365d_GAM_12272012"                #User defined output prefix

bias_val<-0            #if value 1 then training data is used in the bias surface rather than the all monthly stations
bias_prediction<-1     #if value 1 then use GAM for the BIAS prediction otherwise GAM direct repdiction for y_var (daily tmax)
nb_sample<-1           #number of time random sampling must be repeated for every hold out proportion
prop_min<-0.3          #if prop_min=prop_max and step=0 then predicitons are done for the number of dates...
prop_max<-0.3
step<-0         
constant<-0             #if value 1 then use the same samples as date one for the all set of dates
#projection used in the interpolation of the study area: should be read directly from the outline of the study area
CRS_interp<-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84

source("GAM_fusion_function_multisampling_02062013.R")

###################### START OF THE SCRIPT ########################

###Reading the daily station data and setting up for models' comparison
#filename<-sub(".shp","",infile1)             #Removing the extension from file.
#ghcn<-readOGR(".", filename)                 #reading shapefile 
ghcn<-readOGR(dsn=in_path,layer=sub(".shp","",infile_daily))
CRS_interp<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)

#mean_LST<- readGDAL(infile5)                 #Reading the whole raster in memory. This provides a grid for kriging
#proj4string(mean_LST)<-CRS_interp                   #Assigning coordinate information to prediction grid.

#Station location of the study area
#stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)
stat_loc<-readOGR(dsn=in_path,layer=sub(".shp","",infile_locs))

#GHCN Database for 1980-2010 for study area (OR) 
#data3<-read.table(paste(path,"/","ghcn_data_TMAXy1980_2010_OR_0602012.txt",sep=""),sep=",", header=TRUE)
#data3<-file.path(in_path,infile_monthly)
data3<-readOGR(dsn=in_path,layer=sub(".shp","",infile_monthly))

#Remove NA for LC and CANHEIGHT: Need to check this part
ghcn$LC1[is.na(ghcn$LC1)]<-0
ghcn$LC3[is.na(ghcn$LC3)]<-0
ghcn$CANHEIGHT[is.na(ghcn$CANHEIGHT)]<-0
ghcn$LC4[is.na(ghcn$LC4)]<-0
ghcn$LC6[is.na(ghcn$LC6)]<-0

dates <-readLines(paste(file.path(in_path,infile2))
#LST_dates <-readLines(file.path(in_path,infile3))
#models <-readLines(paste(path,"/",infile4, sep=""))

##Extracting the variables values from the raster files                                             

#The names of covariates can be changed...
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
                    "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
                    "nobs_09","nobs_10","nobs_11","nobs_12")
                  
covar_names<-c(rnames,lc_names,lst_names)
                  
s_raster<-stack(infile3)                   #read in the data stack
names(s_raster)<-covar_names               #Assigning names to the raster layers: making sure it is included in the extraction

#Deal with no data value and zero      
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
pos<-match("ELEV_SRTM",layerNames(s_raster)) #Find column with name "ELEV_SRTM"
ELEV_SRTM<-raster(s_raster,layer=pos)             #Select layer from stack on 10/30
s_raster<-dropLayer(s_raster,pos)
ELEV_SRTM[ELEV_SRTM <0]<-NA

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
results_AIC<- matrix(1,1,nmodels+3)  
results_GCV<- matrix(1,1,nmodels+3)
results_DEV<- matrix(1,1,nmodels+3)
#results_RMSE_f<- matrix(1,length(models)+3)

#Model assessment: general diagnostic/metrics 
results_RMSE <- matrix(1,1,nmodels+4)
results_MAE <- matrix(1,1,nmodels+4)
results_ME <- matrix(1,1,nmodels+4)       #There are 8+1 models
results_R2 <- matrix(1,1,nmodels+4)       #Coef. of determination for the validation dataset

results_RMSE_f<- matrix(1,1,nmodels+4)    #RMSE fit, RMSE for the training dataset
results_MAE_f <- matrix(1,1,nmodels+4)

######## Preparing monthly averages from the ProstGres database and extracting covarvariates from stack

# do this work outside of (before) this function
# to avoid making a copy of the data frame inside the function call
# date1<-ISOdate(data3$year,data3$month,data3$day) #Creating a date object from 3 separate column
# date2<-as.POSIXlt(as.Date(date1))
# data3$date<-date2
# d<-subset(data3,year>=2000 & mflag=="0" ) #Selecting dataset 2000-2010 with good quality: 193 stations
# #May need some screeing??? i.e. range of temp and elevation...
# d1<-aggregate(value~station+month, data=d, mean)  #Calculate monthly mean for every station in OR
# id<-as.data.frame(unique(d1$station))     #Unique station in OR for year 2000-2010: 193 but 7 loss of monthly avg    
# 
# dst<-merge(d1, stat_loc, by.x="station", by.y="STAT_ID")   #Inner join all columns are retained
# 
# #This allows to change only one name of the data.frame
# pos<-match("value",names(dst)) #Find column with name "value"
# names(dst)[pos]<-c("TMax")
# dst$TMax<-dst$TMax/10                #TMax is the average max temp for monthy data
# #dstjan=dst[dst$month==9,]  #dst contains the monthly averages for tmax for every station over 2000-2010
# 
# #Extracting covariates from stack
# coords<- dst[c('lon','lat')]              #Define coordinates in a data frame
# coordinates(dst)<-coords                      #Assign coordinates to the data frame
# proj4string(dst)<-CRS_locs_WGS84                  #Assign coordinates reference system in PROJ4 format
# dst_month<-spTransform(dst,CRS(CRS_interp))     #Project from WGS84 to new coord. system
# 
# stations_val<-extract(s_raster,dst_month)  #extraction of the infomration at station location
# stations_val<-as.data.frame(stations_val)
# dst_extract<-cbind(dst_month,stations_val)
# dst<-dst_extract
                  
#Now clean and screen monthly values
#dst_all<-dst
dst_all<-data3
dst<-data3
#dst<-subset(dst,dst$TMax>-15 & dst$TMax<45) #may choose different threshold??
#dst<-subset(dst,dst$ELEV_SRTM>0) #This will drop two stations...or 24 rows

######### Preparing daily values for training and testing
                  
#Screening for bad values: value is tmax in this case
#ghcn$value<-as.numeric(ghcn$value)
#ghcn_all<-ghcn
#ghcn_test<-subset(ghcn,ghcn$value>-150 & ghcn$value<400)
#ghcn_test<-ghcn
#ghcn_test2<-subset(ghcn_test,ghcn_test$elev_1>0)
#ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

##Sampling: training and testing sites.

#Make this a a function
                  
if (seed_number>0) {
  set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...
}
nel<-length(dates)
dates_list<-vector("list",nel) #list of one row data.frame

prop_range<-(seq(from=prop_min,to=prop_max,by=step))*100     #range of proportion to run
sn<-length(dates)*nb_sample*length(prop_range)               #Number of samples to run

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

## adding choice of constant sample 
if (seed_number>0) {
  set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...
}

sampling<-vector("list",length(ghcn.subsets))
sampling_station_id<-vector("list",length(ghcn.subsets))
for(i in 1:length(ghcn.subsets)){
  n<-nrow(ghcn.subsets[[i]])
  prop<-(sampling_dat$prop[i])/100
  ns<-n-round(n*prop)   #Create a sample from the data frame with 70% of the rows
  nv<-n-ns              #create a sample for validation with prop of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
  #Find the corresponding 
  data_sampled<-ghcn.subsets[[i]][ind.training,] #selected the randomly sampled stations
  station_id.training<-data_sampled$station     #selected id for the randomly sampled stations (115)
  #Save the information
  sampling[[i]]<-ind.training
  sampling_station_id[[i]]<- station_id.training
}
## Use same samples across the year...
if (constant==1){
  sampled<-sampling[[1]]
  data_sampled<-ghcn.subsets[[1]][sampled,] #selected the randomly sampled stations
  station_sampled<-data_sampled$station     #selected id for the randomly sampled stations (115)
  list_const_sampling<-vector("list",sn)
  list_const_sampling_station_id<-vector("list",sn)
  for(i in 1:sn){
    station_id.training<-intersect(station_sampled,ghcn.subsets[[i]]$station)
    ind.training<-match(station_id.training,ghcn.subsets[[i]]$station)
    list_const_sampling[[i]]<-ind.training
    list_const_sampling_station_id[[i]]<-station_id.training
  }
  sampling<-list_const_sampling 
  sampling_station_id<-list_const_sampling_station_id
}

######## Prediction for the range of dates and sampling data

#gam_fus_mod<-mclapply(1:length(dates), runGAMFusion,mc.preschedule=FALSE,mc.cores = 8) #This is the end bracket from mclapply(...) statement
#gam_fus_mod_s<-mclapply(1:1, runGAMFusion,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement
gam_fus_mod_s<-mclapply(1:length(ghcn.subsets), runGAMFusion,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
#gam_fus_mod2<-mclapply(4:4, runGAMFusion,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement

save(gam_fus_mod_s,file= paste(path,"/","results2_fusion_Assessment_measure_all",out_prefix,".RData",sep=""))

## Plotting and saving diagnostic measures

tb<-gam_fus_mod_s[[1]][[3]][0,]  #empty data frame with metric table structure that can be used in rbinding...
tb_tmp<-gam_fus_mod_s #copy

for (i in 1:length(tb_tmp)){
  tmp<-tb_tmp[[i]][[3]]
  tb<-rbind(tb,tmp)
}
rm(tb_tmp)

for(i in 4:ncol(tb)){            # start of the for loop #1
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

tb_diagnostic<-do.call(rbind,tb_metric_list)
tb_diagnostic[["prop"]]<-as.factor(tb_diagnostic[["prop"]])

mod_pat<-glob2rx("mod*")   
mod_var<-grep(mod_pat,names(tb_diagnostic),value=TRUE) # using grep with "value" extracts the matching names         

t<-melt(tb_diagnostic,
        measure=mod_var, 
        id=c("dates","metric","prop"),
        na.rm=F)
avg_tb<-cast(t,metric+prop~variable,mean)
median_tb<-cast(t,metric+prop~variable,median)
avg_tb[["prop"]]<-as.numeric(as.character(avg_tb[["prop"]]))
avg_RMSE<-subset(avg_tb,metric=="RMSE")

sampling_obj<-list(sampling_dat=sampling_dat,training=sampling, training_id=sampling_station_id, tb=tb_diagnostic)

write.table(avg_tb, file= paste(path,"/","results2_fusion_Assessment_measure_avg_",out_prefix,".txt",sep=""), sep=",")
write.table(median_tb, file= paste(path,"/","results2_fusion_Assessment_measure_median_",out_prefix,".txt",sep=""), sep=",")
write.table(tb_diagnostic, file= paste(path,"/","results2_fusion_Assessment_measure",out_prefix,".txt",sep=""), sep=",")
write.table(tb, file= paste(path,"/","results2_fusion_Assessment_measure_all",out_prefix,".txt",sep=""), sep=",")

save(sampling_obj, file= paste(path,"/","results2_fusion_sampling_obj",out_prefix,".RData",sep=""))
#save(gam_fus_mod_s,file= paste(path,"/","results2_fusion_Assessment_measure_all",out_prefix,".RData",sep=""))
gam_fus_mod_obj<-list(gam_fus_mod=gam_fus_mod_s,sampling_obj=sampling_obj)
save(gam_fus_mod_obj,file= paste(path,"/","results_mod_obj_",out_prefix,".RData",sep=""))

#### END OF SCRIPT
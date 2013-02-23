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
#5)GAM fusion: possibilty of running GAM+FUSION and other options added
#The interpolation is done first at the monthly time scale then delta surfaces are added.
#AUTHOR: Benoit Parmentier                                                                        
#DATE: 02/20/2013                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--                                   
###################################################################################################

###Loading R library and packages                                                      
library(gtools)                                         # loading some useful tools 
library(mgcv)                                           # GAM package by Simon Wood
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                             # NCAR Spatial Interpolation methods such as kriging, splines
library(raster)                              # Hijmans et al. package for raster processing
library(rasterVis)
library(parallel)                            # Urbanek S. and Ripley B., package for multi cores & parralel processing
library(reshape)
library(plotrix)
library(maptools)

### Parameters and argument

infile2<-"list_365_dates_04212012.txt"
infile_monthly<-"monthly_covariates_ghcn_data_TMAXy2010_2010_VE_02082013.shp"
infile_daily<-"daily_covariates_ghcn_data_TMAXy2010_2010_VE_02082013.shp"
infile_locs<-"stations_venezuela_region_y2010_2010_VE_02082013.shp"
infile3<-"covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script

in_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data"
out_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data"
script_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/"
setwd(in_path)

y_var_name<-"dailyTmax"                                       
out_prefix<-"_365d_GAM_fus5_all_lstd_02202013"                #User defined output prefix
seed_number<- 100  #if seed zero then no seed?     
nb_sample<-1           #number of time random sampling must be repeated for every hold out proportion
prop_min<-0.3          #if prop_min=prop_max and step=0 then predicitons are done for the number of dates...
prop_max<-0.3
step<-0         
constant<-0             #if value 1 then use the same samples as date one for the all set of dates
#projection used in the interpolation of the study area: should be read directly from the outline of the study area
#CRS_interp<-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84

#Models to run...this can be change for each run
list_models<-c("y_var ~ s(elev_1)",
               "y_var ~ s(LST)",
               "y_var ~ s(elev_1,LST)",
               "y_var ~ s(lat) + s(lon)+ s(elev_1)",
               "y_var ~ s(lat,lon,elev_1)",
               "y_var ~ s(lat,lon) + s(elev_1) + s(N_w,E_w) + s(LST)", 
               "y_var ~ s(lat,lon) + s(elev_1) + s(N_w,E_w) + s(LST) + s(LC2)",
               "y_var ~ s(lat,lon) + s(elev_1) + s(N_w,E_w) + s(LST) + s(LC6)", 
               "y_var ~ s(lat,lon) + s(elev_1) + s(N_w,E_w) + s(LST) + s(DISTOC)")

#Default name of LST avg to be matched               
lst_avg<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")  

source(file.path(script_path,"GAM_fusion_function_multisampling_02202013.R"))
source(file.path(script_path,"GAM_fusion_function_multisampling_validation_metrics_02202013.R"))
                              
###################### START OF THE SCRIPT ########################

#Create log file to keep track of details such as processing times and parameters.

log_fname<-paste("R_log_t",out_prefix, ".log",sep="")

if (file.exists(log_fname)){  #Stop the script???
  file.remove(log_fname)
  log_file<-file(log_fname,"w")
}
if (!file.exists(log_fname)){
  log_file<-file(log_fname,"w")
}

time1<-proc.time()    #Start stop watch
writeLines(paste("Starting script at this local Date and Time: ",as.character(Sys.time()),sep=""),
           con=log_file,sep="\n")
writeLines("Starting script process time:",con=log_file,sep="\n")
writeLines(as.character(time1),con=log_file,sep="\n")    

###Reading the daily station data and setting up for models' comparison
ghcn<-readOGR(dsn=in_path,layer=sub(".shp","",infile_daily))
CRS_interp<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)

stat_loc<-readOGR(dsn=in_path,layer=sub(".shp","",infile_locs))

data3<-readOGR(dsn=in_path,layer=sub(".shp","",infile_monthly))

#Remove NA for LC and CANHEIGHT: Need to check this part after
ghcn$LC1[is.na(ghcn$LC1)]<-0
ghcn$LC3[is.na(ghcn$LC3)]<-0
ghcn$CANHEIGHT[is.na(ghcn$CANHEIGHT)]<-0
ghcn$LC4[is.na(ghcn$LC4)]<-0
ghcn$LC6[is.na(ghcn$LC6)]<-0

dates <-readLines(file.path(in_path,infile2)) #dates to be predicted

##Extracting the variables values from the raster files                                             

#The names of covariates can be changed...
rnames <-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
                    "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
                    "nobs_09","nobs_10","nobs_11","nobs_12")
                  
covar_names<-c(rnames,lc_names,lst_names)
                  
s_raster<-stack(infile3)                   #read in the data stack
names(s_raster)<-covar_names               #Assigning names to the raster layers: making sure it is included in the extraction

#Deal with no data value and zero      
#pos<-match("LC1",layerNames(s_raster)) #Find column with name "value"
#LC1<-raster(s_raster,layer=pos)             #Select layer from stack
#s_raster<-dropLayer(s_raster,pos)
#LC1[is.na(LC1)]<-0

#pos<-match("LC3",layerNames(s_raster)) #Find column with name "value"
#LC3<-raster(s_raster,layer=pos)             #Select layer from stack
#s_raster<-dropLayer(s_raster,pos)
#LC3[is.na(LC3)]<-0

#pos<-match("CANHEIGHT",layerNames(s_raster)) #Find column with name "value"
#CANHEIGHT<-raster(s_raster,layer=pos)             #Select layer from stack
#s_raster<-dropLayer(s_raster,pos)
#CANHEIGHT[is.na(CANHEIGHT)]<-0
#pos<-match("ELEV_SRTM",layerNames(s_raster)) #Find column with name "ELEV_SRTM"
#ELEV_SRTM<-raster(s_raster,layer=pos)             #Select layer from stack on 10/30
#s_raster<-dropLayer(s_raster,pos)
#ELEV_SRTM[ELEV_SRTM <0]<-NA

#s_sgdf<-as(s_raster,"SpatialGridDataFrame") #Conversion to spatial grid data frame

######### Preparing daily and monthly values for training and testing
                  
#Screening for daily bad values: value is tmax in this case
#ghcn$value<-as.numeric(ghcn$value)
#ghcn_all<-ghcn
#ghcn_test<-subset(ghcn,ghcn$value>-150 & ghcn$value<400)
#ghcn_test<-ghcn
#ghcn_test2<-subset(ghcn_test,ghcn_test$elev_1>0)
#ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

#Now clean and screen monthly values
#dst_all<-dst
dst_all<-data3
dst<-data3
#dst<-subset(dst,dst$TMax>-15 & dst$TMax<45) #may choose different threshold??
#dst<-subset(dst,dst$ELEV_SRTM>0) #This will drop two stations...or 24 rows

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

#Make this a function??
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

#First predict at the monthly time scale: climatology
writeLines("Predictions at monthly scale:",con=log_file,sep="\n")
t1<-proc.time()
gamclim_fus_mod<-mclapply(1:12, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
#gamclim_fus_mod<-mclapply(1:12, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 4) #This is the end bracket from mclapply(...) statement
save(gamclim_fus_mod,file= paste("gamclim_fus_mod",out_prefix,".RData",sep=""))
t2<-proc.time()-t1
writeLines(as.character(t2),con=log_file,sep="\n")

#now get list of raster clim layers

list_tmp<-vector("list",length(gamclim_fus_mod))
for (i in 1:length(gamclim_fus_mod)){
  tmp<-gamclim_fus_mod[[i]]$clim
  list_tmp[[i]]<-tmp
}

#put together list of clim models per month...
rast_clim_yearlist<-list_tmp
#Second predict at the daily time scale: delta

#gam_fus_mod<-mclapply(1:1, runGAMFusion,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement
writeLines("Predictions at the daily scale:",con=log_file,sep="\n")
t1<-proc.time()
gam_fus_mod<-mclapply(1:length(ghcn.subsets), runGAMFusion,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
save(gam_fus_mod,file= paste("gam_fus_mod",out_prefix,".RData",sep=""))
t2<-proc.time()-t1
writeLines(as.character(t2),con=log_file,sep="\n")

#Add accuracy_metrics section/function
#now get list of raster daily prediction layers
#gam_fus_mod_tmp<-gam_fus_mod
#this should be change later once correction has been made
#for (i in 1:length(gam_fus_mod)){
#  obj_names<-c(y_var_name,"clim","delta","data_s","sampling_dat","data_v",
#               "mod_kr_day")
#  names(gam_fus_mod[[i]])<-obj_names
#  names(gam_fus_mod[[i]][[y_var_name]])<-c("mod1","mod2","mod3","mod4","mod_kr")
#}

##
list_tmp<-vector("list",length(gam_fus_mod))
for (i in 1:length(gam_fus_mod)){
  tmp<-gam_fus_mod[[i]][[y_var_name]]  #y_var_name is the variable predicted (tmax or tmin)
  list_tmp[[i]]<-tmp
}
rast_day_yearlist<-list_tmp #list of predicted images

writeLines("Validation step:",con=log_file,sep="\n")
t1<-proc.time()
#calculate_accuary_metrics<-function(i)
gam_fus_validation_mod<-mclapply(1:length(gam_fus_mod), calculate_accuracy_metrics,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
#gam_fus_validation_mod<-mclapply(1:1, calculate_accuracy_metrics,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement
save(gam_fus_validation_mod,file= paste("gam_fus_validation_mod",out_prefix,".RData",sep=""))
t2<-proc.time()-t1
writeLines(as.character(t2),con=log_file,sep="\n")

####This part concerns validation assessment and must be moved later...
## make this a function??
list_tmp<-vector("list",length(gam_fus_validation_mod))
for (i in 1:length(gam_fus_validation_mod)){
  tmp<-gam_fus_validation_mod[[i]]$metrics_v
  list_tmp[[i]]<-tmp
}
tb_diagnostic<-do.call(rbind,list_tmp) #long rownames
rownames(tb_diagnostic)<-NULL #remove row names

mod_names<-unique(tb_diagnostic$pred_mod) #models that have accuracy metrics

#now boxplots and mean per models
t<-melt(tb_diagnostic,
        #measure=mod_var, 
        id=c("date","pred_mod","prop"),
        na.rm=F)
avg_tb<-cast(t,pred_mod~variable,mean)
tb<-tb_diagnostic
tb_mod_list<-vector("list",length(mod_names))
for(i in 1:length(mod_names)){            # Reorganizing information in terms of metrics 
  mod_name_tb<-paste("tb_",mod_names[i],sep="")
  tb_mod<-subset(tb, pred_mod==mod_names[i])
  assign(mod_name_tb,tb_mod)
  tb_mod_list[[i]]<-tb_mod
}
names(tb_mod_list)<-mod_names

mod_metrics<-do.call(cbind,tb_mod_list)
mod_pat<-glob2rx("*.rmse")   
mod_var<-grep(mod_pat,names(mod_metrics),value=TRUE) # using grep with "value" extracts the matching names         

boxplot(mod_metrics[[mod_var]])
test<-mod_metrics[mod_var]
boxplot(test,outline=FALSE,horizontal=FALSE,cex=0.5)

#close log_file connection and add meta data
writeLines("Finished script process time:",con=log_file,sep="\n")
time2<-proc.time()-time1
writeLines(as.character(time2),con=log_file,sep="\n")
#later on add all the paramters used in the script...
writeLines(paste("Finished script at this local Date and Time: ",as.character(Sys.time()),sep=""),
           con=log_file,sep="\n")
writeLines("End of script",con=log_file,sep="\n")
close(log_file)

####End of part to be changed...

#### END OF SCRIPT #######
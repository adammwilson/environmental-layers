#########################    Raster prediction GAM FUSION    ####################################
############################ Interpolation of temperature for given processing region ##########################################
#This script interpolates temperature values using MODIS LST, covariates and GHCND station data.                      
#It requires the text file of stations and a shape file of the study area.           
#Note that the projection for both GHCND and study area is lonlat WGS84.       
#Options to run this program are:
#1) Multisampling: vary the porportions of hold out and use random samples for each run
#2)Constant sampling: use the same sample over the runs
#3)over dates: run over for example 365 dates without mulitsampling
#4)use seed number: use seed if random samples must be repeatable
#5)GAM fusion: possibilty of running GAM+FUSION or GAM+CAI and other options added
#The interpolation is done first at the monthly time scale then delta surfaces are added.
#AUTHOR: Benoit Parmentier                                                                        
#DATE: 03/12/2013                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#568--                                   
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

### Parameters and arguments

## output param from previous script: Database_stations_covariates_processing_function
#infile_monthly<-"monthly_covariates_ghcn_data_TMAXy2010_2010_VE_02082013.shp"
#infile_daily<-"daily_covariates_ghcn_data_TMAXy2010_2010_VE_02082013.shp"
#infile_locs<-"stations_venezuela_region_y2010_2010_VE_02082013.shp"
infile_covariates<-"covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script
var<-"TMAX"
#out_prefix<-"_365d_GAM_fus5_all_lstd_02202013"                #User defined output prefix should be defined in master script
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84: same as earlier

infile_monthly<-list_outfiles$monthly_covar_ghcn_data #outile4 from database_covar script
infile_daily<-list_outfiles$daily_covar_ghcn_data  #outfile3 from database_covar script
infile_locs<- list_outfiles$loc_stations_ghcn #outfile2? from database covar script
rnames <-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")                  
covar_names<-c(rnames,lc_names,lst_names)  
###
#Input for sampling function...
seed_number<- 100  #if seed zero then no seed?     
nb_sample<-1           #number of time random sampling must be repeated for every hold out proportion
step<-0         
constant<-0             #if value 1 then use the same samples as date one for the all set of dates
prop_minmax<-c(0.3,0.3)  #if prop_min=prop_max and step=0 then predicitons are done for the number of dates...
infile_dates<-"list_365_dates_04212012.txt"

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

#Choose interpolation method...
interpolation_method<-c("gam_fusion","gam_CAI") #other otpions to be added later

#Default name of LST avg to be matched               
lst_avg<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")  

in_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data"
#Create on the fly output folder...
out_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data"
script_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/"
setwd(in_path)


#PARSING INPUTS/ARGUMENTS
list_param_data_prep<-c(infile_monthly,infile_daily,infile_locs,infile_covariates,covar_names,var,out_prefix,CRS_locs_WGS84)
list_param_raster_prediction<-c(list_param_data_prep,
                                seed_number,nb_sample,step,constant,prop_minmax,infile_dates,
                                list_models,lst_avg,in_path,out_path,script_path,
                                interpolation_method)

#9 parameters used in the data preparation stage and input in the current script
list_param_data_prep<-list_param_raster_prediction$list_param_data_prep
infile_monthly<-list_param_data_prep$infile_monthly
infile_daily<-list_param_data_prep$infile_daily
infile_locs<-list_param_data_prep$infile_locs
infile_covariates<-list_param_data_prep$infile_covariates
covar_names<- list_param_data_prep$covar_names
var<-list_param_data_prep$var
out_prefix<-list_param_data_prep$out_prefix
CRS_locs_WGS84<-list_param_data_prep$CRS_locs_WGS84

#6 parameters for sampling function
seed_number<-list_param_raster_prediction$seed_number
nb_sample<-list_param_raster_prediction$nb_sample
step<-list_param_raster_prediction$step
constant<-list_param_raster_prediction$constant
prop_minmax<-list_param_raster_prediction$prop_minmax
infile_dates<-list_param_raster_prediction$infile_dates

#6 additional parameters for monthly climatology and more
list_models<-list_param_raster_prediction$list_models
lst_avg<-list_param_raster_prediction$lst_avg
in_path<-list_param_raster_prediction$in_path
out_path<-list_param_raster_prediction$out_path
script_path<-list_param_raster_prediction$script_path
interpolation_method<-list_param_raster_prediction$interpolation_method

source(file.path(script_path,"sampling_script_functions_03052013.R"))
source(file.path(script_path,"GAM_fusion_function_multisampling_03122013.R"))
source(file.path(script_path,"GAM_fusion_function_multisampling_validation_metrics_02262013.R"))


###################### START OF THE SCRIPT ########################


if (var=="TMAX"){
  y_var_name<-"dailyTmax"                                       
}
if (var=="TMIN"){
  y_var_name<-"dailyTmin"                                       
}

################# CREATE LOG FILE #####################

#create log file to keep track of details such as processing times and parameters.

log_fname<-paste("R_log_raster_prediction",out_prefix, ".log",sep="")

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

############### READING INPUTS: DAILY STATION DATA AND OTEHR DATASETS  #################

ghcn<-readOGR(dsn=in_path,layer=sub(".shp","",basename(infile_daily)))
CRS_interp<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)
stat_loc<-readOGR(dsn=in_path,layer=sub(".shp","",basename(infile_locs)))
dates <-readLines(file.path(in_path,infile_dates)) #dates to be predicted

#Reading of covariate brick covariates can be changed...
              
s_raster<-brick(infile_covariates)                   #read in the data brck
names(s_raster)<-covar_names               #Assigning names to the raster layers: making sure it is included in the extraction
pos<-match("elev",names(s_raster))
names(s_raster)[pos]<-"elev_1"

#Screen for extreme values": this needs more thought, min and max val vary with regions
#min_val<-(-15+273.16) #if values less than -15C then screen out (note the Kelvin units that will need to be changed later in all datasets)
#r1[r1 < (min_val)]<-NA

#Reading monthly data
data3<-readOGR(dsn=in_path,layer=sub(".shp","",basename(infile_monthly)))
dst_all<-data3
dst<-data3

### TO DO -important ###
#Cleaning/sceerniging functions for daily stations, monthly stations and covariates?? do this during the preparation stage!!!??
###

########### CREATE SAMPLING -TRAINING AND TESTING STATIONS ###########

#Input for sampling function...

#dates #list of dates for prediction
#ghcn_name<-"ghcn" #infile daily data 

list_param_sampling<-list(seed_number,nb_sample,step,constant,prop_minmax,dates,ghcn)
#list_param_sampling<-list(seed_number,nb_sample,step,constant,prop_minmax,dates,ghcn_name)
names(list_param_sampling)<-c("seed_number","nb_sample","step","constant","prop_minmax","dates","ghcn")

#run function
sampling_obj<-sampling_training_testing(list_param_sampling)


########### PREDICT FOR MONTHLY SCALE  ##################

#First predict at the monthly time scale: climatology
writeLines("Predictions at monthly scale:",con=log_file,sep="\n")
t1<-proc.time()
j=12
list_param_runClim_KGFusion<-list(j,s_raster,covar_names,list_models,dst,var,y_var_name, out_prefix)
names(list_param_runClim_KGFusion)<-c("list_index","covar_rast","covar_names","list_models","dst","var","y_var_name","out_prefix")
source(file.path(script_path,"GAM_fusion_function_multisampling_03122013.R"))

gamclim_fus_mod<-mclapply(1:6, list_param=list_param_runClim_KGFusion, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
#gamclim_fus_mod<-mclapply(1:6, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
save(gamclim_fus_mod,file= paste("gamclim_fus_mod",out_prefix,".RData",sep=""))
t2<-proc.time()-t1
writeLines(as.character(t2),con=log_file,sep="\n")

#now get list of raster clim layers

list_tmp<-vector("list",length(gamclim_fus_mod))
for (i in 1:length(gamclim_fus_mod)){
  tmp<-gamclim_fus_mod[[i]]$clim
  list_tmp[[i]]<-tmp
}

################## PREDICT AT DAILY TIME SCALE #################

#put together list of clim models per month...
#rast_clim_yearlist<-list_tmp
clim_yearlist<-list_tmp
#Second predict at the daily time scale: delta

#gam_fus_mod<-mclapply(1:1, runGAMFusion,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement
writeLines("Predictions at the daily scale:",con=log_file,sep="\n")
t1<-proc.time()

#input a list:note that ghcn.subsets is not sampling_obj$data_day_ghcn
list_param_runGAMFusion<-list(i,clim_yearlist,sampling_obj,dst,var,y_var_name, out_prefix)
names(list_param_runGAMFusion)<-c("list_index","clim_yearlist","sampling_obj","dst","var","y_var_name","out_prefix")
#test<-mclapply(1:18, runGAMFusion,list_param=list_param_runGAMFusion,mc.preschedule=FALSE,mc.cores = 9)

gam_fus_mod<-mclapply(1:length(sampling_obj$ghcn_data_day),list_param=list_param_runGAMFusion,runGAMFusion,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement

#gam_fus_mod<-mclapply(1:length(sampling_obj$ghcn_data_day),runGAMFusion,list_param_runGAMFusion,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
#gam_fus_mod<-mclapply(1:length(ghcn.subsets), runGAMFusion,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
save(gam_fus_mod,file= paste("gam_fus_mod",out_prefix,".RData",sep=""))
t2<-proc.time()-t1
writeLines(as.character(t2),con=log_file,sep="\n")

############### NOW RUN VALIDATION #########################

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

#################### ASSESSMENT OF PREDICTIONS: PLOTS OF ACCURACY METRICS ###########

##Create data.frame with valiation metrics for a full year
tb_diagnostic_v<-extract_from_list_obj(gam_fus_validation_mod,"metrics_v")
rownames(tb_diagnostic_v)<-NULL #remove row names

#Call function to create plots of metrics for validation dataset
metric_names<-c("rmse","mae","me","r","m50")
summary_metrics<-boxplot_from_tb(tb_diagnostic_v,metric_names,out_prefix)
names(summary_metrics)<-c("avg","median")
##Write out information concerning accuracy and predictions
outfile<-file.path(in_path,paste("assessment_measures_",out_prefix,".txt",sep=""))
write.table(tb_diagnostic_v,file= outfile,row.names=FALSE,sep=",")
write.table(summary_metrics[[1]], file= outfile, append=TRUE,sep=",") #write out avg
write.table(summary_metrics[[2]], file= outfile, append=TRUE,sep=",") #write out median

#################### CLOSE LOG FILE  #############

#close log_file connection and add meta data
writeLines("Finished script process time:",con=log_file,sep="\n")
time2<-proc.time()-time1
writeLines(as.character(time2),con=log_file,sep="\n")
#later on add all the paramters used in the script...
writeLines(paste("Finished script at this local Date and Time: ",as.character(Sys.time()),sep=""),
           con=log_file,sep="\n")
writeLines("End of script",con=log_file,sep="\n")
close(log_file)

############################################################
######################## END OF SCRIPT #####################
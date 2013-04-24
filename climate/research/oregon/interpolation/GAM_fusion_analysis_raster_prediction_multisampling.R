#########################    Raster prediction    ####################################
############################ Interpolation of temperature for given processing region ##########################################
#This script interpolates temperature values using MODIS LST, covariates and GHCND station data.                      
#It requires the text file of stations and a shape file of the study area.           
#Note that the projection for both GHCND and study area is lonlat WGS84.       
#Options to run this program are:
#1) Multisampling: vary the porportions of hold out and use random samples for each run
#2)Constant sampling: use the same sample over the runs
#3)over dates: run over for example 365 dates without mulitsampling
#4)use seed number: use seed if random samples must be repeatable
#5)possibilty of running GAM+FUSION or GAM+CAI and other options added
#The interpolation is done first at the monthly time scale then delta surfaces are added.
#AUTHOR: Benoit Parmentier                                                                        
#DATE: 04/22/2013                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#568--     
#
# TO DO:
# 1) modify to make it general for any method i.e. make call to method e.g. gam_fus, gam_cai etc.
# 2) simplify and bundle validation steps, make it general--method_mod_validation
# 3) solve issues with log file recordings
# 4) output location folder on the fly???

###################################################################################################

raster_prediction_fun <-function(list_param_raster_prediction){

  ##Function to predict temperature interpolation with 21 input parameters
  #9 parameters used in the data preparation stage and input in the current script
  #1)list_param_data_prep: used in earlier code for the query from the database and extraction for raster brick
  #2)infile_monthly:
  #3)infile_daily
  #4)infile_locs:
  #5)infile_covariates: raster covariate brick, tif file
  #6)covar_names: covar_names #remove at a later stage...
  #7)var: variable being interpolated-TMIN or TMAX
  #8)out_prefix
  #9)CRS_locs_WGS84
  #
  #6 parameters for sampling function
  #10)seed_number
  #11)nb_sample
  #12)step
  #13)constant
  #14)prop_minmax
  #15)dates_selected
  #
  #6 additional parameters for monthly climatology and more
  #16)list_models: model formulas in character vector
  #17)lst_avg: LST climatology name in the brick of covariate--change later
  #18)n_path
  #19)out_path
  #20)script_path: path to script
  #21)interpolation_method: c("gam_fusion","gam_CAI") #other otpions to be added later
  
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
  library(gdata) #Nesssary to use cbindX
  
  ### Parameters and arguments
  #PARSING INPUTS/ARGUMENTS
#   
#   names(list_param_raster_prediction)<-c("list_param_data_prep",
#                                          "seed_number","nb_sample","step","constant","prop_minmax","dates_selected",
#                                          "list_models","lst_avg","in_path","out_path","script_path",
#                                          "interpolation_method")
  
  #9 parameters used in the data preparation stage and input in the current script
  list_param_data_prep<-list_param_raster_prediction$list_param_data_prep
  infile_monthly<-list_param_data_prep$infile_monthly
  infile_daily<-list_param_data_prep$infile_daily
  infile_locs<-list_param_data_prep$infile_locs
  infile_covariates<-list_param_data_prep$infile_covariates #raster covariate brick, tif file
  covar_names<- list_param_data_prep$covar_names #remove at a later stage...
  var<-list_param_data_prep$var
  out_prefix<-list_param_data_prep$out_prefix
  CRS_locs_WGS84<-list_param_data_prep$CRS_locs_WGS84
  
  #6 parameters for sampling function
  seed_number<-list_param_raster_prediction$seed_number
  nb_sample<-list_param_raster_prediction$nb_sample
  step<-list_param_raster_prediction$step
  constant<-list_param_raster_prediction$constant
  prop_minmax<-list_param_raster_prediction$prop_minmax
  dates_selected<-list_param_raster_prediction$dates_selected
  
  #6 additional parameters for monthly climatology and more
  list_models<-list_param_raster_prediction$list_models
  lst_avg<-list_param_raster_prediction$lst_avg
  in_path<-list_param_raster_prediction$in_path
  out_path<-list_param_raster_prediction$out_path
  script_path<-list_param_raster_prediction$script_path
  interpolation_method<-list_param_raster_prediction$interpolation_method
  
  setwd(in_path)
  
  #Sourcing in the master script to avoid confusion on the latest versions of scripts and functions!!!
  
  #source(file.path(script_path,"sampling_script_functions_03122013.R"))
  #source(file.path(script_path,"GAM_fusion_function_multisampling_03122013.R"))
  #source(file.path(script_path,"GAM_fusion_function_multisampling_validation_metrics_03182013.R"))
  
  
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
  #dates2 <-readLines(file.path(in_path,dates_selected)) #dates to be predicted, now read directly from the file
  if (dates_selected==""){
    dates<-as.character(sort(unique(ghcn$date))) #dates to be predicted 
  }
  if (dates_selected!=""){
    dates<-dates_selected #dates to be predicted 
  }
  
  #Reading of covariate brick covariates can be changed...
  
  s_raster<-brick(infile_covariates)                   #read in the data brck
  names(s_raster)<-covar_names               #Assigning names to the raster layers: making sure it is included in the extraction
    
  #Reading monthly data
  dst<-readOGR(dsn=in_path,layer=sub(".shp","",basename(infile_monthly)))
  
  ### TO DO -important ###
  #SCREENING IN COVARIATE SCRIPT AND DATA PREP SCRIPT !!! Only perform predictions here
  #Screen for extreme values": this needs more thought, min and max val vary with regions
  #min_val<-(-15+273.16) #if values less than -15C then screen out (note the Kelvin units that will need to be changed later in all datasets)
  #r1[r1 < (min_val)]<-NA
  
  ########### CREATE SAMPLING -TRAINING AND TESTING STATIONS ###########
  
  #Input for sampling function...
  
  #dates #list of dates for prediction
  #ghcn_name<-"ghcn" #infile daily data 
  
  list_param_sampling<-list(seed_number,nb_sample,step,constant,prop_minmax,dates,ghcn)
  #list_param_sampling<-list(seed_number,nb_sample,step,constant,prop_minmax,dates,ghcn_name)
  names(list_param_sampling)<-c("seed_number","nb_sample","step","constant","prop_minmax","dates","ghcn")
  
  #run function, note that dates must be a character vector!!
  sampling_obj<-sampling_training_testing(list_param_sampling)
  
  ########### PREDICT FOR MONTHLY SCALE  ##################
  
  #First predict at the monthly time scale: climatology
  writeLines("Predictions at monthly scale:",con=log_file,sep="\n")
  t1<-proc.time()
  j=12
  #browser()
  if (interpolation_method=="gam_fusion"){
    list_param_runClim_KGFusion<-list(j,s_raster,covar_names,lst_avg,list_models,dst,var,y_var_name, out_prefix)
    names(list_param_runClim_KGFusion)<-c("list_index","covar_rast","covar_names","lst_avg","list_models","dst","var","y_var_name","out_prefix")
    #source(file.path(script_path,"GAM_fusion_function_multisampling_03122013.R"))
    clim_method_mod_obj<-mclapply(1:12, list_param=list_param_runClim_KGFusion, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
    #test<-runClim_KGFusion(1,list_param=list_param_runClim_KGFusion)
    #gamclim_fus_mod<-mclapply(1:6, list_param=list_param_runClim_KGFusion, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
    
  }
  
  if (interpolation_method=="gam_CAI"){
    list_param_runClim_KGCAI<-list(j,s_raster,covar_names,lst_avg,list_models,dst,var,y_var_name, out_prefix)
    names(list_param_runClim_KGCAI)<-c("list_index","covar_rast","covar_names","lst_avg","list_models","dst","var","y_var_name","out_prefix")
    clim_method_mod_obj<-mclapply(1:12, list_param=list_param_runClim_KGCAI, runClim_KGCAI,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
    #test<-runClim_KGCAI(1,list_param=list_param_runClim_KGCAI)
    #gamclim_fus_mod<-mclapply(1:6, list_param=list_param_runClim_KGFusion, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
  }
    
  #gamclim_fus_mod<-runClim_KGFusion(1,list_param=list_param_runClim_KGFusion) #This is the end bracket from mclapply(...) statement
  save(clim_method_mod_obj,file= paste(interpolation_method,"_mod_",y_var_name,out_prefix,".RData",sep=""))
  t2<-proc.time()-t1
  writeLines(as.character(t2),con=log_file,sep="\n")
  
  #now get list of raster clim layers
  
  list_tmp<-vector("list",length(clim_method_mod_obj))
  for (i in 1:length(clim_method_mod_obj)){
    tmp<-clim_method_mod_obj[[i]]$clim
    list_tmp[[i]]<-tmp
  }
  
  ################## PREDICT AT DAILY TIME SCALE #################
  #Predict at daily time scale from single time scale or multiple time scale methods: 2 methods availabe now
  
  #put together list of clim models per month...
  #rast_clim_yearlist<-list_tmp
  clim_yearlist<-list_tmp
  #Second predict at the daily time scale: delta
  
  #method_mod_obj<-mclapply(1:1, runGAMFusion,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement
  writeLines("Predictions at the daily scale:",con=log_file,sep="\n")
  t1<-proc.time()
  
  if (interpolation_method=="gam_CAI" | interpolation_method=="gam_fusion"){
    #input a list:note that ghcn.subsets is not sampling_obj$data_day_ghcn
    list_param_run_prediction_daily_deviation <-list(i,clim_yearlist,sampling_obj,dst,var,y_var_name, interpolation_method,out_prefix)
    names(list_param_run_prediction_daily_deviation)<-c("list_index","clim_yearlist","sampling_obj","dst","var","y_var_name","interpolation_method","out_prefix")
    #test<-mclapply(1:18, runGAMFusion,list_param=list_param_runGAMFusion,mc.preschedule=FALSE,mc.cores = 9)
    #test<-runGAMFusion(1,list_param=list_param_runGAMFusion)
    
    method_mod_obj<-mclapply(1:length(sampling_obj$ghcn_data_day),list_param=list_param_run_prediction_daily_deviation,run_prediction_daily_deviation,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
    #method_mod_obj<-mclapply(1:1,list_param=list_param_run_prediction_daily_deviation,run_prediction_daily_deviation,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
    
    #method_mod_obj<-mclapply(1:length(sampling_obj$ghcn_data_day),runGAMFusion,list_param_runGAMFusion,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
    #method_mod_obj<-mclapply(1:length(ghcn.subsets), runGAMFusion,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
    
  }
    
  save(method_mod_obj,file= paste("method_mod_obj_",interpolation_method,"_",y_var_name,out_prefix,".RData",sep=""))
  t2<-proc.time()-t1
  writeLines(as.character(t2),con=log_file,sep="\n")
  #browser()
  
  ############### NOW RUN VALIDATION #########################
  #SIMPLIFY THIS PART: one call
  
  list_tmp<-vector("list",length(method_mod_obj))
  for (i in 1:length(method_mod_obj)){
    tmp<-method_mod_obj[[i]][[y_var_name]]  #y_var_name is the variable predicted (dailyTmax or dailyTmin)
    list_tmp[[i]]<-tmp
  }
  rast_day_yearlist<-list_tmp #list of predicted images
  
  writeLines("Validation step:",con=log_file,sep="\n")
  t1<-proc.time()
  #calculate_accuary_metrics<-function(i)
  list_param_validation<-list(i,rast_day_yearlist,method_mod_obj,y_var_name, out_prefix)
  names(list_param_validation)<-c("list_index","rast_day_year_list","method_mod_obj","y_var_name","out_prefix") #same names for any method
  
  #gam_fus_validation_mod<-mclapply(1:length(method_mod_obj), calculate_accuracy_metrics,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
  validation_mod_obj <-mclapply(1:length(method_mod_obj), list_param=list_param_validation, calculate_accuracy_metrics,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
  
  #gam_fus_validation_mod<-mclapply(1:1, calculate_accuracy_metrics,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement
  save(validation_mod_obj,file= paste(interpolation_method,"_validation_mod_obj_",y_var_name,out_prefix,".RData",sep=""))
  t2<-proc.time()-t1
  writeLines(as.character(t2),con=log_file,sep="\n")
  
  #################### ASSESSMENT OF PREDICTIONS: PLOTS OF ACCURACY METRICS ###########
  
  ##Create data.frame with valiation metrics for a full year
  tb_diagnostic_v<-extract_from_list_obj(validation_mod_obj,"metrics_v")
  rownames(tb_diagnostic_v)<-NULL #remove row names
  
  #Call function to create plots of metrics for validation dataset
  metric_names<-c("rmse","mae","me","r","m50")
  summary_metrics_v<-boxplot_from_tb(tb_diagnostic_v,metric_names,out_prefix)
  names(summary_metrics_v)<-c("avg","median")
  
  #################### CLOSE LOG FILE  ####################
  
  #close log_file connection and add meta data
  writeLines("Finished script process time:",con=log_file,sep="\n")
  time2<-proc.time()-time1
  writeLines(as.character(time2),con=log_file,sep="\n")
  #later on add all the paramters used in the script...
  writeLines(paste("Finished script at this local Date and Time: ",as.character(Sys.time()),sep=""),
             con=log_file,sep="\n")
  writeLines("End of script",con=log_file,sep="\n")
  close(log_file)
  
  ################### PREPARE RETURN OBJECT ###############
  #Will add more information to be returned
  
  raster_prediction_obj<-list(clim_method_mod_obj,method_mod_obj,validation_mod_obj,tb_diagnostic_v,summary_metrics_v)
  names(raster_prediction_obj)<-c("clim_method_mod_obj","method_mod_obj","validation_mod_obj","tb_diagnostic_v",
                                  "summary_metrics_v")  
  save(raster_prediction_obj,file= paste("raster_prediction_obj_",interpolation_method,"_", y_var_name,out_prefix,".RData",sep=""))
  
  return(raster_prediction_obj)
}

####################################################################
######################## END OF SCRIPT/FUNCTION #####################
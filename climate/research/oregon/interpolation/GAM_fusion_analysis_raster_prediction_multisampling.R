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
#5)possibilty of running single and multiple time scale methods:
   # gam_daily, kriging_daily,gwr_daily,gam_CAI,gam_fusion,kriging_fusion,gwr_fusion and other options added.
#For multiple time scale methods, the interpolation is done first at the monthly time scale then delta surfaces are added.
#AUTHOR: Benoit Parmentier                                                                        
#DATE: 07/30/2013                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#568--     
#
# TO DO:
#Add methods to for CAI

###################################################################################################

raster_prediction_fun <-function(list_param_raster_prediction){

  ##Function to predict temperature interpolation with 21 input parameters
  #9 parameters used in the data preparation stage and input in the current script
  #1)list_param_data_prep: used in earlier code for the query from the database and extraction for raster brick
  #2)infile_monthly: monthly averages with covariates for GHCND stations obtained after query
  #3)infile_daily: daily GHCND stations with covariates, obtained after query
  #4)infile_locs: vector file with station locations for the processing/study area (ESRI shapefile)
  #5)infile_covariates: raster covariate brick, tif file
  #6)covar_names: covar_names #remove at a later stage...
  #7)var: variable being interpolated-TMIN or TMAX
  #8)out_prefix
  #9)CRS_locs_WGS84
  #10)screen_data_training
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
  library(automap)  #autokrige function
  library(spgwr)   #GWR method
  
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
  out_path<-list_param_raster_prediction$out_path
  script_path<-list_param_raster_prediction$script_path
  interpolation_method<-list_param_raster_prediction$interpolation_method
  screen_data_training <-list_param_raster_prediction$screen_data_training
  
  setwd(out_path)
  
  ###################### START OF THE SCRIPT ########################
   
  if (var=="TMAX"){
    y_var_name<-"dailyTmax"                                       
  }
  
  if (var=="TMIN"){
    y_var_name<-"dailyTmin"                                       
  }
  
  ################# CREATE LOG FILE #####################
  
  #create log file to keep track of details such as processing times and parameters.
  
  #log_fname<-paste("R_log_raster_prediction",out_prefix, ".log",sep="")
  log_fname<-paste("R_log_raster_prediction",out_prefix, ".log",sep="")
  #sink(log_fname) #create new log file
  file.create(file.path(out_path,log_fname)) #create new log file
  
  time1<-proc.time()    #Start stop watch
  
  cat(paste("Starting script at this local Date and Time: ",as.character(Sys.time()),sep=""),
             file=log_fname,sep="\n")
  cat("Starting script process time:",file=log_fname,sep="\n",append=TRUE)
  cat(as.character(time1),file=log_fname,sep="\n",append=TRUE)    
  
  ############### READING INPUTS: DAILY STATION DATA AND OTEHR DATASETS  #################
  
  ghcn<-readOGR(dsn=dirname(infile_daily),layer=sub(".shp","",basename(infile_daily)))
  CRS_interp<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)
  stat_loc<-readOGR(dsn=dirname(infile_locs),layer=sub(".shp","",basename(infile_locs)))
  #dates2 <-readLines(file.path(in_path,dates_selected)) #dates to be predicted, now read directly from the file
  if (dates_selected==""){
    dates<-as.character(sort(unique(ghcn$date))) #dates to be predicted 
  }
  if (dates_selected!=""){
    dates<-dates_selected #dates to be predicted 
  }
  
  #Reading in covariate brickcan be changed...
  
  s_raster<-brick(infile_covariates)                   #read in the data brck
  names(s_raster)<-covar_names               #Assigning names to the raster layers: making sure it is included in the extraction
    
  #Reading monthly data
  dst<-readOGR(dsn=dirname(infile_monthly),layer=sub(".shp","",basename(infile_monthly)))
    
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
  cat("Predictions at monthly scale:",file=log_fname,sep="\n", append=TRUE)
  cat(paste("Local Date and Time: ",as.character(Sys.time()),sep=""),
      file=log_fname,sep="\n")
  t1<-proc.time()
  j=12
  #browser() #Missing out_path for gam_fusion list param!!!
  #if (interpolation_method=="gam_fusion"){
  if (interpolation_method %in% c("gam_fusion","kriging_fusion","gwr_fusion")){
    list_param_runClim_KGFusion<-list(j,s_raster,covar_names,lst_avg,list_models,dst,var,y_var_name, out_prefix,out_path)
    names(list_param_runClim_KGFusion)<-c("list_index","covar_rast","covar_names","lst_avg","list_models","dst","var","y_var_name","out_prefix","out_path")
    #source(file.path(script_path,"GAM_fusion_function_multisampling_03122013.R"))
    clim_method_mod_obj<-mclapply(1:12, list_param=list_param_runClim_KGFusion, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
    #clim_method_mod_obj<-mclapply(1:6, list_param=list_param_runClim_KGFusion, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
    #test<-runClim_KGFusion(1,list_param=list_param_runClim_KGFusion)
    save(clim_method_mod_obj,file= file.path(out_path,paste(interpolation_method,"_mod_",y_var_name,out_prefix,".RData",sep="")))
    list_tmp<-vector("list",length(clim_method_mod_obj))
    for (i in 1:length(clim_method_mod_obj)){
      tmp<-clim_method_mod_obj[[i]]$clim
      list_tmp[[i]]<-tmp
    }
    clim_yearlist<-list_tmp
  }
  
  #to be added gwr_CAI and kriging_CAI
  if (interpolation_method=="gam_CAI"){
    list_param_runClim_KGCAI<-list(j,s_raster,covar_names,lst_avg,list_models,dst,var,y_var_name, out_prefix,out_path)
    names(list_param_runClim_KGCAI)<-c("list_index","covar_rast","covar_names","lst_avg","list_models","dst","var","y_var_name","out_prefix","out_path")
    clim_method_mod_obj<-mclapply(1:12, list_param=list_param_runClim_KGCAI, runClim_KGCAI,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
    #test<-runClim_KGCAI(1,list_param=list_param_runClim_KGCAI)
    #gamclim_fus_mod<-mclapply(1:6, list_param=list_param_runClim_KGFusion, runClim_KGFusion,mc.preschedule=FALSE,mc.cores = 6) 
    save(clim_method_mod_obj,file= file.path(out_path,paste(interpolation_method,"_mod_",y_var_name,out_prefix,".RData",sep="")))
    list_tmp<-vector("list",length(clim_method_mod_obj))
    for (i in 1:length(clim_method_mod_obj)){
      tmp<-clim_method_mod_obj[[i]]$clim
      list_tmp[[i]]<-tmp
    }
    clim_yearlist<-list_tmp
  }
  t2<-proc.time()-t1
  cat(as.character(t2),file=log_fname,sep="\n", append=TRUE)

  ################## PREDICT AT DAILY TIME SCALE #################
  #Predict at daily time scale from single time scale or multiple time scale methods: 2 methods availabe now
  
  #put together list of clim models per month...
  #rast_clim_yearlist<-list_tmp
  
  #Second predict at the daily time scale: delta
  
  #method_mod_obj<-mclapply(1:1, runGAMFusion,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement
  cat("Predictions at the daily scale:",file=log_fname,sep="\n", append=TRUE)
  t1<-proc.time()
  cat(paste("Local Date and Time: ",as.character(Sys.time()),sep=""),
      file=log_fname,sep="\n")
  
  #TODO : Same call for all functions!!! Replace by one "if" for all multi time scale methods...
  #The methods could be defined earlier as constant??
  if (interpolation_method %in% c("gam_CAI","gam_fusion","kriging_fusion","gwr_fusion")){
    #input a list:note that ghcn.subsets is not sampling_obj$data_day_ghcn
    i<-1
    list_param_run_prediction_daily_deviation <-list(i,clim_yearlist,sampling_obj,dst,var,y_var_name, interpolation_method,out_prefix,out_path)
    names(list_param_run_prediction_daily_deviation)<-c("list_index","clim_yearlist","sampling_obj","dst","var","y_var_name","interpolation_method","out_prefix","out_path")
    #test<-mclapply(1:18, runGAMFusion,list_param=list_param_runGAMFusion,mc.preschedule=FALSE,mc.cores = 9)
    #test<-runGAMFusion(1,list_param=list_param_runGAMFusion)
    
    method_mod_obj<-mclapply(1:length(sampling_obj$ghcn_data_day),list_param=list_param_run_prediction_daily_deviation,run_prediction_daily_deviation,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
    save(method_mod_obj,file= file.path(out_path,paste("method_mod_obj_",interpolation_method,"_",y_var_name,out_prefix,".RData",sep="")))
  }
  
  #TODO : Same call for all functions!!! Replace by one "if" for all daily single time scale methods...
  if (interpolation_method=="gam_daily"){
    #input a list:note that ghcn.subsets is not sampling_obj$data_day_ghcn
    i<-1
    list_param_run_prediction_gam_daily <-list(i,s_raster,covar_names,lst_avg,list_models,dst,screen_data_training,var,y_var_name, sampling_obj,interpolation_method,out_prefix,out_path)
    names(list_param_run_prediction_gam_daily)<-c("list_index","covar_rast","covar_names","lst_avg","list_models","dst","screen_data_training","var","y_var_name","sampling_obj","interpolation_method","out_prefix","out_path")
    #test <- runGAM_day_fun(1,list_param_run_prediction_gam_daily)
    
    method_mod_obj<-mclapply(1:length(sampling_obj$ghcn_data_day),list_param=list_param_run_prediction_gam_daily,runGAM_day_fun,mc.preschedule=FALSE,mc.cores = 11) #This is the end bracket from mclapply(...) statement
    #method_mod_obj<-mclapply(1:11,list_param=list_param_run_prediction_gam_daily,runGAM_day_fun,mc.preschedule=FALSE,mc.cores = 11) #This is the end bracket from mclapply(...) statement
    
    save(method_mod_obj,file= file.path(out_path,paste("method_mod_obj_",interpolation_method,"_",y_var_name,out_prefix,".RData",sep="")))
    
  }
  
  if (interpolation_method=="kriging_daily"){
    #input a list:note that ghcn.subsets is not sampling_obj$data_day_ghcn
    i<-1
    list_param_run_prediction_kriging_daily <-list(i,s_raster,covar_names,lst_avg,list_models,dst,var,y_var_name, sampling_obj,interpolation_method,out_prefix,out_path)
    names(list_param_run_prediction_kriging_daily)<-c("list_index","covar_rast","covar_names","lst_avg","list_models","dst","var","y_var_name","sampling_obj","interpolation_method","out_prefix","out_path")
    #test <- runKriging_day_fun(1,list_param_run_prediction_kriging_daily)
    method_mod_obj<-mclapply(1:length(sampling_obj$ghcn_data_day),list_param=list_param_run_prediction_kriging_daily,runKriging_day_fun,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
    #method_mod_obj<-mclapply(1:18,list_param=list_param_run_prediction_kriging_daily,runKriging_day_fun,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
    
    save(method_mod_obj,file= file.path(out_path,paste("method_mod_obj_",interpolation_method,"_",y_var_name,out_prefix,".RData",sep="")))
    
  }
  
  if (interpolation_method=="gwr_daily"){
    #input a list:note that ghcn.subsets is not sampling_obj$data_day_ghcn
    i<-1
    list_param_run_prediction_gwr_daily <-list(i,s_raster,covar_names,lst_avg,list_models,dst,var,y_var_name, sampling_obj,interpolation_method,out_prefix,out_path)
    names(list_param_run_prediction_gwr_daily)<-c("list_index","covar_rast","covar_names","lst_avg","list_models","dst","var","y_var_name","sampling_obj","interpolation_method","out_prefix","out_path")
    #test <- run_interp_day_fun(1,list_param_run_prediction_gwr_daily)
    method_mod_obj<-mclapply(1:length(sampling_obj$ghcn_data_day),list_param=list_param_run_prediction_gwr_daily,run_interp_day_fun,mc.preschedule=FALSE,mc.cores = 11) #This is the end bracket from mclapply(...) statement
    #method_mod_obj<-mclapply(1:9,list_param=list_param_run_prediction_gwr_daily,run_interp_day_fun,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
    #method_mod_obj<-mclapply(1:18,list_param=list_param_run_prediction_kriging_daily,runKriging_day_fun,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
    
    save(method_mod_obj,file= file.path(out_path,paste("method_mod_obj_",interpolation_method,"_",y_var_name,out_prefix,".RData",sep="")))
    
  }
  t2<-proc.time()-t1
  cat(as.character(t2),file=log_fname,sep="\n", append=TRUE)
  #browser()
  
  ############### NOW RUN VALIDATION #########################
  #SIMPLIFY THIS PART: one call
  
  list_tmp<-vector("list",length(method_mod_obj))
  for (i in 1:length(method_mod_obj)){
    tmp<-method_mod_obj[[i]][[y_var_name]]  #y_var_name is the variable predicted (dailyTmax or dailyTmin)
    list_tmp[[i]]<-tmp
  }
  rast_day_yearlist<-list_tmp #list of predicted images over full year...
  
  cat("Validation step:",file=log_fname,sep="\n", append=TRUE)
  t1<-proc.time()
  cat(paste("Local Date and Time: ",as.character(Sys.time()),sep=""),
      file=log_fname,sep="\n")
  
  list_param_validation<-list(i,rast_day_yearlist,method_mod_obj,y_var_name, out_prefix, out_path)
  names(list_param_validation)<-c("list_index","rast_day_year_list","method_mod_obj","y_var_name","out_prefix", "out_path") #same names for any method
  
  validation_mod_obj <-mclapply(1:length(method_mod_obj), list_param=list_param_validation, calculate_accuracy_metrics,mc.preschedule=FALSE,mc.cores = 9) 
      #test_val<-calculate_accuracy_metrics(1,list_param_validation)
  save(validation_mod_obj,file= file.path(out_path,paste(interpolation_method,"_validation_mod_obj_",y_var_name,out_prefix,".RData",sep="")))
  t2<-proc.time()-t1
  cat(as.character(t2),file=log_fname,sep="\n", append=TRUE)
  
  #################### ASSESSMENT OF PREDICTIONS: PLOTS OF ACCURACY METRICS ###########
  
  ##Create data.frame with valiation metrics for a full year
  tb_diagnostic_v<-extract_from_list_obj(validation_mod_obj,"metrics_v")
  rownames(tb_diagnostic_v)<-NULL #remove row names
  
  #Call functions to create plots of metrics for validation dataset
  metric_names<-c("rmse","mae","me","r","m50")
  summary_metrics_v<- boxplot_from_tb(tb_diagnostic_v,metric_names,out_prefix,out_path)
  names(summary_metrics_v)<-c("avg","median")
  summary_month_metrics_v<- boxplot_month_from_tb(tb_diagnostic_v,metric_names,out_prefix,out_path)
  
  #################### CLOSE LOG FILE  ####################
  
  #close log_file connection and add meta data
  cat("Finished script process time:",file=log_fname,sep="\n", append=TRUE)
  time2<-proc.time()-time1
  cat(as.character(time2),file=log_fname,sep="\n", append=TRUE)
  #later on add all the parameters used in the script...
  cat(paste("Finished script at this local Date and Time: ",as.character(Sys.time()),sep=""),
             file=log_fname,sep="\n", append=TRUE)
  cat("End of script",file=log_fname,sep="\n", append=TRUE)
  #close(log_fname)
  
  ################### PREPARE RETURN OBJECT ###############
  #Will add more information to be returned
  
  if (interpolation_method %in% c("gam_CAI","gam_fusion","kriging_fusion","gwr_fusion")){
    raster_prediction_obj<-list(clim_method_mod_obj,method_mod_obj,validation_mod_obj,tb_diagnostic_v,
                                summary_metrics_v,summary_month_metrics_v)
    names(raster_prediction_obj)<-c("clim_method_mod_obj","method_mod_obj","validation_mod_obj","tb_diagnostic_v",
                                    "summary_metrics_v","summary_month_metrics_v")  
    save(raster_prediction_obj,file= file.path(out_path,paste("raster_prediction_obj_",interpolation_method,"_", y_var_name,out_prefix,".RData",sep="")))
    
  }
  
  #use %in% instead of "|" operator
  if (interpolation_method=="gam_daily" | interpolation_method=="kriging_daily" | interpolation_method=="gwr_daily"){
    raster_prediction_obj<-list(method_mod_obj,validation_mod_obj,tb_diagnostic_v,
                                summary_metrics_v,summary_month_metrics_v)
    names(raster_prediction_obj)<-c("method_mod_obj","validation_mod_obj","tb_diagnostic_v",
                                    "summary_metrics_v","summary_month_metrics_v")  
    save(raster_prediction_obj,file= file.path(out_path,paste("raster_prediction_obj_",interpolation_method,"_", y_var_name,out_prefix,".RData",sep="")))
    
  }
  
  return(raster_prediction_obj)
}

####################################################################
######################## END OF SCRIPT/FUNCTION #####################
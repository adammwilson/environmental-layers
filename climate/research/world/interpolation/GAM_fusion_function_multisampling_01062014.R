##################  Functions for use in the raster prediction stage   #######################################
############################ Interpolation in a given tile/region ##########################################
#This script contains 5 functions used in the interpolation of temperature in the specfied study/processing area:                             
# 1)predict_raster_model<-function(in_models,r_stack,out_filename)                                                             
# 2)fit_models<-function(list_formulas,data_training)           
# 3)runClim_KGCAI<-function(j,list_param) : function that peforms GAM CAI method
# 4)runClim_KGFusion<-function(j,list_param) function for monthly step (climatology) in the fusion method
# 5)runGAMFusion <- function(i,list_param) : daily step for fusion method, perform daily prediction
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE: 10/03/2013                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--   

##Comments and TODO:
#This script is meant to be for general processing tile by tile or region by region.
# Note that the functions are called from GAM_fusion_analysis_raster_prediction_mutlisampling.R.
# This will be expanded to other methods.
##################################################################################################


predict_raster_model<-function(in_models,r_stack,out_filename){
  #This functions performs predictions on a raster grid given input models.
  #Arguments: list of fitted models, raster stack of covariates
  #Output: spatial grid data frame of the subset of tiles
  list_rast_pred<-vector("list",length(in_models))
  for (i in 1:length(in_models)){
    mod <-in_models[[i]] #accessing GAM model ojbect "j"
    raster_name<-out_filename[[i]]
    if (inherits(mod,"gam")) {           #change to c("gam","autoKrige")
      raster_pred<- predict(object=r_stack,model=mod,na.rm=FALSE,block.size=1000) #Using the coeff to predict new values.
      raster_pred<- predict(object=r_stack,model=mod,na.rm=FALSE) #Using the coeff to predict new values.
      names(raster_pred)<-"y_pred"  
      writeRaster(raster_pred, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
      #print(paste("Interpolation:","mod", j ,sep=" "))
      list_rast_pred[[i]]<-raster_name
    }
  }
  if (inherits(mod,"try-error")) {
    print(paste("no gam model fitted:",mod[1],sep=" ")) #change message for any model type...
  }
  return(list_rast_pred)
}

fit_models<-function(list_formulas,data_training){
  #This functions several models and returns model objects.
  #Arguments: - list of formulas for GAM models
  #           - fitting data in a data.frame or SpatialPointDataFrame
  #Output: list of model objects 
  list_fitted_models<-vector("list",length(list_formulas))
  for (k in 1:length(list_formulas)){
    formula<-list_formulas[[k]]
    mod<- try(gam(formula, data=data_training)) #change to any model!!
    #mod<- try(autoKrige(formula, input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
    model_name<-paste("mod",k,sep="")
    assign(model_name,mod) 
    list_fitted_models[[k]]<-mod
  }
  return(list_fitted_models) 
}

#Function to glue all methods together...still need to separate fit and training for gwr and kriging, ok for now
interpolate_area_fun <- function(method_interp,list_models,s_raster,list_out_filename,data_df){
  ##Function to fit and predict an interpolation surface
  ##Author: Benoit Parmentier
  ##Function depends on other functions!!!
  #inpputs:
  #method_interp: interpolation method with value "gam","gwr","kriging"
  #list_models: models to fit and predict as string (i.e.vector char)
  #s_raster: stack with covariate variables, must match in name the data.frame input
  #data_df: spatial point data.frame with covariates, must be projected match names of covariates
  #list_out_filename: list of char containing output names for models
  
  #Conver to formula object
  list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!
  cname<-paste("mod",1:length(list_formulas),sep="") #change to more meaningful name?
  
  names(list_out_filename)<-cname  
  
  ##Now carry out prediction
  if(method_interp=="gam"){    
    
    #First fitting
    mod_list<-fit_models(list_formulas,data_df) #only gam at this stage
    names(mod_list)<-cname
    
    #if raster provided then predict surface
    if(!is.null(s_raster)){
      #Second predict values for raster image...by providing fitted model list, raster brick and list of output file names
      rast_pred_list<-predict_raster_model(mod_list,s_raster,list_out_filename)
      names(rast_pred_list)<-cname
    }
  }
  
  if(method_interp%in%c("gwr","kriging")){
    
    #Call funciton to fit and predict gwr and/or kriging
    #month_prediction_obj<-predict_auto_krige_raster_model(list_formulas,s_raster,data_month,list_out_filename)
    rast_prediction_obj<-predict_autokrige_gwr_raster_model(method_interp,list_formulas,s_raster,data_df,list_out_filename)
    
    mod_list <-rast_prediction_obj$list_fitted_models
    rast_pred_list <-rast_prediction_obj$list_rast_pred
    names(rast_pred_list)<-cname
  }
  
  #Now prepare to return object
  interp_area_obj <-list(mod_list,list_formulas,rast_pred_list)
  names(interp_area_obj) <- c("mod_list","list_formulas","rast_pred_list")
  return(interp_area_obj)
} 

####
#TODO:
#Add log file and calculate time and sizes for processes-outputs
#Can combine runClim_KGFusion and runClim_KGCAI
runClim_KGCAI <-function(j,list_param){

  #Make this a function with multiple argument that can be used by mcmapply??
  #Arguments: 
  #1)list_index: j 
  #2)covar_rast: covariates raster images used in the modeling
  #3)covar_names: names of input variables 
  #4)lst_avg: list of LST climatogy names, may be removed later on
  #5)list_models: list input models for bias calculation
  #6)dst: data at the monthly time scale
  #7)var: TMAX or TMIN, variable being interpolated
  #8)y_var_name: output name, not used at this stage
  #9)out_prefix
  #10) out_path
  
  #The output is a list of four shapefile names produced by the function:
  #1) clim: list of output names for raster climatogies 
  #2) data_month: monthly training data for bias surface modeling
  #3) mod: list of model objects fitted 
  #4) formulas: list of formulas used in bias modeling
    
  ### PARSING INPUT ARGUMENTS
  #list_param_runGAMFusion<-list(i,clim_yearlist,sampling_obj,var,y_var_name, out_prefix)
    
  index<-list_param$j
  s_raster<-list_param$covar_rast
  covar_names<-list_param$covar_names
  lst_avg<-list_param$lst_avg
  list_models<-list_param$list_models
  dst<-list_param$dst #monthly station dataset
  var<-list_param$var
  y_var_name<-list_param$y_var_name
  out_prefix<-list_param$out_prefix
  out_path<-list_param$out_path

  #inserted #
  sampling_month_obj<-list_param$sampling_month_obj
  ghcn.month.subsets<-sampling_month_obj$ghcn_data
  sampling_month_dat <- sampling_month_obj$sampling_dat
  sampling_month_index <- sampling_month_obj$sampling_index
  
  #Model and response variable can be changed without affecting the script
  #prop_month<-0 #proportion retained for validation...
  #run_samp<-1 #sample number, can be introduced later...

  prop_month <- sampling_month_dat$prop[j] #proportion retained for validation...
  run_samp <- sampling_month_dat$run_samp[j] #sample number if multisampling...will need create mulitple prediction at daily!!! could be complicated
                                       #possibility is to average per proportion !!!
  
  date_month <-strptime(sampling_month_dat$date[j], "%Y%m%d")   # interpolation date being processed
  month_no <-strftime(date_month, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month_no,sep="") # name of LST month to be matched
  LST_name <-LST_month  
  #### STEP 2: PREPARE DATA
    
  #change here...use training data...
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  
  #LST_name <-lst_avg[j] # name of LST month to be matched
  #data_month$LST<-data_month[[LST_name]]
  
  dataset_month <-ghcn.month.subsets[[j]]
  mod_LST <- ghcn.month.subsets[[j]][,match(LST_month, names(ghcn.month.subsets[[j]]))]  #Match interpolation date and monthly LST average
  dataset_month$LST <- as.data.frame(mod_LST)[,1] #Add the variable LST to the dataset
  #change here...
  dst$LST<-dst[[LST_month]] #Add the variable LST to the monthly dataset
  proj_str<-proj4string(dst) #get the local projection information from monthly data
  
  #TMax to model..., add precip later
  if (var=="TMAX"){   
    dataset_month$y_var<-dataset_month$TMax #Adding TMax as the variable modeled
  }
  if (var=="TMIN"){   
    dataset_month$y_var<-dataset_month$TMin #Adding TMin as the variable modeled
  }
  
  ind.training <- sampling_month_index[[j]]
  ind.testing  <- setdiff(1:nrow(dataset_month), ind.training)
  data_month_s <- dataset_month[ind.training, ]   #Training dataset currently used in the modeling
  data_month_v <- dataset_month[ind.testing, ]    #Testing/validation dataset using input sampling
  
  data_month <- data_month_s #training data for  monthhly predictions...

  #date_proc<-strptime(sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
  #mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  #day<-as.integer(strftime(date_proc, "%d"))
  #year<-as.integer(strftime(date_proc, "%Y"))
  ## end of pasted
    
  #end of insert...
  
  #Fit gam models using data and list of formulas
  
  list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!
  cname<-paste("mod",1:length(list_formulas),sep="") #change to more meaningful name?
  
  #mod_list<-fit_models(list_formulas,data_month) #only gam at this stage
  #cname<-paste("mod",1:length(mod_list),sep="") #change to more meaningful name?
  
  #Adding layer LST to the raster stack  
  
  pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
  s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
  LST<-subset(s_raster,LST_name)
  names(LST)<-"LST"
  s_raster<-addLayer(s_raster,LST)            #Adding current month
  
  #Now generate file names for the predictions...
  list_out_filename<-vector("list",length(list_formulas))
  names(list_out_filename)<-cname  
  
  for (k in 1:length(list_out_filename)){
    #j indicate which month is predicted
    data_name<-paste(var,"_clim_month_",as.integer(month_no),"_",cname[k],"_",prop_month,
                     "_",run_samp,sep="")
    raster_name<-file.path(out_path,paste("CAI_",data_name,out_prefix,".tif", sep=""))
    list_out_filename[[k]]<-raster_name
  }
  
  ## Select the relevant method...
  
  if (interpolation_method=="gam_CAI"){
    
    #First fitting
    mod_list<-fit_models(list_formulas,data_month) #only gam at this stage
    names(mod_list)<-cname
    
    #Second predict values for raster image...by providing fitted model list, raster brick and list of output file names
    #now predict values for raster image...
    rast_clim_list<-predict_raster_model(mod_list,s_raster,list_out_filename)
    names(rast_clim_list)<-cname
    #Some models will not be predicted because of the lack of training data...remove empty string from list of models
        
  }
  
  
  if (interpolation_method %in% c("kriging_CAI","gwr_CAI")){
    if(interpolation_method=="kriging_CAI"){
      method_interp <- "kriging"
    }else{
      method_interp <- "gwr"
    }
    #Call function to fit and predict gwr and/or kriging
    #month_prediction_obj<-predict_auto_krige_raster_model(list_formulas,s_raster,data_month,list_out_filename)
    month_prediction_obj<-predict_autokrige_gwr_raster_model(method_interp,list_formulas,s_raster,data_month,list_out_filename)
    
    mod_list <-month_prediction_obj$list_fitted_models
    rast_clim_list <-month_prediction_obj$list_rast_pred
    names(rast_clim_list)<-cname
  }
  
  rast_clim_list<-rast_clim_list[!sapply(rast_clim_list,is.null)] #remove NULL elements in list
  
  #Adding Kriging for Climatology options 

  clim_xy<-coordinates(data_month)
  fitclim<-Krig(clim_xy,data_month$y_var,theta=1e5) #use TPS or krige 
  #fitclim<-Krig(clim_xy,data_month$TMax,theta=1e5) #use TPS or krige 
  mod_krtmp1<-fitclim
  model_name<-"mod_kr"
  
  clim_rast<-interpolate(LST,fitclim) #interpolation using function from raster package
  
  #Write out modeled layers
  data_name<-paste(var,"_clim_month_",as.integer(month_no),"_",model_name,"_",prop_month,
                   "_",run_samp,sep="")
  raster_name_clim<-file.path(out_path,paste("CAI_",data_name,out_prefix,".tif", sep=""))
  writeRaster(clim_rast, filename=raster_name_clim,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  #Adding to current objects
  mod_list[[model_name]]<-mod_krtmp1
  #rast_bias_list[[model_name]]<-raster_name_bias
  rast_clim_list[[model_name]]<-raster_name_clim
  
  #Prepare object to return
  clim_obj<-list(rast_clim_list,data_month,data_month_v,sampling_month_dat[j,],mod_list,list_formulas)
  names(clim_obj)<-c("clim","data_month","data_month_v","sampling_month_dat","mod","formulas")
  
  save(clim_obj,file= file.path(out_path,paste("clim_obj_CAI_month_",as.integer(month_no),"_",var,"_",prop_month,
                                               "_",run_samp,"_",out_prefix,".RData",sep="")))
  
  return(clim_obj) 
}
#

runClim_KGFusion<-function(j,list_param){
  
  #Make this a function with multiple argument that can be used by mcmapply??
  #Arguments: 
  #1)list_index: j 
  #2)covar_rast: covariates raster images used in the modeling
  #3)covar_names: names of input variables 
  #4)lst_avg: list of LST climatogy names, may be removed later on
  #5)list_models: list input models for bias calculation
  #6)dst: data at the monthly time scale
  #7)var: TMAX or TMIN, variable being interpolated
  #8)y_var_name: output name, not used at this stage
  #9)out_prefix
  #
  #The output is a list of four shapefile names produced by the function:
  #1) clim: list of output names for raster climatogies 
  #2) data_month: monthly training data for bias surface modeling
  #3) mod: list of model objects fitted 
  #4) formulas: list of formulas used in bias modeling
  
  ### PARSING INPUT ARGUMENTS
  #list_param_runGAMFusion<-list(i,clim_yearlist,sampling_obj,var,y_var_name, out_prefix)
  
  index<-list_param$j
  s_raster<-list_param$covar_rast
  covar_names<-list_param$covar_names
  lst_avg<-list_param$lst_avg
  list_models<-list_param$list_models
  dst<-list_param$dst #monthly station dataset
  var<-list_param$var
  y_var_name<-list_param$y_var_name
  out_prefix<-list_param$out_prefix
  out_path<-list_param$out_path
  
  #inserted #
  sampling_month_obj<-list_param$sampling_month_obj
  ghcn.month.subsets<-sampling_month_obj$ghcn_data
  sampling_month_dat <- sampling_month_obj$sampling_dat
  sampling_month_index <- sampling_month_obj$sampling_index
  
  #Model and response variable can be changed without affecting the script
  #prop_month<-0 #proportion retained for validation...
  #run_samp<-1 #sample number, can be introduced later...
  
  prop_month <- sampling_month_dat$prop[j] #proportion retained for validation...
  run_samp <- sampling_month_dat$run_samp[j] #sample number if multisampling...
  #will need create mulitple prediction at daily!!! could be complicated
  #possibility is to average per proportion !!!
  
  date_month <-strptime(sampling_month_dat$date[j], "%Y%m%d")   # interpolation date being processed
  month_no <-strftime(date_month, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month_no,sep="") # name of LST month to be matched
  LST_name <-LST_month  
  #### STEP 2: PREPARE DATA
  
  #change here...use training data...
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  
  #LST_name <-lst_avg[j] # name of LST month to be matched
  #data_month$LST<-data_month[[LST_name]]
  
  dataset_month <-ghcn.month.subsets[[j]]
  mod_LST <- ghcn.month.subsets[[j]][,match(LST_month, names(ghcn.month.subsets[[j]]))]  #Match interpolation date and monthly LST average
  dataset_month$LST <- as.data.frame(mod_LST)[,1] #Add the variable LST to the dataset
  #change here...
  dst$LST<-dst[[LST_month]] #Add the variable LST to the monthly dataset
  proj_str<-proj4string(dst) #get the local projection information from monthly data
    
  ind.training <- sampling_month_index[[j]]
  ind.testing  <- setdiff(1:nrow(dataset_month), ind.training)
  data_month_s <- dataset_month[ind.training, ]   #Training dataset currently used in the modeling
  data_month_v <- dataset_month[ind.testing, ]    #Testing/validation dataset using input sampling
  
  data_month <- data_month_s #training data for  monthhly predictions...
  
  #date_proc<-strptime(sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
  #mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  #day<-as.integer(strftime(date_proc, "%d"))
  #year<-as.integer(strftime(date_proc, "%Y"))
  ## end of pasted
  
  #end of insert...09/04
  
  #### STEP 2: PREPARE DATA
  
  #data_month<-dst[dst$month==j,] #Subsetting dataset for the relevant month of the date being processed
  #LST_name<-lst_avg[j] # name of LST month to be matched
  #data_month$LST<-data_month[[LST_name]]
  
  #Adding layer LST to the raster stack  
  covar_rast<-s_raster
  #names(s_raster)<-covar_names
  pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
  s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
  LST<-subset(s_raster,LST_name)
  names(LST)<-"LST"
  s_raster<-addLayer(s_raster,LST)            #Adding current month
  
  #LST bias to model...
  if (var=="TMAX"){
    data_month$LSTD_bias<-data_month$LST-data_month$TMax
    data_month$y_var<-data_month$LSTD_bias #Adding bias as the variable modeled
  }
  if (var=="TMIN"){
    data_month$LSTD_bias<-data_month$LST-data_month$TMin
    data_month$y_var<-data_month$LSTD_bias #Adding bias as the variable modeled
  }
  
  #If CAI model then...
  #TMax to model..., add precip later
  #if (var=="TMAX"){   
  #  dataset_month$y_var<-dataset_month$TMax #Adding TMax as the variable modeled
  #}
  #if (var=="TMIN"){   
  #  dataset_month$y_var<-dataset_month$TMin #Adding TMin as the variable modeled
  #}
  
  #### STEP3:  NOW FIT AND PREDICT  MODEL
  
  list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!
  cname<-paste("mod",1:length(list_formulas),sep="") #change to more meaningful name?
  
  #Now generate file names for the predictions...
  list_out_filename<-vector("list",length(list_formulas))
  names(list_out_filename)<-cname  
  
  ##Change name...
  for (k in 1:length(list_out_filename)){
    #j indicate which month is predicted, var indicates TMIN or TMAX
    data_name<-paste(var,"_bias_LST_month_",as.integer(month_no),"_",cname[k],"_",prop_month,
                     "_",run_samp,sep="")
    raster_name<-file.path(out_path,paste("fusion_",interpolation_method,"_",data_name,out_prefix,".tif", sep=""))
    list_out_filename[[k]]<-raster_name
  }
  
  #for (k in 1:length(list_out_filename)){
  #  #j indicate which month is predicted
  #  data_name<-paste(var,"_clim_month_",as.integer(month_no),"_",cname[k],"_",prop_month,
  #                   "_",run_samp,sep="")
  #  raster_name<-file.path(out_path,paste("CAI_",data_name,out_prefix,".tif", sep=""))
  #  list_out_filename[[k]]<-raster_name
  #}
  
  ## Select the relevant method...
  
  if (interpolation_method=="gam_fusion"){
    #First fitting
    mod_list<-fit_models(list_formulas,data_month) #only gam at this stage
    names(mod_list)<-cname
  
    #Second predict values for raster image...by providing fitted model list, raster brick and list of output file names
    rast_bias_list<-predict_raster_model(mod_list,s_raster,list_out_filename)
    names(rast_bias_list)<-cname
  }
  
  if (interpolation_method %in% c("kriging_fusion","gwr_fusion")){
    if(interpolation_method=="kriging_fusion"){
      method_interp <- "kriging"
    }else{
      method_interp <- "gwr"
    }
    #Call funciton to fit and predict gwr and/or kriging
    #month_prediction_obj<-predict_auto_krige_raster_model(list_formulas,s_raster,data_month,list_out_filename)
    month_prediction_obj<-predict_autokrige_gwr_raster_model(method_interp,list_formulas,s_raster,data_month,list_out_filename)
    
    mod_list <-month_prediction_obj$list_fitted_models
    rast_bias_list <-month_prediction_obj$list_rast_pred
    names(rast_bias_list)<-cname
  }
  
  #Some modles will not be predicted...remove them
  rast_bias_list<-rast_bias_list[!sapply(rast_bias_list,is.null)] #remove NULL elements in list

  mod_rast<-stack(rast_bias_list)  #stack of bias raster images from models
 
  rast_clim_list<-vector("list",nlayers(mod_rast))
  

  names(rast_clim_list)<-names(rast_bias_list)

  for (k in 1:nlayers(mod_rast)){
    clim_fus_rast<-LST-subset(mod_rast,k)
    data_name<-paste(var,"_clim_LST_month_",as.integer(month_no),"_",names(rast_clim_list)[k],"_",prop_month,
                     "_",run_samp,sep="")
    raster_name<-file.path(out_path,paste("fusion_",interpolation_method,"_",data_name,out_prefix,".tif", sep=""))
    rast_clim_list[[k]]<-raster_name
    writeRaster(clim_fus_rast, filename=raster_name,overwrite=TRUE)  #Wri
  }
  
  #### STEP 4:Adding Kriging for Climatology options
  bias_xy<-coordinates(data_month)
  #fitbias<-Krig(bias_xy,data_month$LSTD_bias,theta=1e5) #use TPS or krige 
  fitbias<-try(Krig(bias_xy,data_month$LSTD_bias,theta=1e5)) #use TPS or krige 

  model_name<-"mod_kr"

  if (inherits(fitbias,"Krig")){
    #Saving kriged surface in raster images
    bias_rast<-bias_rast<-interpolate(LST,fitbias) #interpolation using function from raster package
    data_name<-paste(var,"_bias_LST_month_",as.integer(month_no),"_",model_name,"_",prop_month,
                     "_",run_samp,sep="")
    raster_name_bias<-file.path(out_path,paste("fusion_",interpolation_method,"_",data_name,out_prefix,".tif", sep=""))
    writeRaster(bias_rast, filename=raster_name_bias,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
    
    #now climatology layer
    clim_rast<-LST-bias_rast
    data_name<-paste(var,"_clim_LST_month_",as.integer(month_no),"_",model_name,"_",prop_month,
                     "_",run_samp,sep="")
    raster_name_clim<-file.path(out_path,paste("fusion_",interpolation_method,"_",data_name,out_prefix,".tif", sep=""))
    writeRaster(clim_rast, filename=raster_name_clim,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
    #Adding to current objects
    mod_list[[model_name]]<-fitbias
    rast_bias_list[[model_name]]<-raster_name_bias
    rast_clim_list[[model_name]]<-raster_name_clim
  }
  
  if (inherits(fitbias,"try-error")){
    #NEED TO DEAL WITH THIS!!!
    
    #Adding to current objects
    mod_list[[model_name]]<-NULL
    rast_bias_list[[model_name]]<-NULL
    rast_clim_list[[model_name]]<-NULL
  }

  #### STEP 5: Prepare object and return
  #Prepare object to return
  clim_obj<-list(rast_bias_list,rast_clim_list,data_month,data_month_v,sampling_month_dat[j,],mod_list,list_formulas)
  names(clim_obj)<-c("bias","clim","data_month","data_month_v","sampling_month_dat","mod","formulas")
  
  save(clim_obj,file= file.path(out_path,paste("clim_obj_fusion_month_",as.integer(month_no),"_",var,"_",prop_month,
                                               "_",run_samp,"_",out_prefix,".RData",sep=""))) 
  return(clim_obj)
}

## Run function for kriging...?

#runGAMFusion <- function(i,list_param) {            # loop over dates
run_prediction_daily_deviation <- function(i,list_param) {            # loop over dates
  #This function produce daily prediction using monthly predicted clim surface.
  #The output is both daily prediction and daily deviation from monthly steps.
  
  #### Change this to allow explicitly arguments...
  #Arguments: 
  #1)index: loop list index for individual run/fit
  #2)clim_year_list: list of climatology files for all models...(12*nb of models)
  #3)sampling_obj: contains, data per date/fit, sampling information
  #4)dst: data at the monthly time scale
  #5)var: variable predicted -TMAX or TMIN
  #6)y_var_name: name of the variable predicted - dailyTMax, dailyTMin
  #7)out_prefix
  #8)out_path
  #9)list_models2 : interpolation model's formulas as string
  #10)interp_methods2: "gam","gwr","kriging"
  #11)s_raster: stack for covariates and toher variables
  
  #The output is a list of four shapefile names produced by the function:
  #1) list_temp: y_var_name
  #2) rast_clim_list: list of files for temperature climatology predictions
  #3) delta: list of files for temperature delta predictions
  #4) data_s: training data
  #5) data_v: testing data
  #6) sampling_dat: sampling information for the current prediction (date,proportion of holdout and sample number)
  #7) mod_kr: kriging delta fit, field package model object
  
  ### PARSING INPUT ARGUMENTS
  
  #list_param_runGAMFusion<-list(i,clim_yearlist,sampling_obj,var,y_var_name, out_prefix)
  rast_clim_yearlist<-list_param$clim_yearlist
  sampling_obj<-list_param$sampling_obj
  ghcn.subsets<-sampling_obj$ghcn_data
  sampling_dat <- sampling_obj$sampling_dat
  sampling <- sampling_obj$sampling_index
  var<-list_param$var
  y_var_name<-list_param$y_var_name
  out_prefix<-list_param$out_prefix
  dst<-list_param$dst #monthly station dataset
  out_path <-list_param$out_path
  list_models2 <-list_param$list_models2
  interp_method2 <- list_param$interp_method2
  s_raster <- list_param$s_raster
  
  sampling_month_obj <- list_param$sampling_month_obj
  daily_dev_sampling_dat <- list_param$daily_dev_sampling_dat
  
  index_d <- daily_dev_sampling_dat$index_d[i]
  index_m <- daily_dev_sampling_dat$index_m[i]
  
  use_clim_image <- list_param$use_clim_image # use predicted image as a base...rather than average Tmin at the station for delta
  join_daily <- list_param$join_daily # join monthly and daily station before calucating delta
  
  #use_clim_image
  ##########
  # STEP 1 - Read in information and get traing and testing stations
  ############# 
  
  #use index_d and index_m
  
  date<-strptime(daily_dev_sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  proj_str<-proj4string(dst) #get the local projection information from monthly data

  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  
  data_day<-ghcn.subsets[[index_d]]
  mod_LST <- ghcn.subsets[[index_d]][,match(LST_month, names(ghcn.subsets[[index_d]]))]  #Match interpolation date and monthly LST average
  data_day$LST <- as.data.frame(mod_LST)[,1] #Add the variable LST to the dataset
  dst$LST<-dst[[LST_month]] #Add the variable LST to the monthly dataset
  
  ind.training<-sampling[[index_d]]
  ind.testing <- setdiff(1:nrow(data_day), ind.training)
  data_s <- data_day[ind.training, ]   #Training dataset currently used in the modeling
  data_v <- data_day[ind.testing, ]    #Testing/validation dataset using input sampling
  
  ns<-nrow(data_s)
  nv<-nrow(data_v)
  #i=1
  date_proc<-sampling_dat$date[index_d]
  date_proc<-strptime(sampling_dat$date[index_d], "%Y%m%d")   # interpolation date being processed
  mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  day<-as.integer(strftime(date_proc, "%d"))
  year<-as.integer(strftime(date_proc, "%Y"))
  
  #Adding layer LST to the raster stack  
  #names(s_raster)<-covar_names
  pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
  s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
  LST<-subset(s_raster,LST_month)
  names(LST)<-"LST"
  s_raster<-addLayer(s_raster,LST)            #Adding current month
  
  #Now get monthly data...
  
  ghcn.month.subsets<-sampling_month_obj$ghcn_data
  sampling_month_dat <- sampling_month_obj$sampling_dat
  sampling_month_index <- sampling_month_obj$sampling_index
  
  dataset_month <-ghcn.month.subsets[[index_m]]
  mod_LST <- ghcn.month.subsets[[index_m]][,match(LST_month, names(ghcn.month.subsets[[index_m]]))]  #Match interpolation date and monthly LST average
  dataset_month$LST <- as.data.frame(mod_LST)[,1] #Add the variable LST to the dataset
  #change here...
  dst$LST<-dst[[LST_month]] #Add the variable LST to the monthly dataset
  proj_str<-proj4string(dst) #get the local projection information from monthly data
  
  ind.training_month <- sampling_month_index[[index_m]]
  ind.testing_month  <- setdiff(1:nrow(dataset_month), ind.training_month)
  data_month_s <- dataset_month[ind.training_month, ]   #Training dataset currently used in the modeling
  data_month_v <- dataset_month[ind.testing_month, ]    #Testing/validation dataset using input sampling
  
  modst <- data_month_s #training data for  monthhly predictions...
    
  ##########
  # STEP 2 - CLEAN DATA AND JOIN DAILY TO MONTHLY STATION INFORMATION
  ##########
  
  #if use join
  #modst<-dst[dst$month==mo,] #Subsetting dataset for the relevant month of the date being processed
    
  if (var=="TMIN"){
    modst$LSTD_bias <- modst$LST-modst$TMin; #That is the difference between the monthly LST mean and monthly station mean
  }
  if (var=="TMAX"){
    modst$LSTD_bias <- modst$LST-modst$TMax; #That is the difference between the monthly LST mean and monthly station mean    
  }
  #This may be unnecessary since LSTD_bias is already in dst?? check the info
  #Some loss of observations: LSTD_bias for January has only 56 out of 66 possible TMIN!!! We may need to look into this issue
  #to avoid some losses of station data...
  
  #Clearn out this part: make this a function call
  x<-as.data.frame(data_v)
  d<-as.data.frame(data_s)
  for (j in 1:nrow(x)){
    if (x$value[j]== -999.9){
      x$value[j]<-NA
    }
  }
  for (j in 1:nrow(d)){
    if (d$value[j]== -999.9){
      d$value[j]<-NA
    }
  }
  pos<-match("value",names(d)) #Find column with name "value"
  #names(d)[pos]<-c("dailyTmax")
  names(d)[pos]<-y_var_name
  pos<-match("value",names(x)) #Find column with name "value"
  names(x)[pos]<-y_var_name
  pos<-match("station",names(d)) #Find column with station ID
  names(d)[pos]<-c("id")
  pos<-match("station",names(x)) #Find column with name station ID
  names(x)[pos]<-c("id")
  pos<-match("station",names(modst)) #Find column with name station ID
  names(modst)[pos]<-c("id")       #modst contains the average tmax per month for every stations...

  ##########
  # STEP 3 - interpolate daily delta across space
  ##########
  
  #if used images
  # extract from image 
  #Change to take into account TMin and TMax
  
  if(use_clim_image==FALSE){
    
    #must join daily and monthly data first...
    
    dmoday <-merge(modst,d,by="id",suffixes=c("",".y2"))  
    xmoday <-merge(modst,x,by="id",suffixes=c("",".y2"))  
    mod_pat<-glob2rx("*.y2")   #remove duplicate columns that have ".y2" in their names
    var_pat<-grep(mod_pat,names(dmoday),value=FALSE) # using grep with "value" extracts the matching names
    dmoday<-dmoday[,-var_pat] #dropping relevant columns
    mod_pat<-glob2rx("*.y2")   
    var_pat<-grep(mod_pat,names(xmoday),value=FALSE) # using grep with "value" extracts the matching names
    xmoday<-xmoday[,-var_pat] #Removing duplicate columns
      
    data_v<-xmoday
    
    #coords <-dmoday[,c("coords.x1","coords.x2")]
    coords <-dmoday[,c("x","y")]
    coordinates(dmoday)<-coords
    proj4string(dmoday)<-proj_str
    
    #dmoday contains the daily tmax values for training with TMax/TMin being the monthly station tmax/tmin mean
    #xmoday contains the daily tmax values for validation with TMax/TMin being the monthly station tmax/tmin mean
    
    if (var=="TMIN"){
      daily_delta <-dmoday$dailyTmin-dmoday$TMin #daily detl is the difference between monthly and daily temperatures
    }
    if (var=="TMAX"){
      daily_delta <- dmoday$dailyTmax-dmoday$TMax
    }  
    #daily_delta <- dmoday[[y_var_name]] - 
    #only one delta in this case!!!
    #list(mod)
    
    if(is.null(list_models2)){ #change here...
      
      list_daily_delta_rast <- vector("list",length=1) #only one delta surface in this case!!
      list_mod_krtmp2 <- vector("list",length=1) #only one delta model in this case!!
      
      model_name<-paste("mod_stat_kr",sep="_")
      daily_delta_xy<-as.matrix(cbind(dmoday$x,dmoday$y))
      fitdelta<-Krig(daily_delta_xy,daily_delta,theta=1e5) #use TPS or krige
      mod_krtmp2 <- fitdelta
      #names(mod_krtmp2)[k] <- model_name
      #data_s$daily_delta<-daily_delta
      #rast_clim_list<-rast_clim_yearlist[[index_m]]  #select relevant monthly climatology image ...
      rast_clim_list<-rast_clim_yearlist[[index_m]]  #select relevant monthly climatology image ...
      rast_clim_mod <- stack(rast_clim_list)
      names(rast_clim_mod) <- names(rast_clim_list)
      rast_clim_month <- subset(rast_clim_mod,1) #example layer to interpolate to
      
      daily_delta_rast<-interpolate(rast_clim_month,fitdelta) #Interpolation of the of the daily devation
      #there is only one daily devation (delta) sruface in this case
      
      #To many I/O out of swap memory on atlas
      #Saving kriged surface in raster images
      data_name<-paste("daily_delta_",y_var_name,"_",model_name,"_",sampling_month_dat$prop[index_m],"_",sampling_month_dat$run_samp[index_m],"_",
                       sampling_dat$date[index_d],"_",sampling_dat$prop[index_d],"_",sampling_dat$run_samp[index_d],sep="")
      raster_name_delta<-file.path(out_path,paste(interpolation_method,"_",var,"_",data_name,out_prefix,".tif", sep=""))
      writeRaster(daily_delta_rast, filename=raster_name_delta,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
      
      list_daily_delta_rast[[1]] <- raster_name_delta
      list_mod_krtmp2[[1]] <- mod_krtmp2
    }
    
    if(!is.null(list_models2)){ #change here...
      list_daily_delta_rast <- vector("list",length=1) #several delta surfaces in this case but stored as one list!!
      list_mod_krtmp2 <- vector("list",length=1) #several delta model in this case but stored as one list!!
      
      dev_mod_name<-paste("dev_mod",1:length(list_models2),sep="") #change to more meaningful name?
      model_name<-paste("mod_stat_",sep="_")
      #Now generate file names for the predictions...
      list_out_filename<-vector("list",length(list_models2))
      names(list_out_filename)<- dev_mod_name  
      
      ##Change name...
      for (j in 1:length(list_out_filename)){
        #j indicate which month is predicted, var indicates TMIN or TMAX
        data_name<-paste("daily_delta_",y_var_name,"_",model_name,"_",sampling_month_dat$prop[index_m],"_",sampling_month_dat$run_samp[index_m],"_",
                         sampling_dat$date[index_d],"_",sampling_dat$prop[index_d],"_",sampling_dat$run_samp[index_d],
                         "_",interp_method2,"_",dev_mod_name[j],sep="")
        raster_name_delta<-file.path(out_path,paste(interpolation_method,"_",var,"_",data_name,out_prefix,".tif", sep=""))
        
        list_out_filename[[j]]<-raster_name_delta
      }
      
      #Now call function
      
      #for (j in 1:length(list_models2)){
      dmoday$y_var <- daily_delta
      #coordinates(data_s)<-cbind(data_s$x,data_s$y)
      #proj4string(data_s)<-proj_str
      #coordinates(data_v)<-cbind(data_v$x,data_v$y)
      #proj4string(data_v)<-proj_str
      
      interp_area_obj <-interpolate_area_fun(interp_method2,list_models2,s_raster,list_out_filename,dmoday)
      rast_pred_list <- interp_area_obj$rast_pred_list
      rast_pred_list <-rast_pred_list[!sapply(rast_pred_list,is.null)] #remove NULL elements in list
      list_daily_delta_rast[[1]] <-rast_pred_list
      #names(list_daily_delta_rast) <- names(daily_delta_df)
      list_mod_krtmp2[[1]] <-interp_area_obj$mod_list      
    }
  }
  
  if(use_clim_image==TRUE){
    
    # User can choose to join daily and monthly station before interpolation:
    #-this ensures that the delta difference is "more" exact since its starting point is basesd on average value but there is risk to loose some stations
    #may need to change this option later!!
    #if jion_Daily is true then daily station used as training will match monthly station used as training
    
    if(join_daily==TRUE){
      dmoday <-merge(modst,d,by="id",suffixes=c("",".y2"))  
      xmoday <-merge(modst,x,by="id",suffixes=c("",".y2"))  
      mod_pat<-glob2rx("*.y2")   #remove duplicate columns that have ".y2" in their names
      var_pat<-grep(mod_pat,names(dmoday),value=FALSE) # using grep with "value" extracts the matching names
      dmoday<-dmoday[,-var_pat] #dropping relevant columns
      mod_pat<-glob2rx("*.y2")   
      var_pat<-grep(mod_pat,names(xmoday),value=FALSE) # using grep with "value" extracts the matching names
      xmoday<-xmoday[,-var_pat] #Removing duplicate columns
      
      data_v<-xmoday
      
    }else{
      dmoday<-d
      data_v<-x
    }
    
    
    #dmoday contains the daily tmax values for training with TMax/TMin being the monthly station tmax/tmin mean
    #xmoday contains the daily tmax values for validation with TMax/TMin being the monthly station tmax/tmin mean
    
    #coords <-dmoday[,c("coords.x1","coords.x2")]
    coords <-dmoday[,c("x","y")]
    coordinates(dmoday)<-coords
    proj4string(dmoday)<-proj_str
    
    #Now compute daily delta deviation from climatology layer:
    
    rast_clim_list<-rast_clim_yearlist[[index_m]]  #select relevant monthly climatology image ...
    rast_clim_mod <- stack(rast_clim_list)
    names(rast_clim_mod) <- names(rast_clim_list)
    extract_data_s <-extract(rast_clim_mod,dmoday,df=TRUE)
    #list_daily_delta
    daily_delta_df <- dmoday[[y_var_name]] - extract_data_s  
    daily_delta_df <- daily_delta_df[,-1]
    names(daily_delta_df) <- paste(names(daily_delta_df),"_del",sep="")
    
    names(extract_data_s) <- paste(names(extract_data_s),"_m",sep="") # "m" for monthly predictions...
    dmoday <-spCbind(dmoday,extract_data_s) #contains the predicted clim at locations
    dmoday <-spCbind(dmoday,daily_delta_df) #contains the predicted clim at locations
    #Now krige  forevery model !! loop
    list_mod_krtmp2 <- vector("list",length=nlayers(rast_clim_mod)) 
    list_daily_delta_rast <- vector("list",length=nlayers(rast_clim_mod)) 
    names(list_daily_delta_rast) <- names(daily_delta_df)
    names(list_mod_krtmp2) <- names(daily_delta_df)
    for(k in 1:nlayers(rast_clim_mod)){
      
      daily_delta <- daily_delta_df[[k]] #Current daily deviation being process: the reference monthly prediction varies...
      #model_name<-paste("mod_kr","day",sep="_")
      model_name<- names(daily_delta_df)[k]
      
      if(is.null(list_models2)){
        daily_delta_xy<-as.matrix(cbind(dmoday$x,dmoday$y))
        fitdelta<-Krig(daily_delta_xy,daily_delta,theta=1e5) #use TPS or krige
        list_mod_krtmp2[[k]] <-fitdelta
        names(list_mod_krtmp2)[k] <- model_name
        #data_s$daily_delta<-daily_delta
        #rast_clim_list<-rast_clim_yearlist[[index_m]]  #select relevant monthly climatology image ...
        rast_clim_month <- subset(rast_clim_mod,1) #example layer to interpolate to
        
        daily_delta_rast<-interpolate(rast_clim_month,fitdelta) #Interpolation of the bias surface...
        
        #list_daily_delta_rast[[k]] <- raster_name_delta
        
        data_name<-paste("daily_delta_",y_var_name,"_",model_name,"_",sampling_month_dat$prop[index_m],"_",sampling_month_dat$run_samp[index_m],"_",
                         sampling_dat$date[index_d],"_",sampling_dat$prop[index_d],"_",sampling_dat$run_samp[index_d],sep="")
        raster_name_delta<-file.path(out_path,paste(interpolation_method,"_",var,"_",data_name,out_prefix,".tif", sep=""))
        
        writeRaster(daily_delta_rast, filename=raster_name_delta,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
        #writeRaster(r_spat, NAflag=NA_flag_val,filename=raster_name,bylayer=TRUE,bandorder="BSQ",overwrite=TRUE)   
        
        #raster_name_delta <- list_daily_delta_rast
        #mod_krtmp2 <- list_mod_krtmp2
        list_daily_delta_rast[[k]] <- raster_name_delta
        
      }
      
      if (!is.null(list_models2)){
        
        #list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!
        dev_mod_name<-paste("dev_mod",1:length(list_models2),sep="") #change to more meaningful name?
        
        #Now generate file names for the predictions...
        list_out_filename<-vector("list",length(list_models2))
        names(list_out_filename)<- dev_mod_name  
        
        ##Change name...
        for (j in 1:length(list_out_filename)){
          #j indicate which month is predicted, var indicates TMIN or TMAX
          data_name<-paste("daily_delta_",y_var_name,"_",model_name,"_",sampling_month_dat$prop[index_m],"_",sampling_month_dat$run_samp[index_m],"_",
                           sampling_dat$date[index_d],"_",sampling_dat$prop[index_d],"_",sampling_dat$run_samp[index_d],
                           "_",interp_method2,"_",dev_mod_name[j],sep="")
          raster_name_delta<-file.path(out_path,paste(interpolation_method,"_",var,"_",data_name,out_prefix,".tif", sep=""))
          
          list_out_filename[[j]]<-raster_name_delta
        }
        
        #Now call function
        
        #for (j in 1:length(list_models2)){
        dmoday$y_var <- daily_delta
        #coordinates(data_s)<-cbind(data_s$x,data_s$y)
        #proj4string(data_s)<-proj_str
        #coordinates(data_v)<-cbind(data_v$x,data_v$y)
        #proj4string(data_v)<-proj_str
        
        interp_area_obj <-interpolate_area_fun(interp_method2,list_models2,s_raster,list_out_filename,dmoday)
        rast_pred_list <- interp_area_obj$rast_pred_list
        names(rast_pred_list) <- dev_mod_name
        rast_pred_list <-rast_pred_list[!sapply(rast_pred_list,is.null)] #remove NULL elements in list
        list_daily_delta_rast[[k]] <-rast_pred_list
        names(list_daily_delta_rast) <- names(daily_delta_df)
        mod_list <-interp_area_obj$mod_list
        names(mod_list) <- dev_mod_name
        list_mod_krtmp2[[k]] <-interp_area_obj$mod_list
      }
      
    }
    
    #Too many I/O out of swap memory on atlas
    #Saving kriged surface in raster images
    #delta_rast_s <-stack(list_daily_delta_rast)
    #names(delta_rast_s) <- names(daily_delta_df)
    
    #Should check that all delta images have been created for every model!!! remove from list empty elements!!
    
    #data_name<-paste("daily_delta_",y_var_name,"_",model_name,"_",sampling_month_dat$prop[index_m],"_",sampling_month_dat$run_samp[index_m],"_",
    #                 sampling_dat$date[index_d],"_",sampling_dat$prop[index_d],"_",sampling_dat$run_samp[index_d],sep="")
    #raster_name_delta<-file.path(out_path,paste(interpolation_method,"_",var,"_",data_name,out_prefix,".tif", sep=""))
    #writeRaster(daily_delta_rast, filename=raster_name_delta,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
    
    #data_name<-paste("daily_delta_",y_var_name,"_",sampling_month_dat$prop[index_m],"_",sampling_month_dat$run_samp[index_m],"_",
    #                 sampling_dat$date[index_d],"_",sampling_dat$prop[index_d],"_",sampling_dat$run_samp[index_d],sep="")
    #raster_name_delta<-file.path(out_path,paste(interpolation_method,"_",var,"_",data_name,out_prefix,".tif", sep=""))
    
    #writeRaster(delta_rast_s, filename=raster_name_delta,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
    #writeRaster(r_spat, NAflag=NA_flag_val,filename=raster_name,bylayer=TRUE,bandorder="BSQ",overwrite=TRUE)   
    
    #raster_name_delta <- list_daily_delta_rast
    #mod_krtmp2 <- list_mod_krtmp2
  }
  
  #########
  # STEP 4 - Calculate daily predictions - T(day) = clim(month) + delta(day)
  #########
  
  #if(use_clim_image==FALSE){
  #  list_daily_delta_rast <- rep(raster_name_delta,length=nlayers(rast_clim_mod))
  #}
  #Now predict daily after having selected the relevant month
  temp_list<-vector("list",nlayers(rast_clim_mod))  
  for (k in 1:nlayers(rast_clim_mod)){
    if(use_clim_image==TRUE){
      if (is.null(list_models2)){ 
        daily_delta_rast <- raster(list_daily_delta_rast[[k]]) #There is only one image of deviation per model if list_models2 is NULL
      }
      if (!is.null(list_models2)){ #then possible multiple daily dev predictions
        daily_delta_rast <- stack(unlist(list_daily_delta_rast[[k]]))
      }
      #daily_delta_rast <- subset(delta_rast_s,k)
    }
    #if use_clim_image==FALSE then daily__delta_rast already defined earlier...
    
    if(use_clim_image==FALSE){
      if (is.null(list_models2)){ 
        daily_delta_rast <- raster(list_daily_delta_rast[[1]]) #There is only one image of deviation per model if list_models2 is NULL
      }
      if (!is.null(list_models2)){ #then possible multiple daily dev predictions hence use stack
        daily_delta_rast <- stack(unlist(list_daily_delta_rast[[1]]))
      }
      #daily_delta_rast <- subset(delta_rast_s,k)
    }
    
    #rast_clim_month<-raster(rast_clim_list[[k]])
    rast_clim_month <- subset(rast_clim_mod,k) #long term monthly prediction
    if (is.null(list_models2)){ 
      temp_predicted<-rast_clim_month + daily_delta_rast
      data_name<-paste(y_var_name,"_predicted_",names(rast_clim_mod)[k],"_",sampling_month_dat$prop[index_m],"_",sampling_month_dat$run_samp[index_m],"_",
                       sampling_dat$date[index_d],"_",sampling_dat$prop[index_d],"_",sampling_dat$run_samp[index_d],sep="")
      raster_name<-file.path(out_path,paste(interpolation_method,"_",data_name,out_prefix,".tif", sep=""))
      writeRaster(temp_predicted, filename=raster_name,overwrite=TRUE) 
      temp_list[[k]]<-raster_name
    }
    if (!is.null(list_models2)){ 
      dev_mod_name <- paste("dev_mod",1:length(list_models2),sep="") #change to more meaningful name?
      raster_name_list <- vector("list",length(list_models2))
      for(j in 1:length(list_models2)){
        temp_predicted <- rast_clim_month + subset(daily_delta_rast,j)
        data_name<-paste(y_var_name,"_predicted_",names(rast_clim_mod)[k],"_",sampling_month_dat$prop[index_m],"_",sampling_month_dat$run_samp[index_m],"_",
                         sampling_dat$date[index_d],"_",sampling_dat$prop[index_d],"_",sampling_dat$run_samp[index_d],"_",
                         interp_method2,"_",dev_mod_name[j],sep="")
        raster_name<-file.path(out_path,paste(interpolation_method,"_",data_name,out_prefix,".tif", sep=""))
        writeRaster(temp_predicted, filename=raster_name,overwrite=TRUE) 
        raster_name_list[[j]] <- raster_name
      }
      names(raster_name_list) <- dev_mod_name
      temp_list[[k]]<-raster_name_list #record names of the daily predictions 
    }
  }
  
  ##########
  # STEP 5 - Prepare output object to return
  ##########
  
  data_s <- dmoday #put the 
  #coordinates(data_s)<-cbind(data_s$x,data_s$y)
  #proj4string(data_s)<-proj_str
  coordinates(data_v)<-cbind(data_v$x,data_v$y)
  proj4string(data_v)<-proj_str
  
  #mod_krtmp2<-list_mod_krtmp2    
  #mod_krtmp2<-fitdelta

  model_name<-paste("mod_","day",sep="_")
  
  names(temp_list)<-names(rast_clim_list)
  
  #add data_month_s and data_month_v?
  delta_obj<-list(temp_list,rast_clim_list,list_daily_delta_rast, data_s, data_v,
                 modst,data_month_v,sampling_dat[index_d,],daily_dev_sampling_dat[i,],list_mod_krtmp2)
  
  obj_names<-c(y_var_name,"clim","delta","data_s","data_v",
               "data_month_s","data_month_v","sampling_dat","daily_dev_sampling_dat",model_name)
  names(delta_obj)<-obj_names 
  save(delta_obj,file= file.path(out_path,paste("delta_obj_",var,"_",sampling_month_dat$prop[index_m],"_",sampling_month_dat$run_samp[index_m],"_",
                                                sampling_dat$date[index_d],"_",sampling_dat$prop[index_d],"_",sampling_dat$run_samp[index_d],
                                                out_prefix,".RData",sep="")))
  return(delta_obj)
  
}
 
combine_sampling_daily_monthly_for_daily_deviation_pred <- function(sampling_obj,sampling_month_obj){
  #This function combines sampling objects information at the daily and monthly time scale.
  #The data.frame created records propotion of hold out, sampling number as well as provides keys (index_d,index_m)
  #to access training and testing samples at the daily and monthly time scale (long term monthly!!!)
  #Inputs:
  #sampling_obj: contains daily sampling information and is output of "sampling_training_testing" function defined in sampling_script_functions.
  #sampling_month_obj: contains monthly sampling information and is output of "sampling_training_testing" function defined in sampling_script_functions
  
  sampling_month_dat <- sampling_month_obj$sampling_dat #extract monthly information about sampling (in data.frame format)
  sampling_dat <- sampling_obj$sampling_dat #extract daily information about samplint (in data.frame format)
              
  #Build new data.frame with combined information
  data_daily_dev <- sampling_dat 
  data_daily_dev$index_d <- 1:nrow(data_daily_dev) # index_d is a identifier key to the list of daily samples generated earlier in sampling_obj
  data_daily_dev$month_no <- as.integer(strftime(strptime(data_daily_dev$date, "%Y%m%d"),"%m")) #Extract month given specific format  "%Y%m%d"
  
  names(sampling_month_dat) <- c("dates_month","run_samp_month","prop_month") #rename before merging as table product
  sampling_month_dat$month_no <-as.integer(strftime(strptime(sampling_month_dat$dates_month, "%Y%m%d"),"%m")) #month of the record
  sampling_month_dat$index_m <-1:nrow(sampling_month_dat) # index_m is a identifier key to the list of monthly samples 
                                                          #generated earlier in sampling_month_obj
  
  data_daily_dev <-merge(data_daily_dev,sampling_month_dat,by = "month_no")
  
  return(data_daily_dev)

}

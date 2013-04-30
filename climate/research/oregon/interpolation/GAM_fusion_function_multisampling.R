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
#DATE: 04/30/2013                                                                                 
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

####
#TODO:
#Add log file and calculate time and sizes for processes-outputs
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
    
  #Model and response variable can be changed without affecting the script
  prop_month<-0 #proportion retained for validation
  run_samp<-1
  
  #### STEP 2: PREPARE DATA
    
  data_month<-dst[dst$month==j,] #Subsetting dataset for the relevant month of the date being processed
  LST_name<-lst_avg[j] # name of LST month to be matched
  data_month$LST<-data_month[[LST_name]]
  
  #Create formulas object from list of characters...
  list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!  

  #TMax to model...
  if (var=="TMAX"){   
    data_month$y_var<-data_month$TMax #Adding TMax as the variable modeled
  }
  if (var=="TMIN"){   
    data_month$y_var<-data_month$TMin #Adding TMin as the variable modeled
  }
  #Fit gam models using data and list of formulas
  mod_list<-fit_models(list_formulas,data_month) #only gam at this stage
  cname<-paste("mod",1:length(mod_list),sep="") #change to more meaningful name?
  names(mod_list)<-cname
  #Adding layer LST to the raster stack  
  
  pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
  s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
  LST<-subset(s_raster,LST_name)
  names(LST)<-"LST"
  s_raster<-addLayer(s_raster,LST)            #Adding current month
  
  #Now generate file names for the predictions...
  list_out_filename<-vector("list",length(mod_list))
  names(list_out_filename)<-cname  
  
  for (k in 1:length(list_out_filename)){
    #j indicate which month is predicted
    data_name<-paste(var,"_clim_month_",j,"_",cname[k],"_",prop_month,
                     "_",run_samp,sep="")
    raster_name<-paste("CAI_",data_name,out_prefix,".tif", sep="")
    list_out_filename[[k]]<-raster_name
  }
  
  #now predict values for raster image...
  rast_clim_list<-predict_raster_model(mod_list,s_raster,list_out_filename)
  names(rast_clim_list)<-cname
  #Some models will not be predicted because of the lack of training data...remove empty string from list of models
  rast_clim_list<-rast_clim_list[!sapply(rast_clim_list,is.null)] #remove NULL elements in list
  
  #Adding Kriging for Climatology options
  
  clim_xy<-coordinates(data_month)
  fitclim<-Krig(clim_xy,data_month$y_var,theta=1e5) #use TPS or krige 
  #fitclim<-Krig(clim_xy,data_month$TMax,theta=1e5) #use TPS or krige 
  mod_krtmp1<-fitclim
  model_name<-"mod_kr"
  
  clim_rast<-interpolate(LST,fitclim) #interpolation using function from raster package
  #Saving kriged surface in raster images
  #data_name<-paste("clim_month_",j,"_",model_name,"_",prop_month,
  #                 "_",run_samp,sep="")
  #raster_name_clim<-paste("fusion_",data_name,out_prefix,".tif", sep="")
  #writeRaster(clim_rast, filename=raster_name_clim,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  #now climatology layer
  #clim_rast<-LST-bias_rast
  data_name<-paste(var,"_clim_month_",j,"_",model_name,"_",prop_month,
                   "_",run_samp,sep="")
  raster_name_clim<-paste("CAI_",data_name,out_prefix,".tif", sep="")
  writeRaster(clim_rast, filename=raster_name_clim,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  #Adding to current objects
  mod_list[[model_name]]<-mod_krtmp1
  #rast_bias_list[[model_name]]<-raster_name_bias
  rast_clim_list[[model_name]]<-raster_name_clim
  
  #Prepare object to return
  clim_obj<-list(rast_clim_list,data_month,mod_list,list_formulas)
  names(clim_obj)<-c("clim","data_month","mod","formulas")
  
  save(clim_obj,file= paste("clim_obj_CAI_month_",j,"_",var,"_",out_prefix,".RData",sep=""))
  
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

  #Model and response variable can be changed without affecting the script
  prop_month<-0 #proportion retained for validation
  run_samp<-1 #This option can be added later on if/when neeeded
  
  #### STEP 2: PREPARE DATA
  
  data_month<-dst[dst$month==j,] #Subsetting dataset for the relevant month of the date being processed
  LST_name<-lst_avg[j] # name of LST month to be matched
  data_month$LST<-data_month[[LST_name]]
  
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
  
  #### STEP3:  NOW FIT AND PREDICT  MODEL
  
  list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!
  
  mod_list<-fit_models(list_formulas,data_month) #only gam at this stage
  cname<-paste("mod",1:length(mod_list),sep="") #change to more meaningful name?
  names(mod_list)<-cname
  
  #Now generate file names for the predictions...
  list_out_filename<-vector("list",length(mod_list))
  names(list_out_filename)<-cname  
  
  for (k in 1:length(list_out_filename)){
    #j indicate which month is predicted, var indicates TMIN or TMAX
    data_name<-paste(var,"_bias_LST_month_",j,"_",cname[k],"_",prop_month,
                     "_",run_samp,sep="")
    raster_name<-paste("fusion",data_name,out_prefix,".tif", sep="")
    list_out_filename[[k]]<-raster_name
  }

  #now predict values for raster image...
  rast_bias_list<-predict_raster_model(mod_list,s_raster,list_out_filename)
  names(rast_bias_list)<-cname
  #Some modles will not be predicted...remove them
  rast_bias_list<-rast_bias_list[!sapply(rast_bias_list,is.null)] #remove NULL elements in list

  mod_rast<-stack(rast_bias_list)  #stack of bias raster images from models
  rast_clim_list<-vector("list",nlayers(mod_rast))
  names(rast_clim_list)<-names(rast_bias_list)
  for (k in 1:nlayers(mod_rast)){
    clim_fus_rast<-LST-subset(mod_rast,k)
    data_name<-paste(var,"_clim_LST_month_",j,"_",names(rast_clim_list)[k],"_",prop_month,
                     "_",run_samp,sep="")
    raster_name<-paste("fusion_",data_name,out_prefix,".tif", sep="")
    rast_clim_list[[k]]<-raster_name
    writeRaster(clim_fus_rast, filename=raster_name,overwrite=TRUE)  #Wri
  }
  
  #### STEP 4:Adding Kriging for Climatology options
  
  bias_xy<-coordinates(data_month)
  fitbias<-Krig(bias_xy,data_month$LSTD_bias,theta=1e5) #use TPS or krige 
  mod_krtmp1<-fitbias
  model_name<-"mod_kr"
  
   
  bias_rast<-interpolate(LST,fitbias) #interpolation using function from raster package
  #Saving kriged surface in raster images
  data_name<-paste(var,"_bias_LST_month_",j,"_",model_name,"_",prop_month,
                   "_",run_samp,sep="")
  raster_name_bias<-paste("fusion_",data_name,out_prefix,".tif", sep="")
  writeRaster(bias_rast, filename=raster_name_bias,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  #now climatology layer
  clim_rast<-LST-bias_rast
  data_name<-paste(var,"_clim_LST_month_",j,"_",model_name,"_",prop_month,
                   "_",run_samp,sep="")
  raster_name_clim<-paste("fusion_",data_name,out_prefix,".tif", sep="")
  writeRaster(clim_rast, filename=raster_name_clim,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  #Adding to current objects
  mod_list[[model_name]]<-mod_krtmp1
  rast_bias_list[[model_name]]<-raster_name_bias
  rast_clim_list[[model_name]]<-raster_name_clim
  
  #### STEP 5: Prepare object and return
  
  clim_obj<-list(rast_bias_list,rast_clim_list,data_month,mod_list,list_formulas)
  names(clim_obj)<-c("bias","clim","data_month","mod","formulas")
  
  save(clim_obj,file= paste("clim_obj_month_",j,"_",var,"_",out_prefix,".RData",sep=""))
  
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
  #8)
  #
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
  ghcn.subsets<-sampling_obj$ghcn_data_day
  sampling_dat <- sampling_obj$sampling_dat
  sampling <- sampling_obj$sampling_index
  var<-list_param$var
  y_var_name<-list_param$y_var_name
  out_prefix<-list_param$out_prefix
  dst<-list_param$dst #monthly station dataset
  
  ##########
  # STEP 1 - Read in information and get traing and testing stations
  ############# 
  
  date<-strptime(sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  proj_str<-proj4string(dst) #get the local projection information from monthly data

  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  data_day<-ghcn.subsets[[i]]
  mod_LST <- ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]  #Match interpolation date and monthly LST average
  data_day$LST <- as.data.frame(mod_LST)[,1] #Add the variable LST to the dataset
  dst$LST<-dst[[LST_month]] #Add the variable LST to the monthly dataset
  
  ind.training<-sampling[[i]]
  ind.testing <- setdiff(1:nrow(data_day), ind.training)
  data_s <- data_day[ind.training, ]   #Training dataset currently used in the modeling
  data_v <- data_day[ind.testing, ]    #Testing/validation dataset using input sampling
  
  ns<-nrow(data_s)
  nv<-nrow(data_v)
  #i=1
  date_proc<-sampling_dat$date[i]
  date_proc<-strptime(sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
  mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  day<-as.integer(strftime(date_proc, "%d"))
  year<-as.integer(strftime(date_proc, "%Y"))
  
  ##########
  # STEP 2 - JOIN DAILY AND MONTHLY STATION INFORMATION
  ##########
  
  modst<-dst[dst$month==mo,] #Subsetting dataset for the relevant month of the date being processed

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
  
  dmoday <-merge(modst,d,by="id",suffixes=c("",".y2"))  
  xmoday <-merge(modst,x,by="id",suffixes=c("",".y2"))  
  mod_pat<-glob2rx("*.y2")   #remove duplicate columns that have ".y2" in their names
  var_pat<-grep(mod_pat,names(dmoday),value=FALSE) # using grep with "value" extracts the matching names
  dmoday<-dmoday[,-var_pat] #dropping relevant columns
  mod_pat<-glob2rx("*.y2")   
  var_pat<-grep(mod_pat,names(xmoday),value=FALSE) # using grep with "value" extracts the matching names
  xmoday<-xmoday[,-var_pat] #Removing duplicate columns
  
  data_v<-xmoday
  
  #dmoday contains the daily tmax values for training with TMax/TMin being the monthly station tmax/tmin mean
  #xmoday contains the daily tmax values for validation with TMax/TMin being the monthly station tmax/tmin mean
  
  ##########
  # STEP 3 - interpolate daily delta across space
  ##########
  
  #Change to take into account TMin and TMax
  if (var=="TMIN"){
    daily_delta<-dmoday$dailyTmin-dmoday$TMin #daily detl is the difference between monthly and daily temperatures
  }
  if (var=="TMAX"){
    daily_delta<-dmoday$dailyTmax-dmoday$TMax
  }

  daily_delta_xy<-as.matrix(cbind(dmoday$x,dmoday$y))
  fitdelta<-Krig(daily_delta_xy,daily_delta,theta=1e5) #use TPS or krige
  mod_krtmp2<-fitdelta
  model_name<-paste("mod_kr","day",sep="_")
  data_s<-dmoday #put the 
  data_s$daily_delta<-daily_delta
  
  #########
  # STEP 4 - Calculate daily predictions - T(day) = clim(month) + delta(day)
  #########
  
  rast_clim_list<-rast_clim_yearlist[[mo]]  #select relevant month
  rast_clim_month<-raster(rast_clim_list[[1]])
  
  daily_delta_rast<-interpolate(rast_clim_month,fitdelta) #Interpolation of the bias surface...
  
  #Saving kriged surface in raster images
  data_name<-paste("daily_delta_",y_var_name,"_",sampling_dat$date[i],"_",sampling_dat$prop[i],
                   "_",sampling_dat$run_samp[i],sep="")
  raster_name_delta<-paste(interpolation_method,"_",var,"_",data_name,out_prefix,".tif", sep="")
  writeRaster(daily_delta_rast, filename=raster_name_delta,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  #Now predict daily after having selected the relevant month
  temp_list<-vector("list",length(rast_clim_list))  
  for (j in 1:length(rast_clim_list)){
    rast_clim_month<-raster(rast_clim_list[[j]])
    temp_predicted<-rast_clim_month+daily_delta_rast
    
    data_name<-paste(y_var_name,"_predicted_",names(rast_clim_list)[j],"_",
                     sampling_dat$date[i],"_",sampling_dat$prop[i],
                     "_",sampling_dat$run_samp[i],sep="")
    raster_name<-paste(interpolation_method,"_",data_name,out_prefix,".tif", sep="")
    writeRaster(temp_predicted, filename=raster_name,overwrite=TRUE) 
    temp_list[[j]]<-raster_name
  }
  
  ##########
  # STEP 5 - Prepare output object to return
  ##########
  
  mod_krtmp2<-fitdelta
  model_name<-paste("mod_kr","day",sep="_")
  names(temp_list)<-names(rast_clim_list)
  coordinates(data_s)<-cbind(data_s$x,data_s$y)
  proj4string(data_s)<-proj_str
  coordinates(data_v)<-cbind(data_v$x,data_v$y)
  proj4string(data_v)<-proj_str
  
  delta_obj<-list(temp_list,rast_clim_list,raster_name_delta,data_s,
                  data_v,sampling_dat[i,],mod_krtmp2)
  
  obj_names<-c(y_var_name,"clim","delta","data_s","data_v",
               "sampling_dat",model_name)
  names(delta_obj)<-obj_names 
  save(delta_obj,file= paste("delta_obj_",var,"_",sampling_dat$date[i],"_",sampling_dat$prop[i],
                                "_",sampling_dat$run_samp[i],out_prefix,".RData",sep=""))
  return(delta_obj)
  
}
 

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
#DATE: 06/05/2013                                                                                 
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

select_var_stack <-function(r_stack,formula_mod,spdf=TRUE){
  ##Write function to return only the relevant layers!!
  #Note that default behaviour of the function is to remove na values in the subset 
  #of raster layers and return a spdf
  
  ### Start
  
  covar_terms<-all.vars(formula_mod) #all covariates terms...+ y_var
  if (length(covar_terms)==1){
    r_stack_covar<-subset(r_stack,1)
  } #use one layer
  if (length(covar_terms)> 1){
    r_stack_covar <-subset(r_stack,covar_terms[-1])
  }
  if (spdf==TRUE){
    s_sgdf<-as(r_stack_covar,"SpatialGridDataFrame") #Conversion to spatial grid data frame, only convert the necessary layers!!
    s_spdf<-as.data.frame(s_sgdf) #Note that this automatically removes all NA rows
    s_spdf<-na.omit(s_spdf) #removes all rows that have na...
    coords<- s_spdf[,c('s1','s2')]
    coordinates(s_spdf)<-coords
    proj4string(s_spdf)<-proj4string(s_sgdf)  #Need to assign coordinates...
    #raster_pred <- rasterize(s_spdf,r1,"pred",fun=mean)
    covar_obj<-s_spdf
  } else{
    covar_obj<-r_stack_covar
  }
  
  return(covar_obj)
}

remove_na_spdf<-function(col_names,d_spdf){
  #Purpose: remote na items from a subset of a SpatialPointsDataFrame
  x<-d_spdf
  coords <-coordinates(x)
  x$s1<-coords[,1]
  x$s2<-coords[,2]
  
  x1<-x[c(col_names,"s1","s2")]
  #x1$y_var <-data_training$y_var
  #names(x1)
  x1<-na.omit(as.data.frame(x1))
  coordinates(x1)<-x1[c("s1","s2")]
  proj4string(x1)<-proj4string(d_spdf)
  return(x1)
}

predict_auto_krige_raster_model<-function(list_formulas,r_stack,data_training,out_filename){
  #This functions performs predictions on a raster grid given input models.
  #Arguments: list of fitted models, raster stack of covariates
  #Output: spatial grid data frame of the subset of tiles
  
  list_fitted_models<-vector("list",length(list_formulas))
  list_rast_pred<-vector("list",length(list_formulas))
  #s_sgdf<-as(r_stack,"SpatialGridDataFrame") #Conversion to spatial grid data frame, only convert the necessary layers!!
  proj4string(data_training) <- projection(r_stack)
  for (k in 1:length(list_formulas)){
    formula_mod<-list_formulas[[k]]
    raster_name<-out_filename[[k]]
    #mod<- try(gam(formula, data=data_training)) #change to any model!!
    s_spdf<-select_var_stack(r_stack,formula_mod,spdf=TRUE)
    col_names<-all.vars(formula_mod)
    if (length(col_names)==1){
      data_fit <-data_training
    }else{
      data_fit <- remove_na_spdf(col_names,data_training)
    }
    
    mod <- try(autoKrige(formula_mod, input_data=data_fit,new_data=s_spdf,data_variogram=data_fit))
    #mod <- try(autoKrige(formula_mod, input_data=data_training,new_data=s_spdf,data_variogram=data_training))
    model_name<-paste("mod",k,sep="")
    assign(model_name,mod) 
    
    if (inherits(mod,"autoKrige")) {           #change to c("gam","autoKrige")
      rpred<-mod$krige_output  #Extracting the SptialGriDataFrame from the autokrige object
      y_pred<-rpred$var1.pred                  #is the order the same?
      raster_pred <- rasterize(rpred,r_stack,"var1.pred",fun=mean)
      names(raster_pred)<-"y_pred" 
      writeRaster(raster_pred, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...
      #print(paste("Interpolation:","mod", j ,sep=" "))
      list_rast_pred[[k]]<-raster_name
      mod$krige_output<-NULL
      list_fitted_models[[k]]<-mod
      
    }
    if (inherits(mod,"try-error")) {
      print(paste("no autokrige model fitted:",mod,sep=" ")) #change message for any model type...
      list_fitted_models[[k]]<-mod
    }
  }
  day_prediction_obj <-list(list_fitted_models,list_rast_pred)
  names(day_prediction_obj) <-c("list_fitted_models","list_rast_pred")
  return(day_prediction_obj)
}

#Could merge both auto?
predict_autokrige_gwr_raster_model<-function(method_interp,list_formulas,r_stack,data_training,out_filename){
  #This functions performs predictions on a raster grid given input models.
  #It can be used at the daily or/and monthly time scale...
  #Arguments: list of fitted models, raster stack of covariates
  # method_interp must be equal to "gwr" or "kriging"
  #Output: spatial grid data frame of the subset of tiles
  
  list_fitted_models<-vector("list",length(list_formulas))
  list_rast_pred<-vector("list",length(list_formulas))
  #s_sgdf<-as(r_stack,"SpatialGridDataFrame") #Conversion to spatial grid data frame, only convert the necessary layers!!
  proj4string(data_training) <- projection(r_stack)
  for (k in 1:length(list_formulas)){
    formula_mod<-list_formulas[[k]]
    raster_name<-out_filename[[k]]
    #mod<- try(gam(formula, data=data_training)) #change to any model!!
    s_spdf<-select_var_stack(r_stack,formula_mod,spdf=TRUE)
    col_names<-all.vars(formula_mod) #extract terms names from formula object
    if (length(col_names)==1){
      data_fit <-data_training
    }else{
      data_fit <- remove_na_spdf(col_names,data_training)
    }
    
    if(method_interp=="kriging"){
      mod <- try(autoKrige(formula_mod, input_data=data_fit,new_data=s_spdf,data_variogram=data_fit))
    }
    
    if(method_interp=="gwr"){
      bwGm <-try(gwr.sel(formula_mod,data=data_fit,gweight=gwr.Gauss, verbose = FALSE))
      mod <- try(gwr(formula_mod, data=data_fit, bandwidth=bwGm, gweight=gwr.Gauss, hatmatrix=TRUE))
    }
    #mod <- try(autoKrige(formula_mod, input_data=data_training,new_data=s_spdf,data_variogram=data_training))
    
    model_name<-paste("mod",k,sep="")
    assign(model_name,mod) 
    
    if (inherits(mod,"autoKrige") | inherits(mod,"gwr")){           #change to c("gam","autoKrige")
      if(method_interp=="kriging"){
        rpred<-mod$krige_output  #Extracting the SptialGriDataFrame from the autokrige object
        y_pred<-rpred$var1.pred                  #is the order the same?
        raster_pred <- rasterize(rpred,r_stack,"var1.pred",fun=mean)
        mod$krige_output<-NULL
      }
      if(method_interp=="gwr"){
        rpred <- gwr(formula_mod, data_fit, bandwidth=bwGm, fit.points =s_spdf,predict=TRUE, se.fit=TRUE,fittedGWRobject=mod) 
        #y_pred<-rpred$var1.pred                  #is the order the same?
        raster_pred<-rasterize(rpred$SDF,r_stack,"pred",fun=mean)
      }
      
      names(raster_pred)<-"y_pred" 
      writeRaster(raster_pred, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...
      #print(paste("Interpolation:","mod", j ,sep=" "))
      list_rast_pred[[k]]<-raster_name
      list_fitted_models[[k]]<-mod
      
    }
    if (inherits(mod,"try-error")) {
      print(paste("no autokrige/gwr model fitted:",mod,sep=" ")) #change message for any model type...
      list_fitted_models[[k]]<-mod
    }
  }
  day_prediction_obj <-list(list_fitted_models,list_rast_pred)
  names(day_prediction_obj) <-c("list_fitted_models","list_rast_pred")
  return(day_prediction_obj)
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
#TODO:Should use interp_day_fun!!
#Add log file and calculate time and sizes for processes-outputs
runGAM_day_fun <-function(i,list_param){

  #Make this a function with multiple argument that can be used by mcmapply??
  #Arguments: 
  #1)list_index: j 
  #2)covar_rast: covariates raster images used in the modeling
  #3)covar_names: names of input variables 
  #4)lst_avg: list of LST climatogy names, may be removed later on
  #5)list_models: list input models for bias calculation
  #6)sampling_obj: data at the daily time scale
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
    
  index<-list_param$list_index
  s_raster<-list_param$covar_rast
  covar_names<-list_param$covar_names
  lst_avg<-list_param$lst_avg
  list_models<-list_param$list_models
  dst<-list_param$dst #monthly station dataset
  sampling_obj<-list_param$sampling_obj
  var<-list_param$var
  y_var_name<-list_param$y_var_name
  interpolation_method <-list_param$interpolation_method
  out_prefix<-list_param$out_prefix
  out_path<-list_param$out_path
  screen_data_training<-list_param$screen_data_training
  
  #ghcn.subsets<-sampling_obj$ghcn_data_day
  ghcn.subsets <- sampling_obj$ghcn_data
  sampling_dat <- sampling_obj$sampling_dat
  sampling <- sampling_obj$sampling_index
    
  ##########
  # STEP 1 - Read in information and get traing and testing stations
  ############# 
  
  date<-strptime(sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  proj_str<-proj4string(dst) #get the local projection information from monthly data
  
  #Adding layer LST to the raster stack  
  #names(s_raster)<-covar_names
  pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
  s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
  LST<-subset(s_raster,LST_month)
  names(LST)<-"LST"
  s_raster<-addLayer(s_raster,LST)            #Adding current month
  
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  data_day<-ghcn.subsets[[i]]
  mod_LST <- ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]  #Match interpolation date and monthly LST average
  data_day$LST <- as.data.frame(mod_LST)[,1] #Add the variable LST to the daily dataset
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
  
  #### STEP 2: PREPARE DATA
    
  #Clean out this part: make this a function call
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
  names(d)[pos]<-y_var_name
  pos<-match("value",names(x)) #Find column with name "value"
  names(x)[pos]<-y_var_name
  pos<-match("station",names(d)) #Find column with station ID
  names(d)[pos]<-c("id")
  pos<-match("station",names(x)) #Find column with name station ID
  names(x)[pos]<-c("id")
  
  data_s<-d
  data_v<-x

  data_s$y_var <- data_s[[y_var_name]] #Adding the variable modeled
  data_v$y_var <- data_v[[y_var_name]]
  
  #Adding back spatal definition
  
  coordinates(data_s)<-cbind(data_s$x,data_s$y)
  proj4string(data_s)<-proj_str
  coordinates(data_v)<-cbind(data_v$x,data_v$y)
  proj4string(data_v)<-proj_str
  #### STEP3:  NOW FIT AND PREDICT  MODEL
  
  list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!
  
  if(screen_data_training==TRUE){
    col_names <-unlist(lapply(list_formulas,all.vars)) #extract all covariates names used in the models
    col_names<-unique(col_names)
    data_fit <- remove_na_spdf(col_names,data_s)
  }else{
    data_fit <- data_s
  }
  mod_list<-fit_models(list_formulas,data_fit) #only gam at this stage
  #mod_list<-fit_models(list_formulas,data_s) #only gam at this stage
  cname<-paste("mod",1:length(mod_list),sep="") #change to more meaningful name?
  names(mod_list)<-cname
  
  #Now generate file names for the predictions...
  list_out_filename<-vector("list",length(mod_list))
  names(list_out_filename)<-cname  
  
  for (k in 1:length(list_out_filename)){
    #i indicate which day is predicted, y_var_name indicates TMIN or TMAX
    data_name<-paste(y_var_name,"_predicted_",names(mod_list)[k],"_",
                     sampling_dat$date[i],"_",sampling_dat$prop[i],
                     "_",sampling_dat$run_samp[i],sep="")
    raster_name<-file.path(out_path,paste(interpolation_method,"_",data_name,out_prefix,".tif", sep=""))
    list_out_filename[[k]]<-raster_name 
  }
  
  #now predict values for raster image...
  rast_day_list<-predict_raster_model(mod_list,s_raster,list_out_filename)
  names(rast_day_list)<-cname
  #Some models will not be predicted...remove them
  rast_day_list<-rast_day_list[!sapply(rast_day_list,is.null)] #remove NULL elements in list
    
  #Prepare object to return
  
  day_obj<- list(rast_day_list,data_s,data_v,sampling_dat[i,],mod_list,list_models)
  obj_names<-c(y_var_name,"data_s","data_v","sampling_dat","mod","formulas")
  names(day_obj)<-obj_names 
  save(day_obj,file= file.path(out_path,paste("day_obj_",interpolation_method,"_",var,"_",sampling_dat$date[i],"_",sampling_dat$prop[i],
                                                "_",sampling_dat$run_samp[i],out_prefix,".RData",sep="")))
  return(day_obj)
  
}

#Maybe should just use the same code...

runKriging_day_fun <-function(i,list_param){
  
  #Make this a function with multiple argument that can be used by mcmapply??
  #Arguments: 
  #1)list_index: j 
  #2)covar_rast: covariates raster images used in the modeling
  #3)covar_names: names of input variables 
  #4)lst_avg: list of LST climatogy names, may be removed later on
  #5)list_models: list input models for bias calculation
  #6)sampling_obj: data at the daily time scale
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
  
  index<-list_param$list_index
  s_raster<-list_param$covar_rast
  covar_names<-list_param$covar_names
  lst_avg<-list_param$lst_avg
  list_models<-list_param$list_models
  dst<-list_param$dst #monthly station dataset
  sampling_obj<-list_param$sampling_obj
  var<-list_param$var
  y_var_name<-list_param$y_var_name
  interpolation_method <-list_param$interpolation_method
  out_prefix<-list_param$out_prefix
  out_path<-list_param$out_path
  
  
  #ghcn.subsets<-sampling_obj$ghcn_data_day
  ghcn.subsets<-sampling_obj$ghcn_data
  sampling_dat <- sampling_obj$sampling_dat
  sampling <- sampling_obj$sampling_index
  
  ##########
  # STEP 1 - Read in information and get traing and testing stations
  ############# 
  
  date<-strptime(sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  proj_str<-proj4string(dst) #get the local projection information from monthly data
  
  #Adding layer LST to the raster stack  
  #names(s_raster)<-covar_names
  pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
  s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
  LST<-subset(s_raster,LST_month)
  names(LST)<-"LST"
  s_raster<-addLayer(s_raster,LST)            #Adding current month
  
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  data_day<-ghcn.subsets[[i]]
  mod_LST <- ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]  #Match interpolation date and monthly LST average
  data_day$LST <- as.data.frame(mod_LST)[,1] #Add the variable LST to the daily dataset
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
  
  #### STEP 2: PREPARE DATA
  
  #Clean out this part: make this a function call
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
  names(d)[pos]<-y_var_name
  pos<-match("value",names(x)) #Find column with name "value"
  names(x)[pos]<-y_var_name
  pos<-match("station",names(d)) #Find column with station ID
  names(d)[pos]<-c("id")
  pos<-match("station",names(x)) #Find column with name station ID
  names(x)[pos]<-c("id")
  
  data_s<-d
  data_v<-x
  
  data_s$y_var <- data_s[[y_var_name]] #Adding the variable modeled
  data_v$y_var <- data_v[[y_var_name]]
  
  #Adding back spatal definition
  
  coordinates(data_s)<-cbind(data_s$x,data_s$y)
  proj4string(data_s)<-proj_str
  coordinates(data_v)<-cbind(data_v$x,data_v$y)
  proj4string(data_v)<-proj_str
  #### STEP3:  NOW FIT AND PREDICT  MODEL
  
  list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!
  #models names
  cname<-paste("mod",1:length(list_formulas),sep="") #change to more meaningful name?
  names(list_formulas) <- cname
  #Now generate output file names for the predictions...
  list_out_filename<-vector("list",length(list_formulas))
  names(list_out_filename)<-cname  
  
  for (k in 1:length(list_out_filename)){
    #i indicate which day is predicted, y_var_name indicates TMIN or TMAX
    data_name<-paste(y_var_name,"_predicted_",names(list_formulas)[k],"_",
                     sampling_dat$date[i],"_",sampling_dat$prop[i],
                     "_",sampling_dat$run_samp[i],sep="")
    raster_name<-file.path(out_path,paste(interpolation_method,"_",data_name,out_prefix,".tif", sep=""))
    list_out_filename[[k]]<-raster_name 
  }
  
  #now fit and predict values for raster image...
  
  if (interpolation_method=="gam_daily"){
    mod_list<-fit_models(list_formulas,data_s) #only gam at this stage
    names(mod_list)<-cname
    rast_day_list<-predict_raster_model(mod_list,s_raster,list_out_filename)
    names(rast_day_list)<-cname
  }
  
  if (interpolation_method=="kriging_daily"){
    day_prediction_obj<-predict_auto_krige_raster_model(list_formulas,s_raster,data_s,list_out_filename)
    mod_list <-day_prediction_obj$list_fitted_models
    rast_day_list <-day_prediction_obj$list_rast_pred
    names(rast_day_list)<-cname
  }
    
  #Some models will not be predicted...remove them
  rast_day_list<-rast_day_list[!sapply(rast_day_list,is.null)] #remove NULL elements in list
  
  #Prepare object to return
  
  day_obj<- list(rast_day_list,data_s,data_v,sampling_dat[i,],mod_list,list_models)
  obj_names<-c(y_var_name,"data_s","data_v","sampling_dat","mod","formulas")
  names(day_obj)<-obj_names 
  save(day_obj,file= file.path(out_path,paste("day_obj_",interpolation_method,"_",var,"_",sampling_dat$date[i],"_",sampling_dat$prop[i],
                                              "_",sampling_dat$run_samp[i],out_prefix,".RData",sep="")))
  return(day_obj)
  
}

run_interp_day_fun <-function(i,list_param){
  
  #Make this a function with multiple argument that can be used by mcmapply??
  #This function performs interpolation at daily time scale. Modifications made
  #to run three possible methods: gwr, kriging and gam.
  #Arguments: 
  #1)list_index: j 
  #2)covar_rast: covariates raster images used in the modeling
  #3)covar_names: names of input variables 
  #4)lst_avg: list of LST climatogy names, may be removed later on
  #5)list_models: list input models for bias calculation
  #6)sampling_obj: data at the daily time scale
  #7)var: TMAX or TMIN, variable being interpolated
  #8)y_var_name: output name, not used at this stage
  #9)out_prefix
  #10) out_path
  
  #The output is a list of four shapefile names produced by the function:
  #1) clim: list of output names for raster climatologies 
  #2) data_month: monthly training data for bias surface modeling
  #3) mod: list of model objects fitted 
  #4) formulas: list of formulas used in bias modeling
  
  ### PARSING INPUT ARGUMENTS
  #list_param_runGAMFusion<-list(i,clim_yearlist,sampling_obj,var,y_var_name, out_prefix)
  
  index<-list_param$list_index
  s_raster<-list_param$covar_rast
  covar_names<-list_param$covar_names
  lst_avg<-list_param$lst_avg
  list_models<-list_param$list_models
  dst<-list_param$dst #monthly station dataset
  sampling_obj<-list_param$sampling_obj
  var<-list_param$var
  y_var_name<-list_param$y_var_name
  interpolation_method <-list_param$interpolation_method
  out_prefix<-list_param$out_prefix
  out_path<-list_param$out_path
  
  #ghcn.subsets<-sampling_obj$ghcn_data_day
  ghcn.subsets <- sampling_obj$ghcn_data
  sampling_dat <- sampling_obj$sampling_dat
  sampling <- sampling_obj$sampling_index
  
  ##########
  # STEP 1 - Read in information and get traing and testing stations
  ############# 
  
  date<-strptime(sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  proj_str<-proj4string(dst) #get the local projection information from monthly data
  
  #Adding layer LST to the raster stack  
  #names(s_raster)<-covar_names
  pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
  s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
  LST<-subset(s_raster,LST_month)
  names(LST)<-"LST"
  s_raster<-addLayer(s_raster,LST)            #Adding current month
  
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  data_day<-ghcn.subsets[[i]]
  mod_LST <- ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]  #Match interpolation date and monthly LST average
  data_day$LST <- as.data.frame(mod_LST)[,1] #Add the variable LST to the daily dataset
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
  
  #### STEP 2: PREPARE DATA
  
  #Clean out this part: make this a function call, should be done ine data preparation to retain the generality of the function
  
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
  names(d)[pos]<-y_var_name
  pos<-match("value",names(x)) #Find column with name "value"
  names(x)[pos]<-y_var_name
  pos<-match("station",names(d)) #Find column with station ID
  names(d)[pos]<-c("id")
  pos<-match("station",names(x)) #Find column with name station ID
  names(x)[pos]<-c("id")
  
  data_s<-d
  data_v<-x
  
  data_s$y_var <- data_s[[y_var_name]] #Adding the variable modeled
  data_v$y_var <- data_v[[y_var_name]]
  
  #Adding back spatal definition
  
  coordinates(data_s)<-cbind(data_s$x,data_s$y)
  proj4string(data_s)<-proj_str
  coordinates(data_v)<-cbind(data_v$x,data_v$y)
  proj4string(data_v)<-proj_str
  #### STEP3:  NOW FIT AND PREDICT  MODEL
  
  list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!
  #models names
  cname<-paste("mod",1:length(list_formulas),sep="") #change to more meaningful name?
  names(list_formulas) <- cname
  #Now generate output file names for the predictions...
  list_out_filename<-vector("list",length(list_formulas))
  names(list_out_filename)<-cname  
  
  for (k in 1:length(list_out_filename)){
    #i indicate which day is predicted, y_var_name indicates TMIN or TMAX
    data_name<-paste(y_var_name,"_predicted_",names(list_formulas)[k],"_",
                     sampling_dat$date[i],"_",sampling_dat$prop[i],
                     "_",sampling_dat$run_samp[i],sep="")
    raster_name<-file.path(out_path,paste(interpolation_method,"_",data_name,out_prefix,".tif", sep=""))
    list_out_filename[[k]]<-raster_name 
  }
  
  #now fit and predict values for raster image...
  
  if (interpolation_method=="gam_daily"){
    if(screen_data_training==TRUE){
      col_names <-unlist(lapply(list_formulas,all.vars)) #extract all covariates names used in the models
      col_names<-unique(col_names)
      data_fit <- remove_na_spdf(col_names,data_s)
    }else{
      data_fit <- data_s
    }
    #mod_list<-fit_models(list_formulas,data_s) #only gam at this stage
    mod_list<-fit_models(list_formulas,data_fit) #only gam at this stage
    names(mod_list)<-cname
    rast_day_list<-predict_raster_model(mod_list,s_raster,list_out_filename)
    names(rast_day_list)<-cname
  }
  
  ## need to change to use combined gwr autokrige function
  if (interpolation_method=="kriging_daily"){
    day_prediction_obj<-predict_auto_krige_raster_model(list_formulas,s_raster,data_s,list_out_filename)
    mod_list <-day_prediction_obj$list_fitted_models
    rast_day_list <-day_prediction_obj$list_rast_pred
    names(rast_day_list)<-cname
  }
  
  if (interpolation_method=="gwr_daily"){
    method_interp <- "gwr"
    day_prediction_obj<-predict_autokrige_gwr_raster_model(method_interp,list_formulas,s_raster,data_s,list_out_filename)
    mod_list <-day_prediction_obj$list_fitted_models
    rast_day_list <-day_prediction_obj$list_rast_pred
    names(rast_day_list)<-cname
  }
  #Some models will not be predicted...remove them
  rast_day_list<-rast_day_list[!sapply(rast_day_list,is.null)] #remove NULL elements in list
  
  #Prepare object to return
  
  day_obj<- list(rast_day_list,data_s,data_v,sampling_dat[i,],mod_list,list_models)
  obj_names<-c(y_var_name,"data_s","data_v","sampling_dat","mod","formulas")
  names(day_obj)<-obj_names 
  save(day_obj,file= file.path(out_path,paste("day_obj_",interpolation_method,"_",var,"_",sampling_dat$date[i],"_",sampling_dat$prop[i],
                                              "_",sampling_dat$run_samp[i],out_prefix,".RData",sep="")))
  return(day_obj)
  
}


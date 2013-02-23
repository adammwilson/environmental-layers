#Function to be used with GAM_fusion_analysis_raster_prediction_mutlisampling.R
#runClimFusion<-function(r_stack,data_training,data_testing,data_training){

####
#TODO:
#Add log file and calculate time and sizes for processes-outputs

runClim_KGFusion<-function(j){
  #Make this a function with multiple argument that can be used by mcmapply??
  #This creates clim fusion layers...
  
  #Functions used in the script
  predict_raster_model<-function(in_models,r_stack,out_filename){
    #This functions performs predictions on a raster grid given input models.
    #Arguments: list of fitted models, raster stack of covariates
    #Output: spatial grid data frame of the subset of tiles
    list_rast_pred<-vector("list",length(in_models))
    for (i in 1:length(in_models)){
      mod <-in_models[[i]] #accessing GAM model ojbect "j"
      raster_name<-out_filename[[i]]
      if (inherits(mod,"gam")) {           #change to c("gam","autoKrige")
        raster_pred<- predict(object=s_raster,model=mod,na.rm=FALSE) #Using the coeff to predict new values.
        names(raster_pred)<-"y_pred"  
        writeRaster(raster_pred, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
        print(paste("Interpolation:","mod", j ,sep=" "))
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
  #Model and response variable can be changed without affecting the script
  prop_month<-0 #proportion retained for validation
  run_samp<-1
  
  list_formulas<-lapply(list_models,as.formula,env=.GlobalEnv) #mulitple arguments passed to lapply!!

  data_month<-dst[dst$month==j,] #Subsetting dataset for the relevant month of the date being processed
  LST_name<-lst_avg[j] # name of LST month to be matched
  data_month$LST<-data_month[[LST_name]]
  
  #LST bias to model...
  data_month$LSTD_bias<-data_month$LST-data_month$TMax
  data_month$y_var<-data_month$LSTD_bias #Adding bias as the variable modeled
  mod_list<-fit_models(list_formulas,data_month) #only gam at this stage
  cname<-paste("mod",1:length(mod_list),sep="") #change to more meaningful name?
  names(mod_list)<-cname
  #Adding layer LST to the raster stack  
  pos<-match("elev",names(s_raster))
  layerNames(s_raster)[pos]<-"elev_1"
  
  pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
  s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
  LST<-subset(s_raster,LST_name)
  names(LST)<-"LST"
  #Screen for extreme values": this needs more thought, min and max val vary with regions
  #min_val<-(-15+273.16) #if values less than -15C then screen out (note the Kelvin units that will need to be changed later in all datasets)
  #r1[r1 < (min_val)]<-NA
  s_raster<-addLayer(s_raster,LST)            #Adding current month
  
  #Now generate file names for the predictions...
  list_out_filename<-vector("list",length(mod_list))
  names(list_out_filename)<-cname  
  
  for (k in 1:length(list_out_filename)){
    #j indicate which month is predicted
    data_name<-paste("bias_LST_month_",j,"_",cname[k],"_",prop_month,
                     "_",run_samp,sep="")
    raster_name<-paste("fusion_",data_name,out_prefix,".tif", sep="")
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
    data_name<-paste("clim_LST_month_",j,"_",names(rast_clim_list)[k],"_",prop_month,
                     "_",run_samp,sep="")
    raster_name<-paste("fusion_",data_name,out_prefix,".tif", sep="")
    rast_clim_list[[k]]<-raster_name
    writeRaster(clim_fus_rast, filename=raster_name,overwrite=TRUE)  #Wri
  }
  
  #Adding Kriging for Climatology options
  
  bias_xy<-coordinates(data_month)
  fitbias<-Krig(bias_xy,data_month$LSTD_bias,theta=1e5) #use TPS or krige 
  mod_krtmp1<-fitbias
  model_name<-"mod_kr"
   
  bias_rast<-interpolate(LST,fitbias) #interpolation using function from raster package
  #Saving kriged surface in raster images
  data_name<-paste("bias_LST_month_",j,"_",model_name,"_",prop_month,
                   "_",run_samp,sep="")
  raster_name_bias<-paste("fusion_",data_name,out_prefix,".tif", sep="")
  writeRaster(bias_rast, filename=raster_name_bias,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  #now climatology layer
  clim_rast<-LST-bias_rast
  data_name<-paste("clim_LST_month_",j,"_",model_name,"_",prop_month,
                   "_",run_samp,sep="")
  raster_name_clim<-paste("fusion_",data_name,out_prefix,".tif", sep="")
  writeRaster(clim_rast, filename=raster_name_clim,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  #Adding to current objects
  mod_list[[model_name]]<-mod_krtmp1
  rast_bias_list[[model_name]]<-raster_name_bias
  rast_clim_list[[model_name]]<-raster_name_clim
  
  #Prepare object to return
  clim_obj<-list(rast_bias_list,rast_clim_list,data_month,mod_list,list_formulas)
  names(clim_obj)<-c("bias","clim","data_month","mod","formulas")
  
  save(clim_obj,file= paste("clim_obj_month_",j,"_",out_prefix,".RData",sep=""))
  
  return(clim_obj)
}

## Run function for kriging...?

runGAMFusion <- function(i) {            # loop over dates
  #Change this to allow explicitly arguments...
  #Arguments: 
  #1)list of climatology files for all models...(12*nb of models)
  #2)data_s:training
  #3)data_v:testing
  #4)list of dates??
  #5)stack of covariates: not needed at this this stage
  #6)dst: data at the monthly time scale
  
  #Function used in the script
  
  date<-strptime(sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  proj_str<-proj4string(dst)

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
  
  modst<-dst[dst$month==mo,] #Subsetting dataset for the relevant month of the date being processed
  #Change to y_var...could be TMin
  #modst$LSTD_bias <- modst$LST-modst$y_var
  modst$LSTD_bias <- modst$LST-modst$TMax; #That is the difference between the monthly LST mean and monthly station mean

  x<-as.data.frame(data_v)
  d<-as.data.frame(data_s)
  #x[x$value==-999.9]<-NA
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
  #x[x$value==-999.9]<-NA
  #d[d$value==-999.9]<-NA
  pos<-match("value",names(d)) #Find column with name "value"
  #names(d)[pos]<-c("dailyTmax")
  names(d)[pos]<-y_var_name
  names(x)[pos]<-y_var_name
  #names(x)[pos]<-c("dailyTmax")
  pos<-match("station",names(d)) #Find column with name "value"
  names(d)[pos]<-c("id")
  names(x)[pos]<-c("id")
  names(modst)[1]<-c("id")       #modst contains the average tmax per month for every stations...
  
  dmoday <-merge(modst,d,by="id",suffixes=c("",".y2"))  
  xmoday <-merge(modst,x,by="id",suffixes=c("",".y2"))  
  mod_pat<-glob2rx("*.y2")   
  var_pat<-grep(mod_pat,names(dmoday),value=FALSE) # using grep with "value" extracts the matching names
  dmoday<-dmoday[,-var_pat]
  mod_pat<-glob2rx("*.y2")   
  var_pat<-grep(mod_pat,names(xmoday),value=FALSE) # using grep with "value" extracts the matching names
  xmoday<-xmoday[,-var_pat] #Removing duplicate columns
  
  data_v<-xmoday
  
  #dmoday contains the daily tmax values for training with TMax being the monthly station tmax mean
  #xmoday contains the daily tmax values for validation with TMax being the monthly station tmax mean
  
  ##########
  # STEP 7 - interpolate delta across space
  ##########
  
  daily_delta<-dmoday$dailyTmax-dmoday$TMax
  daily_delta_xy<-as.matrix(cbind(dmoday$x,dmoday$y))
  fitdelta<-Krig(daily_delta_xy,daily_delta,theta=1e5) #use TPS or krige
  mod_krtmp2<-fitdelta
  model_name<-paste("mod_kr","day",sep="_")
  data_s<-dmoday #put the 
  data_s$daily_delta<-daily_delta
  
  #########
  # STEP 8 - assemble final answer - T=LST+Bias(interpolated)+delta(interpolated)
  #########
  
  rast_clim_list<-rast_clim_yearlist[[mo]]  #select relevant month
  rast_clim_month<-raster(rast_clim_list[[1]])
  
  daily_delta_rast<-interpolate(rast_clim_month,fitdelta) #Interpolation of the bias surface...
  
  #Saving kriged surface in raster images
  data_name<-paste("daily_delta_",sampling_dat$date[i],"_",sampling_dat$prop[i],
                   "_",sampling_dat$run_samp[i],sep="")
  raster_name_delta<-paste("fusion_",data_name,out_prefix,".tif", sep="")
  writeRaster(daily_delta_rast, filename=raster_name_delta,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  #Now predict daily after having selected the relevant month
  temp_list<-vector("list",length(rast_clim_list))  
  for (j in 1:length(rast_clim_list)){
    rast_clim_month<-raster(rast_clim_list[[j]])
    temp_predicted<-rast_clim_month+daily_delta_rast
    
    data_name<-paste(y_var_name,"_predicted_",names(rast_clim_list)[j],"_",
                     sampling_dat$date[i],"_",sampling_dat$prop[i],
                     "_",sampling_dat$run_samp[i],sep="")
    raster_name<-paste("fusion_",data_name,out_prefix,".tif", sep="")
    writeRaster(temp_predicted, filename=raster_name,overwrite=TRUE) 
    temp_list[[j]]<-raster_name
  }
  
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
  save(delta_obj,file= paste("delta_obj_",sampling_dat$date[i],"_",sampling_dat$prop[i],
                                "_",sampling_dat$run_samp[i],out_prefix,".RData",sep=""))
  return(delta_obj)
  
}
 

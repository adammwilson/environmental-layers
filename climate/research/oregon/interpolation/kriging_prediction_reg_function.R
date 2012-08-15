runKriging <- function(i) {            # loop over dates
  
  date<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  
  #i=1
  date_proc<-dates[i]
  date_proc<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  day<-as.integer(strftime(date_proc, "%d"))
  year<-as.integer(strftime(date_proc, "%Y"))
  
  
  #Adding layer LST to the raster stack
  
  pos<-match(LST_month,layerNames(s_raster)) #Find column with the current month for instance mm12
  r1<-raster(s_raster,layer=pos)             #Select layer from stack
  layerNames(r1)<-"LST"
  s_raster<-addLayer(s_raster,r1)            #Adding current month
  s_sgdf<-as(s_raster,"SpatialGridDataFrame") #Conversion to spatial grid data frame
  
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  
  mod_LST <-ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]  #Match interpolation date and monthly LST average
  ghcn.subsets[[i]] = transform(ghcn.subsets[[i]],LST = mod_LST)            #Add the variable LST to the subset dataset
  #n<-nrow(ghcn.subsets[[i]])
  #ns<-n-round(n*prop)   #Create a sample from the data frame with 70% of the rows
  #nv<-n-ns              #create a sample for validation with prop of the rows
  #ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.training<-sampling[[i]]
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
  data_s <- ghcn.subsets[[i]][ind.training, ]   #Training dataset currently used in the modeling
  data_v <- ghcn.subsets[[i]][ind.testing, ]    #Testing/validation dataset using input sampling
  
  ns<-nrow(data_s)
  nv<-nrow(data_v)
  
  ###BEFORE model prediction the data object must be transformed to SDF
  
  coords<- data_v[,c('x_OR83M','y_OR83M')]
  coordinates(data_v)<-coords
  proj4string(data_v)<-CRS  #Need to assign coordinates...
  coords<- data_s[,c('x_OR83M','y_OR83M')]
  coordinates(data_s)<-coords
  proj4string(data_s)<-CRS  #Need to assign coordinates..
  
  ns<-nrow(data_s) #This is added to because some loss of data might have happened because of the averaging...
  nv<-nrow(data_v)
  
  ### PREDICTION/ Interpolation
  
  pos<-match("value",names(data_s)) #Find column with name "value"
  names(data_s)[pos]<-y_var_name
  pos<-match("value",names(data_v)) #Find column with name "value"
  names(data_v)[pos]<-y_var_name
  
  #if y_var_name=="dailyTmax"
  data_v$y_var<-data_v[[y_var_name]]/10  #Note that values are divided by 10 because the var is temp
  data_s$y_var<-data_s[[y_var_name]]/10
  
  #Model and response variable can be changed without affecting the script
  
  formula1 <- as.formula("y_var ~1", env=.GlobalEnv)
  formula2 <- as.formula("y_var~ x_OR83M+y_OR83M", env=.GlobalEnv)
  formula3 <- as.formula("y_var~ x_OR83M+y_OR83M+ELEV_SRTM", env=.GlobalEnv)
  formula4 <- as.formula("y_var~ x_OR83M+y_OR83M+DISTOC", env=.GlobalEnv)
  formula5 <- as.formula("y_var~ x_OR83M+y_OR83M+ELEV_SRTM+DISTOC", env=.GlobalEnv)
  formula6 <- as.formula("y_var~ x_OR83M+y_OR83M+Northness+Eastness", env=.GlobalEnv)
  formula7 <- as.formula("y_var~ LST", env=.GlobalEnv)
  formula8 <- as.formula("y_var~ x_OR83M+y_OR83M+LST", env=.GlobalEnv)
  formula9 <- as.formula("y_var~ x_OR83M+y_OR83M+ELEV_SRTM+LST", env=.GlobalEnv)
                        
  mod1<- try(autoKrige(formula1, input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  mod2<- try(autoKrige(formula2, input_data=data_s,new_data=s_sgdf,data_variogram=data_s)) 
  mod3<- try(autoKrige(formula3, input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  mod4<- try(autoKrige(formula4, input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  mod5<- try(autoKrige(formula5, input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  mod6<- try(autoKrige(formula6, input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  mod7<- try(autoKrige(formula7, input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  mod8<- try(autoKrige(formula8, input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  mod9<- try(autoKrige(formula9, input_data=data_s,new_data=s_sgdf,data_variogram=data_s))

  #tmax_predicted=themolst+daily_delta_rast-bias_rast #Final surface?? but daily_rst
  
  ### Model assessment
  
  for (j in 1:nmodels){
    
    ##Model assessment: specific diagnostic/metrics for GAM
    
    name<-paste("mod",j,sep="")  #modj is the name of The "j" model (mod1 if j=1) 
    mod<-get(name)               #accessing GAM model ojbect "j"
    #krmod_auto<-get(mod)

    #If mod "j" is not a model object
    if (inherits(mod,"try-error")) {
      
      results_m1[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_m1[1,2]<- ns        #number of stations used in the training stage
      results_m1[1,3]<- "SSERR"
      results_m1[1,j+3]<- NA
      
      results_m2[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_m2[1,2]<- ns        #number of stations used in the training 
      results_m2[1,3]<- "GCV"
      results_m2[1,j+3]<- NA
      
      results_m3[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_m3[1,2]<- ns        #number of stations used in the training stage
      results_m3[1,3]<- "DEV"
      results_m3[1,j+3]<- NA
      
      results_RMSE_f[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_RMSE_f[1,2]<- ns        #number of stations used in the training stage
      results_RMSE_f[1,3]<- "RSME_f"
      results_RMSE_f[1,j+3]<- NA
      
      results_MAE_f[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_MAE_f[1,2]<- ns        #number of stations used in the training stage
      results_MAE_f[1,3]<- "MAE_f"
      results_MAE_f[1,j+3]<-NA
      
      results_RMSE[1,1]<- dates[i]    #storing the interpolation dates in the first column
      results_RMSE[1,2]<- ns          #number of stations used in the training stage
      results_RMSE[1,3]<- "RMSE"
      results_RMSE[1,j+3]<- NA  #Storing RMSE for the model j
      results_MAE[1,1]<- dates[i]     #storing the interpolation dates in the first column
      results_MAE[1,2]<- ns           #number of stations used in the training stage
      results_MAE[1,3]<- "MAE"
      results_MAE[1,j+3]<- NA    #Storing MAE for the model j
      results_ME[1,1]<- dates[i]      #storing the interpolation dates in the first column
      results_ME[1,2]<- ns            #number of stations used in the training stage
      results_ME[1,3]<- "ME"
      results_ME[1,j+3]<- NA      #Storing ME for the model j
      results_R2[1,1]<- dates[i]      #storing the interpolation dates in the first column
      results_R2[1,2]<- ns            #number of stations used in the training stage
      results_R2[1,3]<- "R2"
      results_R2[1,j+3]<- NA      #Storing R2 for the model j
      
    }
    
    #If mod is a modelobject
    
    #If mod "j" is not a model object
    if (inherits(mod,"autoKrige")) {
      
      rpred<-mod$krige_output  #Extracting the SptialGriDataFrame from the autokrige object

      #rpred<- predict(mod, newdata=s_sgdf, se.fit = TRUE) #Using the coeff to predict new values.
      y_pred<-rpred$var1.pred                  #is the order the same?
      #y_prederr<-rpred$var1.var
      raster_pred<-r1
      layerNames(raster_pred)<-"y_pred"
      clearValues(raster_pred)        #Clear values in memory, just in case...
      values(raster_pred)<-as.numeric(y_pred)  #Assign values to every pixels 
      
      data_name<-paste("predicted_mod",j,"_",dates[[i]],sep="")
      raster_name<-paste("Kriging_",data_name,out_prefix,".rst", sep="")
      writeRaster(raster_pred, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
      #writeRaster(r2, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
        
      #Save png plot here...
      data_name<-paste("predicted_mod",j,"_",dates[[i]],sep="")
      png_name<-paste("Kriging_plot_",data_name,out_prefix,".png", sep="")
      png(png_name) #Create file to write a plot
      #datelabel2=format(ISOdate(year,mo,day),"%B ") #Plot label
      plot(mod) #Plot to file the autokrige object
      #savePlot(paste("Bias_surface_LST_TMax_",dates[i],out_prefix,".png", sep=""), type="png")
      dev.off()  #Release the hold to the file
      
      pred_sgdf<-as(raster_pred,"SpatialGridDataFrame" ) #Conversion to spatial grid data frame
      #rpred_val_s <- overlay(raster_pred,data_s)             #This overlays the kriged surface tmax and the location of weather stations
        
      rpred_val_s <- overlay(pred_sgdf,data_s)             #This overlays the kriged surface tmax and the location of weather stations
      rpred_val_v <- overlay(pred_sgdf,data_v)             #This overlays the kriged surface tmax and the location of weather stations
        
      pred_mod<-paste("pred_mod",j,sep="")
      #Adding the results back into the original dataframes.
      data_s[[pred_mod]]<-rpred_val_s$y_pred
      data_v[[pred_mod]]<-rpred_val_v$y_pred  
        
      #Model assessment: RMSE and then krig the residuals....!
        
      res_mod_s<- data_s$y_var - data_s[[pred_mod]]           #Residuals from kriging training
      res_mod_v<- data_v$y_var - data_v[[pred_mod]]           #Residuals from kriging validation

      ####ADDED ON JULY 20th
      res_mod<-res_mod_v
      
      #RMSE_mod <- sqrt(sum(res_mod^2)/nv)                 #RMSE FOR REGRESSION STEP 1: GAM  
      RMSE_mod<- sqrt(mean(res_mod^2,na.rm=TRUE))
      #MAE_mod<- sum(abs(res_mod),na.rm=TRUE)/(nv-sum(is.na(res_mod)))        #MAE from kriged surface validation
      MAE_mod<- mean(abs(res_mod), na.rm=TRUE)
      #ME_mod<- sum(res_mod,na.rm=TRUE)/(nv-sum(is.na(res_mod)))                    #ME, Mean Error or bias FOR REGRESSION STEP 1: GAM
      ME_mod<- mean(res_mod,na.rm=TRUE)                            #ME, Mean Error or bias FOR REGRESSION STEP 1: GAM
      #R2_mod<- cor(data_v$y_var,data_v[[pred_mod]])^2              #R2, coef. of var FOR REGRESSION STEP 1: GAM
      R2_mod<- cor(data_v$y_var,data_v[[pred_mod]], use="complete")^2
      
      R2_mod_f<- cor(data_s$y_var,data_s[[pred_mod]], use="complete")^2
      RMSE_mod_f<- sqrt(mean(res_mod_s^2,na.rm=TRUE))
      #MAE_mod<- sum(abs(res_mod),na.rm=TRUE)/(nv-sum(is.na(res_mod)))        #MAE from kriged surface validation
      MAE_mod_f<- mean(abs(res_mod_s), na.rm=TRUE)
      
      results_m1[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_m1[1,2]<- ns        #number of stations used in the training stage
      results_m1[1,3]<- "SSERR"
      results_m1[1,j+3]<- mod$sserr
      
      results_m2[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_m2[1,2]<- ns        #number of stations used in the training 
      results_m2[1,3]<- "GCV"
      results_m2[1,j+3]<- NA
      
      results_m3[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_m3[1,2]<- ns        #number of stations used in the training stage
      results_m3[1,3]<- "DEV"
      results_m3[1,j+3]<- NA
      
      results_RMSE_f[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_RMSE_f[1,2]<- ns        #number of stations used in the training stage
      results_RMSE_f[1,3]<- "RSME_f"
      results_RMSE_f[1,j+3]<-RMSE_mod_f
      
      results_MAE_f[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_MAE_f[1,2]<- ns        #number of stations used in the training stage
      results_MAE_f[1,3]<- "MAE_f"
      results_MAE_f[1,j+3]<-MAE_mod_f
      
      results_R2_f[1,1]<- dates[i]      #storing the interpolation dates in the first column
      results_R2_f[1,2]<- ns            #number of stations used in the training stage
      results_R2_f[1,3]<- "R2_f"
      results_R2_f[1,j+3]<- R2_mod_f      #Storing R2 for the model j
      
      results_RMSE[1,1]<- dates[i]    #storing the interpolation dates in the first column
      results_RMSE[1,2]<- ns          #number of stations used in the training stage
      results_RMSE[1,3]<- "RMSE"
      results_RMSE[1,j+3]<- RMSE_mod  #Storing RMSE for the model j
      results_MAE[1,1]<- dates[i]     #storing the interpolation dates in the first column
      results_MAE[1,2]<- ns           #number of stations used in the training stage
      results_MAE[1,3]<- "MAE"
      results_MAE[1,j+3]<- MAE_mod    #Storing MAE for the model j
      results_ME[1,1]<- dates[i]      #storing the interpolation dates in the first column
      results_ME[1,2]<- ns            #number of stations used in the training stage
      results_ME[1,3]<- "ME"
      results_ME[1,j+3]<- ME_mod      #Storing ME for the model j
      results_R2[1,1]<- dates[i]      #storing the interpolation dates in the first column
      results_R2[1,2]<- ns            #number of stations used in the training stage
      results_R2[1,3]<- "R2"
      results_R2[1,j+3]<- R2_mod      #Storing R2 for the model j
      
      #Saving residuals and prediction in the dataframes: tmax predicted from GAM

      name2<-paste("res_mod",j,sep="")
      data_v[[name2]]<-as.numeric(res_mod_v)
      data_s[[name2]]<-as.numeric(res_mod_s)
      #end of loop calculating RMSE
    }
  }
  
  #if (i==length(dates)){
  
  #Specific diagnostic measures related to the testing datasets

  results_table_RMSE<-as.data.frame(results_RMSE)
  results_table_MAE<-as.data.frame(results_MAE)
  results_table_ME<-as.data.frame(results_ME)
  results_table_R2<-as.data.frame(results_R2)
  results_table_RMSE_f<-as.data.frame(results_RMSE_f)
  results_table_MAE_f<-as.data.frame(results_MAE_f)
  results_table_R2_f<-as.data.frame(results_R2_f)
                        
  results_table_m1<-as.data.frame(results_m1)
  results_table_m2<-as.data.frame(results_m2)
  results_table_m3<-as.data.frame(results_m3)
  
  tb_metrics1<-rbind(results_table_RMSE,results_table_MAE, results_table_ME, 
                     results_table_R2,results_table_RMSE_f,results_table_MAE_f,results_table_R2_f)   #
  tb_metrics2<-rbind(results_table_m1,results_table_m2, results_table_m3)
  cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7","mod8","mod9")
  colnames(tb_metrics1)<-cname
  cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7","mod8","mod9")
  colnames(tb_metrics2)<-cname
  #colnames(results_table_RMSE)<-cname
  #colnames(results_table_RMSE_f)<-cname
  #tb_diagnostic1<-results_table_RMSE      #measures of validation
  #tb_diagnostic2<-results_table_RMSE_f    #measures of fit
  
  #write.table(tb_diagnostic1, file= paste(path,"/","results_fusion_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
  
  #}  
  print(paste(dates[i],"processed"))
  # Kriging object may need to be modified...because it contains the full image of prediction!!
  ##loop through model objects data frame and set field to zero...

  mod_obj<-list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9)
  names(mod_obj)<-c("mod1","mod2","mod3","mod4","mod5","mod6","mod7","mod8","mod9") #generate names automatically??
  #results_list<-list(data_s,data_v,tb_metrics1,tb_metrics2)
  #save(mod_obj,file= paste(path,"/","results_list_mod_objects_",dates[i],out_prefix,".RData",sep=""))
  
  for (j in 1:nmodels){
    if (inherits(mod_obj[[j]],"autoKrige")){
      mod_obj[[j]]$krige_output<-NULL
    }
  }
  results_list<-list(data_s,data_v,tb_metrics1,tb_metrics2,mod_obj)
  names(results_list)<-c("data_s","data_v","tb_metrics1","tb_metrics2","mod_obj")
  save(results_list,file= paste(path,"/","results_list_metrics_objects_",dates[i],out_prefix,".RData",sep=""))
  return(results_list)
  #return(tb_diagnostic1)
}
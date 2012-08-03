runGWR <- function(i) {            # loop over dates
  
  date<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  
  #i=1
  date_proc<-dates[i]
  date_proc<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  day<-as.integer(strftime(date_proc, "%d"))
  year<-as.integer(strftime(date_proc, "%Y"))
  
  
  #Adding layer LST to the raster stack and preparing terms
  
  pos<-match(LST_month,layerNames(s_raster)) #Find column with the current month for instance mm12
  r1<-raster(s_raster,layer=pos)             #Select layer from stack
  layerNames(r1)<-"LST"
  s_raster_f<-s_raster                    #make a local copy for debugging
  s_raster_f<-addLayer(s_raster_f,r1)            #Adding current month
  #s_sgdf<-as(s_raster,"SpatialGridDataFrame") #Conversion to spatial grid data frame
  
  nel<-12
  #tab_range<-data.frame(varname=character(nel),varterm=character(nel),vmin=numeric(nel),vmax=numeric(nel))
  val_range<-vector("list",nel) #list of one row data.frame
  val_rst<-vector("list",nel) #list of one row data.frame
  
  val_range[[1]]<-data.frame(varname="lon",varterm="lon",vmin=-180,vmax=180)
  val_range[[2]]<-data.frame(varname="lat",varterm="lat",vmin=-90,vmax=90)
  val_range[[3]]<-data.frame(varname="ELEV_SRTM",varterm="ELEV_SRTM",vmin=0,vmax=6000)
  val_range[[4]]<-data.frame(varname="Eastness",varterm="Eastness",vmin=-1,vmax=1)
  val_range[[5]]<-data.frame(varname="Northness",varterm="Northness",vmin=-1,vmax=1)
  val_range[[6]]<-data.frame(varname="Northness_w",varterm="Northness_w",vmin=-1,vmax=1)
  val_range[[7]]<-data.frame(varname="Eastness_w",varterm="Eastness_w",vmin=-1,vmax=1)
  val_range[[8]]<-data.frame(varname="mm_01",varterm="LST",vmin=-258.16,vmax=313.16)
  val_range[[9]]<-data.frame(varname="DISTOC",varterm="DISTOC",vmin=-0,vmax=10000000)
  val_range[[10]]<-data.frame(varname="LC1",varterm="LC1",vmin=0,vmax=100)
  val_range[[11]]<-data.frame(varname="LC3",varterm="LC3",vmin=0,vmax=100)
  val_range[[12]]<-data.frame(varname="slope",varterm="slope",vmin=0,vmax=90)
  tab_range<-do.call(rbind,val_range)
  
  #pos<-match("ELEV_SRTM",layerNames(s_raster)) #Find column with the current month for instance mm12
  #ELEV_SRTM<-raster(s_raster,pos)
  
  for (k in 1:length(val_range)){
    avl<-c(-Inf,tab_range$vmin[k],NA, tab_range$vmax[k],+Inf,NA)   #This creates a input vector...val 1 are -9999, 2 neg, 3 positive
    rclmat<-matrix(avl,ncol=3,byrow=TRUE)
    s_raster_r<-raster(s_raster_f,match(tab_range$varterm[k],layerNames(s_raster_f)))
    s_raster_r<-reclass(s_raster_r,rclmat)  #Loss of layer names when using reclass
    layerNames(s_raster_r)<-tab_range$varterm[k]
    val_rst[[k]]<-s_raster_r
  }
  s_rst_m<-stack(val_rst) #This a stacked with valid range of values
  
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

  formula1 <- as.formula("y_var~ lat + lon + ELEV_SRTM", env=.GlobalEnv)
  formula2 <- as.formula("y_var~ I(lat*lon) + ELEV_SRTM", env=.GlobalEnv)
  formula3 <- as.formula("y_var~ lat + lon + ELEV_SRTM + Northness + Eastness + DISTOC", env=.GlobalEnv)
  formula4 <- as.formula("y_var~ I(lat*lon) + ELEV_SRTM + I(Northness*Eastness) + DISTOC + LST", env=.GlobalEnv)
  formula5 <- as.formula("y_var~ lat + lon + ELEV_SRTM + Northness_w + Eastness_w + DISTOC + LST", env=.GlobalEnv)
  formula6 <- as.formula("y_var~ lat + lon + ELEV_SRTM + Northness_w + Eastness_w + DISTOC + LST + LC1", env=.GlobalEnv)
  formula7 <- as.formula("y_var~ lat + lon + ELEV_SRTM + Northness_w + Eastness_w + DISTOC + LST + LC3", env=.GlobalEnv)
  formula8 <- as.formula("y_var~ lat + lon + ELEV_SRTM + Northness_w + Eastness_w + DISTOC + LST + I(LC1*LC3)", env=.GlobalEnv)
  
  #   bwG <- gwr.sel(tmax~ lon + lat + ELEV_SRTM + Eastness + Northness + DISTOC,data=data_s,gweight=gwr.Gauss, verbose = FALSE)
  #   gwrG<- gwr(tmax~ lon + lat + ELEV_SRTM + Eastness + Northness + DISTOC, data=data_s, bandwidth=bwG, gweight=gwr.Gauss, hatmatrix=TRUE)
  
  bwGm1 <-try(gwr.sel(formula1,data=data_s,gweight=gwr.Gauss, verbose = FALSE))
  bwGm2 <-try(gwr.sel(formula2,data=data_s,gweight=gwr.Gauss, verbose = FALSE))
  bwGm3 <-try(gwr.sel(formula3,data=data_s,gweight=gwr.Gauss, verbose = FALSE))
  bwGm4 <-try(gwr.sel(formula4,data=data_s,gweight=gwr.Gauss, verbose = FALSE))
  bwGm5 <-try(gwr.sel(formula5,data=data_s,gweight=gwr.Gauss, verbose = FALSE))
  bwGm6 <-try(gwr.sel(formula6,data=data_s,gweight=gwr.Gauss, verbose = FALSE))
  bwGm7 <-try(gwr.sel(formula7,data=data_s,gweight=gwr.Gauss, verbose = FALSE))
  bwGm8 <-try(gwr.sel(formula8,data=data_s,gweight=gwr.Gauss, verbose = FALSE))
  
  mod1<- try(gwr(formula1, data=data_s, bandwidth=bwGm1, gweight=gwr.Gauss, hatmatrix=TRUE))
  mod2<- try(gwr(formula2, data=data_s, bandwidth=bwGm2, gweight=gwr.Gauss, hatmatrix=TRUE))
  mod3<- try(gwr(formula3, data=data_s, bandwidth=bwGm3, gweight=gwr.Gauss, hatmatrix=TRUE))
  mod4<- try(gwr(formula4, data=data_s, bandwidth=bwGm4, gweight=gwr.Gauss, hatmatrix=TRUE))
  mod5<- try(gwr(formula5, data=data_s, bandwidth=bwGm5, gweight=gwr.Gauss, hatmatrix=TRUE))
  mod6<- try(gwr(formula6, data=data_s, bandwidth=bwGm6, gweight=gwr.Gauss, hatmatrix=TRUE))
  mod7<- try(gwr(formula7, data=data_s, bandwidth=bwGm7, gweight=gwr.Gauss, hatmatrix=TRUE))
  mod8<- try(gwr(formula8, data=data_s, bandwidth=bwGm8, gweight=gwr.Gauss, hatmatrix=TRUE))
  
  #tmax_predicted=themolst+daily_delta_rast-bias_rast #Final surface?? but daily_rst
  #pred1 <- gwr(formula1, data_s, bandwidth=bwGm1, fit.points =s_sgdf_m,predict=TRUE, se.fit=TRUE,fittedGWRobject=mod1)
  #pred2 <- gwr(formula1, data_s, bandwidth=bwGm2, fit.points =s_sgdf_m,predict=TRUE, se.fit=TRUE,fittedGWRobject=mod1)
  
  pred_gwr<-vector("list",nmodels) #This will contain the nine gwr predictions...
  mod_obj<-vector("list",nmodels)  #This will contain the nine gwr fitting
    
  for (j in 1:nmodels){
    
    ##Model assessment: specific diagnostic/metrics for GAM

    name<-paste("mod",j,sep="")  #modj is the name of The "j" model (mod1 if j=1) 
    mod<-get(name)               #accessing GAM model ojbect "j"
    
    mod_obj[[j]]<-mod #Storing the current mod in a list
    formula_name<-paste("formula",j,sep="")
    formula<-get(formula_name)
    bwGm_name<-paste("bwGm",j,sep="")
    bwGm<-get(bwGm_name)
    t<-terms(formula) #extract information from formula
    t_l<-labels(t)        #extract labels from formula: this is a string (character), this can be used directly to find the variables in the stack!!
    
    #inter<-grepl(pattern="I", t_l,ignore.case=FALSE)
    inter<-grepl(pattern="I\\(", t_l,ignore.case=FALSE)
    if (sum(inter)>0){
      interaction<-vector("list",length(sum(inter)))
      ks<-0
      for(k in 1:length(inter)){
        if (inter[k]==TRUE){
          ks<-ks+1
          postion<-regexpr("\\(.*\\)$",t_l[k])                   
          x<-substring(t_l[k],first=postion+1,last=postion+attr(postion,"match.length")-2) #extract characters between brackets
          t_inter <-as.character(unlist(strsplit(x, "[*]"))) #strsplit creates a list!!                          
          interaction[[ks]]<-trim(t_inter)
        }
      }
    mod_varn<-c(unlist(interaction),t_l[inter==FALSE])  #"trim" white space in string character  
    }
    
    if (sum(inter)==0){
      #interaction<-vector("list",length(sum(inter)))
      mod_varn<-t_l
    }
    #browser()
    list_rst<-vector("list",length(mod_varn))
    pos<-match(mod_varn,layerNames(s_rst_m)) #Find column with the current month for instance mm12 
    s_rst_mod<-subset(s_rst_m,pos)
    
    #NOW CREATE THE MASK?? OR REMOVE NA AT THE data.frame stage...
    ### Model assessment
    s_sgdf<-as(s_rst_mod,"SpatialGridDataFrame")
    s_spdf<-as.data.frame(s_sgdf) #Note that this automatically removes all NA rows
    s_spdf<-na.omit(s_spdf) #removes all rows that have na...
    coords<- s_spdf[,c('s1','s2')]
    coordinates(s_spdf)<-coords
    proj4string(s_spdf)<-CRS  #Need to assign coordinates...
    

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
    if (inherits(mod,"gwr")) {
      
      pred <- gwr(formula1, data_s, bandwidth=bwGm, fit.points =s_spdf,predict=TRUE, se.fit=TRUE,fittedGWRobject=mod)
      
      pred_gwr[[j]]<-pred   #prediction stored in a list
      
      raster_pred<-rasterize(pred$SDF,r1,"pred",fun=mean)

      layerNames(raster_pred)<-"y_pred"
      
      data_name<-paste("predicted_mod",j,"_",dates[[i]],sep="")
      raster_name<-paste("GWR_",data_name,out_prefix,".rst", sep="")
      writeRaster(raster_pred, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
      #writeRaster(r2, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
        
      #Save png plot here...
      data_name<-paste("predicted_mod",j,"_",dates[[i]],sep="")
      png_name<-paste("GWR_plot_",data_name,out_prefix,".png", sep="")
      png(png_name) #Create file to write a plot
      #datelabel2=format(ISOdate(year,mo,day),"%B ") #Plot label
      plot(raster_pred) #Plot to file the autokrige object
      #savePlot(paste("Bias_surface_LST_TMax_",dates[i],out_prefix,".png", sep=""), type="png")
      dev.off()  #Release the hold to the file
      
      
      #rpred<-as #Extracting the SptialGriDataFrame from the autokrige object
      
      #rpred<- predict(mod, newdata=s_sgdf, se.fit = TRUE) #Using the coeff to predict new values.
      #y_pred<-rpred$var1.pred                  #is the order the same?
      #y_prederr<-rpred$var1.var
      
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
      results_m1[1,3]<- "AICb"
      results_m1[1,j+3]<- mod$results$AICb
      
      results_m2[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_m2[1,2]<- ns        #number of stations used in the training 
      results_m2[1,3]<- "RSS"
      results_m2[1,j+3]<- mod$results$rss
      
      results_m3[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_m3[1,2]<- ns        #number of stations used in the training stage
      results_m3[1,3]<- "AICc"
      results_m3[1,j+3]<- mod$results$AICc
      
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
  
  #Preparing labels
  mod_labels<-rep("mod",nmodels)
  index<-as.character(1:nmodels)
  mod_labels<-paste(mod_labels,index,sep="")

  tb_metrics1<-rbind(results_table_RMSE,results_table_MAE, results_table_ME, 
                     results_table_R2,results_table_RMSE_f,results_table_MAE_f,results_table_R2_f)   #
  tb_metrics2<-rbind(results_table_m1,results_table_m2, results_table_m3)
  cname<-c("dates","ns","metric", mod_labels)
  #cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7","mod8")
  colnames(tb_metrics1)<-cname
  #cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7","mod8")
  colnames(tb_metrics2)<-cname

  #}  
  print(paste(dates[i],"processed"))
  # Kriging object may need to be modified...because it contains the full image of prediction!!
  ##loop through model objects data frame and set field to zero...

  #mod_obj<-list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)
  #names(mod_obj)<-c("mod1","mod2","mod3","mod4","mod5","mod6","mod7","mod8") #generate names automatically??
  names(mod_obj)<-mod_labels
  #results_list<-list(data_s,data_v,tb_metrics1,tb_metrics2)
  save(mod_obj,file= paste(path,"/","results_list_mod_objects_",dates[i],out_prefix,".RData",sep=""))
  
  names(pred_gwr)<-mod_labels
  save(pred_gwr,file= paste(path,"/","results_list_pred_mod_objects_",dates[i],out_prefix,".RData",sep=""))
  
  results_list<-list(data_s,data_v,tb_metrics1,tb_metrics2,mod_obj)
  names(results_list)<-c("data_s","data_v","tb_metrics1","tb_metrics2","mod_obj")
  save(results_list,file= paste(path,"/","results_list_metrics_objects_",dates[i],out_prefix,".RData",sep=""))
  return(results_list)
  #return(tb_diagnostic1)
}
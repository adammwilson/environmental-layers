
runKriging <- function(i) {            # loop over dates
  
  #This allows to change only one name of the 
  
  date<-strptime(dates[i], "%Y%m%d")
  month<-strftime(date, "%m")
  LST_month<-paste("mm_",month,sep="")

  mod <-ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]
  ghcn.subsets[[i]]$LST <-mod[[1]]
  #                    
  #   n<-nrow(ghcn.subsets[[i]])
  #   ns<-n-round(n*prop)                             #Create a sample from the data frame with 70% of the rows
  #   nv<-n-ns                                        #create a sample for validation with prop of the rows
  #   ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE)  #This selects the index position for 70% of the rows taken randomly
  ind.training<-sampling[[i]]
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)         #This selects the index position for testing subset stations.
  data_s <- ghcn.subsets[[i]][ind.training, ]
  data_v <- ghcn.subsets[[i]][ind.testing, ]
  
  #adding to SpatialGridDataFrame

  pos<-match(LST_month,layerNames(s_raster)) #Find column with the current month for instance mm12
  r1<-raster(s_raster,layer=pos)             #Select layer from stack
  layerNames(r1)<-"LST"
  s_raster<-addLayer(s_raster,r1)
  s_sgdf<-as(s_raster,"SpatialGridDataFrame") #Conversion to spatial grid data frame
  
  ###BEFORE Kringing the data object must be transformed to SDF
  
  coords<- data_v[,c('x_OR83M','y_OR83M')]
  coordinates(data_v)<-coords
  proj4string(data_v)<-CRS  #Need to assign coordinates...
  coords<- data_s[,c('x_OR83M','y_OR83M')]
  coordinates(data_s)<-coords
  proj4string(data_s)<-CRS  #Need to assign coordinates..
  
  #This allows to change only one name of the data.frame
  pos<-match("value",names(data_s)) #Find column with name "value"
  names(data_s)[pos]<-c("tmax")
  data_s$tmax<-data_s$tmax/10                #TMax is the average max temp for months
  pos<-match("value",names(data_v)) #Find column with name "value"
  names(data_v)[pos]<-c("tmax")
  data_v$tmax<-data_v$tmax/10
  #dstjan=dst[dst$month==9,]  #dst contains the monthly averages for tmax for every station over 2000-2010
  ##############
  ###STEP 2 KRIGING###
  
  #Kriging tmax
  
  #   hscat(tmax~1,data_s,(0:9)*20000)                       # 9 lag classes with 20,000m width
  #   v<-variogram(tmax~1, data_s)                           # This plots a sample varigram for date 10 fir the testing dataset
  #   plot(v)
  #   v.fit<-fit.variogram(v,vgm(2000,"Sph", 150000,1000))   #Model variogram: sill is 2000, spherical, range 15000 and nugget 1000
  #   plot(v, v.fit)                                         #Compare model and sample variogram via a graphical plot
  #   tmax_krige<-krige(tmax~1, data_s,mean_LST, v.fit)      #mean_LST provides the data grid/raster image for the kriging locations to be predicted.
  
#   krmod1<-try(autoKrige(tmax~1, data_s,s_sgdf,data_s)) #Use autoKrige instead of krige: with data_s for fitting on a grid
#   krmod2<-try(autoKrige(tmax~x_OR83M+y_OR83M,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
#   krmod3<-try(autoKrige(tmax~x_OR83M+y_OR83M+ELEV_SRTM,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
#   krmod4<-try(autoKrige(tmax~x_OR83M+y_OR83M+DISTOC,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
#   krmod5<-try(autoKrige(tmax~x_OR83M+y_OR83M+ELEV_SRTM+DISTOC,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
#   krmod6<-try(autoKrige(tmax~x_OR83M+y_OR83M+Northness+Eastness,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
#   krmod7<-try(autoKrige(tmax~x_OR83M+y_OR83M+Northness+Eastness,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
#   
  krmod1<-try(autoKrige(tmax~1, data_s,s_sgdf,data_s)) #Use autoKrige instead of krige: with data_s for fitting on a grid
  krmod2<-try(autoKrige(tmax~lat+lon,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  krmod3<-try(autoKrige(tmax~lat+lon+ELEV_SRTM,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  krmod4<-try(autoKrige(tmax~lat+lon+DISTOC,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  krmod5<-try(autoKrige(tmax~lat+lon+ELEV_SRTM+DISTOC,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  krmod6<-try(autoKrige(tmax~lat+lon+Northness+Eastness,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  krmod7<-try(autoKrige(tmax~lat+lon+Northness+Eastness,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  
  krmod8<-try(autoKrige(tmax~LST,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  krmod9<-try(autoKrige(tmax~lat+lon+LST,input_data=data_s,new_data=s_sgdf,data_variogram=data_s))
  
#   krig1<-krmod1$krige_output                   #Extracting Spatial Grid Data frame                    
#   krig2<-krmod2$krige_output
#   krig3<-krmod3$krige_outpu
#   krig4<-krmod4$krige_output
#   krig5<-krmod5$krige_output
#   krig6<-krmod6$krige_output                   #Extracting Spatial Grid Data frame                    
#   krig7<-krmod7$krige_output
  #krig8<-krmod8$krige_outpu
  #krig9<-krmod9$krige_output
  
  #tmax_krig1_s <- overlay(krige,data_s)             #This overlays the kriged surface tmax and the location of weather stations
  #tmax_krig1_v <- overlay(krige,data_v)
  #   
  #   #Cokriging tmax
  #   g<-gstat(NULL,"tmax", tmax~1, data_s)                   #This creates a gstat object "g" that acts as container for kriging specifications.
  #   g<-gstat(g, "SRTM_elev",ELEV_SRTM~1,data_s)            #Adding variables to gstat object g
  #   g<-gstat(g, "LST", LST~1,data_s)
  
  #   vm_g<-variogram(g)                                     #Visualizing multivariate sample variogram.
  #   vm_g.fit<-fit.lmc(vm_g,g,vgm(2000,"Sph", 100000,1000)) #Fitting variogram for all variables at once.
  #   plot(vm_g,vm_g.fit)                                    #Visualizing variogram fit and sample
  #   vm_g.fit$set <-list(nocheck=1)                         #Avoid checking and allow for different range in variogram
  #   co_kriged_surf<-predict(vm_g.fit,mean_LST) #Prediction using co-kriging with grid location defined from input raster image.
  #   #co_kriged_surf$tmax.pred                              #Results stored in SpatialGridDataFrame with tmax prediction accessible in dataframe.
  
  #spplot.vcov(co_kriged_surf)                           #Visualizing the covariance structure
  
  #   tmax_cokrig1_s<- overlay(co_kriged_surf,data_s)        #This overalys the cokriged surface tmax and the location of weather stations
  #   tmax_cokrig1_v<- overlay(co_kriged_surf,data_v)
  
  for (j in 1:models){
    
    #mod<-paste("krig",j,sep="")
    mod<-paste("krmod",j,sep="")
    
    krmod_auto<-get(mod)
    
    #If mod "j" is not a model object
    if (inherits(krmod_auto,"try-error")) {
      
      #Model assessment:results are NA
      
      results_RMSE[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_RMSE[1,2]<- ns        #number of stations used in the training stage
      results_RMSE[1,3]<- "RMSE"
      results_RMSE[1,j+3]<- NA
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_MAE[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_MAE[1,2]<- ns        #number of stations used in the training stage
      results_MAE[1,3]<- "MAE"
      results_MAE[1,j+3]<- NA
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_ME[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_ME[1,2]<- ns        #number of stations used in the training stage
      results_ME[1,3]<- "ME"
      results_ME[1,j+3]<- NA
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_R2[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_R2[1,2]<- ns        #number of stations used in the training stage
      results_R2[1,3]<- "R2"
      results_R2[1,j+3]<- NA
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_RMSE_f[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_RMSE_f[1,2]<- ns        #number of stations used in the training stage
      results_RMSE_f[1,3]<- "RMSE_f"
      results_RMSE_f[1,j+3]<- NA
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_MAE_f[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_MAE_f[1,2]<- ns        #number of stations used in the training stage
      results_MAE_f[1,3]<- "MAE_f"
      results_MAE_f[1,j+3]<- NA
      name3<-paste("res_kr_mod",j,sep="")
      
    }
    
    #If mod "j" is not a model object
    if (inherits(krmod_auto,"autoKrige")) {
      krmod<-krmod_auto$krige_output                   #Extracting Spatial Grid Data frame                    
      
      krig_val_s <- overlay(krmod,data_s)             #This overlays the kriged surface tmax and the location of weather stations
      krig_val_v <- overlay(krmod,data_v)             #This overlays the kriged surface tmax and the location of weather stations
      
      pred_krmod<-paste("pred_krmod",j,sep="")
      #Adding the results back into the original dataframes.
      data_s[[pred_krmod]]<-krig_val_s$var1.pred
      data_v[[pred_krmod]]<-krig_val_v$var1.pred  
      
      #Model assessment: RMSE and then krig the residuals....!
      
      res_mod_kr_s<- data_s$tmax - data_s[[pred_krmod]]           #Residuals from kriging training
      res_mod_kr_v<- data_v$tmax - data_v[[pred_krmod]]           #Residuals from kriging validation
      
      RMSE_mod_kr_s <- sqrt(sum(res_mod_kr_s^2,na.rm=TRUE)/(nv-sum(is.na(res_mod_kr_s))))         #RMSE from kriged surface training
      RMSE_mod_kr_v <- sqrt(sum(res_mod_kr_v^2,na.rm=TRUE)/(nv-sum(is.na(res_mod_kr_v))))         #RMSE from kriged surface validation
      MAE_mod_kr_s<- sum(abs(res_mod_kr_s),na.rm=TRUE)/(nv-sum(is.na(res_mod_kr_s)))        #MAE from kriged surface training                    #MAE, Mean abs. Error FOR REGRESSION STEP 1: GAM   
      MAE_mod_kr_v<- sum(abs(res_mod_kr_v),na.rm=TRUE)/(nv-sum(is.na(res_mod_kr_v)))        #MAE from kriged surface validation
      ME_mod_kr_s<- sum(res_mod_kr_s,na.rm=TRUE)/(nv-sum(is.na(res_mod_kr_s)))                    #ME, Mean Error or bias FOR REGRESSION STEP 1: GAM
      ME_mod_kr_v<- sum(res_mod_kr_v,na.rm=TRUE)/(nv-sum(is.na(res_mod_kr_v)))                    #ME, Mean Error or bias FOR REGRESSION STEP 1: GAM
      R2_mod_kr_s<- cor(data_s$tmax,data_s[[pred_krmod]],use="complete.obs")^2                  #R2, coef. of determination FOR REGRESSION STEP 1: GAM
      R2_mod_kr_v<- cor(data_v$tmax,data_v[[pred_krmod]],use="complete.obs")^2                  #R2, coef. of determinationFOR REGRESSION STEP 1: GAM
      #(nv-sum(is.na(res_mod2)))
      #Writing out results
      
      results_RMSE[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_RMSE[1,2]<- ns        #number of stations used in the training stage
      results_RMSE[1,3]<- "RMSE"
      results_RMSE[1,j+3]<- RMSE_mod_kr_v
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_MAE[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_MAE[1,2]<- ns        #number of stations used in the training stage
      results_MAE[1,3]<- "MAE"
      results_MAE[1,j+3]<- MAE_mod_kr_v
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_ME[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_ME[1,2]<- ns        #number of stations used in the training stage
      results_ME[1,3]<- "ME"
      results_ME[1,j+3]<- ME_mod_kr_v
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_R2[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_R2[1,2]<- ns        #number of stations used in the training stage
      results_R2[1,3]<- "R2"
      results_R2[1,j+3]<- R2_mod_kr_v
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_RMSE_f[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_RMSE_f[1,2]<- ns        #number of stations used in the training stage
      results_RMSE_f[1,3]<- "RMSE_f"
      results_RMSE_f[1,j+3]<- RMSE_mod_kr_s
      #results_RMSE_kr[i,3]<- res_mod_kr_v
      
      results_MAE_f[1,1]<- dates[i]  #storing the interpolation dates in the first column
      results_MAE_f[1,2]<- ns        #number of stations used in the training stage
      results_MAE_f[1,3]<- "MAE_f"
      results_MAE_f[1,j+3]<- MAE_mod_kr_s
      name3<-paste("res_kr_mod",j,sep="")
      
      #as.numeric(res_mod)
      #data_s[[name3]]<-res_mod_kr_s
      data_s[[name3]]<-as.numeric(res_mod_kr_s)
      #data_v[[name3]]<-res_mod_kr_v 
      data_v[[name3]]<-as.numeric(res_mod_kr_v)
      #Writing residuals from kriging
      
      #Saving kriged surface in raster images
      data_name<-paste("mod",j,"_",dates[[i]],sep="")
      #krig_raster_name<-paste("krmod_",data_name,out_prefix,".tif", sep="")
      #writeGDAL(krmod,fname=krig_raster_name, driver="GTiff", type="Float32",options ="INTERLEAVE=PIXEL", overwrite=TRUE)
      krig_raster_name<-paste("krmod_",data_name,out_prefix,".rst", sep="")
      writeRaster(raster(krmod), filename=krig_raster_name, overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
      
      #krig_raster_name<-paste("Kriged_tmax_",data_name,out_prefix,".tif", sep="")
      #writeGDAL(tmax_krige,fname=krig_raster_name, driver="GTiff", type="Float32",options ="INTERLEAVE=PIXEL")
      #X11()
      #plot(raster(co_kriged_surf))
      #title(paste("Tmax cokriging for date ",dates[[i]],sep=""))
      #savePlot(paste("Cokriged_tmax",data_name,out_prefix,".png", sep=""), type="png")
      #dev.off()
      #X11()
      #plot(raster(tmax_krige))
      #title(paste("Tmax Kriging for date ",dates[[i]],sep=""))
      #savePlot(paste("Kriged_res_",data_name,out_prefix,".png", sep=""), type="png")
      #dev.off()
      # end of if krige object
    }
    
  }
  
  #   #Co-kriging only on the validation sites for faster computing
  #   
  #   cokrig1_dv<-predict(vm_g.fit,data_v)
  #   cokrig1_ds<-predict(vm_g.fit,data_s)
  # #   data_s$tmax_cokr<-cokrig1_ds$tmax.pred    
  # #   data_v$tmax_cokr<-cokrig1_dv$tmax.pred
  #   
  #   #Calculate RMSE and then krig the residuals....!
  #   
  #   res_mod1<- data_v$tmax - data_v$tmax_kr              #Residuals from kriging.
  #   res_mod2<- data_v$tmax - data_v$tmax_cokr            #Residuals from cokriging.
  #   
  #   RMSE_mod1 <- sqrt(sum(res_mod1^2,na.rm=TRUE)/(nv-sum(is.na(res_mod1))))                  #RMSE from kriged surface.
  #   RMSE_mod2 <- sqrt(sum(res_mod2^2,na.rm=TRUE)/(nv-sum(is.na(res_mod2))))                  #RMSE from co-kriged surface.
  #   #(nv-sum(is.na(res_mod2)))       
  
  #Saving the subset in a dataframe
  data_name<-paste("ghcn_v_",dates[[i]],sep="")
  assign(data_name,data_v)
  data_name<-paste("ghcn_s_",dates[[i]],sep="")
  assign(data_name,data_s)
  
  #   results[i,1]<- dates[i]  #storing the interpolation dates in the first column
  #   results[i,2]<- ns     #number of stations in training
  #   results[i,3]<- RMSE_mod1
  #   results[i,4]<- RMSE_mod2  
  #   
  #   results_mod_n[i,1]<-dates[i]
  #   results_mod_n[i,2]<-(nv-sum(is.na(res_mod1)))
  #   results_mod_n[i,3]<-(nv-sum(is.na(res_mod2)))
  
  #Specific diagnostic measures related to the testing datasets
  #browser()
  results_table_RMSE<-as.data.frame(results_RMSE)
  results_table_MAE<-as.data.frame(results_MAE)
  results_table_ME<-as.data.frame(results_ME)
  results_table_R2<-as.data.frame(results_R2)
  results_table_RMSE_f<-as.data.frame(results_RMSE_f)
  results_table_MAE_f<-as.data.frame(results_MAE_f)
  
  results_table_AIC<-as.data.frame(results_AIC) #Other tables for kriging
  results_table_GCV<-as.data.frame(results_GCV)
  results_table_DEV<-as.data.frame(results_DEV)
  
  tb_metrics1<-rbind(results_table_RMSE,results_table_MAE, results_table_ME, results_table_R2,results_table_RMSE_f,results_table_MAE_f)   #
  tb_metrics2<-rbind(results_table_AIC,results_table_GCV, results_table_DEV)
  cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7")
  colnames(tb_metrics1)<-cname
  cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7")
  colnames(tb_metrics2)<-cname
  #colnames(results_table_RMSE)<-cname
  #colnames(results_table_RMSE_f)<-cname
  #tb_diagnostic1<-results_table_RMSE      #measures of validation
  #tb_diagnostic2<-results_table_RMSE_f    #measures of fit
  
  #write.table(tb_diagnostic1, file= paste(path,"/","results_fusion_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
  
  #}  
  print(paste(dates[i],"processed"))
  mod_obj<-list(krmod1,krmod2,krmod3,krmod4,krmod5,krmod6,krmod7)
  # end of the for loop1
  #results_list<-list(data_s,data_v,tb_metrics1,tb_metrics2)
  results_list<-list(data_s,data_v,tb_metrics1,tb_metrics2,mod_obj)
  return(results_list)
  #return(tb_diagnostic1)
}
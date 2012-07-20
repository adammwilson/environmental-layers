runFusion <- function(i) {            # loop over dates
  
  date<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  
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
  data_v <- ghcn.subsets[[i]][ind.testing, ]    #Testing/validation dataset
  
  ns<-nrow(data_s)
  nv<-nrow(data_v)
  #i=1
  date_proc<-dates[i]
  date_proc<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  day<-as.integer(strftime(date_proc, "%d"))
  year<-as.integer(strftime(date_proc, "%Y"))

  datelabel=format(ISOdate(year,mo,day),"%b %d, %Y")
  
  ###########
  #  STEP 1 - 10 year monthly averages
  ###########
  
  #l=list.files(pattern="mean_month.*rescaled.rst")
  l <-readLines(paste(path,"/",infile6, sep=""))
  molst<-stack(l)  #Creating a raster stack...
  #setwd(old)
  molst=molst-273.16  #K->C
  idx <- seq(as.Date('2010-01-15'), as.Date('2010-12-15'), 'month')
  molst <- setZ(molst, idx)
  layerNames(molst) <- month.abb
  themolst<-raster(molst,mo) #current month being processed saved in a raster image
  plot(themolst)
  
  ###########
  # STEP 2 - Weather station means across same days: Monthly mean calculation
  ###########
  
  ##Added by Benoit ######
  date1<-ISOdate(data3$year,data3$month,data3$day) #Creating a date object from 3 separate column
  date2<-as.POSIXlt(as.Date(date1))
  data3$date<-date2
  d<-subset(data3,year>=2000 & mflag=="0" ) #Selecting dataset 2000-2010 with good quality: 193 stations
  #May need some screeing??? i.e. range of temp and elevation...
  d1<-aggregate(value~station+month, data=d, mean)  #Calculate monthly mean for every station in OR
  id<-as.data.frame(unique(d1$station))     #Unique station in OR for year 2000-2010: 193 but 7 loss of monthly avg    
  
  dst<-merge(d1, stat_loc, by.x="station", by.y="STAT_ID")   #Inner join all columns are retained
  
  #This allows to change only one name of the data.frame
  pos<-match("value",names(dst)) #Find column with name "value"
  names(dst)[pos]<-c("TMax")
  dst$TMax<-dst$TMax/10                #TMax is the average max temp for months
  #dstjan=dst[dst$month==9,]  #dst contains the monthly averages for tmax for every station over 2000-2010
  ##############
  
  modst=dst[dst$month==mo,] #Subsetting dataset for the relevant month of the date being processed
  
  ##########
  # STEP 3 - get LST at stations
  ##########
  
  sta_lola=modst[,c("lon","lat")] #Extracting locations of stations for the current month..
  
  proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
  lookup<-function(r,lat,lon) {
    xy<-project(cbind(lon,lat),proj_str);
    cidx<-cellFromXY(r,xy);
    return(r[cidx])
  }
  sta_tmax_from_lst=lookup(themolst,sta_lola$lat,sta_lola$lon) #Extracted values of LST for the stations
  
  #########
  # STEP 4 - bias at stations     
  #########
  
  sta_bias=sta_tmax_from_lst-modst$TMax; #That is the difference between the monthly LST mean and monthly station mean
  #Added by Benoit
  modst$LSTD_bias<-sta_bias  #Adding bias to data frame modst containning the monthly average for 10 years
  
  bias_xy=project(as.matrix(sta_lola),proj_str)
  # windows()
  png(paste("LST_TMax_scatterplot_",dates[i],out_prefix,".png", sep=""))
  plot(modst$TMax,sta_tmax_from_lst,xlab="Station mo Tmax",ylab="LST mo Tmax",main=paste("LST vs TMax for",datelabel,sep=" "))
  abline(0,1)
  dev.off()
  
  #added by Benoit 
  #x<-ghcn.subsets[[i]]  #Holds both training and testing for instance 161 rows for Jan 1
  x<-data_v
  d<-data_s
  
  pos<-match("value",names(d)) #Find column with name "value"
  names(d)[pos]<-c("dailyTmax")
  names(x)[pos]<-c("dailyTmax")
  d$dailyTmax=(as.numeric(d$dailyTmax))/10 #stored as 1/10 degree C to allow integer storage
  x$dailyTmax=(as.numeric(x$dailyTmax))/10 #stored as 1/10 degree C to allow integer storage
  pos<-match("station",names(d)) #Find column with name "value"
  names(d)[pos]<-c("id")
  names(x)[pos]<-c("id")
  names(modst)[1]<-c("id")       #modst contains the average tmax per month for every stations...
  dmoday=merge(modst,d,by="id")  #LOOSING DATA HERE!!! from 113 t0 103
  xmoday=merge(modst,x,by="id")  #LOOSING DATA HERE!!! from 48 t0 43
  names(dmoday)[4]<-c("lat")
  names(dmoday)[5]<-c("lon")     #dmoday contains all the the information: BIAS, monn
  names(xmoday)[4]<-c("lat")
  names(xmoday)[5]<-c("lon")     #dmoday contains all the the information: BIAS, monn
  
  data_v<-xmoday
  ###
  
  #dmoday contains the daily tmax values for training with TMax being the monthly station tmax mean
  #xmoday contains the daily tmax values for validation with TMax being the monthly station tmax mean
  
  # windows()
  #png(paste("LST_TMax_scatterplot_",dates[i],out_prefix,".png", sep=""))
  png(paste("Daily_tmax_monthly_TMax_scatterplot_",dates[i],out_prefix,".png", sep=""))
  plot(dailyTmax~TMax,data=dmoday,xlab="Mo Tmax",ylab=paste("Daily for",datelabel),main="across stations in OR")
  #savePlot(paste("Daily_tmax_monthly_TMax_scatterplot_",dates[i],out_prefix,".png", sep=""), type="png")
  #png(paste("LST_TMax_scatterplot_",dates[i],out_prefix,".png", sep=""))
  dev.off()
  
  ########
  # STEP 5 - interpolate bias
  ########
  
  # ?? include covariates like elev, distance to coast, cloud frequency, tree height
  #library(fields)
  #windows()
  quilt.plot(sta_lola,sta_bias,main="Bias at stations",asp=1)
  US(add=T,col="magenta",lwd=2)
  #fitbias<-Tps(bias_xy,sta_bias) #use TPS or krige
  
  #Adding options to use only training stations: 07/11/2012
  bias_xy=project(as.matrix(sta_lola),proj_str)
  #bias_xy2=project(as.matrix(c(dmoday$lon,dmoday$lat),proj_str)
  if(bias_val==1){
    sta_bias<-dmoday$LSTD_bias
    bias_xy<-cbind(dmoday$x_OR83M,dmoday$y_OR83M)
  }
  
  fitbias<-Krig(bias_xy,sta_bias,theta=1e5) #use TPS or krige 
  #The output is a krig object using fields
  mod9a<-fitbias
  # Creating plot of bias surface and saving it
  #X11()
  png(paste("Bias_surface_LST_TMax_",dates[i],out_prefix,".png", sep="")) #Create file to write a plot
  datelabel2=format(ISOdate(year,mo,day),"%B ") #added by Benoit, label
  surface(fitbias,col=rev(terrain.colors(100)),asp=1,main=paste("Interpolated bias for",datelabel2,sep=" ")) #Plot to file
  #savePlot(paste("Bias_surface_LST_TMax_",dates[i],out_prefix,".png", sep=""), type="png")
  dev.off()  #Release the hold to the file
  
  #US(add=T,col="magenta",lwd=2)
  
  ##########
  # STEP 7 - interpolate delta across space
  ##########
  
  daily_sta_lola=dmoday[,c("lon","lat")] #could be same as before but why assume merge does this - assume not
  daily_sta_xy=project(as.matrix(daily_sta_lola),proj_str)
  daily_delta=dmoday$dailyTmax-dmoday$TMax
  #windows()
  quilt.plot(daily_sta_lola,daily_delta,asp=1,main="Station delta for Jan 15")
  US(add=T,col="magenta",lwd=2)
  #fitdelta<-Tps(daily_sta_xy,daily_delta) #use TPS or krige
  fitdelta<-Krig(daily_sta_xy,daily_delta,theta=1e5) #use TPS or krige
  #Kriging using fields package
  mod9b<-fitdelta
  # Creating plot of bias surface and saving it
  #X11()
  png(paste("Delta_surface_LST_TMax_",dates[i],out_prefix,".png", sep=""))
  surface(fitdelta,col=rev(terrain.colors(100)),asp=1,main=paste("Interpolated delta for",datelabel,sep=" "))
  #savePlot(paste("Delta_surface_LST_TMax_",dates[i],out_prefix,".png", sep=""), type="png")
  dev.off()
  #US(add=T,col="magenta",lwd=2)
  #
  
  #### Added by Benoit on 06/19
  data_s<-dmoday #put the 
  data_s$daily_delta<-daily_delta
  
  
  #data_s$y_var<-daily_delta  #y_var is the variable currently being modeled, may be better with BIAS!!
  data_s$y_var<-data_s$LSTD_bias
  #data_s$y_var<-(data_s$dailyTmax)*10
  #Model and response variable can be changed without affecting the script
  
  mod1<- try(gam(y_var~ s(lat) + s (lon) + s (ELEV_SRTM), data=data_s))
  mod2<- try(gam(y_var~ s(lat,lon)+ s(ELEV_SRTM), data=data_s)) #modified nesting....from 3 to 2
  mod3<- try(gam(y_var~ s(lat) + s (lon) + s (ELEV_SRTM) +  s (Northness)+ s (Eastness) + s(DISTOC), data=data_s))
  mod4<- try(gam(y_var~ s(lat) + s (lon) + s(ELEV_SRTM) + s(Northness) + s (Eastness) + s(DISTOC) + s(LST), data=data_s))
  mod5<- try(gam(y_var~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST), data=data_s))
  mod6<- try(gam(y_var~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST)+s(LC1), data=data_s))
  mod7<- try(gam(y_var~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST)+s(LC3), data=data_s))
  mod8<- try(gam(y_var~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST) + s(LC1), data=data_s))
  
  #Added
  #tmax_predicted=themolst+daily_delta_rast-bias_rast #Final surface?? but daily_rst
  
  #### Added by Benoit ends
  
  #########
  # STEP 8 - assemble final answer - T=LST+Bias(interpolated)+delta(interpolated)
  #########

  
  bias_rast=interpolate(themolst,fitbias) #interpolation using function from raster package
  #themolst is raster layer, fitbias is "Krig" object from bias surface
  #plot(bias_rast,main="Raster bias") #This not displaying...
  
  #Saving kriged surface in raster images
  data_name<-paste("bias_LST_",dates[[i]],sep="")
  raster_name<-paste("fusion_",data_name,out_prefix,".rst", sep="")
  writeRaster(bias_rast, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  daily_delta_rast=interpolate(themolst,fitdelta) #Interpolation of the bias surface...
  
  #plot(daily_delta_rast,main="Raster Daily Delta")
  
  #Saving kriged surface in raster images
  data_name<-paste("daily_delta_",dates[[i]],sep="")
  raster_name<-paste("fusion_",data_name,out_prefix,".rst", sep="")
  writeRaster(daily_delta_rast, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  tmax_predicted=themolst+daily_delta_rast-bias_rast #Final surface  as a raster layer...
  #tmax_predicted=themolst+daily_delta_rast+bias_rast #Added by Benoit, why is it -bias_rast
  #plot(tmax_predicted,main="Predicted daily")
  
  #Saving kriged surface in raster images
  data_name<-paste("tmax_predicted_",dates[[i]],sep="")
  raster_name<-paste("fusion_",data_name,out_prefix,".rst", sep="")
  writeRaster(tmax_predicted, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
  
  ########
  # check: assessment of results: validation
  ########
  RMSE<-function(x,y) {return(mean((x-y)^2)^0.5)}
  MAE_fun<-function(x,y) {return(mean(abs(x-y)))}
  #ME_fun<-function(x,y){return(mean(abs(y)))}
  #FIT ASSESSMENT
  sta_pred_data_s=lookup(tmax_predicted,data_s$lat,data_s$lon)
  rmse_fit=RMSE(sta_pred_data_s,data_s$dailyTmax)
  mae_fit=MAE_fun(sta_pred_data_s,data_s$dailyTmax)
    
  sta_pred=lookup(tmax_predicted,data_v$lat,data_v$lon)
  #sta_pred=lookup(tmax_predicted,daily_sta_lola$lat,daily_sta_lola$lon)
  #rmse=RMSE(sta_pred,dmoday$dailyTmax)
  #pos<-match("value",names(data_v)) #Find column with name "value"
  #names(data_v)[pos]<-c("dailyTmax")
  tmax<-data_v$dailyTmax
  #data_v$dailyTmax<-tmax
  rmse=RMSE(sta_pred,tmax)
  mae<-MAE_fun(sta_pred,tmax)
  r2<-cor(sta_pred,tmax)^2              #R2, coef. of var
  me<-mean(sta_pred-tmax)
 
  #plot(sta_pred~dmoday$dailyTmax,xlab=paste("Actual daily for",datelabel),ylab="Pred daily",main=paste("RMSE=",rmse))
  
  png(paste("Predicted_tmax_versus_observed_scatterplot_",dates[i],out_prefix,".png", sep=""))
  plot(sta_pred~tmax,xlab=paste("Actual daily for",datelabel),ylab="Pred daily",main=paste("RMSE=",rmse))
  abline(0,1)
  #savePlot(paste("Predicted_tmax_versus_observed_scatterplot_",dates[i],out_prefix,".png", sep=""), type="png")
  dev.off()
  #resid=sta_pred-dmoday$dailyTmax
  resid=sta_pred-tmax
  quilt.plot(daily_sta_lola,resid)
  
  ### END OF BRIAN's code
  
  ### Added by benoit
  #Store results using TPS
#   j=9
#   results_RMSE[i,1]<- dates[i]    #storing the interpolation dates in the first column
#   results_RMSE[i,2]<- ns          #number of stations used in the training stage
#   results_RMSE[i,3]<- "RMSE"
#   results_RMSE[i,j+3]<- rmse  #Storing RMSE for the model j
#   
#   results_RMSE_f[i,1]<- dates[i]    #storing the interpolation dates in the first column
#   results_RMSE_f[i,2]<- ns          #number of stations used in the training stage
#   results_RMSE_f[i,3]<- "RMSE"
#   results_RMSE_f[i,j+3]<- rmse_fit  #Storing RMSE for the model j
#   
  ns<-nrow(data_s) #This is added to because some loss of data might have happened because of the averaging...
  nv<-nrow(data_v)       
  
  ### Added by benoit
  #Store results using TPS
  j=9
  results_RMSE[1]<- dates[i]    #storing the interpolation dates in the first column
  results_RMSE[2]<- ns          #number of stations used in the training stage
  results_RMSE[3]<- "RMSE"
  results_RMSE[j+3]<- rmse  #Storing RMSE for the model j
  
  results_RMSE_f[1]<- dates[i]    #storing the interpolation dates in the first column
  results_RMSE_f[2]<- ns          #number of stations used in the training stage
  results_RMSE_f[3]<- "RMSE_f"
  results_RMSE_f[j+3]<- rmse_fit  #Storing RMSE for the model j
  
  results_MAE_f[1]<- dates[i]    #storing the interpolation dates in the first column
  results_MAE_f[2]<- ns          #number of stations used in the training stage
  results_MAE_f[3]<- "RMSE_f"
  results_MAE_f[j+3]<- mae_fit  #Storing RMSE for the model j

  results_MAE[1]<- dates[i]    #storing the interpolation dates in the first column
  results_MAE[2]<- ns          #number of stations used in the training stage
  results_MAE[3]<- "MAE"
  results_MAE[j+3]<- mae  #Storing RMSE for the model j

  results_ME[1]<- dates[i]    #storing the interpolation dates in the first column
  results_ME[2]<- ns          #number of stations used in the training stage
  results_ME[3]<- "ME"
  results_ME[j+3]<- me  #Storing RMSE for the model j
  
  results_R2[1]<- dates[i]    #storing the interpolation dates in the first column
  results_R2[2]<- ns          #number of stations used in the training stage
  results_R2[3]<- "R2"
  results_R2[j+3]<- r2  #Storing RMSE for the model j
  
  #ns<-nrow(data_s) #This is added to because some loss of data might have happened because of the averaging...
  #nv<-nrow(data_v)
  
  for (j in 1:length(models)){
    
    ##Model assessment: specific diagnostic/metrics for GAM
    
    name<-paste("mod",j,sep="")  #modj is the name of The "j" model (mod1 if j=1) 
    mod<-get(name)               #accessing GAM model ojbect "j"
    
    #If mod "j" is not a model object
    if (inherits(mod,"try-error")) {
      results_AIC[1]<- dates[i]  #storing the interpolation dates in the first column
      results_AIC[2]<- ns        #number of stations used in the training stage
      results_AIC[3]<- "AIC"
      results_AIC[j+3]<- NA
      
      results_GCV[1]<- dates[i]  #storing the interpolation dates in the first column
      results_GCV[2]<- ns        #number of stations used in the training 
      results_GCV[3]<- "GCV"
      results_GCV[j+3]<- NA
      
      results_DEV[1]<- dates[i]  #storing the interpolation dates in the first column
      results_DEV[2]<- ns        #number of stations used in the training stage
      results_DEV[3]<- "DEV"
      results_DEV[j+3]<- NA
      
      results_RMSE_f[1]<- dates[i]  #storing the interpolation dates in the first column
      results_RMSE_f[2]<- ns        #number of stations used in the training stage
      results_RMSE_f[3]<- "RSME_f"
      results_RMSE_f[j+3]<- NA
      
      results_MAE_f[1]<- dates[i]  #storing the interpolation dates in the first column
      results_MAE_f[2]<- ns        #number of stations used in the training stage
      results_MAE_f[3]<- "MAE_f"
      results_MAE_f[j+3]<-NA
      
      results_RMSE[1]<- dates[i]    #storing the interpolation dates in the first column
      results_RMSE[2]<- ns          #number of stations used in the training stage
      results_RMSE[3]<- "RMSE"
      results_RMSE[j+3]<- NA  #Storing RMSE for the model j
      results_MAE[1]<- dates[i]     #storing the interpolation dates in the first column
      results_MAE[2]<- ns           #number of stations used in the training stage
      results_MAE[3]<- "MAE"
      results_MAE[j+3]<- NA    #Storing MAE for the model j
      results_ME[1]<- dates[i]      #storing the interpolation dates in the first column
      results_ME[2]<- ns            #number of stations used in the training stage
      results_ME[3]<- "ME"
      results_ME[j+3]<- NA      #Storing ME for the model j
      results_R2[1]<- dates[i]      #storing the interpolation dates in the first column
      results_R2[2]<- ns            #number of stations used in the training stage
      results_R2[3]<- "R2"
      results_R2[j+3]<- NA      #Storing R2 for the model j
      
    }
    
    #If mod is a modelobject
    
    #If mod "j" is not a model object
    if (inherits(mod,"gam")) {
      
      results_AIC[1]<- dates[i]  #storing the interpolation dates in the first column
      results_AIC[2]<- ns        #number of stations used in the training stage
      results_AIC[3]<- "AIC"
      results_AIC[j+3]<- AIC (mod)
      
      results_GCV[1]<- dates[i]  #storing the interpolation dates in the first column
      results_GCV[2]<- ns        #number of stations used in the training 
      results_GCV[3]<- "GCV"
      results_GCV[j+3]<- mod$gcv.ubre
      
      results_DEV[1]<- dates[i]  #storing the interpolation dates in the first column
      results_DEV[2]<- ns        #number of stations used in the training stage
      results_DEV[3]<- "DEV"
      results_DEV[j+3]<- mod$deviance
      
      sta_LST_s=lookup(themolst,data_s$lat,data_s$lon)
      sta_delta_s=lookup(daily_delta_rast,data_s$lat,data_s$lon) #delta surface has been calculated before!!
      sta_bias_s= mod$fit
      #Need to extract values from the kriged delta surface...
      #sta_delta= lookup(delta_surface,data_v$lat,data_v$lon)
      #tmax_predicted=sta_LST+sta_bias-y_mod$fit
      tmax_predicted_s= sta_LST_s-sta_bias_s+sta_delta_s
      
      results_RMSE_f[1]<- dates[i]  #storing the interpolation dates in the first column
      results_RMSE_f[2]<- ns        #number of stations used in the training stage
      results_RMSE_f[3]<- "RSME_f"
      results_RMSE_f[j+3]<- sqrt(sum((tmax_predicted_s-data_s$dailyTmax)^2)/ns)
      
      results_MAE_f[1]<- dates[i]  #storing the interpolation dates in the first column
      results_MAE_f[2]<- ns        #number of stations used in the training stage
      results_MAE_f[3]<- "MAE_f"
      results_MAE_f[j+3]<-sum(abs(tmax_predicted_s-data_s$dailyTmax))/ns
      
      ##Model assessment: general diagnostic/metrics
      ##validation: using the testing data
      
      #data_v$y_var<-data_v$LSTD_bias
      #data_v$y_var<-tmax
      y_mod<- predict(mod, newdata=data_v, se.fit = TRUE) #Using the coeff to predict new values.
      
      ####ADDED ON JULY 5th
      sta_LST_v=lookup(themolst,data_v$lat,data_v$lon)
      sta_delta_v=lookup(daily_delta_rast,data_v$lat,data_v$lon) #delta surface has been calculated before!!
      sta_bias_v= y_mod$fit
      #Need to extract values from the kriged delta surface...
      #sta_delta= lookup(delta_surface,data_v$lat,data_v$lon)
      #tmax_predicted=sta_LST+sta_bias-y_mod$fit
      tmax_predicted_v= sta_LST_v-sta_bias_v+sta_delta_v
      
      #data_v$tmax<-(data_v$tmax)/10
      res_mod<- data_v$dailyTmax - tmax_predicted_v              #Residuals for the model for fusion
      #res_mod<- data_v$y_var - y_mod$fit                  #Residuals for the model
      
      RMSE_mod <- sqrt(sum(res_mod^2)/nv)                 #RMSE FOR REGRESSION STEP 1: GAM     
      MAE_mod<- sum(abs(res_mod))/nv                     #MAE, Mean abs. Error FOR REGRESSION STEP 1: GAM   
      ME_mod<- sum(res_mod)/nv                            #ME, Mean Error or bias FOR REGRESSION STEP 1: GAM
      R2_mod<- cor(data_v$dailyTmax,tmax_predicted_v)^2              #R2, coef. of var FOR REGRESSION STEP 1: GAM
      
      results_RMSE[1]<- dates[i]    #storing the interpolation dates in the first column
      results_RMSE[2]<- ns          #number of stations used in the training stage
      results_RMSE[3]<- "RMSE"
      results_RMSE[j+3]<- RMSE_mod  #Storing RMSE for the model j
      results_MAE[1]<- dates[i]     #storing the interpolation dates in the first column
      results_MAE[2]<- ns           #number of stations used in the training stage
      results_MAE[3]<- "MAE"
      results_MAE[j+3]<- MAE_mod    #Storing MAE for the model j
      results_ME[1]<- dates[i]      #storing the interpolation dates in the first column
      results_ME[2]<- ns            #number of stations used in the training stage
      results_ME[3]<- "ME"
      results_ME[j+3]<- ME_mod      #Storing ME for the model j
      results_R2[1]<- dates[i]      #storing the interpolation dates in the first column
      results_R2[2]<- ns            #number of stations used in the training stage
      results_R2[3]<- "R2"
      results_R2[j+3]<- R2_mod      #Storing R2 for the model j
      
      #Saving residuals and prediction in the dataframes: tmax predicted from GAM
      pred<-paste("pred_mod",j,sep="")
      #data_v[[pred]]<-as.numeric(y_mod$fit)
      data_v[[pred]]<-as.numeric(tmax_predicted_v)
      data_s[[pred]]<-as.numeric(tmax_predicted_s) #Storing model fit values (predicted on training sample)
      #data_s[[pred]]<-as.numeric(mod$fit) #Storing model fit values (predicted on training sample)
      
      name2<-paste("res_mod",j,sep="")
      data_v[[name2]]<-as.numeric(res_mod)
      temp<-tmax_predicted_s-data_s$dailyTmax
      data_s[[name2]]<-as.numeric(temp)
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
  
  results_table_AIC<-as.data.frame(results_AIC)
  results_table_GCV<-as.data.frame(results_GCV)
  results_table_DEV<-as.data.frame(results_DEV)
  
  tb_metrics1<-rbind(results_table_RMSE,results_table_MAE, results_table_ME, results_table_R2,results_table_RMSE_f,results_table_MAE_f)   #
  tb_metrics2<-rbind(results_table_AIC,results_table_GCV, results_table_DEV)
  cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7","mod8","mod9")
  colnames(tb_metrics1)<-cname
  cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7","mod8")
  colnames(tb_metrics2)<-cname
  #colnames(results_table_RMSE)<-cname
  #colnames(results_table_RMSE_f)<-cname
  #tb_diagnostic1<-results_table_RMSE      #measures of validation
  #tb_diagnostic2<-results_table_RMSE_f    #measures of fit
  
  #write.table(tb_diagnostic1, file= paste(path,"/","results_fusion_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
  
  #}  
  print(paste(dates[i],"processed"))
  mod_obj<-list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9a,mod9b)
  # end of the for loop1
  #results_list<-list(data_s,data_v,tb_metrics1,tb_metrics2)
  results_list<-list(data_s,data_v,tb_metrics1,tb_metrics2,mod_obj)
  return(results_list)
  #return(tb_diagnostic1)
}
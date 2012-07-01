##################    CLIMATE INTERPOLATION FUSION METHOD   #######################################
############################ Merging LST and station data ##########################################
#This script interpolates tmax values using MODIS LST and GHCND station data                     #
#interpolation area. It requires the text file of stations and a shape file of the study area.   #       
#Note that the projection for both GHCND and study area is lonlat WGS84.                         #
#AUTHOR: Brian McGill                                                                            #
#DATE: 06/19/212                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--                                  #
###################################################################################################

###Loading R library and packages                                                      
library(gtools)                                         # loading some useful tools 
library(mgcv)                                           # GAM package by Simon Wood
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                              # NCAR Spatial Interpolation methods such as kriging, splines
library(raster)                              # Hijmans et al. package for raster processing
### Parameters and argument

infile1<- "ghcn_or_tmax_covariates_06262012_OR83M.shp"             #GHCN shapefile containing variables for modeling 2010                 
infile2<-"list_10_dates_04212012.txt"                     #List of 10 dates for the regression
#infile2<-"list_365_dates_04212012.txt"
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       #Raster or grid for the locations of predictions

#path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"
path<-"M:/Data/IPLANT_project/data_Oregon_stations"   #Locations on Atlas

#Station location of the study area
stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)
#GHCN Database for 1980-2010 for study area (OR) 
data3<-read.table(paste(path,"/","ghcn_data_TMAXy1980_2010_OR_0602012.txt",sep=""),sep=",", header=TRUE)

prop<-0.3                                                                           #Proportion of testing retained for validation   
seed_number<- 100                                                                   #Seed number for random sampling
out_prefix<-"_07022012_10d_fusion8"                                                   #User defined output prefix
setwd(path)
############ START OF THE SCRIPT ##################

#
### Step 0/Step 6 in Brian's code...preparing year 2010 data for modeling 
#


###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)

mean_LST<- readGDAL(infile5)                 #Reading the whole raster in memory. This provides a grid for kriging
proj4string(mean_LST)<-CRS                   #Assigning coordinate information to prediction grid.

ghcn = transform(ghcn,Northness = cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness = sin(ASPECT))  #adding variable to the dataframe.
ghcn = transform(ghcn,Northness_w = sin(slope)*cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness_w = sin(slope)*sin(ASPECT))  #adding variable to the dataframe.

set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...

dates <-readLines(paste(path,"/",infile2, sep=""))
LST_dates <-readLines(paste(path,"/",infile3, sep=""))
models <-readLines(paste(path,"/",infile4, sep=""))

#Model assessment: specific diagnostic/metrics for GAM
results_AIC<- matrix(1,length(dates),length(models)+3)  
results_GCV<- matrix(1,length(dates),length(models)+3)
results_DEV<- matrix(1,length(dates),length(models)+3)
results_RMSE_f<- matrix(1,length(dates),length(models)+3)

#Model assessment: general diagnostic/metrics 
results_RMSE <- matrix(1,length(dates),length(models)+4)
results_MAE <- matrix(1,length(dates),length(models)+4)
results_ME <- matrix(1,length(dates),length(models)+4)       #There are 8+1 models
results_R2 <- matrix(1,length(dates),length(models)+4)       #Coef. of determination for the validation dataset
results_RMSE_f<- matrix(1,length(dates),length(models)+4)    #RMSE fit, RMSE for the training dataset
results_RMSE_f_kr<- matrix(1,length(dates),length(models)+4)

# #Tracking relationship between LST AND LC
# cor_LST_LC1<-matrix(1,10,1)      #correlation LST-LC1
# cor_LST_LC3<-matrix(1,10,1)      #correlation LST-LC3
# cor_LST_tmax<-matrix(1,10,1)     #correlation LST-tmax

#Screening for bad values: element is tmax in this case
ghcn$element<-as.numeric(ghcn$element)
ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$element>-150 & ghcn$element<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

month_var<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09", "mm_10", "mm_11", "mm_12")
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 or 365 subsets dataset based on dates


#Start loop here...

## looping through the dates...this is the main part of the code
#i=1 #for debugging
#j=1 #for debugging
for(i in 1:length(dates)){            # start of the for loop #1
  
  date<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  
  mod_LST <-ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]  #Match interpolation date and monthly LST average
  ghcn.subsets[[i]] = transform(ghcn.subsets[[i]],LST = mod_LST)            #Add the variable LST to the subset dataset
  n<-nrow(ghcn.subsets[[i]])
  ns<-n-round(n*prop)   #Create a sample from the data frame with 70% of the rows
  nv<-n-ns              #create a sample for validation with prop of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
  data_s <- ghcn.subsets[[i]][ind.training, ]   #Training dataset currently used in the modeling
  data_v <- ghcn.subsets[[i]][ind.testing, ]    #Testing/validation dataset
  
  #i=1
  date_proc<-dates[i]
  date_proc<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  day<-as.integer(strftime(date_proc, "%d"))
  year<-as.integer(strftime(date_proc, "%Y"))
  
  #setup
  
  #mo=9           #Commented out by Benoit on June 14
  #day=2
  #year=2010
  datelabel=format(ISOdate(year,mo,day),"%b %d, %Y")
  
  ###########
  #  STEP 1 - 10 year monthly averages
  ###########
  
  #library(raster)
  #old<-getwd()
  #setwd("c:/data/benoit/data_Oregon_stations_Brian_04242012")
  #l=list.files(pattern="mean_month.*rescaled.tif")
  l=list.files(pattern="mean_month.*rescaled.rst")
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
  
  # ??? which years & what quality flags???
  #select ghcn.id, lat,lon, elev, month, avg(value/10) as "TMax", count(*) as "NumDays" from ghcn, stations where ghcn.id in (select id from stations where state=='OR') and ghcn.id==stations.id and value<>-9999 and year>=2000 and  element=='TMAX' group by stations.id, month;select ghcn.id, lat,lon, elev, month, avg(value/10) as "TMax", count(*) as "NumDays" from ghcn, stations where ghcn.id in (select id from stations where state=='OR') and ghcn.id==stations.id and value<>-9999 and year>=2000 and  element=='TMAX' group by stations.id, month;
  #below table from above SQL query
  #dst<-read.csv('/data/benoit/data_oregon_stations_brian_04242012/station_means.csv',h=T)
  
  ##Added by Benoit ######
  date1<-ISOdate(data3$year,data3$month,data3$day) #Creating a date object from 3 separate column
  date2<-as.POSIXlt(as.Date(date1))
  data3$date<-date2
  d<-subset(data3,year>=2000 & mflag=="0" ) #Selecting dataset 2000-2010 with good quality
  d1<-aggregate(value~station+month, data=d, mean)  #Calculate monthly mean for every station in OR
  id<-as.data.frame(unique(d1$station))     #Unique station in OR for year 2000-2010      
  
  dst<-merge(d1, stat_loc, by.x="station", by.y="STAT_ID")
  #This allows to change only one name of the data.frame
  names(dst)[3]<-c("TMax")
  dst$TMax<-dst$TMax/10                #TMax is the average max temp for months
  #dstjan=dst[dst$month==9,]  #dst contains the monthly averages for tmax for every station over 2000-2010
  ##############
  
  modst=dst[dst$month==mo,] #Subsetting dataset for the relevnat month of the date being processed
  
  ##########
  # STEP 3 - get LST at stations
  ##########
  
  sta_lola=modst[,c("lon","lat")] #Extracting locations of stations for the current month...
  library(rgdal)
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
  
  bias_xy=project(as.matrix(sta_lola),proj_str)
  # windows()
  X11()
  plot(modst$TMax,sta_tmax_from_lst,xlab="Station mo Tmax",ylab="LST mo Tmax",main=paste("LST vs TMax for",datelabel,sep=" "))
  abline(0,1)
  savePlot(paste("LST_TMax_scatterplot_",dates[i],out_prefix,".png", sep=""), type="png")
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
  fitbias<-Krig(bias_xy,sta_bias,theta=1e5) #use TPS or krige 
                                            #The output is a krig object using fields
  # Creating plot of bias surface and saving it
  X11()
  datelabel2=format(ISOdate(year,mo,day),"%B ") #added by Benoit, label
  surface(fitbias,col=rev(terrain.colors(100)),asp=1,main=paste("Interpolated bias for",datelabel2,sep=" "))
  savePlot(paste("Bias_surface_LST_TMax_",dates[i],out_prefix,".png", sep=""), type="png")
  dev.off()
  
  #US(add=T,col="magenta",lwd=2)
  
  ##########
  # STEP 6 - return to daily station data & calcualate delta=daily T-monthly T from stations
  ##########
  
  #Commmented out by Benoit 06/14
  # library(RSQLite)
  # m<-dbDriver("SQLite")
  # con<-dbConnect(m,dbname='c:/data/ghcn_tmintmax.db')
  # querystr=paste("select ghcn.id, value as 'dailyTmax' from ghcn where ghcn.id in (select id from stations where state=='OR') and value<>-9999",
  #    "and year==",year,"and month==",mo,"and day==",day,
  #                "and element=='TMAX' ")
  # rs<-dbSendQuery(con,querystr)
  # d<-fetch(rs,n=-1)
  # dbClearResult(rs)
  # dbDisconnect(con)
  # 
  # d$dailyTmax=d$dailyTmax/10 #stored as 1/10 degree C to allow integer storage
  # dmoday=merge(modst,d,by="id")
  ##########################Commented out by Benoit
  
  #added by Benoit 
  #d<-ghcn.subsets[[i]]
  d<-data_s
  names(d)[5]<-c("dailyTmax")
  d$dailyTmax=(as.numeric(d$dailyTmax))/10 #stored as 1/10 degree C to allow integer storage
  names(d)[1]<-c("id")
  names(modst)[1]<-c("id")       #modst contains the average tmax per month for every stations...
  dmoday=merge(modst,d,by="id")  #LOOSING DATA HERE!!! from 162 t0 146
  names(dmoday)[4]<-c("lat")
  names(dmoday)[5]<-c("lon")
  ###
  
  #dmoday contains the daily tmax values with TMax being the monthly station tmax mean
  
  # windows()
  X11()
  plot(dailyTmax~TMax,data=dmoday,xlab="Mo Tmax",ylab=paste("Daily for",datelabel),main="across stations in OR")
  savePlot(paste("Daily_tmax_monthly_TMax_scatterplot_",dates[i],out_prefix,".png", sep=""), type="png")
  dev.off()
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
 
  # Creating plot of bias surface and saving it
  X11()
  surface(fitdelta,col=rev(terrain.colors(100)),asp=1,main=paste("Interpolated delta for",datelabel,sep=" "))
  savePlot(paste("Delta_surface_LST_TMax_",dates[i],out_prefix,".png", sep=""), type="png")
  dev.off()
  #US(add=T,col="magenta",lwd=2)
  #
  
  #### Added by Benoit on 06/19
  data_s<-dmoday #put the 
  data_s$daily_delta<-daily_delta
  #data_s$y_var<-daily_delta  #y_var is the variable currently being modeled, may be better with BIAS!!
  data_s$y_var<-data_s$dailyTmax
  #Model and response variable can be changed without affecting the script
  
  mod1<- gam(y_var~ s(lat) + s (lon) + s (ELEV_SRTM), data=data_s)
  mod2<- gam(y_var~ s(lat,lon)+ s(ELEV_SRTM), data=data_s) #modified nesting....from 3 to 2
  mod3<- gam(y_var~ s(lat) + s (lon) + s (ELEV_SRTM) +  s (Northness)+ s (Eastness) + s(DISTOC), data=data_s)
  mod4<- gam(y_var~ s(lat) + s (lon) + s(ELEV_SRTM) + s(Northness) + s (Eastness) + s(DISTOC) + s(LST), data=data_s)
  mod5<- gam(y_var~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST), data=data_s)
  mod6<- gam(y_var~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST)+s(LC1), data=data_s)
  mod7<- gam(y_var~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST)+s(LC3), data=data_s)
  mod8<- gam(y_var~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST) + s(LC1), data=data_s)
  #### Added by Benoit ends

  #########
  # STEP 8 - assemble final answer - T=LST+Bias(interpolated)+delta(interpolated)
  #########
  
  bias_rast=interpolate(themolst,fitbias) #interpolation using function from raster package
                                          #themolst is raster layer, fitbias is "Krig" object from bias surface
  plot(bias_rast,main="Raster bias") #This not displaying...
  
  daily_delta_rast=interpolate(themolst,fitdelta) #Interpolation of the bias surface...
  
  plot(daily_delta_rast,main="Raster Daily Delta")
  
  tmax_predicted=themolst+daily_delta_rast-bias_rast #Final surface?? but daily_rst
  #tmax_predicted=themolst+daily_delta_rast+bias_rast #Added by Benoit, why is it -bias_rast
  plot(tmax_predicted,main="Predicted daily")
  
  ########
  # check: assessment of results: validation
  ########
  RMSE<-function(x,y) {return(mean((x-y)^2)^0.5)}
  
  #FIT ASSESSMENT
  sta_pred_data_s=lookup(tmax_predicted,data_s$lat,data_s$lon)
  rmse_fit=RMSE(sta_pred_data_s,data_s$dailyTmax)
  
  sta_pred=lookup(tmax_predicted,data_v$lat,data_v$lon)
  #sta_pred=lookup(tmax_predicted,daily_sta_lola$lat,daily_sta_lola$lon)
  #rmse=RMSE(sta_pred,dmoday$dailyTmax)
  tmax<-data_v$tmax/10
  rmse=RMSE(sta_pred,tmax)
  #plot(sta_pred~dmoday$dailyTmax,xlab=paste("Actual daily for",datelabel),ylab="Pred daily",main=paste("RMSE=",rmse))
  X11()
  plot(sta_pred~tmax,xlab=paste("Actual daily for",datelabel),ylab="Pred daily",main=paste("RMSE=",rmse))
  abline(0,1)
  savePlot(paste("Predicted_tmax_versus_observed_scatterplot_",dates[i],out_prefix,".png", sep=""), type="png")
  dev.off()
  #resid=sta_pred-dmoday$dailyTmax
  resid=sta_pred-tmax
  quilt.plot(daily_sta_lola,resid)
  
  ### END OF BRIAN's code
  
  ### Added by benoit
  #Store results using TPS
  j=9
  results_RMSE[i,1]<- dates[i]    #storing the interpolation dates in the first column
  results_RMSE[i,2]<- ns          #number of stations used in the training stage
  results_RMSE[i,3]<- "RMSE"
  results_RMSE[i,j+3]<- rmse  #Storing RMSE for the model j
  
  results_RMSE_f[i,1]<- dates[i]    #storing the interpolation dates in the first column
  results_RMSE_f[i,2]<- ns          #number of stations used in the training stage
  results_RMSE_f[i,3]<- "RMSE"
  results_RMSE_f[i,j+3]<- rmse_fit  #Storing RMSE for the model j
  
  ns<-nrow(data_s)
           
  
  for (j in 1:length(models)){
    
    ##Model assessment: specific diagnostic/metrics for GAM
    
    name<-paste("mod",j,sep="")  #modj is the name of The "j" model (mod1 if j=1) 
    mod<-get(name)               #accessing GAM model ojbect "j"
    results_AIC[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_AIC[i,2]<- ns        #number of stations used in the training stage
    results_AIC[i,3]<- "AIC"
    results_AIC[i,j+3]<- AIC (mod)
    
    results_GCV[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_GCV[i,2]<- ns        #number of stations used in the training 
    results_GCV[i,3]<- "GCV"
    results_GCV[i,j+3]<- mod$gcv.ubre
    
    results_DEV[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_DEV[i,2]<- ns        #number of stations used in the training stage
    results_DEV[i,3]<- "DEV"
    results_DEV[i,j+3]<- mod$deviance
    
    results_RMSE_f[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_RMSE_f[i,2]<- ns        #number of stations used in the training stage
    results_RMSE_f[i,3]<- "RSME"
    results_RMSE_f[i,j+3]<- sqrt(sum((mod$residuals)^2)/nv)
    
    ##Model assessment: general diagnostic/metrics
    ##validation: using the testing data
    
    #This was modified on 06192012
    
    #data_v$y_var<-data_v$tmax/10
    data_v$y_var<-tmax
    y_mod<- predict(mod, newdata=data_v, se.fit = TRUE) #Using the coeff to predict new values.
    
    #sta_LST=lookup(themolst,data_v$lat,data_v$lon)
    #sta_bias=lookup(bias_rast,data_v$lat,data_v$lon)
    #tmax_predicted=sta_LST+sta_bias-y_mod$fit
    
    #data_v$tmax<-(data_v$tmax)/10
    #res_mod<- data_v$tmax - tmax_predicted              #Residuals for the model for fusion
    res_mod<- data_v$y_var - y_mod$fit                  #Residuals for the model
    
    RMSE_mod <- sqrt(sum(res_mod^2)/nv)                 #RMSE FOR REGRESSION STEP 1: GAM     
    MAE_mod<- sum(abs(res_mod))/nv                     #MAE, Mean abs. Error FOR REGRESSION STEP 1: GAM   
    ME_mod<- sum(res_mod)/nv                            #ME, Mean Error or bias FOR REGRESSION STEP 1: GAM
    R2_mod<- cor(data_v$tmax,y_mod$fit)^2              #R2, coef. of var FOR REGRESSION STEP 1: GAM
    
    results_RMSE[i,1]<- dates[i]    #storing the interpolation dates in the first column
    results_RMSE[i,2]<- ns          #number of stations used in the training stage
    results_RMSE[i,3]<- "RMSE"
    results_RMSE[i,j+3]<- RMSE_mod  #Storing RMSE for the model j
    results_MAE[i,1]<- dates[i]     #storing the interpolation dates in the first column
    results_MAE[i,2]<- ns           #number of stations used in the training stage
    results_MAE[i,3]<- "MAE"
    results_MAE[i,j+3]<- MAE_mod    #Storing MAE for the model j
    results_ME[i,1]<- dates[i]      #storing the interpolation dates in the first column
    results_ME[i,2]<- ns            #number of stations used in the training stage
    results_ME[i,3]<- "ME"
    results_ME[i,j+3]<- ME_mod      #Storing ME for the model j
    results_R2[i,1]<- dates[i]      #storing the interpolation dates in the first column
    results_R2[i,2]<- ns            #number of stations used in the training stage
    results_R2[i,3]<- "R2"
    results_R2[i,j+3]<- R2_mod      #Storing R2 for the model j
    
    #Saving residuals and prediction in the dataframes: tmax predicted from GAM
    pred<-paste("pred_mod",j,sep="")
    data_v[[pred]]<-as.numeric(y_mod$fit)
    data_s[[pred]]<-as.numeric(mod$fit) #Storing model fit values (predicted on training sample)
    
    name2<-paste("res_mod",j,sep="")
    data_v[[name2]]<-as.numeric(res_mod)
    data_s[[name2]]<-as.numeric(mod$residuals)
    #end of loop calculating RMSE
    
  }

  # end of the for loop1
  
}


## Plotting and saving diagnostic measures

#Specific diagnostic measures related to the testing datasets

results_table_RMSE<-as.data.frame(results_RMSE)
results_table_MAE<-as.data.frame(results_MAE)
results_table_ME<-as.data.frame(results_ME)
results_table_R2<-as.data.frame(results_R2)

results_table_RMSE_f<-as.data.frame(results_RMSE_f)

cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7","mod8","mod9")
colnames(results_table_RMSE)<-cname
colnames(results_table_RMSE_f)<-cname

#tb_diagnostic1<-rbind(results_table_RMSE,results_table_MAE, results_table_ME, results_table_R2)   #
tb_diagnostic1<-results_table_RMSE      #measures of validation
tb_diagnostic2<-results_table_RMSE_f    #measures of fit

write.table(tb_diagnostic1, file= paste(path,"/","results_fusion_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
write.table(tb_diagnostic2, file= paste(path,"/","results_fusion_Assessment_measure2",out_prefix,".txt",sep=""), sep=",")

#### END OF SCRIPT
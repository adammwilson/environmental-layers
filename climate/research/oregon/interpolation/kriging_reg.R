##################    Interpolation of Tmax Using Kriging  #######################################
########################### Kriging and Cokriging   ###############################################
#This script interpolates station values for the Oregon case study using Kriging and Cokring.    #
#The script uses LST monthly averages as input variables and  loads the station data             # 
#from a shape file with projection information.                                                  #
#Note that this program:                                                                         #
#1)assumes that the shape file is in the current working.                                        # 
#2)relevant variables were extracted from raster images before performing the regressions        #
#  and stored shapefile                                                                          #
#This scripts predicts tmax using autokrige, gstat and LST derived from MOD11A1.                 #
#also included and assessed using the RMSE,MAE,ME and R2 from validation dataset.                #
#TThe dates must be provided as a textfile.                                                      #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 07/07/2012                                                                                #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#364--                                  #
##################################################################################################

###Loading R library and packages                                                      
#library(gtools)                                         # loading some useful tools 
library(mgcv)                                           # GAM package by Wood 2006 (version 2012)
library(sp)                                             # Spatial pacakge with class definition by Bivand et al. 2008
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al. 2012
library(rgdal)                                          # GDAL wrapper for R, spatial utilities (Keitt et al. 2012)
library(gstat)                                          # Kriging and co-kriging by Pebesma et al. 2004
library(automap)                                        # Automated Kriging based on gstat module by Hiemstra et al. 2008
library(spgwr)
library(gpclib)
library(maptools)
library(graphics)

###Parameters and arguments

infile1<- "ghcn_or_tmax_covariates_06262012_OR83M.shp"             #GHCN shapefile containing variables for modeling 2010                 
infile2<-"list_10_dates_04212012.txt"                     #List of 10 dates for the regression
#infile2<-"list_365_dates_04212012.txt"
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"

# infile1<- "ghcn_or_tmax_b_04142012_OR83M.shp"             #GHCN shapefile containing variables                  
# infile2<-"list_10_dates_04212012.txt"                      #List of 10 dates for the regression
# #infile2<-"list_365_dates_04212012.txt"
# infile3<-"mean_day244_rescaled.rst"          #This image serves as the reference grid for kriging
# infile4<- "orcnty24_OR83M.shp"               #Vector file defining the study area: Oregon state and its counties.

path<-"/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations_07152012"         #Jupiter LOCATION on EOS
#path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"                 #Jupiter LOCATION on EOS/Atlas
#path<-"H:/Data/IPLANT_project/data_Oregon_stations"                                 #Jupiter Location on XANDERS
setwd(path) 
prop<-0.3                                                                       #Proportion of testing retained for validation   
seed_number<- 100                                                               #Seed number for random sampling
models<-5                                                                       #Number of kriging model
out_prefix<-"_07132012_auto_krig_"                                              #User defined output prefix

###STEP 1 DATA PREPARATION AND PROCESSING#####

###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)

mean_LST<- readGDAL(infile5)                 #Reading the whole raster in memory. This provides a grid for kriging
proj4string(mean_LST)<-CRS                   #Assigning coordinate information to prediction grid.

ghcn = transform(ghcn,Northness = cos(ASPECT*pi/180)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness = sin(ASPECT*pi/180))  #adding variable to the dataframe.
ghcn = transform(ghcn,Northness_w = sin(slope*pi/180)*cos(ASPECT*pi/180)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness_w = sin(slope*pi/180)*sin(ASPECT*pi/180))  #adding variable to the dataframe.

#Remove NA for LC and CANHEIGHT
ghcn$LC1[is.na(ghcn$LC1)]<-0
ghcn$LC3[is.na(ghcn$LC3)]<-0
ghcn$CANHEIGHT[is.na(ghcn$CANHEIGHT)]<-0

set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...

dates <-readLines(paste(path,"/",infile2, sep=""))
LST_dates <-readLines(paste(path,"/",infile3, sep=""))
models <-readLines(paste(path,"/",infile4, sep=""))
#Model assessment: specific diagnostic/metrics for GAM
results_AIC<- matrix(1,length(dates),models+3)  
results_GCV<- matrix(1,length(dates),models+3)

#Model assessment: general diagnostic/metrics 
results_RMSE <- matrix(1,length(dates),models+3)
results_MAE <- matrix(1,length(dates),models+3)
results_ME <- matrix(1,length(dates),models+3)
results_R2 <- matrix(1,length(dates),models+3)       #Coef. of determination for the validation dataset
results_RMSE_f<- matrix(1,length(dates),models+3)

###Reading the shapefile and raster image from the local directory

mean_LST<- readGDAL(infile3)                  #This reads the whole raster in memory and provide a grid for kriging in a SpatialGridDataFrame object
filename<-sub(".shp","",infile1)              #Removing the extension from file.
ghcn<-readOGR(".", filename)                  #Reading station locations from vector file using rgdal and creating a SpatialPointDataFrame
CRS_ghcn<-proj4string(ghcn)                   #This retrieves the coordinate system information for the SDF object (PROJ4 format)
proj4string(mean_LST)<-CRS_ghcn               #Assigning coordinates information to SpatialGridDataFrame object

# Adding variables for the regressions

ghcn$Northness<- cos(ghcn$ASPECT*pi/180)             #Adding a variable to the dataframe by calculating the cosine of Aspect
ghcn$Eastness <- sin(ghcn$ASPECT*pi/180)             #Adding variable to the dataframe.
ghcn$Northness_w <- sin(ghcn$slope*pi/180)*cos(ghcn$ASPECT*pi/180)  #Adding a variable to the dataframe
ghcn$Eastness_w  <- sin(ghcn$slope*pi/180)*sin(ghcn$ASPECT*pi/180)  #Adding variable to the dataframe.

set.seed(seed_number)                                 #This set a seed number for the random sampling to make results reproducible.

dates <-readLines(paste(path,"/",infile2, sep=""))  #Reading dates in a list from the textile.


#Screening for bad values and setting the valid range

ghcn_test<-subset(ghcn,ghcn$tmax>-150 & ghcn$tmax<400) #Values are in tenth of degrees C
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)    #No elevation below sea leve is allowed.
ghcn<-ghcn_test2

###CREATING SUBSETS BY INPUT DATES AND SAMPLING
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, ghcn$date==as.numeric(d)))   #Producing a list of data frame, one data frame per date.

for(i in 1:length(dates)){            # start of the for loop #1
#i<-3                                           #Date 10 is used to test kriging
  date<-strptime(dates[i], "%Y%m%d")
  month<-strftime(date, "%m")
  LST_month<-paste("mm_",month,sep="")
  mod <-ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]
  ghcn.subsets[[i]]$LST <-mod[[1]]
                   
  n<-nrow(ghcn.subsets[[i]])
  ns<-n-round(n*prop)                             #Create a sample from the data frame with 70% of the rows
  nv<-n-ns                                        #create a sample for validation with prop of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE)  #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)         #This selects the index position for testing subset stations.
  data_s <- ghcn.subsets[[i]][ind.training, ]
  data_v <- ghcn.subsets[[i]][ind.testing, ]
  
  ###STEP 2 KRIGING###
  
  #Kriging tmax
  
#   hscat(tmax~1,data_s,(0:9)*20000)                       # 9 lag classes with 20,000m width
#   v<-variogram(tmax~1, data_s)                           # This plots a sample varigram for date 10 fir the testing dataset
#   plot(v)
#   v.fit<-fit.variogram(v,vgm(2000,"Sph", 150000,1000))   #Model variogram: sill is 2000, spherical, range 15000 and nugget 1000
#   plot(v, v.fit)                                         #Compare model and sample variogram via a graphical plot
#   tmax_krige<-krige(tmax~1, data_s,mean_LST, v.fit)      #mean_LST provides the data grid/raster image for the kriging locations to be predicted.
  
  krmod1<-autoKrige(tmax~1, data_s,mean_LST,data_s) #Use autoKrige instead of krige: with data_s for fitting on a grid
  krmod2<-autoKrige(tmax~lat+lon,input_data=data_s,new_data=mean_LST,data_variogram=data_s)
  krmod2<-autoKrige(tmax~lat+lon,data_s,mean_LST, verbose=TRUE)
  
  krmod3<-autoKrige(tmax~LST, data_s,mean_LST,data_s)
  krmod4<-autoKrige(tmax~LST+ELEV_SRTM, data_s,mean_LST,data_s)
  krmod5<-autoKrige(tmax~LST+ELEV_SRTM+DISTOC, data_s,mean_LST,data_s)
  
  krig1<-krmod1$krige_output                   #Extracting Spatial Grid Data frame                    
  krig2<-krmod2$krige_output
  krig3<-krmod3$krige_outpu
  krig4<-krmod4$krige_output
  krig5<-krmod5$krige_output
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
    
    krmod<-paste("krig",j,sep="")
    
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
    R2_mod_kr_s<- cor(data_s$tmax,data_s[[gam_kr]],use="complete.obs")^2                  #R2, coef. of determination FOR REGRESSION STEP 1: GAM
    R2_mod_kr_v<- cor(data_v$tmax,data_v[[gam_kr]],use="complete.obs")^2                  #R2, coef. of determinationFOR REGRESSION STEP 1: GAM
    #(nv-sum(is.na(res_mod2)))
    #Writing out results
    
    results_RMSE[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_RMSE[i,2]<- ns        #number of stations used in the training stage
    results_RMSE[i,3]<- "RMSE"
    results_RMSE[i,j+3]<- RMSE_mod_kr_v
    #results_RMSE_kr[i,3]<- res_mod_kr_v
    
    results_MAE[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_MAE[i,2]<- ns        #number of stations used in the training stage
    results_MAE[i,3]<- "MAE"
    results_MAE[i,j+3]<- MAE_mod_kr_v
    #results_RMSE_kr[i,3]<- res_mod_kr_v
    
    results_ME[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_ME[i,2]<- ns        #number of stations used in the training stage
    results_ME[i,3]<- "ME"
    results_ME[i,j+3]<- ME_mod_kr_v
    #results_RMSE_kr[i,3]<- res_mod_kr_v
    
    results_R2[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_R2[i,2]<- ns        #number of stations used in the training stage
    results_R2[i,3]<- "R2"
    results_R2[i,j+3]<- R2_mod_kr_v
    #results_RMSE_kr[i,3]<- res_mod_kr_v
    
    name3<-paste("res_kr_mod",j,sep="")
    #as.numeric(res_mod)
    #data_s[[name3]]<-res_mod_kr_s
    data_s[[name3]]<-as.numeric(res_mod_kr_s)
    #data_v[[name3]]<-res_mod_kr_v 
    data_v[[name3]]<-as.numeric(res_mod_kr_v)
    #Writing residuals from kriging
    
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
  
  #Saving kriged surface in raster images
  
  #krig_raster_name<-paste("coKriged_tmax_",data_name,out_prefix,".tif", sep="")
  #writeGDAL(co_kriged_surf,fname=krig_raster_name, driver="GTiff", type="Float32",options ="INTERLEAVE=PIXEL")
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
#   
#   results[i,1]<- dates[i]  #storing the interpolation dates in the first column
#   results[i,2]<- ns     #number of stations in training
#   results[i,3]<- RMSE_mod1
#   results[i,4]<- RMSE_mod2  
#   
#   results_mod_n[i,1]<-dates[i]
#   results_mod_n[i,2]<-(nv-sum(is.na(res_mod1)))
#   results_mod_n[i,3]<-(nv-sum(is.na(res_mod2)))
  }

## Plotting and saving diagnostic measures
results_table_RMSE<-as.data.frame(results_RMSE)
results_table_MAE<-as.data.frame(results_MAE)
results_table_ME<-as.data.frame(results_ME)
results_table_R2<-as.data.frame(results_R2)

cname<-c("dates","ns","metric","krmod1", "krmod2","krmod3", "krmod4", "mkrod5")
colnames(results_table_RMSE)<-cname
colnames(results_table_MAE)<-cname
colnames(results_table_ME)<-cname
colnames(results_table_R2)<-cname


#Summary of diagnostic measures are stored in a data frame
tb_diagnostic1<-rbind(results_table_RMSE,results_table_MAE, results_table_ME, results_table_R2)   #
#tb_diagnostic1_kr<-rbind(results_table_RMSE_kr,results_table_MAE_kr, results_table_ME_kr, results_table_R2_kr)
#tb_diagnostic2<-rbind(results_table_AIC,results_table_GCV, results_table_DEV,results_table_RMSE_f)

write.table(tb_diagnostic1, file= paste(path,"/","results_GAM_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
#write.table(tb_diagnostic1_kr, file= paste(path,"/","results_GAM_Assessment_measure1_kr_",out_prefix,".txt",sep=""), sep=",")
#write.table(tb_diagnostic2, file= paste(path,"/","results_GAM_Assessment_measure2_",out_prefix,".txt",sep=""), sep=",")


#### END OF SCRIPT #####
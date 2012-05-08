####################Interpolation of Tmax for 10 dates.#####################
#This script interpolates station values for the Oregon case study using a two stage regression.
#This program loads the station data from a shape file and perform 8 regressions using general additive model (GAM) followed by kriging on the residuals.
#It uses LST monthly averages as input variables.
#Note that this program:
#1)assumes that the shape file is in the current working 
#2)extract relevant variables from raster images before performing the regressions. 
#This scripts predicts tmax using ing GAM and LST derived from MOD11A1.
#Interactions terms are also included and assessed using the RMSE from validation dataset.
#There are 10 dates used for the GAM interpolation. The dates must be provided as a textfile.
#Script created by Benoit Parmentier on May 6, 2012. 

###Loading r library and packages                                                      # loading the raster package
library(gtools)                                                                        # loading ...
library(mgcv)
library(sp)
library(spdep)
library(rgdal)
library(gstat)

###Parameters and arguments

infile1<-"ghcn_or_tmax_b_04142012_OR83M.shp"
#path<-"/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations"
path<-"H:/Data/IPLANT_project/data_Oregon_stations"
setwd(path) 
infile2<-"dates_interpolation_03052012_2dates_test.txt"                                          #List of 10 dates for the regression
prop<-0.3                                                                            #Proportion of testing retained for validation   
out_prefix<-"_05062012_Kr_LST"
infile3<-"LST_dates_var_names.txt"
infile4<-"models_interpolation_04032012b.txt"

#######START OF THE SCRIPT #############

###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)              #Removing the extension from file.
ghcn<-readOGR(".", filename)                  #reading shapefile 

CRS<-proj4string(ghcn)

mean_LST<- readGDAL("mean_day244_rescaled.rst")  #This reads the whole raster in memory and provide a grid for kriging
proj4string(mean_LST)<-CRS #Assigning coordinates information


ghcn = transform(ghcn,Northness = cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness = sin(ASPECT))  #adding variable to the dataframe.
ghcn = transform(ghcn,Northness_w = sin(slope)*cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness_w = sin(slope)*sin(ASPECT))  #adding variable to the dataframe.

set.seed(100)
dates <-readLines(paste(path,"/",infile2, sep=""))
LST_dates <-readLines(paste(path,"/",infile3, sep=""))
models <-readLines(paste(path,"/",infile4, sep=""))

results <- matrix(1,length(dates),14)            #This is a matrix containing the diagnostic measures from the GAM models.

results_AIC<- matrix(1,length(dates),length(models)+2)  
results_GCV<- matrix(1,length(dates),length(models)+2)
results_DEV<- matrix(1,length(dates),length(models)+2)
results_RMSE<- matrix(1,length(dates),length(models)+2)
results_RMSE_kr<- matrix(1,length(dates),length(models)+2)
cor_LST_LC1<-matrix(1,10,1)      #correlation LST-LC1
cor_LST_LC3<-matrix(1,10,1)      #correlation LST-LC3
cor_LST_tmax<-matrix(1,10,1)    #correlation LST-tmax
#Screening for bad values

ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$tmax>-150 & ghcn$tmax<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

month_var<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09", "mm_10", "mm_11", "mm_12")
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 subsets data
#note that compare to the previous version date_ column was changed to date

## looping through the dates...
#Change this into  a nested loop, looping through the number of models

for(i in 1:length(dates)){            # start of the for loop #1
  date<-strptime(dates[i], "%Y%m%d")
  month<-strftime(date, "%m")
  LST_month<-paste("mm_",month,sep="")
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  
  mod <-ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]
  ghcn.subsets[[i]] = transform(ghcn.subsets[[i]],LST = mod)
  #Screening LST values
  #ghcn.subsets[[i]]<-subset(ghcn.subsets[[i]],ghcn.subsets[[i]]$LST> 258 & ghcn.subsets[[i]]$LST<313)
  n<-nrow(ghcn.subsets[[i]])
  ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
  nv<-n-ns             #create a sample for validation with prop of the rows
  #ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
  data_s <- ghcn.subsets[[i]][ind.training, ]
  data_v <- ghcn.subsets[[i]][ind.testing, ]
  #mod <-data_s[,match(LST_dates[i], names(data_s))]
  #data_s = transform(data_s,LST = mod)
  #data_v = transform(data_v,LST = mod)
  ####Regression part 2: GAM models

  mod1<- gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM), data=data_s)
  mod2<- gam(tmax~ s(lat,lon,ELEV_SRTM), data=data_s)
  mod3<- gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) +  s (Northness)+ s (Eastness) + s(DISTOC), data=data_s)
  mod4<- gam(tmax~ s(lat) + s (lon) + s(ELEV_SRTM) + s(Northness) + s (Eastness) + s(DISTOC) + s(LST), data=data_s)
  mod5<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST), data=data_s)
  mod6<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC1), data=data_s)
  mod7<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC3), data=data_s)
  
  ####Regression part 3: Calculating and storing diagnostic measures
  
  results_AIC[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results_AIC[i,2]<- ns        #number of stations used in the training stage
  results_AIC[i,3]<- AIC (mod1)
  results_AIC[i,4]<- AIC (mod2)
  results_AIC[i,5]<- AIC (mod3)
  results_AIC[i,6]<- AIC (mod4)
  results_AIC[i,7]<- AIC (mod5)
  results_AIC[i,8]<- AIC (mod6)
  results_AIC[i,9]<- AIC (mod7)
  
  results_GCV[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results_GCV[i,2]<- ns        #number of stations used in the training stage
  results_GCV[i,3]<- mod1$gcv.ubre
  results_GCV[i,4]<- mod2$gcv.ubre
  results_GCV[i,5]<- mod3$gcv.ubre
  results_GCV[i,6]<- mod4$gcv.ubre
  results_GCV[i,7]<- mod5$gcv.ubre
  results_GCV[i,8]<- mod6$gcv.ubre
  results_GCV[i,9]<- mod7$gcv.ubre
  
  results_DEV[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results_DEV[i,2]<- ns        #number of stations used in the training stage
  results_DEV[i,3]<- mod1$deviance
  results_DEV[i,4]<- mod2$deviance
  results_DEV[i,5]<- mod3$deviance
  results_DEV[i,6]<- mod4$deviance
  results_DEV[i,7]<- mod5$deviance
  results_DEV[i,8]<- mod6$deviance
  results_DEV[i,9]<- mod7$deviance
  
  #####VALIDATION: Prediction checking the results using the testing data########
 
  #Automate this using a data frame of size??
  y_mod1<- predict(mod1, newdata=data_v, se.fit = TRUE) #Using the coeff to predict new values.
  y_mod2<- predict(mod2, newdata=data_v, se.fit = TRUE)            
  y_mod3<- predict(mod3, newdata=data_v, se.fit = TRUE) 
  y_mod4<- predict(mod4, newdata=data_v, se.fit = TRUE) 
  y_mod5<- predict(mod5, newdata=data_v, se.fit = TRUE) 
  y_mod6<- predict(mod6, newdata=data_v, se.fit = TRUE)
  y_mod7<- predict(mod7, newdata=data_v, se.fit = TRUE)
  
  res_mod1<- data_v$tmax - y_mod1$fit #Residuals for GMA model that resembles the ANUSPLIN interpolation
  res_mod2<- data_v$tmax - y_mod2$fit   #Residuals for GAM model that resembles the PRISM interpolation                               
  res_mod3<- data_v$tmax - y_mod3$fit  
  res_mod4<- data_v$tmax - y_mod4$fit
  res_mod5<- data_v$tmax - y_mod5$fit
  res_mod6<- data_v$tmax - y_mod6$fit
  res_mod7<- data_v$tmax - y_mod7$fit
  
  RMSE_mod1 <- sqrt(sum(res_mod1^2)/nv)          
  RMSE_mod2 <- sqrt(sum(res_mod2^2)/nv)
  RMSE_mod3 <- sqrt(sum(res_mod3^2)/nv)
  RMSE_mod4 <- sqrt(sum(res_mod4^2)/nv)
  RMSE_mod5 <- sqrt(sum(res_mod5^2)/nv)
  RMSE_mod6 <- sqrt(sum(res_mod6^2)/nv)
  RMSE_mod7 <- sqrt(sum(res_mod7^2)/nv)

  results_RMSE[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results_RMSE[i,2]<- ns        #number of stations used in the training stage
  results_RMSE[i,3]<- RMSE_mod1
  results_RMSE[i,4]<- RMSE_mod2
  results_RMSE[i,5]<- RMSE_mod3
  results_RMSE[i,6]<- RMSE_mod4
  results_RMSE[i,7]<- RMSE_mod5
  results_RMSE[i,8]<- RMSE_mod6
  results_RMSE[i,9]<- RMSE_mod7
  
  #Saving dataset in dataframes: residuals from RMSE
  
#   data_v$mod1<-y_mod1$fit
#   data_v$mod2<-y_mod2$fit
#   data_v$mod3<-y_mod3$fit
#   data_v$mod4<-y_mod4$fit
#   data_v$mod5<-y_mod5$fit
#   data_v$mod6<-y_mod6$fit
#   data_v$mod7<-y_mod7$fit
#   
#   data_s$mod1<-mod1$fit
#   data_s$mod2<-mod2$fit
#   data_s$mod3<-mod3$fit
#   data_s$mod4<-mod4$fit
#   data_s$mod5<-mod5$fit
#   data_s$mod6<-mod6$fit
#   data_s$mod7<-mod7$fit
#   
  data_v$res_mod1<-as.numeric(res_mod1)
  data_v$res_mod2<-as.numeric(res_mod2)
  data_v$res_mod3<-as.numeric(res_mod3)
  data_v$res_mod4<-as.numeric(res_mod4)
  data_v$res_mod5<-as.numeric(res_mod5)
  data_v$res_mod6<-as.numeric(res_mod6)
  data_v$res_mod7<-as.numeric(res_mod7)
  
  data_s$res_mod1<-as.numeric(mod1$residuals)
  data_s$res_mod2<-as.numeric(mod2$residuals)
  data_s$res_mod3<-as.numeric(mod3$residuals)
  data_s$res_mod4<-as.numeric(mod4$residuals)
  data_s$res_mod5<-as.numeric(mod5$residuals)
  data_s$res_mod6<-as.numeric(mod6$residuals)
  data_s$res_mod7<-as.numeric(mod7$residuals)
  
  ###BEFORE Kringing the data object must be transformed to SDF
  
  coords<- data_v[,c('x_OR83M','y_OR83M')]
  coordinates(data_v)<-coords
  proj4string(data_v)<-CRS  #Need to assign coordinates...
  coords<- data_s[,c('x_OR83M','y_OR83M')]
  coordinates(data_s)<-coords
  proj4string(data_s)<-CRS  #Need to assign coordinates..
  
  #Kriging residuals!!

  for (j in 1:length(models)){
    name<-paste("res_mod",j,sep="")
    data_s$residuals<-data_s[[name]]
    X11()
    hscat(residuals~1,data_s,(0:9)*20000) # 9 lag classes with 20,000m width
    v<-variogram(residuals~1, data_s)
    plot(v)
    v.fit<-fit.variogram(v,vgm(1,"Sph", 150000,1))
    res_krige<-krige(residuals~1, data_s,mean_LST, v.fit)#mean_LST provides the data grid/raster image for the kriging locations.
  
    # Kriging visualization of Residuals fit over space
  
    #spplot.vcov(co_kriged_surf)                           #Visualizing the covariance structure
  
    res_krig1_s <- overlay(res_krige,data_s)             #This overlays the kriged surface tmax and the location of weather stations
    res_krig1_v <- overlay(res_krige,data_v)             #This overlays the kriged surface tmax and the location of weather stations
  
    name2<-paste("pred_kr_mod",j,sep="")
    #Adding the results back into the original dataframes.
    data_s[[name2]]<-res_krig1_s$var1.pred
    data_v[[name2]]<-res_krig1_v$var1.pred  
  
    #Calculate RMSE and then krig the residuals....!
  
    res_mod_kr_s<- data_s$tmax - data_s[[name2]]           #Residuals from kriging.
    res_mod_kr_v<- data_v$tmax - data_v[[name2]]           #Residuals from cokriging.
  
    RMSE_mod_kr_s <- sqrt(sum(res_mod_kr_s^2,na.rm=TRUE)/(nv-sum(is.na(res_mod_kr_s))))                  #RMSE from kriged surface.
    RMSE_mod_kr_v <- sqrt(sum(res_mod_kr_v^2,na.rm=TRUE)/(nv-sum(is.na(res_mod_kr_v))))                  #RMSE from co-kriged surface.
    #(nv-sum(is.na(res_mod2)))
    #Writing out results
  
    results_RMSE_kr[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_RMSE_kr[i,2]<- ns        #number of stations used in the training stage
    results_RMSE_kr[i,j+2]<- RMSE_mod_kr_v
    #results_RMSE_kr[i,3]<- res_mod_kr_v
    name3<-paste("res_kr_mod",j,sep="")
    data_s[[name3]]<-res_mod_kr_s
    data_v[[name3]]<-res_mod_kr_v #Writing residuals from kriging
    }
  data_name<-paste("ghcn_v_",out_prefix,"_",dates[[i]],sep="")
  assign(data_name,data_v)
  write.table(data_v, file= paste(path,"/",data_name,".txt",sep=""), sep=" ")
  #write out a new shapefile (including .prj component)
  outfile<-sub(".shp","",data_name)   #Removing extension if it is present
  writeOGR(data_v,".", outfile, driver ="ESRI Shapefile")
  
  data_name<-paste("ghcn_s_",out_prefix,"_",dates[[i]],sep="")
  assign(data_name,data_s)
  write.table(data_s, file= paste(path,"/",data_name,".txt",sep=""), sep=" ")
  outfile<-sub(".shp","",data_name)   #Removing extension if it is present
  writeOGR(data_s,".", outfile, driver ="ESRI Shapefile")
  
  # end of the for loop #1
  }

## Plotting and saving diagnostic measures

results_RMSEnum <-results_RMSE
results_AICnum <-results_AIC
mode(results_RMSEnum)<- "numeric"         # Make it numeric first
mode(results_AICnum)<- "numeric"          # Now turn it into a data.frame...

results_table_RMSE<-as.data.frame(results_RMSEnum)
results_table_AIC<-as.data.frame(results_AICnum)
colnames(results_table_RMSE)<-c("dates","ns","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7")
colnames(results_table_AIC)<-c("dates","ns","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7")

results_RMSE_kr_num <-results_RMSE_kr
mode(results_RMSE_kr_num)<- "numeric"         # Make it numeric first
results_table_RMSE_kr<-as.data.frame(results_RMSE_kr_num)
colnames(results_table_RMSE_kr)<-c("dates","ns","mod1k", "mod2k","mod3k", "mod4k", "mod5k", "mod6k", "mod7k")
#results_table_RMSE

write.table(results_table_RMSE, file= paste(path,"/","results_GAM_Assessment",out_prefix,".txt",sep=""), sep=",")
write.table(results_table_AIC, file= paste(path,"/","results_GAM_Assessment",out_prefix,".txt",sep=""),sep=",", append=TRUE)
write.table(results_table_RMSE_kr, file= paste(path,"/","results_GAM_Assessment_kr",out_prefix,".txt",sep=""), sep=",")

##Visualization of results##

for(i in 1:length(dates)){
  X11()
  RMSE_kr<-results_table_RMSE_kr[i,]
  RMSE_ga<-results_table_RMSE[i,]
  
  RMSE_kr<-RMSE_kr[,1:length(models)+2]
  RMSE_ga<-RMSE_ga[,1:length(models)+2]
  colnames(RMSE_kr)<-names(RMSE_ga)
  height<-rbind(RMSE_ga,RMSE_kr)
  rownames(height)<-c("GAM","GAM_KR")
  height<-as.matrix(height)
  barplot(height,ylim=c(0,105),ylab="RMSE in tenth deg C",beside=TRUE,
          legend.text=rownames(height),
          args.legend=list(x="topright"),
          main=paste("RMSE for date ",dates[i], sep=""))
  savePlot(paste("Barplot_results_RMSE_GAM_KR_",dates[i],out_prefix,".png", sep=""), type="png")
  }
  
# End of script##########




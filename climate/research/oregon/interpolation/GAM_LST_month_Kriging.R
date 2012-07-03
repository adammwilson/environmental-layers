##################    Interpolation of Tmax for 10 dates.  #######################################
###########################  TWO-STAGE REGRESSION  ###############################################
#This script interpolates station values for the Oregon case study using a two-stage regression. #
#For each input dates, it performs 1) Step 1: General Additive Model (GAM)                       #
#                                  2) Step 2: Kriging on residuals from step 1                   #
#                                                                                                #
#The script uses LST monthly averages as input variables and  loads the station data             # 
#from a shape file with projection information.                                                  #
#Note that this program:                                                                         #
#1)assumes that the shape file is in the current working.                                        # 
#2)relevant variables were extracted from raster images before performing the regressions        #
#  and stored shapefile                                                                          #
#This scripts predicts tmax using ing GAM and LST derived from MOD11A1.Interactions terms are    #
#also included and assessed using the RMSE,MAE,ME and R2 from validation dataset.                #
#There are #10 dates used for the GAM interpolation. The dates must be provided as a textfile.   #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 05/31/212                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#364--                                  #
##################################################################################################

###Loading R library and packages                                                      
library(gtools)                                         # loading some useful tools 
library(mgcv)                                           # GAM package by Simon Wood
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                                          # GDAL wrapper for R, spatial utilities
library(gstat)                                          # Kriging and co-kriging by Pebesma et al.

###Parameters and arguments

infile1<- "ghcn_or_tmax_b_04142012_OR83M.shp"             #GHCN shapefile containing variables                  
infile2<-"list_10_dates_04212012.txt"                      #List of 10 dates for the regression
#infile2<-"list_365_dates_04212012.txt"
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       #Raster or grid for the locations of predictions

path<-"/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations"         #Jupiter LOCATION on EOS
#path<-"H:/Data/IPLANT_project/data_Oregon_stations"                                 #Jupiter Location on XANDERS
setwd(path) 
prop<-0.3                                                                           #Proportion of testing retained for validation   
seed_number<- 100                                                                   #Seed number for random sampling
out_prefix<-"_05312012_2d_Kr_LST"                                                   #User defined output prefix

############ START OF THE SCRIPT ##################

###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

CRS<-proj4string(ghcn)                       #Storing projectionminformation (ellipsoid, datum,etc.)

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
results_RMSE <- matrix(1,length(dates),length(models)+3)
results_RMSE_kr <- matrix(1,length(dates),length(models)+3)
results_MAE <- matrix(1,length(dates),length(models)+3)
results_MAE_kr <- matrix(1,length(dates),length(models)+3)
results_ME <- matrix(1,length(dates),length(models)+3)
results_ME_kr <- matrix(1,length(dates),length(models)+3)    #ME corresponds to the bias
results_R2 <- matrix(1,length(dates),length(models)+3)       #Coef. of determination for the validation dataset
results_R2_kr <- matrix(1,length(dates),length(models)+3)
results_RMSE_f<- matrix(1,length(dates),length(models)+3)    #RMSE fit, RMSE for the training dataset
results_RMSE_f_kr<- matrix(1,length(dates),length(models)+3)

#Tracking relationship between LST AND LC
cor_LST_LC1<-matrix(1,10,1)      #correlation LST-LC1
cor_LST_LC3<-matrix(1,10,1)      #correlation LST-LC3
cor_LST_tmax<-matrix(1,10,1)     #correlation LST-tmax

#Screening for bad values
ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$tmax>-150 & ghcn$tmax<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

month_var<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09", "mm_10", "mm_11", "mm_12")
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 or 365 subsets dataset based on dates

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

  ####Regression part 2: GAM models (REGRESSION STEP1)

  #Model can be changed without affecting the script
  mod1<- gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM), data=data_s)
  mod2<- gam(tmax~ s(lat,lon,ELEV_SRTM), data=data_s)
  mod3<- gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) +  s (Northness)+ s (Eastness) + s(DISTOC), data=data_s)
  mod4<- gam(tmax~ s(lat) + s (lon) + s(ELEV_SRTM) + s(Northness) + s (Eastness) + s(DISTOC) + s(LST), data=data_s)
  mod5<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST), data=data_s)
  mod6<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC1), data=data_s)
  mod7<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC3), data=data_s)
  mod8<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST) + s(LC1), data=data_s)
  
  ####Regression part 3: Calculating and storing diagnostic measures
  #listmod can be created and looped over. In this case we loop around the GAM objects in memory...

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
 
    #Automate this using a data frame of size??
    y_mod<- predict(mod, newdata=data_v, se.fit = TRUE) #Using the coeff to predict new values.
    res_mod<- data_v$tmax - y_mod$fit                   #Residuals for the model
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
  
  ###BEFORE Kringing the data object must be transformed to SDF
  
  coords<- data_v[,c('x_OR83M','y_OR83M')]
  coordinates(data_v)<-coords
  proj4string(data_v)<-CRS  #Need to assign coordinates...
  coords<- data_s[,c('x_OR83M','y_OR83M')]
  coordinates(data_s)<-coords
  proj4string(data_s)<-CRS  #Need to assign coordinates..
  
  #KRIGING ON GAM RESIDUALS: REGRESSION STEP2

  for (j in 1:length(models)){
    name<-paste("res_mod",j,sep="")
    data_s$residuals<-data_s[[name]]
    X11()
    hscat(residuals~1,data_s,(0:9)*20000) # 9 lag classes with 20,000m width
    v<-variogram(residuals~1, data_s)   
    plot(v)                               # This plot may be saved at a later stage...
    dev.off()
    v.fit<-fit.variogram(v,vgm(1,"Sph", 150000,1))
    res_krige<-krige(residuals~1, data_s,mean_LST, v.fit)#mean_LST provides the data grid/raster image for the kriging locations.
  
    res_krig1_s <- overlay(res_krige,data_s)             #This overlays the kriged surface tmax and the location of weather stations
    res_krig1_v <- overlay(res_krige,data_v)             #This overlays the kriged surface tmax and the location of weather stations
  
    name2<-paste("pred_kr_mod",j,sep="")
    #Adding the results back into the original dataframes.
    data_s[[name2]]<-res_krig1_s$var1.pred
    data_v[[name2]]<-res_krig1_v$var1.pred  
    
    #NEED TO ADD IT BACK TO THE PREDICTION FROM GAM
    gam_kr<-paste("pred_gam_kr",j,sep="")
    pred_gam<-paste("pred_mod",j,sep="")
    data_s[[gam_kr]]<-data_s[[pred_gam]]+ data_s[[name2]]
    data_v[[gam_kr]]<-data_v[[pred_gam]]+ data_v[[name2]]
    
    #Model assessment: RMSE and then krig the residuals....!
  
    res_mod_kr_s<- data_s$tmax - data_s[[gam_kr]]           #Residuals from kriging training
    res_mod_kr_v<- data_v$tmax - data_v[[gam_kr]]           #Residuals from kriging validation
  
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
  
    results_RMSE_kr[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_RMSE_kr[i,2]<- ns        #number of stations used in the training stage
    results_RMSE_kr[i,3]<- "RMSE"
    results_RMSE_kr[i,j+3]<- RMSE_mod_kr_v
    #results_RMSE_kr[i,3]<- res_mod_kr_v
    
    results_MAE_kr[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_MAE_kr[i,2]<- ns        #number of stations used in the training stage
    results_MAE_kr[i,3]<- "MAE"
    results_MAE_kr[i,j+3]<- MAE_mod_kr_v
    #results_RMSE_kr[i,3]<- res_mod_kr_v
    
    results_ME_kr[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_ME_kr[i,2]<- ns        #number of stations used in the training stage
    results_ME_kr[i,3]<- "ME"
    results_ME_kr[i,j+3]<- ME_mod_kr_v
    #results_RMSE_kr[i,3]<- res_mod_kr_v
    
    results_R2_kr[i,1]<- dates[i]  #storing the interpolation dates in the first column
    results_R2_kr[i,2]<- ns        #number of stations used in the training stage
    results_R2_kr[i,3]<- "R2"
    results_R2_kr[i,j+3]<- R2_mod_kr_v
    #results_RMSE_kr[i,3]<- res_mod_kr_v
    
    name3<-paste("res_kr_mod",j,sep="")
    #as.numeric(res_mod)
    #data_s[[name3]]<-res_mod_kr_s
    data_s[[name3]]<-as.numeric(res_mod_kr_s)
    #data_v[[name3]]<-res_mod_kr_v 
    data_v[[name3]]<-as.numeric(res_mod_kr_v)
    #Writing residuals from kriging
    
    }
  
  ###SAVING THE DATA FRAME IN SHAPEFILES AND TEXTFILES
  
  data_name<-paste("ghcn_v_",out_prefix,"_",dates[[i]],sep="")
  assign(data_name,data_v)
  #write.table(data_v, file= paste(path,"/",data_name,".txt",sep=""), sep=" ")
  #write out a new shapefile (including .prj component)
  #outfile<-sub(".shp","",data_name)   #Removing extension if it is present
  #writeOGR(data_v,".", outfile, driver ="ESRI Shapefile")
  
  data_name<-paste("ghcn_s_",out_prefix,"_",dates[[i]],sep="")
  assign(data_name,data_s)
  #write.table(data_s, file= paste(path,"/",data_name,".txt",sep=""), sep=" ")
  #outfile<-sub(".shp","",data_name)   #Removing extension if it is present
  #writeOGR(data_s,".", outfile, driver ="ESRI Shapefile")
  
  # end of the for loop1
  
  }

## Plotting and saving diagnostic measures

#Specific diagnostic measures related to the testing datasets
results_table_AIC<-as.data.frame(results_AIC)
results_table_GCV<-as.data.frame(results_GCV)
results_table_DEV<-as.data.frame(results_DEV)
results_table_RMSE_f<-as.data.frame(results_RMSE_f)

results_table_RMSE<-as.data.frame(results_RMSE)
results_table_MAE<-as.data.frame(results_MAE)
results_table_ME<-as.data.frame(results_ME)
results_table_R2<-as.data.frame(results_R2)

cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7","mod8")
colnames(results_table_RMSE)<-cname
colnames(results_table_MAE)<-cname
colnames(results_table_ME)<-cname
colnames(results_table_R2)<-cname
#Specific diagnostic measures
colnames(results_table_AIC)<-cname
colnames(results_table_GCV)<-cname
colnames(results_table_DEV)<-cname
colnames(results_table_RMSE_f)<-cname

#General diagnostic measures

results_table_RMSE_kr<-as.data.frame(results_RMSE_kr)
results_table_MAE_kr<-as.data.frame(results_MAE_kr)
results_table_ME_kr<-as.data.frame(results_ME_kr)
results_table_R2_kr<-as.data.frame(results_R2_kr)

cname<-c("dates","ns","metric","mod1k", "mod2k","mod3k", "mod4k", "mod5k", "mod6k", "mod7k","mod8k")
colnames(results_table_RMSE_kr)<-cname
colnames(results_table_MAE_kr)<-cname
colnames(results_table_ME_kr)<-cname
colnames(results_table_R2_kr)<-cname

#Summary of diagnostic measures are stored in a data frame
tb_diagnostic1<-rbind(results_table_RMSE,results_table_MAE, results_table_ME, results_table_R2)   #
tb_diagnostic1_kr<-rbind(results_table_RMSE_kr,results_table_MAE_kr, results_table_ME_kr, results_table_R2_kr)
tb_diagnostic2<-rbind(results_table_AIC,results_table_GCV, results_table_DEV,results_table_RMSE_f)

write.table(tb_diagnostic1, file= paste(path,"/","results_GAM_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")
write.table(tb_diagnostic1_kr, file= paste(path,"/","results_GAM_Assessment_measure1_kr_",out_prefix,".txt",sep=""), sep=",")
write.table(tb_diagnostic2, file= paste(path,"/","results_GAM_Assessment_measure2_",out_prefix,".txt",sep=""), sep=",")

# ##Visualization of results##
# 
# for(i in 1:length(dates)){
#   X11()
#   RMSE_kr<-results_table_RMSE_kr[i,]
#   RMSE_ga<-results_table_RMSE[i,]
#   
#   RMSE_kr<-RMSE_kr[,1:length(models)+2]
#   RMSE_ga<-RMSE_ga[,1:length(models)+2]
#   colnames(RMSE_kr)<-names(RMSE_ga)
#   height<-rbind(RMSE_ga,RMSE_kr)
#   rownames(height)<-c("GAM","GAM_KR")
#   height<-as.matrix(height)
#   barplot(height,ylim=c(14,36),ylab="RMSE in tenth deg C",beside=TRUE,
#           legend.text=rownames(height),
#           args.legend=list(x="topright"),
#           main=paste("RMSE for date ",dates[i], sep=""))
#   savePlot(paste("Barplot_results_RMSE_GAM_KR_",dates[i],out_prefix,".png", sep=""), type="png")
#   dev.off()
#   }
#   
# r1<-(results_table_RMSE[,3:10]) #selecting only the columns related to models and method 1
# r2<-(results_table_RMSE_kr[,3:10]) #selecting only the columns related to models and method 1
# mean_r1<-mean(r1)
# mean_r2<-mean(r2)
# median_r1<-sapply(r1, median)   #Calulcating the mean for every model (median of columns)
# median_r2<-sapply(r2, median)
# sd_r1<-sapply(r1, sd)
# sd_r2<-sapply(r2, sd)
# 
# barplot(mean_r1,ylim=c(23,26),ylab="RMSE in tenth deg C")
# barplot(mean_r2,ylim=c(23,26),ylab="RMSE in tenth deg C")
# barplot(median_r1,ylim=c(23,26),ylab="RMSE in tenth deg C",add=TRUE,inside=FALSE,beside=TRUE) # put both on the same plot
# barplot(median_r2,ylim=c(23,26),ylab="RMSE in tenth deg C",add=TRUE,inside=FALSE,beside=TRUE) # put both on the same plot
# 
# barplot(sd_r1,ylim=c(6,8),ylab="RMSE in tenth deg C") # put both on the same plot
# barplot(sd_r2,ylim=c(6,8),ylab="RMSE in tenth deg C") # put both on the same plot
# 
# height<-rbind(mean_r1,mean_r2)
# barplot(height,ylim=c(20,26),ylab="RMSE in tenth deg C",beside=TRUE,legend=rownames(height))
# barplot(height,ylim=c(20,26),ylab="RMSE in tenth deg C",beside=TRUE, col=c("darkblue","red"),legend=rownames(height)) # put both on the same plot
# 
# height<-rbind(median_r1,median_r2)
# barplot(height,ylim=c(20,26),ylab="RMSE in tenth deg C",beside=TRUE,legend=rownames(height))
# barplot(height,ylim=c(20,26),ylab="RMSE in tenth deg C",beside=TRUE, col=c("darkblue","red"),legend=rownames(height)) # put both on the same plot
# 
# height<-rbind(mean_r2,median_r2)
# barplot(height,ylim=c(20,26),ylab="RMSE in tenth deg C",beside=TRUE,legend=rownames(height))
# barplot(height,ylim=c(20,26),ylab="RMSE in tenth deg C",beside=TRUE, col=c("darkblue","red"),legend=rownames(height)) # put both on the same plot

#barplot2(mean_r,median_r,ylim=c(23,26),ylab="RMSE in tenth deg C") # put both on the same plot
#Collect var explained and p values for each var...


##### END OF SCRIPT ##########




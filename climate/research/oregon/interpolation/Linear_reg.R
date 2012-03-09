####################Interpolation of Tmax for 10 dates.#####################
#This script interpolates station values for the Oregon case study. This program loads the station data from a csv file 
#and perform two types  of regression: multiple linear model and general additive model (GAM). Note that this program:
#1)assumes that the csv file is in the current working 
#2)extract relevant variables from raster images before performing the regressions. 
#The user must provide the list of raster images in  a textile.
#Script created by Benoit Parmentier on March 3, 2012. 

###Loading r library and packages                                                                       # loading the raster package
library(gtools)                                                                        # loading ...
library(mgcv)

###Parameters and arguments
#infile1<-"ghcn_or_b_02122012_OR83M.csv"
infile1<-"ghcn_or_tmax_b_03032012_OR83M.csv"
path<-"C:/Data/Benoit/NCEAS/window_Oregon_data"
setwd(path)
#infile2<-"dates_interpolation_03012012.txt"  # list of 10 dates for the regression
infile2<-"dates_interpolation_03052012.txt"
prop<-0.3                                                                            #Proportion of testing retained for validation   
out_prefix<-"_03042012_r1"
infile3<-"models_interpolation_03052012.txt"


#######START OF THE SCRIPT #############

###Reading the station data and setting up for models' comparison
ghcn<-read.csv(paste(path,"/",infile1, sep=""), header=TRUE)                            #The "paste" function concatenates the path and file name in common string. 
ghcn = transform(ghcn,Northness = cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness = sin(ASPECT))  #adding variable to the dataframe.

ghcn = transform(ghcn,Northness_w = sin(slope)*cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness_w = sin(slope)*sin(ASPECT))  #adding variable to the dataframe.
set.seed(100)
dates <-readLines(paste(path,"/",infile2, sep=""))
models <-readLines(paste(path,"/",infile3, sep=""))

results <- matrix(1,length(dates),14)            #This is a matrix containing the diagnostic measures from the GAM models.

results_AIC<- matrix(1,length(dates),length(models)+3)  
results_GCV<- matrix(1,length(dates),length(models)+3)
results_RMSE<- matrix(1,length(dates),length(models)+3)

ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 subsets data
#note that compare to the previous version date_ column was changed to date

## looping through the dates...
#Change this into  a nested loop, looping through the number of models

for(i in 1:length(dates)){            # start of the for loop #1
  
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  
  n<-nrow(ghcn.subsets[[i]])
  ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
  nv<-n-ns             #create a sample for validation with prop of the rows
  #ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
  data_s <- ghcn.subsets[[i]][ind.training, ]
  data_v <- ghcn.subsets[[i]][ind.testing, ]
  
  ####Regression part 2: GAM models

  mod1<-gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM), data=data_s)
  mod2<- gam(tmax~ s(lat,lon,ELEV_SRTM), data=data_s)
  mod3<-gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) +  s (Northness)+ s (Eastness) + s(DISTOC), data=data_s)
  mod4<-gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) + s (Northness_w)+ s (Eastness_w) + s(DISTOC), data=data_s)
  mod5<- gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM, Northness) + s (Eastness) + s(DISTOC), data=data_s)
  mod6<- gam(tmax~ s(lat,lon) + s (ELEV_SRTM, Northness) + s (Eastness) + s(DISTOC), data=data_s)
  
  
  ####Regression part 3: Calculating and storing diagnostic measures
  
  results_AIC[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results_AIC[i,2]<- ns        #number of stations used in the training stage
  results_AIC[i,3]<- AIC (mod1)
  results_AIC[i,4]<- AIC (mod2)
  results_AIC[i,5]<- AIC (mod3)
  results_AIC[i,6]<- AIC (mod4)
  results_AIC[i,7]<- AIC (mod5)
  results_AIC[i,8]<- AIC (mod6)
  
  results_GCV[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results_GCV[i,2]<- ns        #number of stations used in the training stage
  results_GCV[i,3]<- mod1$gcv.ubre
  results_GCV[i,4]<- mod2$gcv.ubre
  results_GCV[i,5]<- mod3$gcv.ubre
  results_GCV[i,6]<- mod4$gcv.ubre
  results_GCV[i,7]<- mod5$gcv.ubre
  results_GCV[i,8]<- mod6$gcv.ubre
  
  results_DEV[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results_DEV[i,2]<- ns        #number of stations used in the training stage
  results_DEV[i,3]<- mod1$deviance
  results_DEV[i,4]<- mod2$deviance
  results_DEV[i,5]<- mod3$deviance
  results_DEV[i,6]<- mod4$deviance
  results_DEV[i,7]<- mod5$deviance
  results_DEV[i,8]<- mod6$deviance
  
  #####VALIDATION: Prediction checking the results using the testing data########
 
  y_mod1<- predict(mod1, newdata=data_v, se.fit = TRUE) #Using the coeff to predict new values.
  y_mod2<- predict(mod2, newdata=data_v, se.fit = TRUE)            
  y_mod3<- predict(mod3, newdata=data_v, se.fit = TRUE) 
  y_mod4<- predict(mod4, newdata=data_v, se.fit = TRUE) 
  y_mod5<- predict(mod5, newdata=data_v, se.fit = TRUE) 
  y_mod5<- predict(mod6, newdata=data_v, se.fit = TRUE)
  
  res_mod1<- data_v$tmax - y_mod1$fit #Residuals for GMA model that resembles the ANUSPLIN interpolation
  res_mod2<- data_v$tmax - y_mod2$fit   #Residuals for GAM model that resembles the PRISM interpolation                               
  res_mod3<- data_v$tmax - y_mod3$fit  
  res_mod4<- data_v$tmax - y_mod4$fit
  res_mod5<- data_v$tmax - y_mod5$fit
  res_mod6<- data_v$tmax - y_mod6$fit
  
  RMSE_mod1 <- sqrt(sum(res_mod1^2)/nv)          
  RMSE_mod2 <- sqrt(sum(res_mod2^2)/nv)
  RMSE_mod3 <- sqrt(sum(res_mod3^2)/nv)
  RMSE_mod4 <- sqrt(sum(res_mod4^2)/nv)
  RMSE_mod5 <- sqrt(sum(res_mod5^2)/nv)
  RMSE_mod6 <- sqrt(sum(res_mod6^2)/nv)
  

  results_RMSE[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results_RMSE[i,2]<- ns        #number of stations used in the training stage
  results_RMSE[i,3]<- RMSE_mod1
  results_RMSE[i,4]<- RMSE_mod2
  results_RMSE[i,5]<- RMSE_mod3
  results_RMSE[i,6]<- RMSE_mod4
  results_RMSE[i,7]<- RMSE_mod5
  results_RMSE[i,8]<- RMSE_mod6
  
  data_name<-paste("ghcn_v_",dates[[i]],sep="")
  assign(data_name,data_v)
  #ghcn_v<-ls(pattern="ghcn_v_")
  
  # end of the for loop #1
  }

## Plotting and saving diagnostic measures

results_RMSEnum <-results_RMSE
mode(results_RMSEnum)<- "numeric"
# Make it numeric first
# Now turn it into a data.frame...

results_table_RMSE<-as.data.frame(results_RMSEnum)
colnames(results_table_RMSE)<-c("dates","ns","mod1", "mod2","mod3", "mod4", "mod5", "mod6")

win.graph()
barplot(results_table$RMSE_A1/10,main="RMSE for the A1 models",names.arg=results_table$dates,ylab="Temp (deg. C)",xlab="interpolated date")
savePlot(paste("GAM_ANUSPLIN1_RMSE",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$RMSE_P1/10,main="RMSE for the P1 models",names.arg=results_table$dates,ylab="Temp ( deg. C)",xlab="interpolated date")
savePlot(paste("GAM_PRISM1_RMSE",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$RMSE_P2/10,main="RMSE for the P2 models",names.arg=results_table$dates,ylab="Temp ( deg. C)",xlab="interpolated date")
savePlot(paste("GAM_PRISM1_RMSE",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$AIC_A1,main="AIC for the A1 models",names.arg=results_table$dates,ylab="Temp ( deg. C)",xlab="interpolated date")
savePlot(paste("GAM_PRISM1_RMSE",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$AIC_P1/10,main="AIC for the P1 models",names.arg=results_table$dates,ylab="Temp ( deg. C)",xlab="interpolated date")
savePlot(paste("GAM_PRISM1_RMSE",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$AIC_P2/10,main="AIC for the P2 models",names.arg=results_table$dates,ylab="Temp (10 X deg. C)",xlab="interolated date")
savePlot(paste("GAM_PRISM1_RMSE",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$Deviance_A1/10,main="Deviance for the A1 models",names.arg=results_table$dates,ylab="Temp (10 X deg. C)",xlab="interolated date")
savePlot(paste("GAM_ANUSPLIN1_Deviance",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$Deviance_P1/10,main="Deviance for the P1 models",names.arg=results_table$dates,ylab="Temp (10 X deg. C)",xlab="interolated date")
savePlot(paste("GAM_PRISM1_Deviance",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$Deviance_P2/10,main="Deviance for the P2 models",names.arg=results_table$dates,ylab="Temp (10 X deg. C)",xlab="interolated date")
savePlot(paste("GAM_PRISM2_Deviance",out_prefix,".emf", sep=""), type="emf")

write.csv(results_table, file= paste(path,"/","results_GAM_Assessment",out_prefix,".txt",sep=""))


# End of script##########

# ###############################

# 
# ############Diagnostic GAM plots#############
# win.graph()
# gam.check(GAM_ANUSPLIN1)
# savePlot("GAM_ANUSPLIN1_diagnostic1.emf", type="emf")
# win.graph()
# gam.check(GAM_PRISM1)   #This will produce basic plots of residuals
# savePlot("GAM_PRISM_diagnostic1.emf", type="emf")
# gam.check(GAM_ANUSPLIN1)
# win.graph()
# vis.gam(GAM_ANUSPLIN1)
# savePlot("GAM_ANUSPLIN1_prediction.emf", type="emf")        
# win.graph()
# vis.gam(GAM_PRISM1)
# savePlot("GAM_PRISM1_prediction.emf", type="emf")
# win.graph()
# vis.gam(GAM_ANUSPLIN1, view=c("lat","ELEV_SRTM"))
# #vis.gam(GAM_ANUSPLIN1, view=c("lat","ELEV_SRTM", theta=100,phi=200))
# savePlot("GAM_ANUSPLIN1_prediction2.emf", type="emf")
#

#                 




 
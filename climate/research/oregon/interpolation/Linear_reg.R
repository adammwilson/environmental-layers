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

#######START OF THE SCRIPT #############

###Reading the station data and setting up for models' comparison
ghcn<-read.csv(paste(path,"/",infile1, sep=""), header=TRUE)                            #The "paste" function concatenates the path and file name in common string. 
ghcn = transform(ghcn,Northness = cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness = sin(ASPECT))  #adding variable to the dataframe.

ghcn = transform(ghcn,Northness_w = sin(slope)*cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness_w = sin(slope)*sin(ASPECT))  #adding variable to the dataframe.
set.seed(100)
dates <-readLines(paste(path,"/",infile2, sep=""))

results <- matrix(1,length(dates),14)            #This is a matrix containing the diagnostic measures from the GAM models.

ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 subsets data
#note that compare to the previous version date_ column was changed to date

## looping through the dates...

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

  GAM_ANUSPLIN1<-gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM), data=data_s)
  GAM_PRISM1<-gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) +  s (Northness)+ s (Eastness) + s(DISTOC), data=data_s)
  GAM_PRISM2<-gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) + s (Northness_w)+ s (Eastness_w) + s(DISTOC), data=data_s)
  
  
  ####Regression part 3: Calculating and storing diagnostic measures
  
  results[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results[i,2]<- ns        #number of stations used in the training stage
  
  results[i,6]<- AIC (GAM_ANUSPLIN1)
  results[i,7]<- AIC (GAM_PRISM1)
  results[i,8]<- AIC (GAM_PRISM2)
  
  results[i,9]<- GAM_ANUSPLIN1$gcv.ubre
  results[i,10]<- GAM_PRISM1$gcv.ubre
  results[i,11]<- GAM_PRISM2$gcv.ubre
  
  results[i,12]<-GAM_ANUSPLIN1$deviance
  results[i,13]<-GAM_PRISM1$deviance
  results[i,14]<-GAM_PRISM2$deviance
  
  #####VALIDATION: Prediction checking the results using the testing data########
 
  y_pgANUSPLIN1<- predict(GAM_ANUSPLIN1, newdata=data_v, se.fit = TRUE) #Using the coeff to predict new values.
  y_pgPRISM1<- predict(GAM_PRISM1, newdata=data_v, se.fit = TRUE)            
  y_pgPRISM2<- predict(GAM_PRISM2, newdata=data_v, se.fit = TRUE) 
  
  res_ypgA1<- data_v$tmax - y_pgANUSPLIN1$fit #Residuals for GMA model that resembles the ANUSPLIN interpolation
  res_ypgP1<- data_v$tmax - y_pgPRISM1$fit   #Residuals for GAM model that resembles the PRISM interpolation                               
  res_ypgP2<- data_v$tmax - y_pgPRISM2$fit  
  
  RMSE_ypgA1 <- sqrt(sum(res_ypgA1^2)/nv)          
  RMSE_ypgP1 <- sqrt(sum(res_ypgP1^2)/nv)
  RMSE_ypgP2 <- sqrt(sum(res_ypgP2^2)/nv)
  
  results[i,3]<-RMSE_ypgA1
  results[i,4]<-RMSE_ypgP1
  results[i,5]<-RMSE_ypgP2
  
  data_name<-paste("ghcn_v_",dates[[i]],sep="")
  assign(data_name,data_v)
  #ghcn_v<-ls(pattern="ghcn_v_")
  
  # end of the for loop #1
  }

## Plotting and saving diagnostic measures

results_num <-results
mode(results_num)<- "numeric"
# Make it numeric first
# Now turn it into a data.frame...

results_table<-as.data.frame(results_num)
colnames(results_table)<-c("dates","ns","RMSE_A1", "RMSE_P1","RMSE_P2", "AIC_A1", "AIC_P1", "AIC_P2", "GCV_A1", "GCV_P1", "GCV_P2", "Deviance_A1", "Deviance_P1", "Deviance_P2")

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




 
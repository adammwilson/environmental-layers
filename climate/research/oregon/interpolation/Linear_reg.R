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
infile1<-"ghcn_or_b_02122012_OR83M.csv"
path<-"C:/Data/Benoit/NCEAS/window_Oregon_data"
setwd(path)
#infile2<-"dates_interpolation_03012012.txt"  # list of 10 dates for the regression
infile2<-"dates_interpolation_03032012.txt"
prop<-0.3                                                                            #Proportion of testing retained for validation   
out_prefix<-"_03022012_r3"

#######START OF THE SCRIPT #############

###Reading the station data and setting up for models' comparison
ghcn<-read.csv(paste(path,"/",infile1, sep=""), header=TRUE)                            #The "paste" function concatenates the path and file name in common string. 
dates <-readLines(paste(path,"/",infile2, sep=""))

results <- matrix(1,length(dates),10)            #This is a matrix containing the diagnostic measures from the GAM models.
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date_==d)) #this creates a list of 10 subsets data

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
  GAM_PRISM1<-gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) + s (ASPECT)+ s(DISTOC), data=data_s)
  
  ####Regression part 3: Calculating and storing diagnostic measures
  
  results[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results[i,2]<- ns        #number of stations used in the training stage
  
  results[i,5]<- AIC (GAM_ANUSPLIN1)
  results[i,6]<- AIC (GAM_PRISM1)
  GCVA1<-GAM_ANUSPLIN1$gcv.ubre
  results[i,7]<- GCVA1
  GCVP1<-GAM_PRISM1$gcv.ubre
  results[i,8]<- GCVP1
  
  results[i,9]<-GAM_ANUSPLIN1$deviance
  results[i,10]<-GAM_PRISM1$deviance
  
  #####VALIDATION: Prediction checking the results using the testing data########
 
  y_pgANUSPLIN1<- predict(GAM_ANUSPLIN1, newdata=data_v, se.fit = TRUE) #Using the coeff to predict new values.
  y_pgPRISM1<- predict(GAM_PRISM1, newdata=data_v, se.fit = TRUE)            
           
  res_ypgA1<- data_v$tmax - y_pgANUSPLIN1$fit #Residuals for GMA model that resembles the ANUSPLIN interpolation
  res_ypgP1<- data_v$tmax - y_pgPRISM1$fit   #Residuals for GAM model that resembles the PRISM interpolation                               
                             
  RMSE_ypgA1 <- sqrt(sum(res_ypgA1^2)/nv)          
  RMSE_ypgP1 <- sqrt(sum(res_ypgP1^2)/nv)
  
  results[i,3]<-RMSE_ypgA1
  results[i,4]<-RMSE_ypgP1
  
  # end of the for loop #1
  }

## Plotting and saving diagnostic measures

results_num <-results
mode(results_num)<- "numeric"
# Make it numeric first
# Now turn it into a data.frame...

results_table<-as.data.frame(results_num)
colnames(results_table)<-c("dates","ns","RMSE_A1", "RMSE_P1", "AIC_A1", "AIC_P1", "GCV_A1", "GCV_P1", "Deviance_A1", "Deviance_P1")

win.graph()
barplot(results_table$RMSE_A1,main="RMSE for the A1 models",names.arg=results_table$dates,ylab="Temp (0.1 X deg. C)",xlab="interolated date")
savePlot(paste("GAM_ANUSPLIN1_RMSE",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$RMSE_P1,main="RMSE for the P1 models",names.arg=results_table$dates,ylab="Temp (0.1 X deg. C)",xlab="interolated date")
savePlot(paste("GAM_PRISM1_RMSE",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$AIC_P1,main="AIC for the P1 models",names.arg=results_table$dates,ylab="Temp (0.1 X deg. C)",xlab="interolated date")
savePlot(paste("GAM_PRISM1_RMSE",out_prefix,".emf", sep=""), type="emf")
win.graph()
barplot(results_table$AIC_A1,main="AIC for the A1 models",names.arg=results_table$dates,ylab="Temp (0.1 X deg. C)",xlab="interolated date")
savePlot(paste("GAM_PRISM1_RMSE",out_prefix,".emf", sep=""), type="emf")

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
# #results_val <-c(res_yplA1,res_ypgA1,res_yplP1,res_ypgP1)
# results_val<-data.frame(res_yplA1=res_yplA1,res_ypgA1=res_ypgA1,res_yplP1=res_yplP1,res_ypgP1=res_ypgP1)
# nv<- nrow(ghcn1507_v)
#                    
# mod_name<-c("yplA1","ypgA1","yplP1","ypgP1")
# mod_type<-c("lm_ANUSPLIN1","GAM_ANUSPLIN1","lm_PRISM1","GAM_PRISM1")
# 
# RMSE_all<-c(RMSE_yplA1,RMSE_ypgA1,RMSE_yplP1,RMSE_ypgP1)
# AIC_all<-AIC(lm_ANUSPLIN1,GAM_ANUSPLIN1,lm_PRISM1,GAM_PRISM1)
# GCV_all<-c(0,GCVA1,0,GCVP1)     #This places the GCV values for each model in a vector
# Deviance_all<-c(0,DevianceA1,0,DevianceP1)
# 
# #results<-data.frame(model=mod_name,RMSE=RMSE_all,df=AIC_all$df,AIC=AIC_all$AIC)
# #results_val<-data.frame(model=mod_name,RMSE=RMSE_all,df=AIC_all$df,AIC=AIC_all$AIC)  
# results<-data.frame(model=mod_name,RMSE=RMSE_all,df=AIC_all$df,AIC=AIC_all$AIC, GCV=GCV_all,Deviance=Deviance_all)
# 
# #Add deviance
# 
# barplot(results$RMSE,main="RMSE for the models",names.arg=c("yplA1","ypgA1","yplP1","ypgP1"),ylab="Temp (deg. C)")                  
# barplot(results$AIC,main="AIC for the models",names.arg=c("yplA1","ypgA1","yplP1","ypgP1"),ylab="AIC")
# #dump("results", file= paste(path,"/","results_reg1507.txt",sep=""))
# write.csv(results, file= paste(path,"/","results_reg1507_Assessment.txt",sep=""))
# #write.csv(results_val, file= paste(path,"/","results_reg1507_val.txt",sep=""))
#                 




 
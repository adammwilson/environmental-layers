####################Interpolation of Tmax for 10 dates.#####################
#This script interpolates station values for the Oregon case study. This program loads the station data from a csv file 
#and perform two types  of regression: multiple linear model and general additive model (GAM). Note that this program:
#1)assumes that the csv file is in the current working 
#2)extract relevant variables from raster images before performing the regressions. 
#The user must provide the list of raster images in  a textile.
#Script created by Benoit Parmentier on March 1, 2012. 

###Loading r library and packages                                                                       # loading the raster package
library(gtools)                                                                        # loading ...
library(mgcv)

###Parameters and arguments
infile1<-"ghcn_or_b_02122012_OR83M.csv"
path<-"C:/Data/Benoit/NCEAS/window_Oregon_data"
setwd(path)
infile2<-"dates_interpolation_03012012.txt"  # list of 10 dates
#dates<-"20101507"  # list of 10 dates in a textfile                                   #Date selected for the regression 
#prop<-0.3                                                                            #Proportion of testing retained for validation   

#######START OF THE SCRIPT #############

###Reading the station data
ghcn<-read.csv(paste(path,"/",infile1, sep=""), header=TRUE)                            #The "paste" function concatenates the path and file name in common string. 
#ghcn<-read.csv(infile1)                                                                #Use read.table if the input file is space delimited or read.table(infile1, headername=TRUE, sep=',')                                                                            #Checking that the columns are correctly labelled.
dates <-readLines(paste(path,"/",infile2, sep=""))
                  
###Creating a validation dataset by creating training and testing datasets (%30)
#ghcn1507 <-subset(ghcn,ghcn$date_== date)
#for loops
ddat <- as.list(rep("", length(dates)))

for(i in 1:length(dates)){
  data<-cat("ghcn_",dates[[1]],sep="")
  data<-as.data.frame(data)
  ddat[[i]] <- data.frame(subset(ghcn,ghcn$date_==dates[[i]]))
  #data<-subset(ghcn,ghcn$date_==dates[[i]])
  n<-nrow(data)
  ns<-n-round(n*0.3)  #Create a sample from the data frame with 70% of the rows
  #ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
  ind.training <- sample(nrow(data), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(data), ind.training)
  data_s <- data[ind.training, ]
  data_v <- data[ind.testing, ]
  }

# ############ REGRESSION ###############
# 
# ###Regression part 1: linear models
# 
# lm_ANUSPLIN1<-lm(tmax~lat+lon+ELEV_SRTM, data=ghcn1507_s)
# lm_PRISM1<-lm(tmax~lat+lon+ELEV_SRTM+ASPECT+DISTOC, data=ghcn1507_s) #Note that a variable on inversion is missing
# summary(lm_ANUSPLIN1)
# summary(lm_PRISM1)
# 
# ###Regression part 2: GAM models
# GAM_ANUSPLIN1<-gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM), data=ghcn1507_s)
# GAM_PRISM1<-gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) + s (ASPECT)+ s(DISTOC), data=ghcn1507_s)
# #use the s() for smoothing function
# 
# ###Compare the models
# #Show AIC, Cook distance, p values and residuals plot
# ###Access the R2 and significance to give a report
# 
# AIC (lm_ANUSPLIN1,GAM_ANUSPLIN1) #list the AIC and for the results
# AIC (lm_PRISM1,GAM_PRISM1) #list the AIC and for the results
# 
# GCVA1<-GAM_ANUSPLIN1$gcv.ubre
# GCVP1<-GAM_PRISM1$gcv.ubre
# 
# DevianceA1<-GAM_ANUSPLIN1$deviance
# DevianceP1<-GAM_PRISM1$deviance
# 
# anova(lm_ANUSPLIN1, GAM_ANUSPLIN1,test="F") #compare the different models in terms of F; a reference model should be set for comparison. 
# anova(lm_PRISM1, GAM_PRISM1,test="F")
# 
# summary(lm_ANUSPLIN1)$r.squared
# summary(GAM_ANUSPLIN1)$r.squared
# summary(lm_PRISM1)$r.squared                
# summary(GAM_PRISM1)$r.squared
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
# ####VALIDATION: Prediction checking the results using the testing data########
# y_plANUSPLIN1<- predict(lm_ANUSPLIN1, newdata=ghcn1507_v, se.fit = TRUE)
# y_pgANUSPLIN1<- predict(GAM_ANUSPLIN1, newdata=ghcn1507_v, se.fit = TRUE) #Using the coeff to predict new values.
# y_plPRISM1<- predict(lm_PRISM1, newdata=ghcn1507_v, se.fit = TRUE)
# y_pgPRISM1<- predict(GAM_PRISM1, newdata=ghcn1507_v, se.fit = TRUE)            
#          
# res_yplA1<- ghcn1507_v$tmax - y_plANUSPLIN1$fit
# res_ypgA1<- ghcn1507_v$tmax - y_pgANUSPLIN1$fit
# res_yplP1<- ghcn1507_v$tmax - y_plPRISM1$fit   #Residuals for lm model that resembles the PRISM interpolation
# res_ypgP1<- ghcn1507_v$tmax - y_pgPRISM1$fit   #Residuals for GAM model that resembles the PRISM interpolation                               
# 
# #results_val <-c(res_yplA1,res_ypgA1,res_yplP1,res_ypgP1)
# results_val<-data.frame(res_yplA1=res_yplA1,res_ypgA1=res_ypgA1,res_yplP1=res_yplP1,res_ypgP1=res_ypgP1)
# nv<- nrow(ghcn1507_v)
#                   
# RMSE_yplA1 <- sqrt(sum(res_yplA1^2)*1/nv)            
# #RMSE_ypgA1 <- sqrt(sum(res_ypgA1^2)/nv)
# RMSE_ypgA1 <- sqrt(sum(res_ypgA1^2)/nv)
# RMSE_yplP1 <- sqrt(sum(res_yplP1^2)/nv)            
# RMSE_ypgP1 <- sqrt(sum(res_ypgP1^2)/nv)
#       
# #Printing the RMSE values for the different models                  
# RMSE_yplA1             
# RMSE_ypgA1 
# RMSE_yplP1          
# RMSE_ypgP1
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
#                 
# library(lattice)                 
# g <- expand.grid(x = ghcn1507_v$lon, y = ghcn1507_v$lat)
# g$z <- y_plANUSPLIN1$fit
# wireframe(z ~ x * y, data = g)                   
# 
# #####End of script##########
# ###############################



 
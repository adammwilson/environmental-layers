####################Interpolation of Tmax for 10 dates.#####################
#This script interpolates station tmax values for the Oregon case study.It provides a mean to asssess the effect of random sampling and proportion
# of validation hold out on the RMSE.This program loads the station data from a csv file 
#and perform one type of regression:  general additive model (GAM) with different variables: 
# Lat, long, ELEV_SRTM, Eastness, Northness, DISTOC, mean_LST_monthly, Land Cover proportions.
#Note that this program:
#1)assumes that the csv file is in the current working 
#2)extract relevant variables from raster images before performing the regressions. 
#3)does not clear memory workspace at the start or end of script.
#This scripts predicts tmax using GAM and LST derived from MOD11A1.
#Interactions terms are also included and assessed using the RMSE from validation dataset.
#There are 10 dates used for the GAM interpolation. The dates must be provided as a textfile.
#Script created by Benoit Parmentier on April 25, 2012. 

###Loading r library and packages                                                      # loading the raster package
library(gtools)                                                                        # loading ...
library(mgcv)
library(sp)
library(spdep)
library(rgdal)

###Parameters and arguments

infile1<-"ghcn_or_tmax_b_04142012_OR83M.shp"
path<-"/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations"
#path<-"H:/Data/IPLANT_project/data_Oregon_stations"
setwd(path) 
infile2<-"dates_interpolation_03052012.txt"               #List of 10 dates for the regression
prop<-0.3  #Proportion of testing retained for validation   
n_runs<- 30 #Number of runs
out_prefix<-"_05142012_run03_LST"
infile3<-"LST_dates_var_names.txt"     #List of LST averages.
infile4<-"models_interpolation_05142012.txt"

#######START OF THE SCRIPT #############

###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)              #Removing the extension from file.
ghcn<-readOGR(".", filename)                  #reading shapefile 
                  
ghcn = transform(ghcn,Northness = cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness = sin(ASPECT))  #adding variable to the dataframe.
ghcn = transform(ghcn,Northness_w = sin(slope)*cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness_w = sin(slope)*sin(ASPECT))  #adding variable to the dataframe.
                                              #Note that "transform" output is a data.frame not spatial object 
#set.seed(100) #modify this to a seed variable allowing different runs.

dates <-readLines(paste(path,"/",infile2, sep=""))
LST_dates <-readLines(paste(path,"/",infile3, sep=""))
models <-readLines(paste(path,"/",infile4, sep=""))

#results <- matrix(1,length(dates),14)            #This is a matrix containing the diagnostic measures from the GAM models.

results_AIC<- matrix(1,length(dates),length(models)+3)  
results_GCV<- matrix(1,length(dates),length(models)+3)
results_DEV<- matrix(1,length(dates),length(models)+3)
results_RMSE<- matrix(1,length(dates),length(models)+3)
RMSE_run<-matrix(1,length(dates),1)
cor_LST_LC1<-matrix(1,length(dates),1)      #correlation LST-LC1
cor_LST_LC3<-matrix(1,length(dates),1)      #correlation LST-LC3
cor_LST_tmax<-matrix(1,length(dates),1)    #correlation LST-tmax
#Screening for bad values
#RMSE_run<-matrix(1,lenth)
results_RMSE_all<- matrix(1,length(dates)*n_runs,length(models)+3)
results_RMSE_all2<- matrix(1,0,length(models)+3)
ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$tmax>-150 & ghcn$tmax<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2

#month_var<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09", "mm_10", "mm_11", "mm_12")

## looping through the dates...
#Changed this section into  a nested loop, looping through the number of models

for(j in 1:n_runs){
  
  ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 subset data
  
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
    ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
    ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
    data_s <- ghcn.subsets[[i]][ind.training, ]
    data_v <- ghcn.subsets[[i]][ind.testing, ]
    
    ####Regression part 2: GAM models
    
    mod1<- gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM), data=data_s)
    mod2<- gam(tmax~ s(lat,lon,ELEV_SRTM), data=data_s)
    mod3<- gam(tmax~ s(lat) + s (lon) + s (ELEV_SRTM) +  s (Northness)+ s (Eastness) + s(DISTOC), data=data_s)
    mod4<- gam(tmax~ s(lat) + s (lon) + s(ELEV_SRTM) + s(Northness) + s (Eastness) + s(DISTOC) + s(LST), data=data_s)
    mod5<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST), data=data_s)
    mod6<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC1), data=data_s)
    mod7<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC3), data=data_s)
    mod8<- gam(tmax~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST) + s(LC1), data=data_s)
    
    ####Regression part 3: Calculating and storing diagnostic measures
    
    results_AIC[i,1]<- j
    results_AIC[i,2]<- dates[i]  #storing the interpolation dates in the first column
    results_AIC[i,3]<- ns        #number of stations used in the training stage
    results_AIC[i,4]<- AIC (mod1)
    results_AIC[i,5]<- AIC (mod2)
    results_AIC[i,6]<- AIC (mod3)
    results_AIC[i,7]<- AIC (mod4)
    results_AIC[i,8]<- AIC (mod5)
    results_AIC[i,9]<- AIC (mod6)
    results_AIC[i,10]<- AIC (mod7)
    
    results_GCV[i,1]<- j         # "j" is the counter index for the number of runs.
    results_GCV[i,2]<- dates[i]  #storing the interpolation dates in the first column
    results_GCV[i,3]<- ns        #number of stations used in the training stage
    results_GCV[i,4]<- mod1$gcv.ubre
    results_GCV[i,5]<- mod2$gcv.ubre
    results_GCV[i,6]<- mod3$gcv.ubre
    results_GCV[i,7]<- mod4$gcv.ubre
    results_GCV[i,8]<- mod5$gcv.ubre
    results_GCV[i,9]<- mod6$gcv.ubre
    results_GCV[i,10]<- mod7$gcv.ubre
    
    results_DEV[i,1]<- j         # "j" is the counter index for the number of runs.
    results_DEV[i,2]<- dates[i]  #storing the interpolation dates in the first column
    results_DEV[i,3]<- ns        #number of stations used in the training stage
    results_DEV[i,4]<- mod1$deviance
    results_DEV[i,5]<- mod2$deviance
    results_DEV[i,6]<- mod3$deviance
    results_DEV[i,7]<- mod4$deviance
    results_DEV[i,8]<- mod5$deviance
    results_DEV[i,9]<- mod6$deviance
    results_DEV[i,10]<- mod7$deviance
    
    #####VALIDATION: Prediction checking the results using the testing data########
    
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
    
    results_RMSE[i,1]<- j
    results_RMSE[i,2]<- dates[i]  #storing the interpolation dates in the first column
    results_RMSE[i,3]<- ns        #number of stations used in the training stage
    results_RMSE[i,4]<- RMSE_mod1
    results_RMSE[i,5]<- RMSE_mod2
    results_RMSE[i,6]<- RMSE_mod3
    results_RMSE[i,7]<- RMSE_mod4
    results_RMSE[i,8]<- RMSE_mod5
    results_RMSE[i,9]<- RMSE_mod6
    results_RMSE[i,10]<- RMSE_mod7
    #Saving dataset in dataframes
    data_name<-paste("ghcn_v_",dates[[i]],sep="")
    assign(data_name,data_v)
    data_name<-paste("ghcn_s_",dates[[i]],sep="")
    assign(data_name,data_s)
    #ghcn_v<-ls(pattern="ghcn_v_")
    # end of the for loop #2 (nested in loop #1)
  }

  results_RMSEnum <-results_RMSE
  results_AICnum <-results_AIC
  mode(results_RMSEnum)<- "numeric"
  mode(results_AICnum)<- "numeric"
  # Make it numeric first
  # Now turn it into a data.frame...
  
  results_table_RMSE<-as.data.frame(results_RMSEnum)
  results_table_AIC<-as.data.frame(results_AICnum)
  colnames(results_table_RMSE)<-c("run","dates","ns","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7")
  colnames(results_table_AIC)<-c("run","dates","ns","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7")
  
  write.table(results_table_RMSE, file= paste(path,"/","results_GAM_Assessment",out_prefix,".txt",sep=""), sep=",", col.names=FALSE, append=TRUE)
  #write.table(results_table_AIC, file= paste(path,"/","results_GAM_Assessment",out_prefix,".txt",sep=""),sep=",", append=TRUE)
  
  #results_RMSE_all Need to put all the results in one data.frame...
  results_RMSE_all2<-rbind(results_RMSE_all2,results_table_RMSE)
  #Print(j) #This is the run umber printed to the console.
} #end of loop 1

write.table(results_RMSE_all2, file= paste(path,"/","results_GAM_Assessment",out_prefix,"all.txt",sep=""), sep=",", col.names=TRUE)


###Analysing the results from the 365 days run: Summarize by month
# 
# for(i in 1:nrow(results_table_RMSE)){
#   date<-results_table_RMSE$dates[i]
#   date<-strptime(date, "%Y%m%d")
#   results_table_RMSE$month[i]<-as.integer(strftime(date, "%m"))
# }
# 
# average<-aggregate(cbind(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)~month,data=results_table_RMSE,mean, na.rm=TRUE)
# average<-aggregate(cbind(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)~month,data=results_table_RMSE, FUN=mean)
# #average on all the data.frame
# averaget<-aggregate(results_table_RMSE, by=list(results_table_RMSE$month),FUN=mean, na.rm=TRUE)
# #mediant<-aggregate(results_table_RMSE, by=list(results_table_RMSE$month),FUN=median, na.rm=TRUE)
# #average_lowt<-aggregate(results_table_RMSE, by=list(results_table_RMSE$month), FUN=function(v) t.test(v)$conf.int[1])
# #average_up<-aggregate(cbind(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)~month,data=results_table_RMSE, function(v) t.test(v)$conf.int[2])
# 
# median<-aggregate(cbind(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)~month,data=results_table_RMSE, median, na.rm=TRUE)
# average_low<-aggregate(cbind(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)~month,data=results_table_RMSE, function(v) t.test(v)$conf.int[1])
# average_up<-aggregate(cbind(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)~month,data=results_table_RMSE, function(v) t.test(v)$conf.int[2])
# 
# mod<-names(averaget)
# mod<-mod[4:11]
# #Saving graphic plots
# for(i in 1:length(mod)){
#   X11(width=14,height=10)
#   name<-mod[i]
#   barplot2(average[[name]],plot.ci=TRUE, ci.l=average_low[[name]], ci.u=average_up[[name]],main="Mean RMSE per month", names.arg=c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct", "Nov", "Dec"),ylim=c(20,30),ylab="RMSE in tenth deg C",xlab=name)
#   #title(paste("Sampling RMSE for mod",i,sep=""))
#   savePlot(paste("barplot_results_RMSE_month_",name,out_prefix,".png", sep=""), type="png")
#   dev.off() 
# }
# 
# 
# for(i in 1:length(mod)){
#   X11(width=14,height=10)
#   name<-mod[i]
#   barplot2(average[[name]],plot.ci=TRUE, ci.l=average_low[[name]], ci.u=average_up[[name]],main=paste(" Mean RMSE per month ",name, sep=""), names.arg=c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct", "Nov", "Dec"),ylim=c(20,30),ylab="RMSE in tenth deg C",xlab=name)
#   #title(paste("Sampling RMSE for mod",i,sep=""))
#   savePlot(paste("barplot_results_RMSE_month_",name,out_prefix,".png", sep=""), type="png")
#   dev.off() 
#   
#   X11(width=14,height=10)
#   name<-mod[i]
#   hist(results_table_RMSE[[name]],breaks=15, main=paste(" Histogram RMSE_",name, sep=""),xlab=paste("RMSE ",name, sep=""))
#   savePlot(paste("Hist_results_RMSE_365_",name,out_prefix,".png", sep=""), type="png")
#   dev.off()
#   
# }
# 
# for(i in 1:length(mod)){
#   X11(width=14,height=10)
#   name<-mod[i]
#   hist(results_table_RMSE[[name]],breaks=15, main=paste(" Histogram RMSE_",name, sep=""),xlab=paste("RMSE ",name, sep=""))
#   savePlot(paste("Hist_results_RMSE_365_",name,out_prefix,".png", sep=""), type="png")
#   dev.off()
# }
# 
# r<-(results_RMSE_all2[,4:10]) #selecting only the columns related to models...
# 
# mean_r<-sapply(r, mean)
# median_r<-sapply(r, median)
# sd_r<-sapply(r, sd)
# 
# barplot(mean_r,ylim=c(20,26),ylab="RMSE in tenth deg C")
# barplot(median_r,ylim=c(20,26),ylab="RMSE in tenth deg C",add=TRUE,inside=FALSE,beside=TRUE) # put both on the same plot
# barplot(sd_r,ylim=c(6,8),ylab="RMSE in tenth deg C") # put both on the same plot
# 
# height<-rbind(mean_r,median_r)
# barplot(height,ylim=c(20,26),ylab="RMSE in tenth deg C",beside=TRUE,legend=rownames(height))
# barplot(height,ylim=c(20,26),ylab="RMSE in tenth deg C",beside=TRUE, col=c("darkblue","red"),legend=rownames(height)) # put both on the same plot
# PNG(paste("Barplot_results_RMSE_sampling_",out_prefix,".png", sep=""))
# 
# barplot2(mean_r,median_r,ylim=c(23,26),ylab="RMSE in tenth deg C") # put both on the same plot
# #Collect var explained and p values for each var...
# 
# mod<-names(mean_r)
# average<-mean_r
# average_low<-mean_r-sd_r
# average_up<-mean_r+sd_r
# 
# for(i in 1:length(mod)){
#   #X11(width=14,height=10)
#   name<-mod[i]
#   barplot2(average[[name]],plot.ci=TRUE, ci.l=average_low[[name]], ci.u=average_up[[name]],main=paste(" Mean RMSE per month ",name, sep=""), names.arg=c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6","mod7"),ylim=c(20,30),ylab="RMSE in tenth deg C",xlab=name)
#   #title(paste("Sampling RMSE for mod",i,sep=""))
#   #savePlot(paste("barplot_results_RMSE_month_",name,out_prefix,".png", sep=""), type="png")
#   #dev.off()
#   }

# End of script##########

#Selecting dates and files based on names
#cor_LST_LC<-matrix(1,10,1)
# for(i in 1:length(dates)){
#   cor_LST_LC1[i]<-cor(ghcn.subsets[[i]]$LST,ghcn.subsets[[i]]$LC1)
# }
# for(i in 1:length(dates)){
#   cor_LST_LC3[i]<-cor(ghcn.subsets[[i]]$LST,ghcn.subsets[[i]]$LC3)
# }


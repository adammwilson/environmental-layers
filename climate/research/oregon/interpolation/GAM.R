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
library(multicore)  # if installed allows easy parallelization
library(reshape)    # very useful for switching from 'wide' to 'long' data formats

###Parameters and arguments

infile1<-"ghcn_or_ppt_covariates_20120705_OR83M.shp"
path<-"/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations"
#path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"
                                        #path<-"H:/Data/IPLANT_project/data_Oregon_stations"
path="/home/wilson/data"
setwd(path) 
infile2<-"dates_interpolation_03052012.txt"               #List of 10 dates for the regression
prop<-0.3  #Proportion of testing retained for validation   
n_runs<- 30 #Number of runs
out_prefix<-"_20120705_run01_LST"
infile3<-"LST_dates_var_names.txt"     #List of LST averages.
infile4<-"models_interpolation_05142012.txt"

#######START OF THE SCRIPT #############

###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)              #Removing the extension from file.
ghcn<-readOGR(".", filename)                  #reading shapefile 
                  
ghcn = transform(ghcn,Northness = cos(ASPECT*pi/180)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness = sin(ASPECT*pi/180))  #adding variable to the dataframe.
ghcn = transform(ghcn,Northness_w = sin(slope*pi/180)*cos(ASPECT*pi/180)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness_w = sin(slope*pi/180)*sin(ASPECT*pi/180))  #adding variable to the dataframe.
                                              #Note that "transform" output is a data.frame not spatial object 
#set.seed(100) #modify this to a seed variable allowing different runs.

 dates <-readLines(paste(path,"/",infile2, sep=""))
LST_dates <-readLines(paste(path,"/",infile3, sep=""))
models <-readLines(paste(path,"/",infile4, sep=""))

#results <- matrix(1,length(dates),14)            #This is a matrix containing the diagnostic measures from the GAM models.

####  Define GAM models
var="tmax"

mods=data.frame(formula=c(    
                  paste(var,"~ s(lat) + s (lon) + s (ELEV_SRTM)",sep=""),
                  paste(var,"~ s(lat,lon,ELEV_SRTM)",sep=""),
                  paste(var,"~ s(lat) + s (lon) + s (ELEV_SRTM) +  s (Northness)+ s (Eastness) + s(DISTOC)",sep=""),
                  paste(var,"~ s(lat) + s (lon) + s(ELEV_SRTM) + s(Northness) + s (Eastness) + s(DISTOC) + s(LST)",sep=""),
                  paste(var,"~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST)",sep=""),
                  paste(var,"~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC1)",sep=""),
                  paste(var,"~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST,LC3)",sep=""),
                  paste(var,"~ s(lat,lon) +s(ELEV_SRTM) + s(Northness,Eastness) + s(DISTOC) + s(LST) + s(LC1)",sep="")
                  ),stringsAsFactors=F)
mods$model=paste("mod",1:nrow(mods),sep="")


### subset dataset?
ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$tmax>-150 & ghcn$tmax<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2


## loop through the dates...
  
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 subset data
  
 results=do.call(rbind.data.frame,                   # Collect the results in a single data.frame
   mclapply(1:length(dates),function(i) {            # loop over dates
     date<-strptime(dates[i], "%Y%m%d")              # get date
     month<-strftime(date, "%m")                     # get month
     LST_month<-paste("mm_",month,sep="")            # get LST_month name

     ##Regression part 1: Creating a validation dataset by creating training and testing datasets
     mod <-ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]
     ghcn.subsets[[i]] = transform(ghcn.subsets[[i]],LST = mod)
     ##Screening LST values
     ##ghcn.subsets[[i]]<-subset(ghcn.subsets[[i]],ghcn.subsets[[i]]$LST> 258 & ghcn.subsets[[i]]$LST<313)
     n<-nrow(ghcn.subsets[[i]])
     ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
     nv<-n-ns             #create a sample for validation with prop of the rows
     ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
     ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
     data_s <- ghcn.subsets[[i]][ind.training, ]
     data_v <- ghcn.subsets[[i]][ind.testing, ]
     
     ## lapply loops through models for the ith day, calculates the validation metrics, and saves them as R objects
     results=do.call(rbind.data.frame,
       lapply(1:nrow(mods),function(m,savemodel=F,saveFullPrediction=T) {  
         ## run the model
         mod=gam(formula(mods$formula[m]),data=data_s)
         
         ##VALIDATION: Prediction checking the results using the testing data########
         y_mod<- predict(mod, newdata=data_v, se.fit = TRUE) #Using the coeff to predict new values.
         res_mod<- data_v$tmax - y_mod$fit #Residuals for GMA model that resembles the ANUSPLIN interpolation
         
         ##Regression part 3: Calculating and storing diagnostic measures
         tresults=data.frame(            # build 1-row dataframe for this model-date
           date=dates[i],                # interpolation date
           model=mods$model[m],          # model number
           ns=ns,                        # number of stations used in the training stage
           AIC=AIC(mod),                # AIC
           GCV=mod$gcv.ubre,             # GCV
           DEV=mod$deviance,             # Deviance
           RMSE=sqrt(sum(res_mod^2)/nv)  # RMSE
           )
         
         ## Save the model object if desired
         if(savemodel)  save(mod,file= paste(path,"/","results_GAM_Model","_",out_prefix,"_",dates[i],".Rdata",sep=""))

         ## do the full prediction and save it if desired
         if(saveFullPrediction){
           s_raster=readRaster(filename=paste(path,"covariates.gri"))
           predict(s_raster,mod,filename=paste(outpath,"_",sub("-","",date),"_prediction.tif",sep=""),format="GTiff",progress="text",fun="predict")
           predict(s_raster,mod,filename=paste(outpath,"_",sub("-","",date),"_prediction.se.tif",sep=""),format="GTiff",progress="text",fun="predict.se")
         }
         
         ## print progress
         print(paste("Finshed Model:",mods$model[m]," for Date:",dates[i]))
         ## return the results table
         return(tresults)
                                        # end of the for loop #2 (nested in loop #1)
       }))
     print(paste("Finshed Date:",dates[i]))
     return(results)
   }
            ))



  write.table(results_RMSE_all2, file= paste(path,"/","results_GAM_Assessment",out_prefix,"all.txt",sep=""), sep=",", col.names=TRUE)

resl=melt(results,id=c("run","date","model","ns"))

  xyplot(value~date|variable,groups=model,data=resl,scales=list(y=list(relation="free")),auto.key=list()

  

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


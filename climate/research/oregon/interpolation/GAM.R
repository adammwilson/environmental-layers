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
library(lattice); library(latticeExtra)
library(raster)

###Parameters and arguments

infile1<-"station_monthly_PRCP_covariates_20120705.shp"
monthly=T   # indicate if these are monthly or daily data

path<-"/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations"
path<-"/home/adamw/acrobates/projects/interp/data/regions/oregon"                                 #Path to all datasets on Atlas

                                        #path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"
                                        #path<-"H:/Data/IPLANT_project/data_Oregon_stations"
setwd(path) 
out_prefix<-"_20120713_precip"
prop=0.7                                # proportion of data used for fitting

##################################
## Load covariate raster brick
covar=brick(paste(path,"/covariates.gri",sep=""))
## udpate layer names (deleted in save)
covar=setZ(covar,scan(paste(path,"/covariates-Z.csv",sep=""),what="char"),name="month")

#######START OF THE SCRIPT #############

###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)              #Removing the extension from file.
ghcn<-readOGR(".", filename)                  #reading shapefile 

                                              #Note that "transform" output is a data.frame not spatial object 
#set.seed(100) #modify this to a seed variable allowing different runs.

#dates <-readLines(paste(path,"/",infile2, sep=""))
#LST_dates <-readLines(paste(path,"/",infile3, sep=""))
#models <-readLines(paste(path,"/",infile4, sep=""))

if(monthly)  ghcn$date=as.Date(paste(2000,ghcn$month,15,sep="-"))  # generate dates for monthly (climate) data
dates=unique(ghcn$date)
#results <- matrix(1,length(dates),14)            #This is a matrix containing the diagnostic measures from the GAM models.


####  Define GAM models
mods=data.frame(formula=c(    
                  "value ~ s(x_OR83M,y_OR83M)",
                  "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev",
                  "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev + ns + ew",
                  "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev + ns + ew + s(CER_mean)",
                  "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev + ns + ew + s(CER_P20um)",
                  "value ~ s(x_OR83M,y_OR83M) + elev + ns + ew + s(CER_P20um)",
                  "value ~ s(x_OR83M,y_OR83M,CER_P20um) + elev + ns + ew",
                  "value ~ s(x_OR83M,y_OR83M) + s(distoc,CER_P20um)+elev + ns + ew",
                  "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev + ns + ew + s(CLD_mean)",
                  "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev + ns + ew + s(COT_mean)"
                  ),stringsAsFactors=F)
mods$model=paste("mod",1:nrow(mods),sep="")

## confirm all model terms are in covar raster object for prediction
terms=gsub("[ ]|s[(]|[)]","",                      # clean up model terms (remove "s(", etc.)
  unique(do.call(c,                                # find unique terms
                 lapply(mods$formula,function(i)   #get terms from all models & split smoothed terms
                        unlist(strsplit(attr(terms(as.formula(i)),"term.labels"),split=","))))))
if(any(terms%in%layerNames(covarm)))
  print("All model terms are present in the raster object") else
warning("Some model terms not present in raster object, prediction may fail for some models")

### define transformation functions
     ##Transform response?
transform=T
if(transform) {
  trans=function(x) log(x+1)
  itrans=function(x) exp(x)-1
}
if(!transform) {
  trans=function(x) x
  itrans=function(x) x
}       


### subset dataset?
#sdata_all<-sdata
#sdata_test<-subset(sdata,sdata$tmax>-150 & sdata$tmax<400)
#sdata_test2<-subset(sdata_test,sdata_test$ELEV_SRTM>0)
#sdata<-sdata_test2


## loop through the dates...
  
ghcn.subsets <-lapply(dates, function(d) subset(ghcn@data, date==d)) #this creates a list of 10 subset data
  
 results=do.call(rbind.data.frame,                   # Collect the results in a single data.frame
   lapply(1:length(dates),function(i) {            # loop over dates
     date<-dates[i]                                  # get date
     month<-strftime(date, "%m")                     # get month
#     LST_month<-paste("mm_",month,sep="")            # get LST_month name

     ## subset full raster brick to include correct month of satellite data
     covarm=subset(covar,subset=which(covar@z$month=="00"|covar@z$month==month))
     ## update layer names to match those in ghcn table
     layerNames(covarm)[getZ(covarm)!="00"]=sub(paste("_",month,sep=""),"",layerNames(covarm)[getZ(covarm)!="00"])

     ## extract subset of data for this day
     tdata=ghcn.subsets[[i]]
     ##Regression part 1: Creating a validation dataset by creating training and testing datasets
#     mod <-ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]
#     ghcn.subsets[[i]] = transform(ghcn.subsets[[i]],LST = mod)
     ##Screening LST values
     ##ghcn.subsets[[i]]<-subset(ghcn.subsets[[i]],ghcn.subsets[[i]]$LST> 258 & ghcn.subsets[[i]]$LST<313)

     ## transform the response
     tdata$value<-trans(tdata$value)
     tdata$weights=tdata$count/max(tdata$count)

     n<-nrow(tdata)
     ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
     nv<-n-ns             #create a sample for validation with prop of the rows
     ind.training <- sample(nrow(tdata), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
     ind.testing <- setdiff(1:nrow(tdata), ind.training)
     data_s <- tdata[ind.training, ]
     data_v <- tdata[ind.testing, ]
     
     ## lapply loops through models for the ith day, calculates the validation metrics, and saves them as R objects
     results=do.call(rbind.data.frame,
       lapply(1:nrow(mods),function(m,savemodel=F,saveFullPrediction=F) {  
         ## will gam() fail?  If so, return NAs instead of crashing 
         err=try(gam(formula(mods$formula[m]),data=data_s),silent=T)
         if(length(attr(err,"class"))==1) if(attr(err,"class")=="try-error")
           return(## this table must match the results table below
                  data.frame(date=dates[i],month=format(dates[i],"%m"),
                             model=mods$model[m],form=mods$formula[m],ns=ns,
                             AIC=NA, GCV=NA,DEV=NA,RMSE=NA,R2=NA))
         ## run the models

         mod=gam(formula(mods$formula[m]),data=data_s,weights=data_s$weights)
         
         ##VALIDATION: Prediction checking the results using the testing data   ########
         y_mod<- predict(mod, newdata=data_v, se.fit = TRUE) #Using the coeff to predict new values.
         res_mod<- itrans(data_v$value) - itrans(y_mod$fit) #Residuals 
         plot(itrans(y_mod$fit)~itrans(data_v$value),cex=data_v$weights);abline(0,1)

         ##Regression part 3: Calculating and storing diagnostic measures
         tresults=data.frame(            # build 1-row dataframe for this model-date
           date=dates[i],                # interpolation date
           month=format(dates[i],"%m"),                # interpolation month
           model=mods$model[m],          # model number
           form=mods$formula[m],          # model number
           ns=ns,                        # number of stations used in the training stage
           AIC=AIC(mod),                # AIC
           GCV=mod$gcv.ubre,             # GCV
           DEV=mod$deviance,             # Deviance
           RMSE=sqrt(sum(res_mod^2,na.rm=T)/nv),  # RMSE
           R2=summary(lm(itrans(y_mod$fit)~itrans(data_v$value)))$r.squared  # R^2
           )

      
         ## Save the model object if desired
         if(savemodel)  save(mod,file= paste(path,"/","results_GAM_Model","_",out_prefix,"_",
                                   dates[i],"_",mods$model[m],".Rdata",sep=""))

         ## do the full prediction and save it if desired
         if(saveFullPrediction){
           p1=predict(covarm,mod,progress="text",fun="predict")
           p1=predict(mod,sdata@data,type="response",progress="text",fun="predict")

           predict(covarm,mod,type="response",
                   filename=paste(outpath,"/",gsub("-","",date),"_",mods$model[m],"_prediction.tif",sep=""),
                   format="GTiff",progress="text",fun="predict",overwrite=T)
           predict(covarm,mod,filename=paste(outpath,"/",gsub("-","",date),"_",mods$model[m],"_prediction.se.tif",sep=""),
                   format="GTiff",progress="text",fun="predict.se",overwrite=T)
         }
         
         ## print progress
         print(paste("Finshed Model:",mods$model[m]," for Date:",dates[i]))
         ## return the results table
         return(tresults)
                                        # end of the for loop #2 (nested in loop #1)
       }))
     ## identify model with minium AIC
     results$lowAIC=F
     results$lowAIC[which.min(results$AIC)]=T

     ## print progress and return the results
     print(paste("Finshed Date:",dates[i]))
     return(results)
   }
            ))



  write.table(results_RMSE_all2, file= paste(path,"/","results_GAM_Assessment",out_prefix,"all.txt",sep=""), sep=",", col.names=TRUE)

resl=melt(results,id=c("date","month","model","form","ns","lowAIC"))

xyplot(value~date|variable,groups=form,data=resl,scales=list(y=list(relation="free")),auto.key=T)

xyplot(form~value|variable,groups=month,data=resl,scales=list(x=list(relation="free")),auto.key=T)

bwplot(form~value|variable,data=resl,scales=list(x=list(relation="free")))

resl2=cast(resl,date+month+variable~model)
xyplot(mod5~mod6|variable,groups=month,data=resl2,scales=list(relation="free"))+layer(panel.abline(0,1))

  

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


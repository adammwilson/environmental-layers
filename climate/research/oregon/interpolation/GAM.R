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
library(raster); library(rasterVis)

###Parameters and arguments

infile1<-"station_monthly_PRCP_covariates_20120705.shp"
monthly=T   # indicate if these are monthly or daily data

path<-"/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations"
path<-"/home/adamw/acrobates/projects/interp/data/regions/oregon"                                 #Path to all datasets on Atlas

                                        #path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"
                                        #path<-"H:/Data/IPLANT_project/data_Oregon_stations"
setwd(path) 
out_prefix<-"20120723PRCP"
prop=0.7                                # proportion of data used for fitting

### load ROI
roi <- readOGR("../Test_sites/","Oregon")
roi=spTransform(roi,CRS("+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs"))


##################################
## Load covariate raster brick
covar=brick(paste(path,"/covariates.gri",sep=""))
## udpate layer names (deleted in save)
covar=setZ(covar,scan(paste(path,"/covariates-Z.csv",sep=""),what="char"),name="month")

#######START OF THE SCRIPT #############

###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)              #Removing the extension from file.
ghcn<-readOGR(".", filename)                  #reading shapefile
ghcn@data[,c("x","y")]=coordinates(ghcn)

                                              #Note that "transform" output is a data.frame not spatial object 
#set.seed(100) #modify this to a seed variable allowing different runs.

#dates <-readLines(paste(path,"/",infile2, sep=""))
#LST_dates <-readLines(paste(path,"/",infile3, sep=""))
#models <-readLines(paste(path,"/",infile4, sep=""))

if(monthly)  ghcn$date=as.Date(paste(2000,ghcn$month,15,sep="-"))  # generate dates for monthly (climate) data
dates=unique(ghcn$date)
#results <- matrix(1,length(dates),14)            #This is a matrix containing the diagnostic measures from the GAM models.


####  Define GAM models
mods=data.frame(
  formula=c(    
    "value ~ s(x_OR83M,y_OR83M)", 
#    "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev",
    "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev + ns + ew",
    "value ~ s(distoc) + s(CER_P20um) + elev + ns + ew",
    "value ~ s(distoc) + s(COT_mean) + elev + ns + ew",
    "value ~ s(distoc) + s(CLD_mean) + elev + ns + ew",
    "value ~ s(CLD_mean) + elev + ns + ew",
    "value ~ s(COT_mean) + elev + ns + ew",
    "value ~ s(CER_P20um) + elev + ns + ew",
    "value ~ s(CER_mean) + elev + ns + ew"
                                        #    "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev + ns + ew + s(CER_P20um)",
#    "value ~ s(x_OR83M,y_OR83M,CER_P20um) +s(x_OR83M,y_OR83M,CLD_mean) + elev + ns + ew",
#    "value ~ s(x_OR83M,y_OR83M) + s(CER_P20um,CLD_mean) + elev + ns + ew",
#    "value ~ x_OR83M + y_OR83M +s(CER_P20um) + elev + ns + ew",
#    "value ~ s(x_OR83M,y_OR83M,CER_P20um) + elev + ns + ew",
#    "value ~ s(x_OR83M,y_OR83M) + s(distoc,CER_P20um)+elev + ns + ew"
#    "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev + ns + ew + s(CLD_mean)",
#    "value ~ s(x_OR83M,y_OR83M) + s(distoc) + elev + ns + ew + s(COT_mean)"
    ),stringsAsFactors=F)
mods$model=paste("mod",1:nrow(mods),sep="")

## confirm all model terms are in covar raster object for prediction
terms=gsub("[ ]|s[(]|[)]","",                      # clean up model terms (remove "s(", etc.)
  unique(do.call(c,                                # find unique terms
                 lapply(mods$formula,function(i)   #get terms from all models & split smoothed terms
                        unlist(strsplit(attr(terms(as.formula(i)),"term.labels"),split=","))))))
if(any(terms%in%layerNames(covar)))
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
i=1
m=3
savemodel=T;saveFullPrediction=T;scale=F;verbose=T

ghcn.subsets <-lapply(dates, function(d) subset(ghcn@data, date==d)) #this creates a list of 10 subset data
  
  results=do.call(rbind.data.frame,                   # Collect the results in a single data.frame
   lapply(1:length(dates),function(i,savemodel=T,saveFullPrediction=T,scale=F,verbose=T) {            # loop over dates
     if(verbose)      print(paste("Starting Date:",dates[i]))
     date<-dates[i]                                  # get date
     month<-strftime(date, "%m")                     # get month
     ## extract subset of data for this day
     tdata=ghcn.subsets[[i]]
     ## drop stations with no x coordinates (these are stations in the buffer region)
     tdata=tdata[!is.na(tdata$x_OR83M),]  ## REMOVE THIS WHEN FULL REGION IS INCLUDED
     ## transform the response
     tdata$value<-trans(tdata$value)
     tdata$weights=tdata$count/max(tdata$count)

     #######
     if(saveFullPrediction){
       if(verbose) print(paste("Extracting covar information for this date (",dates[i],")"))
 
       ## subset full raster brick to include correct month of satellite data
       covarm=subset(covar,subset=which(covar@z$month=="00"|covar@z$month==month))
       covarm=setZ(covarm,getZ(covar)[which(covar@z$month=="00"|covar@z$month==month)])
       ## update layer names to match those in ghcn table
       layerNames(covarm)[getZ(covarm)!="00"]=sub(paste("_",month,sep=""),"",layerNames(covarm)[getZ(covarm)!="00"])
       ## convert to matrix for prediction later
       pred=extract(covarm,coordinates(covarm))
     }
     

     if(scale){
       ## get list of variables that need to be scaled to assess relative importance
       scalevars=!colnames(tdata)%in%c("station","month","count",
                                       "value","latitude","longitude","date","weights")
       tdata[,colnames(tdata)[scalevars]]=scale(tdata[,scalevars])
     }


     ## Create subsets for fitting and validation
     n = nrow(tdata)
     ns = round(n*prop)  #Create a sample from the data frame with 70% of the rows
     nv = n-ns             #create a sample for validation with prop of the rows
     ind.training = sample(nrow(tdata), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
     tdata$fit=F;tdata$fit[ind.training]=T   #add column to tdata with fit/training status
     tdata$class=as.factor(ifelse(tdata$fit,"Fitting","Validation"))


     ## lapply loops through models for the ith day, fits models, calculates the validation metrics, and saves them as R objects (if desired)
     if(verbose) print(paste("Starting the model fitting for this date (",dates[i],")"))
     results=do.call(rbind.data.frame,
       lapply(1:nrow(mods),function(m,data=tdata[tdata$fit],...) {
         if(verbose) print(paste("Fitting Model:",mods$model[m]))
         ## will gam() fail?  If so, return NAs instead of crashing 
         err=try(gam(formula(mods$formula[m]),data=tdata[tdata$fit,]),silent=T)
         if(length(attr(err,"class"))==1) if(attr(err,"class")=="try-error")
           return(## this table must match the results table below
                  data.frame(date=dates[i],month=format(dates[i],"%m"),
                             model=mods$model[m],form=mods$formula[m],ns=ns,
                             AIC=NA, GCV=NA,DEV=NA,RMSE=NA,R2=NA))

         ## define the spatial covariance structure
#         library(geoR)
#         plot(variog(coords=as.matrix(data_s[,c("x","y")]),data=tdata$value[tdata$fit]))
#         plot(variofit(variog(coords=as.matrix(tdata[tdata$fit,c("x","y")]),data=tdata$value[tdata$fit]),cov.model="exponential"))
#         vg=likfit(geodata=list(coords=tdata[tdata$fit,c("x","y")],data=tdata$value[tdata$fit]),cov.model="exponential",ini.cov.pars=expand.grid(seq(0.1,2,len=5),seq(1000,500000,len=5)))
#         lines.variomodel(vg)
         corr=corExp(c(100000,0.1),form=~x+y,nugget=T,fixed=F)
#         corr=Initialize(corr,tdata[tdata$fit,])

         ## run the model
         mod=gamm(formula(mods$formula[m]),data=tdata[tdata$fit,],correlation=corr,weights=weights,method="ML")
#         mod=gam(formula(mods$formula[m]),data=tdata[tdata$fit,],weights=tdata$weights[tdata$fit])


         ### generate knots
#         knots=makegrid(bbox(ghcn),cellsize=100000)
#         coordinates(knots)=c("x1","x2")
#         proj4string(knots)=proj4string(ghcn)
#        niter=1500
#         t1=Sys.time()
#         f1=formula(value~CER_P20um+elevation+ns+ew)
#         m1=spLM(f1,data=tdata[tdata$fit,],coords=as.matrix(tdata[tdata$fit,c("x","y")]), 
#           starting=list("phi"=2.5,"sigma.sq"=4, "tau.sq"=1,"nu"=0.5),
#           sp.tuning=list("phi"=0.8, "sigma.sq"=0.1, "tau.sq"=0.2,"nu"=0.5),
#           priors=list("phi.Unif"=c(0.1, 10), "sigma.sq.IG"=c(2, 1),
#             "tau.sq.IG"=c(2, 1),"nu.unif"=c(0.01,3)),
#           cov.model="matern",
#           knots=coordinates(knots),
#           n.samples=niter, verbose=TRUE, n.report=100,sub.samples=c(500,niter,1))
#         t2=Sys.time()
#         y_mod<- spPredict(m1, tdata[,c("x","y")],pred.covars=model.matrix(lm(f1,data=tdata))) #Using the coeff to predict new values.
#         y_mod<- spPredict(m1, pred[,c("x_OR83M","y_OR83M")],pred.covars=model.matrix(lm(f1,data=data.frame(value=0,pred)))) #Using the coeff to predict new values.
#         y_mod$fit=apply(y_mod$y.pred,1,mean)
#         tdata$pred=y_mod$fit
         
         ##VALIDATION: Prediction checking the results using the testing data   ########
         tpred=predict(mod$gam, newdata=tdata, se.fit = TRUE) #Using the coeff to predict new values.
         tdata$pred=as.vector(tpred$fit)
         tdata$pred.se=as.vector(tpred$se)

#         y_mod$fit[y_mod$fit<0]=NA
         tdata$resid<- itrans(tdata$value) - itrans(tdata$pred) #Residuals 

         ##Regression part 3: Calculating and storing diagnostic measures
         tresults=data.frame(            # build 1-row dataframe for this model-date
           date=dates[i],                # interpolation date
           month=format(dates[i],"%m"),                # interpolation month
           model=mods$model[m],          # model number
           form=mods$formula[m],          # model number
           ns=ns,                        # number of stations used in the training stage
#           AIC=AIC(mod$gam),                # AIC
#           GCV=mod$gam$gcv.ubre,             # GCV
#           DEV=mod$gam$deviance,             # Deviance
           ME=mean(tdata$resid[!tdata$fit],na.rm=T),  # Mean error
           MAE=mean(abs(tdata$resid[!tdata$fit]),na.rm=T),  # Mean absolute error
           pME=mean((itrans(tdata$pred[!tdata$fit])/itrans(tdata$value[!tdata$fit]))[itrans(tdata$pred[!tdata$fit])/itrans(tdata$value[!tdata$fit])<Inf],na.rm=T),  # Mean % of total
           RMSE=sqrt(sum(tdata$resid[!tdata$fit]^2,na.rm=T)/nv),  # RMSE
           R2=summary(lm(itrans(tdata$pred[!tdata$fit])~itrans(tdata$value[!tdata$fit])))$r.squared,  # R^2
           R2w=summary(lm(itrans(tdata$pred[!tdata$fit])~itrans(tdata$value[!tdata$fit]),weights=tdata$weights[!tdata$fit]))$r.squared,
           R2lw=summary(lm(tdata$pred[!tdata$fit]~tdata$value[!tdata$fit],weights=tdata$weights[!tdata$fit]))$r.squared)
              if(verbose) print(tresults)
         write.csv(tresults,file=paste(path,"/",out_prefix,"_",gsub("-","",date),"_",mods$model[m],".csv",sep=""))
      
         ## Save the model object if desired
         if(savemodel)  save(mod,file= paste(path,"/",out_prefix,"_",gsub("-","",date),"_",mods$model[m],".Rdata",sep=""))

         ## do the full prediction and save it if desired
         if(saveFullPrediction){
           if(verbose) print(paste("Doing predictions for model ",mods$model[m]," for this date (",dates[i],")"))
           ## do the prediction
           p1=predict(mod$gam,as.data.frame(pred),type="response",se.fit=T,block.size=10000)
           ## convert to raster stack (mean and se)
           p2=stack(SpatialPixelsDataFrame(coordinates(covarm),
             data=data.frame(
               pred=itrans(as.numeric(p1$fit)),
               se=itrans(as.numeric(p1$se.fit)))))
          
            writeRaster(p2,filename=paste(path,"/",out_prefix,"_",gsub("-","",date),"_",
                            mods$model[m],"_prediction.tif",sep=""),
                     format="GTiff",overwrite=T)
           ## draw map of full prediction
            ncols=100
           c.at=unique(quantile(ghcn$value,seq(0,1,len=ncols)))  #get quantile bins of raw data for all months
           c.ramp=colorRampPalette(c("brown","grey","lightgreen","darkgreen"))  #assign color ramp
           c.cols=c.ramp(length(c.at)-1)  #get ncol-1 colors to corresponding to c.at
           ## define se colors
           s.at=unique(quantile(subset(p2,"se"),seq(0,1,len=ncols),na.rm=T))  #get quantile bins of raw data for all months
           s.ramp=colorRampPalette(c("blue","red"))  #assign color ramp
           s.cols=s.ramp(length(s.at)-1)  #get ncol-1 colors to corresponding to c.at
           ## define residual color maps
           r.at=c(-Inf,-20,-5,-1,1,5,20,Inf)
           r.ramp=colorRampPalette(c("blue","darkblue","grey","darkred","red"))
           r.cols=r.ramp(length(r.at)-1)
           r.cut=cut(tdata$resid,breaks=r.at)
           ## plot the estimated values
           pdf(paste(path,"/",gsub("-","",date),"_",mods$model[m],"_prediction.pdf",sep=""),width=11,height=8.5)

           print(levelplot(subset(p2,"pred"),col.regions=c.ramp(length(c.at)),at=c.at,main=paste("Mean Predictions  (Month",m,")"),
                           sub=paste("Model:",mods$formula[m],"\n  Circles indicate training stations, triangles indicate validation stations"),margin=F)+
                 layer(panel.points(tdata$x_OR83M,tdata$y_OR83M,
                                    fill=as.character(cut(itrans(tdata$value),breaks=c.at,labels=c.cols)),col="black",
                                    pch=21,cex=tdata$weights),data=list(tdata=tdata[tdata$fit,],c.at=c.at,c.cols=c.cols))+
                 layer(panel.points(tdata$x_OR83M,tdata$y_OR83M,
                                    fill=as.character(cut(itrans(tdata$value),breaks=c.at,labels=c.cols)),col="black",pch=24,cex=tdata$weights),
                       data=list(tdata=tdata[!tdata$fit,],c.cols=c.cols,c.at=c.at))+
                 layer(sp.lines(as(roi,"SpatialLinesDataFrame"),col="black")))
           ## plot of prediction errors
           print(levelplot(subset(p2,"se"),col.regions=s.ramp(length(s.at)),at=s.at,main=paste("Prediction Standard Errors (Month",m,")"),
                     sub=paste("Model:",mods$formula[m],"\n  Circles indicate training stations, triangles indicate validation stations"),margin=F)+
                       layer(panel.points(tdata[,c("x_OR83M","y_OR83M")],fill=as.character(cut(itrans(tdata$pred.se),breaks=s.at,labels=s.cols)),col="black",pch=21),
                             data=list(tdata=tdata[tdata$fit,],s.at=s.at,s.cols=s.cols))+
               layer(panel.points(tdata[,c("x_OR83M","y_OR83M")],fill=as.character(cut(itrans(tdata$pred.se),breaks=s.at,labels=s.cols)),col="black",pch=24),
                     data=list(tdata=tdata[!tdata$fit,],s.cols=s.cols,s.at=s.at))+
                 layer(sp.lines(as(roi,"SpatialLinesDataFrame"),col="black")))
           ## plot of residuals
           print(levelplot(subset(p2,"pred"),col.regions=c.ramp(length(c.at)),at=c.at,main=paste("Mean Predictions with station residuals (month",m,")"),
                     sub=paste("Model:",mods$formula[m],"\n  Circles indicate training stations, triangles indicate validation stations"),margin=F,
                     key=list(text=list(rev(levels(r.cut)),col=rev(r.cols)),points=list(pch=21,fill=rev(r.cols)),space="left",title="Station \n Residuals"))+
             layer(panel.points(tdata[,c("x_OR83M","y_OR83M")],
                                fill=as.character(cut(itrans(tdata$resid),breaks=r.at,labels=r.cols)),col="black",pch=21,cex=tdata$weights),
                                        data=list(tdata=tdata[tdata$fit,],r.cols=r.cols,r.at=r.at))+
               layer(panel.points(tdata[,c("x_OR83M","y_OR83M")],
                                  fill=as.character(cut(itrans(tdata$resid),breaks=r.at,labels=r.cols)),col="black",pch=24,cex=tdata$weights),
                     data=list(tdata=tdata[!tdata$fit,],r.cols=r.cols,r.at=r.at))+
                 layer(sp.lines(as(roi,"SpatialLinesDataFrame"),col="black")))

          #         pdata=data.frame(Predicted=apply(itrans(as.matrix(y_mod$fit)),1,function(x) max(x,0)),Observed=itrans(data_v$value))
           print(xyplot(itrans(pred)~itrans(value),groups=class,ylab="Predicted",xlab="Observed",main=mods$formula[m],data=tdata,asp=.5,auto.key=T)+layer(panel.abline(0,1,col="red")))
 #          xyplot(pred~value,groups=class,ylab="Predicted",xlab="Observed",main=mods$formula[m],data=tdata,asp=.5,auto.key=T)+layer(panel.abline(0,1,col="red"))
           plot(mod$gam,pages=1,residuals=T,scale=-1,pers=T,shade=T,seWithMean=T); mtext(mods$formula[m],3,padj=-2)
           dev.off()
           
         }
         
         ## print progress
         print(paste("Finshed Model:",mods$model[m]," for Date:",dates[i]))
         ## return the results table
         return(tresults)
                                        # end of the for loop #2 (nested in loop #1)
       }))  #end of loop for this time step
     
     ## identify model with minimum AIC
     results$lowAIC=F
     results$lowAIC[which.min(results$AIC)]=T

     ## print progress and return the results
     print(paste("Finshed Date:",dates[i]))
     return(results)
   }
            ))



  write.csv(results, file= paste(path,"/",out_prefix,"_results.csv",sep=""))



## some plots
myTheme <- standard.theme(col = FALSE)
myTheme$superpose.symbol$pch <-1:20
myTheme$superpose.symbol$col=1:12

pdf(paste(path,"/",out_prefix,"_ModelSummary.pdf",sep=""),width=11,height=8.5)

for(i in c("CER_mean","CER_P20um","CLD","COT")) {
  maps=subset(covar,grep(i,layerNames(covar)))
  ncols=100
  c.at=unique(quantile(as.matrix(maps),seq(0,1,len=ncols)))  #get quantile bins of raw data for all months
  c.ramp=colorRampPalette(c("brown","grey","lightgreen","darkgreen"))  #assign color ramp
  print(levelplot(maps,col.regions=c.ramp(length(c.at)-1),at=c.at,main=i)+layer(sp.lines(as(roi,"SpatialLinesDataFrame"),col="black")))
}

## histogram of observed values by month
histogram(~value|format(as.Date(date),"%m"),data=ghcn@data,points=F,auto.key=list(space="right"),
          breaks=seq(0,50,len=50),fill="grey",col="grey",border="transparent",scales=list(x=list(log=F),y=list(relation="free",crt=90)),
          main="Histograms of observed mean daily precipitation, by month",xlab="Mean Daily Precipitation (mm)")

## plot scatterplots by month
for(i in sprintf("%02d",1:12))   print(splom(subset(covar,grep(paste("[CER|CLD|COT].*",i,sep=""),layerNames(covar),value=T)),main=paste("Month",i),
                                       sub="MOD06 cloud product climatologies for a single month"))

resl=melt(results,id=c("date","month","model","form","ns","lowAIC"))

xyplot(value~date|variable,groups=form,data=resl,scales=list(y=list(relation="free")),auto.key=T,par.settings = myTheme)

resl2=cast(resl,date+month+variable~model)

myTheme$superpose.symbol$col=c("blue",rep("green",3),rep("red",3),rep("purple",3),rep("blue",2))

xyplot(mod6~mod8|variable,groups=month,data=resl2,scales=list(relation="free"),auto.key=list(space="right"),par.settings = myTheme)+layer(panel.abline(0,1))


dev.off()

print(xtable(mods),include.rownames=F,file=paste(path,"/",out_prefix,"_Models.tex",sep=""))

xyplot(form~value|variable,groups=month,data=resl,scales=list(x=list(relation="free"),y=list(draw=T, alternating=1)),auto.key=list(space="right"),par.settings = myTheme)

bwplot(form~value|variable,data=resl,scales=list(x=list(relation="free")))


  

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


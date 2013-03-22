##################    Validation and analyses of results  #######################################
############################ Covariate production for a given tile/region ##########################################
#This script examines inputs and outputs from the interpolation step.                             
#Part 1: Script produces plots for every selected date
#Part 2: Examine 
#AUTHOR: Benoit Parmentier                                                                       
#DATE: 03/18/2013                                                                                 

#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#???--   

##Comments and TODO:
#Separate inteprolation results analyses from covariates analyses 

##################################################################################################

###Loading R library and packages   
library(RPostgreSQL)
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                                          # GDAL wrapper for R, spatial utilities
library(raster)
library(gtools)
library(rasterVis)
library(graphics)
library(grid)
library(lattice)

### Parameters and arguments

##Paths to inputs and output
#Select relevant dates and load R objects created during the interpolation step

##Paths to inputs and output
script_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/"
in_path <- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data/"
out_path<- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data/"
infile_covar<-"covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script
date_selected<-c("20000101") ##This is for year 2000!!!
raster_prediction_obj<-"raster_prediction_obj__365d_GAM_fus5_all_lstd_03132013.RData"
#out_prefix<-"_365d_GAM_fus5_all_lstd_03132013"
#out_prefix<-"_365d_GAM_fus5_all_lstd_03142013"                #User defined output prefix
out_prefix<-"_365d_GAM_fus5_all_lstd_03132013"                #User defined output prefix
var<-"TMAX"
#gam_fus_mod<-load_obj("gam_fus_mod_365d_GAM_fus5_all_lstd_02202013.RData")
#validation_obj<-load_obj("gam_fus_validation_mod_365d_GAM_fus5_all_lstd_02202013.RData")
#clim_obj<-load_obj("gamclim_fus_mod_365d_GAM_fus5_all_lstd_02202013.RData")

rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
covar_names<-c(rnames,lc_names,lst_names)

list_param<-list(in_path,out_path,script_path,raster_prediction_obj,infile_covar,covar_names,date_selected,var,out_prefix)
names(list_param)<-c("in_path","out_path","script_path","raster_prediction_obj",
                     "infile_covar","covar_names","date_selected","var","out_prefix")

setwd(in_path)

## make this a script that calls several function:
#1) covariate script
#2) plots by dates
#3) number of data points monthly and daily

### Functions used in the script

load_obj <- function(f) 
{
  env <- new.env()
  nm <- load(f, env)[1]	
  env[[nm]]
}

extract_number_obs<-function(list_param){
  
  method_mod_obj<-list_param$method_mod_obj
  #Change to results_mod_obj[[i]]$data_s to make it less specific
  lapply(1:length(method_obj),function(k) nrow(method_mod_obj[[k]]$data_s))
  lapply(1:length(method_obj),function(k) nrow(method_mod_obj[[k]]$data_v))
  lapply(1:length(clim_obj),function(k) nrow(method_mod_obj[[k]]$data_v))
  return()
}

### PLOTTING RESULTS FROM VENEZUELA INTERPOLATION FOR ANALYSIS
#source(file.path(script_path,"results_interpolation_date_output_analyses_03182013.R"))
#j=1
#plots_assessment_by_date(1,list_param)
plots_assessment_by_date<-function(j,list_param){
  
  date_selected<-list_param$date_selected
  var<-list_param$var
  #gam_fus_mod<-load_obj("gam_fus_mod_365d_GAM_fus5_all_lstd_02202013.RData")
  #validation_obj<-load_obj("gam_fus_validation_mod_365d_GAM_fus5_all_lstd_02202013.RData")
  #clim_obj<-load_obj("gamclim_fus_mod_365d_GAM_fus5_all_lstd_02202013.RData")
  
  raster_prediction_obj<-load_obj(list_param$raster_prediction_obj)
  #method_mod_obj<-raster_prediction_obj$method_mod_obj
  method_mod_obj<-raster_prediction_obj$gam_fus_mod #change later for any model type
  #validation_obj<-raster_prediction_obj$validation_obj
  validation_obj<-raster_prediction_obj$gam_fus_validation_mod #change later for any model type
  #clim_obj<-raster_prediction_obj$clim_obj
  clim_obj<-raster_prediction_obj$gamclim_fus_mod #change later for any model type
  
  if (var=="TMAX"){
    y_var_name<-"dailyTmax"                                       
  }
  if (var=="TMIN"){
    y_var_name<-"dailyTmin"                                       
  }
  
  ## Read covariate stack...
  covar_names<-list_param$covar_names
  s_raster<-brick(infile_covar)                   #read in the data stack
  names(s_raster)<-covar_names               #Assigning names to the raster layers: making sure it is included in the extraction
  
  ## Figure 0: study area based on LC12 (water) mask
  
  LC_mask<-subset(s_raster,"LC12")
  LC_mask[LC_mask==100]<-NA
  LC_mask <- LC_mask < 100
  LC_mask_rec<-LC_mask
  LC_mask_rec[is.na(LC_mask_rec)]<-0
  
  #Add proportion covered by study area+ total of image pixels
  tmp_tb<-freq(LC_mask_rec)
  tmp_tb[2,2]/sum(tmp_tb[,2])*100
  png(paste("Study_area_",
            out_prefix,".png", sep=""))
  plot(LC_mask_rec,legend=FALSE,col=c("black","red"))
  legend("topright",legend=c("Outside","Inside"),title="Study area",
         pt.cex=0.9,fill=c("black","red"),bty="n")
  title("Study area")
  dev.off()
  
  #determine index position matching date selected
  
  for (j in 1:length(date_selected)){
    for (i in 1:length(method_mod_obj)){
      if(method_mod_obj[[i]]$sampling_dat$date==date_selected[j]){  
        i_dates[[j]]<-i
      }
    }
  }
  #Examine the first select date add loop or function later
  #j=1
  date<-strptime(date_selected[j], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  
  #Get raster stack of interpolated surfaces
  index<-i_dates[[j]]
  pred_temp<-as.character(method_mod_obj[[index]]$dailyTmax) #list of files
  rast_pred_temp<-stack(pred_temp) #stack of temperature predictions from models 
  
  #Get validation metrics, daily spdf training and testing stations, monthly spdf station input
  sampling_dat<-method_mod_obj[[index]]$sampling_dat
  metrics_v<-validation_obj[[index]]$metrics_v
  metrics_s<-validation_obj[[index]]$metrics_s
  data_v<-validation_obj[[index]]$data_v
  data_s<-validation_obj[[index]]$data_s
  data_month<-clim_obj[[index]]$data_month
  formulas<-clim_obj[[index]]$formulas
  
  #Adding layer LST to the raster stack of covariates
  #The names of covariates can be changed...
  
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  pos<-match("LST",layerNames(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
  s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
  pos<-match(LST_month,layerNames(s_raster)) #Find column with the current month for instance mm12
  r1<-raster(s_raster,layer=pos)             #Select layer from stack
  layerNames(r1)<-"LST"
  #Get mask image!!
  
  date_proc<-strptime(sampling_dat$date, "%Y%m%d")   # interpolation date being processed
  mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  day<-as.integer(strftime(date_proc, "%d"))
  year<-as.integer(strftime(date_proc, "%Y"))
  datelabel=format(ISOdate(year,mo,day),"%b %d, %Y")
  
  ## Figure 1: LST_TMax_scatterplot 
  
  rmse<-metrics_v$rmse[nrow(metrics_v)]
  rmse_f<-metrics_s$rmse[nrow(metrics_s)]  
  
  png(paste("LST_TMax_scatterplot_",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  plot(data_month$TMax,data_month$LST,xlab="Station mo Tmax",ylab="LST mo Tmax")
  title(paste("LST vs TMax for",datelabel,sep=" "))
  abline(0,1)
  nb_point<-paste("n=",length(data_month$TMax),sep="")
  mean_bias<-paste("Mean LST bias= ",format(mean(data_month$LSTD_bias,na.rm=TRUE),digits=3),sep="")
  #Add the number of data points on the plot
  legend("topleft",legend=c(mean_bias,nb_point),bty="n")
  dev.off()
  
  ## Figure 2: Daily_tmax_monthly_TMax_scatterplot, modify for TMin!!
  
  png(paste("Daily_tmax_monthly_TMax_scatterplot_",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  plot(dailyTmax~TMax,data=data_s,xlab="Mo Tmax",ylab=paste("Daily for",datelabel),main="across stations in VE")
  nb_point<-paste("ns=",length(data_s$TMax),sep="")
  nb_point2<-paste("ns_obs=",length(data_s$TMax)-sum(is.na(data_s[[y_var_name]])),sep="")
  nb_point3<-paste("n_month=",length(data_month$TMax),sep="")
  #Add the number of data points on the plot
  legend("topleft",legend=c(nb_point,nb_point2,nb_point3),bty="n",cex=0.8)
  dev.off()
  
  ## Figure 3: Predicted_tmax_versus_observed_scatterplot 
  
  #This is for mod_kr!! add other models later...
  png(paste("Predicted_tmax_versus_observed_scatterplot_",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  #plot(data_s$mod_kr~data_s[[y_var_name]],xlab=paste("Actual daily for",datelabel),ylab="Pred daily")
  
  y_range<-range(c(data_s$mod_kr,data_v$mod_kr),na.rm=T)
  x_range<-range(c(data_s[[y_var_name]],data_v[[y_var_name]]),na.rm=T)
  col_t<- c("black","red")
  pch_t<- c(1,2)
  plot(data_s$mod_kr,data_s[[y_var_name]], 
       xlab=paste("Actual daily for",datelabel),ylab="Pred daily", 
       ylim=y_range,xlim=x_range,col=col_t[1],pch=pch_t[1])
  points(data_v$mod_kr,data_v[[y_var_name]],col=col_t[2],pch=pch_t[2])
  grid(lwd=0.5, col="black")
  #plot(data_v$mod_kr~data_v[[y_var_name]],xlab=paste("Actual daily for",datelabel),ylab="Pred daily")
  abline(0,1)
  legend("topleft",legend=c("training","testing"),pch=pch_t,col=col_t,bty="n",cex=0.8)
  title(paste("Predicted_tmax_versus_observed_scatterplot for",datelabel,sep=" "))
  nb_point1<-paste("ns_obs=",length(data_s$TMax)-sum(is.na(data_s[[y_var_name]])),sep="")
  rmse_str1<-paste("RMSE= ",format(rmse,digits=3),sep="")
  rmse_str2<-paste("RMSE_f= ",format(rmse_f,digits=3),sep="")
  
  #Add the number of data points on the plot
  legend("bottomright",legend=c(nb_point1,rmse_str1,rmse_str2),bty="n",cex=0.8)
  dev.off()
  
  ## Figure 5: prediction raster images
  png(paste("Raster_prediction_",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  #paste(metrics_v$pred_mod,format(metrics_v$rmse,digits=3),sep=":")
  names(rast_pred_temp)<-paste(metrics_v$pred_mod,format(metrics_v$rmse,digits=3),sep=":")
  #plot(rast_pred_temp)
  levelplot(rast_pred_temp)
  dev.off()
  
  ## Figure 5b: prediction raster images
  png(paste("Raster_prediction_plot",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  #paste(metrics_v$pred_mod,format(metrics_v$rmse,digits=3),sep=":")
  names(rast_pred_temp)<-paste(metrics_v$pred_mod,format(metrics_v$rmse,digits=3),sep=":")
  #plot(rast_pred_temp)
  plot(rast_pred_temp)
  dev.off()
  
  ## Figure 6: training and testing stations used
  png(paste("Training_testing_stations_map_",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  plot(raster(rast_pred_temp,layer=5))
  plot(data_s,col="black",cex=1.2,pch=2,add=TRUE)
  plot(data_v,col="red",cex=1.2,pch=1,add=TRUE)
  legend("topleft",legend=c("training stations", "testing stations"), 
         cex=1, col=c("black","red"),
         pch=c(2,1),bty="n")
  dev.off()
  
  ## Figure 7: monthly stations used
  
  png(paste("Monthly_data_study_area",
            out_prefix,".png", sep=""))
  plot(raster(rast_pred_temp,layer=5))
  plot(data_month,col="black",cex=1.2,pch=4,add=TRUE)
  title("Monthly ghcn station in Venezuela for January")
  dev.off()
  
  ## Figure 8: delta surface and bias
  
  png(paste("Bias_delta_surface_",sampling_dat$date[i],"_",sampling_dat$prop[i],
            "_",sampling_dat$run_samp[i],out_prefix,".png", sep=""))
  
  bias_rast<-stack(clim_obj[[index]]$bias)
  delta_rast<-raster(method_mod_obj[[index]]$delta) #only one delta image!!!
  names(delta_rast)<-"delta"
  rast_temp_date<-stack(bias_rast,delta_rast)
  rast_temp_date<-mask(rast_temp_date,LC_mask,file="test.tif",overwrite=TRUE)
  #bias_d_rast<-raster("fusion_bias_LST_20100103_30_1_10d_GAM_fus5_all_lstd_02082013.rst")
  plot(rast_temp_date)
  
  dev.off()
  
  #Figure 9: histogram for all images...
  
  #histogram(rast_pred_temp)
  list_output_analyses<-list(metrics_s,metrics_v)
  return(list_output_analyses)
  
}



## Summarize information for the day: write out textfile...

#Number of station per month
#Number of station per day (training, testing,NA)
#metrics_v,metrics_s
#

# ################
# #PART 2: Region Covariate analyses ###
# ################
# 
# # This should be in a separate script to analyze covariates from region.
# 
# #MAP1:Study area with LC mask and tiles/polygon outline
# 
# 
# #MAP 2: plotting land cover in the study region:
# 
# l1<-"LC1,Evergreen/deciduous needleleaf trees"
# l2<-"LC2,Evergreen broadleaf trees"
# l3<-"LC3,Deciduous broadleaf trees"
# l4<-"LC4,Mixed/other trees"
# l5<-"LC5,Shrubs"
# l6<-"LC6,Herbaceous vegetation"
# l7<-"LC7,Cultivated and managed vegetation"
# l8<-"LC8,Regularly flooded shrub/herbaceous vegetation"
# l9<-"LC9,Urban/built-up"
# l10<-"LC10,Snow/ice"
# l11<-"LC11,Barren lands/sparse vegetation"
# l12<-"LC12,Open water"
# lc_names_str<-c(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12)
# 
# names(lc_reg_s)<-lc_names_str
# 
# png(paste("LST_TMax_scatterplot_",sampling_dat$date[i],"_",sampling_dat$prop[i],"_",sampling_dat$run_samp[i], out_prefix,".png", sep=""))
# plot(modst$TMax,sta_tmax_from_lst,xlab="Station mo Tmax",ylab="LST mo Tmax",main=paste("LST vs TMax for",datelabel,sep=" "))
# abline(0,1)
# nb_point<-paste("n=",length(modst$TMax),sep="")
# mean_bias<-paste("LST bigrasas= ",format(mean(modst$LSTD_bias,na.rm=TRUE),digits=3),sep="")
# #Add the number of data points on the plot
# legend("topleft",legend=c(mean_bias,nb_point),bty="n")
# dev.off()
# 
# #Map 3: Elevation and LST in January
# tmp_s<-stack(LST,elev_1)
# png(paste("LST_elev_",sampling_dat$date[i],"_",sampling_dat$prop[i],"_",sampling_dat$run_samp[i], out_prefix,".png", sep=""))
# plot(tmp_s)
# 
# #Map 4: LST climatology per month
#         
# names_tmp<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")
# LST_s<-subset(s_raster,names_tmp)
# names_tmp<-c("nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
#              "nobs_09","nobs_10","nobs_11","nobs_12")
# LST_nobs<-subset(s_raster,names_tmp)
# 
# LST_nobs<-mask(LST_nobs,LC_mask,filename="test2.tif")
# LST_s<-mask(LST_s,LC_mask,filename="test3.tif")
# c("Jan","Feb")
# plot(LST_s)
# plot(LST_nobs)
# 
# #Map 5: LST and TMax
# 
# #note differnces in patternin agricultural areas and 
# min_values<-cellStats(LST_s,"min")
# max_values<-cellStats(LST_s,"max")
# mean_values<-cellStats(LST_s,"mean")
# sd_values<-cellStats(LST_s,"sd")
# #median_values<-cellStats(molst,"median") Does not extist
# statistics_LST_s<-cbind(min_values,max_values,mean_values,sd_values) #This shows that some values are extremes...especially in October
# LST_stat_data<-as.data.frame(statistics_LST_s)
# names(LST_stat_data)<-c("min","max","mean","sd")
# # Statistics for number of valid observation stack
# min_values<-cellStats(nobslst,"min")
# max_values<-cellStats(nobslst,"max")
# mean_values<-cellStats(nobslst,"mean")
# sd_values<-cellStats(nobslst,"sd")
# statistics_LSTnobs_s<-cbind(min_values,max_values,mean_values,sd_values) #This shows that some values are extremes...especially in October
# LSTnobs_stat_data<-as.data.frame(statistics_LSTnobs_s)
# 
# X11(width=12,height=12)
# #Plot statiscs (mean,min,max) for monthly LST images
# plot(1:12,LST_stat_data$mean,type="b",ylim=c(-15,60),col="black",xlab="month",ylab="tmax (degree C)")
# lines(1:12,LST_stat_data$min,type="b",col="blue")
# lines(1:12,LST_stat_data$max,type="b",col="red")
# text(1:12,LST_stat_data$mean,rownames(LST_stat_data),cex=1,pos=2)
# 
# legend("topleft",legend=c("min","mean","max"), cex=1.5, col=c("blue","black","red"),
#        lty=1)
# title(paste("LST statistics for Oregon", "2010",sep=" "))
# savePlot("lst_statistics_OR.png",type="png")
# 
# #Plot number of valid observations for LST
# plot(1:12,LSTnobs_stat_data$mean,type="b",ylim=c(0,280),col="black",xlab="month",ylab="tmax (degree C)")
# lines(1:12,LSTnobs_stat_data$min,type="b",col="blue")
# lines(1:12,LSTnobs_stat_data$max,type="b",col="red")
# text(1:12,LSTnobs_stat_data$mean,rownames(LSTnobs_stat_data),cex=1,pos=2)
# 
# legend("topleft",legend=c("min","mean","max"), cex=1.5, col=c("blue","black","red"),
#        lty=1)
# title(paste("LST number of valid observations for Oregon", "2010",sep=" "))
# savePlot("lst_nobs_OR.png",type="png")
# 
# plot(data_month$TMax,add=TRUE)
# 
# ### Map 6: station in the region
# 
# plot(tmax_predicted)
# plot(data_s,col="black",cex=1.2,pch=4,add=TRUE)
# plot(data_v,col="blue",cex=1.2,pch=2,add=TRUE)
# 
# plot(tmax_predicted)
# plot(data_month,col="black",cex=1.2,pch=4,add=TRUE)
# title("Monthly ghcn station in Venezuela for 2000-2010")
# 
#png...output?
# plot(interp_area, axes =TRUE)
# plot(stat_reg, pch=1, col="red", cex= 0.7, add=TRUE)
# plot(data_reg,pch=2,col="blue",cex=2,add=TRUE)

#### End of script ####
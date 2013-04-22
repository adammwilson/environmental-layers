##################    Validation and analyses of results  #######################################
############################ Covariate production for a given tile/region ##########################################
#This script examines inputs and outputs from the interpolation step.                             
#Part 1: Script produces plots for every selected date
#Part 2: Examine 
#AUTHOR: Benoit Parmentier                                                                       
#DATE: 04/16/2013                                                                                 

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
#script_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/"
in_path <- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data/"
out_path<- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data/"
infile_covar<-"covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script
date_selected<-c("20000101") ##This is for year 2000!!!
#raster_prediction_obj<-load_obj("raster_prediction_obj_dailyTmin_365d_GAM_fus5_all_lstd_03292013.RData")
#out_prefix<-"_365d_GAM_fus5_all_lstd_03132013"
#out_prefix<-"_365d_GAM_fus5_all_lstd_03142013"                #User defined output prefix
out_prefix<-"_365d_GAM_fus5_all_lstd_03292013"                #User defined output prefix
var<-"TMIN"
#gam_fus_mod<-load_obj("gam_fus_mod_365d_GAM_fus5_all_lstd_02202013.RData")
#validation_obj<-load_obj("gam_fus_validation_mod_365d_GAM_fus5_all_lstd_02202013.RData")
#clim_obj<-load_obj("gamclim_fus_mod_365d_GAM_fus5_all_lstd_02202013.RData")

rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
covar_names<-c(rnames,lc_names,lst_names)

list_param_results_analyses<-list(in_path,out_path,script_path,raster_prediction_obj,interpolation_method,infile_covar,covar_names,date_selected,var,out_prefix)
names(list_param_results_analyses)<-c("in_path","out_path","script_path","raster_prediction_obj", "interpolation_method",
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


### PLOTTING RESULTS FROM VENEZUELA INTERPOLATION FOR ANALYSIS
#source(file.path(script_path,"results_interpolation_date_output_analyses_04022013.R"))
#j=1
#plots_assessment_by_date(1,list_param_results_analyses)


plots_assessment_by_date<-function(j,list_param){
  
  date_selected<-list_param$date_selected
  var<-list_param$var
  interpolation
  #gam_fus_mod<-load_obj("gam_fus_mod_365d_GAM_fus5_all_lstd_02202013.RData")
  #validation_obj<-load_obj("gam_fus_validation_mod_365d_GAM_fus5_all_lstd_02202013.RData")
  #clim_obj<-load_obj("gamclim_fus_mod_365d_GAM_fus5_all_lstd_02202013.RData")
  
  raster_prediction_obj<-list_param$raster_prediction_obj
  method_mod_obj<-raster_prediction_obj$method_mod_obj
  method_mod_obj<-raster_prediction_obj$gam_fus_mod #change later for any model type
  #validation_obj<-raster_prediction_obj$validation_obj
  validation_obj<-raster_prediction_obj$gam_fus_validation_mod #change later for any model type
  #clim_obj<-raster_prediction_obj$clim_obj
  clim_obj<-raster_prediction_obj$gamclim_fus_mod #change later for any model type
  
  if (var=="TMAX"){
    y_var_name<-"dailyTmax"
    y_var_month<-"TMax"
  }
  if (var=="TMIN"){
    y_var_name<-"dailyTmin"
    y_var_month <-"TMin"
  }
  
  ## Read covariate stack...
  covar_names<-list_param$covar_names
  s_raster<-brick(infile_covar)                   #read in the data stack
  names(s_raster)<-covar_names               #Assigning names to the raster layers: making sure it is included in the extraction
  
  ## Prepare study area  mask: based on LC12 (water)
  
  LC_mask<-subset(s_raster,"LC12")
  LC_mask[LC_mask==100]<-NA
  LC_mask <- LC_mask < 100
  LC_mask_rec<-LC_mask
  LC_mask_rec[is.na(LC_mask_rec)]<-0
    
  #determine index position matching date selected
  i_dates<-vector("list",length(date_selected))
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
  pred_temp<-as.character(method_mod_obj[[index]][[y_var_name]]) #list of files
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
  
  png(paste("LST_",y_var_month,"_scatterplot_",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  plot(data_month[[y_var_month]],data_month$LST,xlab=paste("Station mo ",y_var_month,sep=""),ylab=paste("LST mo ",y_var_month,sep=""))
  title(paste("LST vs ", y_var_month,"for",datelabel,sep=" "))
  abline(0,1)
  nb_point<-paste("n=",length(data_month[[y_var_month]]),sep="")
  mean_bias<-paste("Mean LST bias= ",format(mean(data_month$LSTD_bias,na.rm=TRUE),digits=3),sep="")
  #Add the number of data points on the plot
  legend("topleft",legend=c(mean_bias,nb_point),bty="n")
  dev.off()
  
  ## Figure 2: Daily_tmax_monthly_TMax_scatterplot, modify for TMin!!
  
  png(paste("Monhth_day_scatterplot_",y_var_name,"_",y_var_month,"_",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  plot(data_s[[y_var_name]]~data_s[[y_var_month]],xlab=paste("Month") ,ylab=paste("Daily for",datelabel),main="across stations in VE")
  nb_point<-paste("ns=",length(data_s[[y_var_month]]),sep="")
  nb_point2<-paste("ns_obs=",length(data_s[[y_var_month]])-sum(is.na(data_s[[y_var_name]])),sep="")
  nb_point3<-paste("n_month=",length(data_month[[y_var_month]]),sep="")
  #Add the number of data points on the plot
  legend("topleft",legend=c(nb_point,nb_point2,nb_point3),bty="n",cex=0.8)
  dev.off()
  
  ## Figure 3: Predicted_tmax_versus_observed_scatterplot 
  
  #This is for mod_kr!! add other models later...
  model_name<-"mod_kr" #can be looped through models later on...
  
  png(paste("Predicted_versus_observed_scatterplot_",y_var_name,"_",model_name,"_",sampling_dat$date,"_",sampling_dat$prop,"_",
            sampling_dat$run_samp,out_prefix,".png", sep=""))
  y_range<-range(c(data_s[[model_name]],data_v[[model_name]]),na.rm=T)
  x_range<-range(c(data_s[[y_var_name]],data_v[[y_var_name]]),na.rm=T)
  col_t<- c("black","red")
  pch_t<- c(1,2)
  plot(data_s[[model_name]],data_s[[y_var_name]], 
       xlab=paste("Actual daily for",datelabel),ylab="Pred daily", 
       ylim=y_range,xlim=x_range,col=col_t[1],pch=pch_t[1])
  points(data_v[[model_name]],data_v[[y_var_name]],col=col_t[2],pch=pch_t[2])
  grid(lwd=0.5, col="black")
  abline(0,1)
  legend("topleft",legend=c("training","testing"),pch=pch_t,col=col_t,bty="n",cex=0.8)
  title(paste("Predicted_versus_observed_",y_var_name,"_",model_name,"_",datelabel,sep=" "))
  nb_point1<-paste("ns_obs=",length(data_s[[y_var_name]])-sum(is.na(data_s[[model_name]])),sep="")
  nb_point2<-paste("nv_obs=",length(data_v[[y_var_name]])-sum(is.na(data_v[[model_name]])),sep="")

  rmse_str1<-paste("RMSE= ",format(rmse,digits=3),sep="")
  rmse_str2<-paste("RMSE_f= ",format(rmse_f,digits=3),sep="")
  
  #Add the number of data points on the plot
  legend("bottomright",legend=c(nb_point1,nb_point2,rmse_str1,rmse_str2),bty="n",cex=0.8)
  dev.off()
  
  ## Figure 4a: prediction raster images
  png(paste("Raster_prediction_",y_var_name,"_",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  #paste(metrics_v$pred_mod,format(metrics_v$rmse,digits=3),sep=":")
  names(rast_pred_temp)<-paste(metrics_v$pred_mod,format(metrics_v$rmse,digits=3),sep=":")
  #plot(rast_pred_temp)
  levelplot(rast_pred_temp)
  dev.off()
  
  ## Figure 4b: prediction raster images
  png(paste("Raster_prediction_plot",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  #paste(metrics_v$pred_mod,format(metrics_v$rmse,digits=3),sep=":")
  names(rast_pred_temp)<-paste(metrics_v$pred_mod,format(metrics_v$rmse,digits=3),sep=":")
  #plot(rast_pred_temp)
  plot(rast_pred_temp)
  dev.off()
  
  ## Figure 5: training and testing stations used
  png(paste("Training_testing_stations_map_",y_var_name,"_",sampling_dat$date,"_",sampling_dat$prop,"_",sampling_dat$run_samp,
            out_prefix,".png", sep=""))
  plot(raster(rast_pred_temp,layer=5))
  plot(data_s,col="black",cex=1.2,pch=2,add=TRUE)
  plot(data_v,col="red",cex=1.2,pch=1,add=TRUE)
  legend("topleft",legend=c("training stations", "testing stations"), 
         cex=1, col=c("black","red"),
         pch=c(2,1),bty="n")
  dev.off()
  
  ## Figure 6: monthly stations used
  
  png(paste("Monthly_data_study_area_", y_var_name,
            out_prefix,".png", sep=""))
  plot(raster(rast_pred_temp,layer=5))
  plot(data_month,col="black",cex=1.2,pch=4,add=TRUE)
  title("Monthly ghcn station in Venezuela for January")
  dev.off()
  
  ## Figure 7: delta surface and bias
  
  png(paste("Bias_delta_surface_",y_var_name,"_",sampling_dat$date[i],"_",sampling_dat$prop[i],
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


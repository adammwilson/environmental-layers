######################################## Paper Methods_comparison: Analyses part 5 #######################################
############################ Scripts for figures and analyses for paper 2 #####################################
#This script performs analyses and create figures for the FSS paper.
#It uses inputs from interpolation objects created at earlier stages...     
#Note that this is exploratory code i.e. not part of the worklfow.
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 09/06/2013                                                                                #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#491--                                  #
###################################################################################################

###Loading R library and packages                                                      
#library(gtools)                                        # loading some useful tools 
library(mgcv)                   # GAM package by Wood 2006 (version 2012)
library(sp)                     # Spatial pacakge with class definition by Bivand et al. 2008
library(spdep)                  # Spatial package with methods and spatial stat. by Bivand et al. 2012
library(rgdal)                  # GDAL wrapper for R, spatial utilities (Keitt et al. 2012)
library(gstat)                  # Kriging and co-kriging by Pebesma et al. 2004
library(automap)                # Automated Kriging based on gstat module by Hiemstra et al. 2008
library(spgwr)
library(maptools)
library(graphics)
library(parallel)               # Urbanek S. and Ripley B., package for multi cores & parralel processing
library(raster)
library(rasterVis)
library(plotrix)                # Draw circle on graph and additional plotting options
library(reshape)                # Data format and type transformation

##################### Function used in the script ##############

## Extract a list of object from an object: Useful to extract information from
## RData objects saved in the interpolation phase.

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

### Need to improve this function!!!
calc_stat_prop_tb_diagnostic <-function(names_mod,names_id,tb){
  
  t<-melt(subset(tb,pred_mod==names_mod),
          measure=c("mae","rmse","r","me","m50"), 
          id=names_id,
          na.rm=T)
  char_tmp <-rep("+",length=length(names_id)-1)
  var_summary <-paste(names_id,sep="",collapse=char_tmp)
  var_summary_formula <-paste(var_summary,collpase="~variable")
  avg_tb<-cast(t,var_summary_formula,mean)
  sd_tb<-cast(t,var_summary_formula,sd)
  n_tb<-cast(t,var_summary_formula,length)
  #n_NA<-cast(t,dst_cat1~variable,is.na)
  
  #### prepare returning object
  prop_obj<-list(tb,avg_tb,sd_tb,n_tb)
  names(prop_obj) <-c("tb","avg_tb","sd_tb","n_tb")
  
  return(prop_obj)
}

################## PARAMETERS ##########


#"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb4_07152013/"
in_dir1 <- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_CAI_lst_comb3_08312013/"
in_dir <- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_CAI_lst_comb3_09012013"
in_dir2 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_CAI_lst_comb3_09032013"
in_dir4 <- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_kriging_CAI_lst_comb3_09042013"

raster_prediction_obj1 <-load_obj(file.path(in_dir1,"raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_CAI_lst_comb3_08312013.RData"))
raster_prediction_obj <-load_obj(file.path(in_dir,"raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_CAI_lst_comb3_09012013.RData"))
raster_prediction_obj2 <-load_obj(file.path(in_dir2,"raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_CAI_lst_comb3_09032013.RData"))
raster_prediction_obj4 <-load_obj(file.path(in_dir4,"raster_prediction_obj_kriging_CAI_dailyTmax_365d_kriging_CAI_lst_comb3_09042013.RData"))
                                                                    
out_dir<-"/home/parmentier/Data/IPLANT_project/paper_multitime_scale__analyses_tables_fig_09032013"
setwd(out_dir)
y_var_name <- "dailyTmax"
y_var_month <- "TMax"
#y_var_month <- "LSTD_bias"
out_suffix <- "_OR_09032013"
#script_path<-"/data/project/layers/commons/data_workflow/env_layers_scripts/"
#### FUNCTION USED IN SCRIPT

function_analyses_paper <-"contribution_of_covariates_paper_interpolation_functions_08152013.R"

script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/" #path to script
source(file.path(script_path,function_analyses_paper)) #source all functions used in this script.

#################################################################################
############ ANALYSES 1: Average accuracy per proportion for monthly hold out in muli-timescale mehtods... #######

tb_mv_gam_CAI <-rbind(raster_prediction_obj1$tb_month_diagnostic_v,raster_prediction_obj$tb_month_diagnostic_v,raster_prediction_obj2$tb_month_diagnostic_v)
tb_ms_gam_CAI <-rbind(raster_prediction_obj1$tb_month_diagnostic_s,raster_prediction_obj$tb_month_diagnostic_s,raster_prediction_obj2$tb_month_diagnostic_s)

tb_v_gam_CAI <-rbind(raster_prediction_obj1$tb_diagnostic_v,raster_prediction_obj$tb_diagnostic_v,raster_prediction_obj2$tb_diagnostic_v)
tb_s_gam_CAI <-rbind(raster_prediction_obj1$tb_diagnostic_s,raster_prediction_obj$tb_diagnostic_s,raster_prediction_obj2$tb_diagnostic_s)

prop_obj_gam_CAI_v <- calc_stat_prop_tb_diagnostic(names_mod,names_id,tb_v)

tb_mv_kriging_CAI <- raster_prediction_obj4$tb_month_diagnostic_v
tb_ms_kriging_CAI <- raster_prediction_obj4$tb_month_diagnostic_s

tb_v_kriging_CAI <- raster_prediction_obj4$tb_diagnostic_v
tb_s_kriging_CAI <- raster_prediction_obj4$tb_diagnostic_s
                                  
list_tb <-list(tb_v_gam_CAI,tb_v_kriging_CAI,tb_s_gam_CAI,tb_s_kriging_CAI,tb_mv_gam_CAI,tb_mv_kriging_CAI,tb_ms_gam_CAI,tb_ms_kriging_CAI) #Add fusion here
names(list_tb) <- c("tb_v_gam_CAI","tb_v_kriging_CAI","tb_s_gam_CAI","tb_s_kriging_CAI","tb_mv_gam_CAI","tb_mv_kriging_CAI","tb_ms_gam_CAI","tb_ms_kriging_CAI") #Add fusion here

##### DAILY AVERAGE ACCURACY : PLOT AND DIFFERENCES...

for(i in 1:length(list_tb)){
  i<-i+1
  tb <-list_tb[[i]]
  plot_name <- names(list_tb)[i]
  pat_str <- "tb_m"
  if(substr(plot_name,start=1,stop=4)== pat_str){
    names_id <- c("pred_mod","prop")
    plot_formula <- paste("rmse","~prop",sep="",collapse="")
    
  }else{
    names_id <- c("pred_mod","prop_month")
    plot_formula <- paste("rmse","~prop_month",collapse="")
  }
  names_mod <-unique(tb$pred_mod)
  
  prop_obj <- calc_stat_prop_tb_diagnostic(names_mod,names_id,tb)
  
  avg_tb <- prop_obj$avg_tb
  
  layout_m<-c(1,1) #one row two columns
  par(mfrow=layout_m)
    
  png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
      height=480*layout_m[1],width=480*layout_m[2])
    
  xyplot(as.formula(plot_formula),group=pred_mod,type="b",
          data=avg_tb,
          main=paste("rmse ",plot_name,sep=" "),
          pch=1:length(avg_tb$pred_mod),
          par.settings=list(superpose.symbol = list(
            pch=1:length(avg_tb$pred_mod))),
          auto.key=list(columns=5))
  
  dev.off()
  
}

#xyplot( rmse ~ prop_month | pred_mod,type="b",data=as.data.frame(avg_tb))

##### Calculate differences

#Calculate the difference between training and testing in two different data.frames. Columns to substract are provided.
diff_df<-function(tb_s,tb_v,list_metric_names){
  tb_diff<-vector("list", length(list_metric_names))
  for (i in 1:length(list_metric_names)){
    metric_name<-list_metric_names[i]
    tb_diff[[i]] <-tb_s[,c(metric_name)] - tb_v[,c(metric_name)]
  }
  names(tb_diff)<-list_metric_names
  tb_diff<-as.data.frame(do.call(cbind,tb_diff))
  return(tb_diff)
}

metric_names <- c("mae","rmse","me","r")
diff_kriging_CAI <- diff_df(tb_s_kriging_CAI,tb_v_kriging_CAI,metric_names)
diff_gam_CAI <- diff_df(tb_s_gam_CAI,tb_v_gam_CAI,metric_names)

boxplot(diff_kriging_CAI$rmse,diff_gam_CAI$rmse)


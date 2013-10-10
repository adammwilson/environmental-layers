######################################## Paper Methods_comparison: Analyses part 5 #######################################
############################ Scripts for figures and analyses for paper 2 #####################################
#This script performs analyses and create figures for the FSS paper.
#It uses inputs from interpolation objects created at earlier stages...     
#Note that this is exploratory code i.e. not part of the worklfow.
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 09/13/2013                                                                                #
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

################## PARAMETERS ##########


#path to gam CAI and kriging analyes with hold-out
in_dir1 <- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_CAI_lst_comb3_08312013/"
in_dir2 <- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_CAI_lst_comb3_09012013"
in_dir3 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_CAI_lst_comb3_09032013"
in_dir4 <- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_kriging_CAI_lst_comb3_09042013"

#path to gam fusion and kriging fusion analyes with hold-out
in_dir5 <- "/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_fus_lst_comb3_09092013"
in_dir6 <- "/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_fus_lst_comb3_09102013"
in_dir7 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_fus_lst_comb3_09112013"
in_dir8 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_fus_lst_comb3_09122013"
in_dir9 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_fus_lst_comb3_09132013"
in_dir10 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_fus_lst_comb3_09142013"
in_dir11 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_CAI_lst_comb3_09162013"
in_dir12 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_CAI_lst_comb3_09172013"
in_dir13 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_cai_lst_comb3_09282013"
in_dir14 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_fus_lst_comb3_09232013"
in_dir15 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_fus_lst_comb3_09262013"
in_dir16 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_cai_lst_comb3_10042013"


#better as list and load one by one specific element from the object
raster_prediction_obj1 <-load_obj(file.path(in_dir1,"raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_CAI_lst_comb3_08312013.RData"))
raster_prediction_obj2 <-load_obj(file.path(in_dir2,"raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_CAI_lst_comb3_09012013.RData"))
raster_prediction_obj3 <-load_obj(file.path(in_dir3,"raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_CAI_lst_comb3_09032013.RData"))
raster_prediction_obj4 <-load_obj(file.path(in_dir4,"raster_prediction_obj_kriging_CAI_dailyTmax_365d_kriging_CAI_lst_comb3_09042013.RData"))
          
raster_prediction_obj5 <-load_obj(file.path(in_dir5,"raster_prediction_obj_gam_fusion_dailyTmax_365d_gam_fus_lst_comb3_09092013.RData"))
raster_prediction_obj6 <-load_obj(file.path(in_dir6,"raster_prediction_obj_gam_fusion_dailyTmax_365d_gam_fus_lst_comb3_09102013.RData"))
raster_prediction_obj7 <-load_obj(file.path(in_dir7,"raster_prediction_obj_gam_fusion_dailyTmax_365d_gam_fus_lst_comb3_09112013.RData"))
raster_prediction_obj8 <-load_obj(file.path(in_dir8,"raster_prediction_obj_kriging_fusion_dailyTmax_365d_kriging_fus_lst_comb3_09122013.RData"))
raster_prediction_obj9 <-load_obj(file.path(in_dir9,"raster_prediction_obj_kriging_fusion_dailyTmax_365d_kriging_fus_lst_comb3_09132013.RData"))
raster_prediction_obj10 <-load_obj(file.path(in_dir10,"raster_prediction_obj_kriging_fusion_dailyTmax_365d_kriging_fus_lst_comb3_09142013.RData"))
raster_prediction_obj11 <-load_obj(file.path(in_dir11,"raster_prediction_obj_gwr_CAI_dailyTmax_365d_gwr_CAI_lst_comb3_09162013.RData"))
raster_prediction_obj12 <-load_obj(file.path(in_dir12,"raster_prediction_obj_gwr_CAI_dailyTmax_365d_gwr_CAI_lst_comb3_09172013.RData"))
raster_prediction_obj13 <-load_obj(file.path(in_dir13,"raster_prediction_obj_gwr_CAI_dailyTmax_365d_gwr_cai_lst_comb3_09282013.RData"))
raster_prediction_obj14 <-load_obj(file.path(in_dir14,"raster_prediction_obj_gwr_fusion_dailyTmax_365d_gwr_fus_lst_comb3_09232013.RData"))
raster_prediction_obj15 <-load_obj(file.path(in_dir15,"raster_prediction_obj_gwr_fusion_dailyTmax_365d_gwr_fus_lst_comb3_09262013.RData"))

raster_prediction_obj16 <-load_obj(file.path(in_dir16,"raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_cai_lst_comb3_10042013.RData"))

out_dir<-"/home/parmentier/Data/IPLANT_project/paper_multitime_scale__analyses_tables_fig_09032013"
setwd(out_dir)
y_var_name <- "dailyTmax"
y_var_month <- "TMax"
#y_var_month <- "LSTD_bias"
out_suffix <- "_OR_10102013"
#script_path<-"/data/project/layers/commons/data_workflow/env_layers_scripts/"
#### FUNCTION USED IN SCRIPT

function_analyses_paper <-"contribution_of_covariates_paper_interpolation_functions_09232013.R"

script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/" #path to script
source(file.path(script_path,function_analyses_paper)) #source all functions used in this script.

#################################################################################
############ ANALYSES 1: Average accuracy per proportion for monthly hold out in muli-timescale mehtods... #######

tb_mv_gam_CAI <-rbind(raster_prediction_obj1$tb_month_diagnostic_v,raster_prediction_obj2$tb_month_diagnostic_v,raster_prediction_obj3$tb_month_diagnostic_v)
tb_ms_gam_CAI <-rbind(raster_prediction_obj1$tb_month_diagnostic_s,raster_prediction_obj2$tb_month_diagnostic_s,raster_prediction_obj3$tb_month_diagnostic_s)

tb_v_gam_CAI <-rbind(raster_prediction_obj1$tb_diagnostic_v,raster_prediction_obj2$tb_diagnostic_v,raster_prediction_obj3$tb_diagnostic_v)
tb_s_gam_CAI <-rbind(raster_prediction_obj1$tb_diagnostic_s,raster_prediction_obj2$tb_diagnostic_s,raster_prediction_obj3$tb_diagnostic_s)
#prop_obj_gam_CAI_v <- calc_stat_prop_tb_diagnostic(names_mod,names_id,tb_v)

tb_mv_gwr_CAI <-rbind(raster_prediction_obj11$tb_month_diagnostic_v,raster_prediction_obj12$tb_month_diagnostic_v,raster_prediction_obj13$tb_month_diagnostic_v)
tb_ms_gwr_CAI <-rbind(raster_prediction_obj11$tb_month_diagnostic_s,raster_prediction_obj12$tb_month_diagnostic_s,raster_prediction_obj13$tb_month_diagnostic_s)

tb_v_gwr_CAI <-rbind(raster_prediction_obj11$tb_diagnostic_v,raster_prediction_obj12$tb_diagnostic_v,raster_prediction_obj13$tb_diagnostic_v)
tb_s_gwr_CAI <-rbind(raster_prediction_obj11$tb_diagnostic_s,raster_prediction_obj12$tb_diagnostic_s,raster_prediction_obj13$tb_diagnostic_s)

tb_mv_kriging_CAI <- raster_prediction_obj4$tb_month_diagnostic_v
tb_ms_kriging_CAI <- raster_prediction_obj4$tb_month_diagnostic_s

tb_v_kriging_CAI <- raster_prediction_obj4$tb_diagnostic_v
tb_s_kriging_CAI <- raster_prediction_obj4$tb_diagnostic_s

### SAME for gam fusion

tb_mv_gam_fus <-rbind(raster_prediction_obj5$tb_month_diagnostic_v,raster_prediction_obj6$tb_month_diagnostic_v,raster_prediction_obj7$tb_month_diagnostic_v)
tb_ms_gam_fus <-rbind(raster_prediction_obj5$tb_month_diagnostic_s,raster_prediction_obj6$tb_month_diagnostic_s,raster_prediction_obj7$tb_month_diagnostic_s)

tb_v_gam_fus <-rbind(raster_prediction_obj5$tb_diagnostic_v,raster_prediction_obj6$tb_diagnostic_v,raster_prediction_obj7$tb_diagnostic_v)
tb_s_gam_fus <-rbind(raster_prediction_obj5$tb_diagnostic_s,raster_prediction_obj6$tb_diagnostic_s,raster_prediction_obj7$tb_diagnostic_s)

tb_mv_gwr_fus <-rbind(raster_prediction_obj14$tb_month_diagnostic_v,raster_prediction_obj15$tb_month_diagnostic_v)
tb_ms_gwr_fus <-rbind(raster_prediction_obj14$tb_month_diagnostic_s,raster_prediction_obj15$tb_month_diagnostic_s)

tb_v_gwr_fus <-rbind(raster_prediction_obj14$tb_diagnostic_v,raster_prediction_obj15$tb_diagnostic_v)
tb_s_gwr_fus <-rbind(raster_prediction_obj14$tb_diagnostic_s,raster_prediction_obj15$tb_diagnostic_s)

tb_mv_kriging_fus <-rbind(raster_prediction_obj8$tb_month_diagnostic_v,raster_prediction_obj9$tb_month_diagnostic_v,raster_prediction_obj10$tb_month_diagnostic_v)
tb_ms_kriging_fus <-rbind(raster_prediction_obj8$tb_month_diagnostic_s,raster_prediction_obj9$tb_month_diagnostic_s,raster_prediction_obj10$tb_month_diagnostic_s)

tb_v_kriging_fus <-rbind(raster_prediction_obj8$tb_diagnostic_v,raster_prediction_obj9$tb_diagnostic_v,raster_prediction_obj10$tb_diagnostic_v)
tb_s_kriging_fus <-rbind(raster_prediction_obj8$tb_diagnostic_s,raster_prediction_obj9$tb_diagnostic_s,raster_prediction_obj10$tb_diagnostic_s)

list_tb <- list(tb_v_gam_CAI,tb_v_kriging_CAI,tb_v_gwr_CAI,tb_s_gam_CAI,tb_s_kriging_CAI,tb_s_gwr_CAI,
           tb_mv_gam_CAI,tb_mv_kriging_CAI,tb_mv_gwr_CAI,tb_ms_gam_CAI,tb_ms_kriging_CAI,tb_ms_gwr_CAI,
           tb_v_gam_fus,tb_v_kriging_fus,tb_v_gwr_fus,tb_s_gam_fus,tb_s_kriging_fus,tb_s_gwr_fus,
           tb_mv_gam_fus,tb_mv_kriging_fus,tb_mv_gwr_fus,tb_ms_gam_fus,tb_ms_kriging_fus,tb_ms_gwr_fus)

names(list_tb) <- c("tb_v_gam_CAI","tb_v_kriging_CAI","tb_v_gwr_CAI","tb_s_gam_CAI","tb_s_kriging_CAI","tb_s_gwr_CAI",
        "tb_mv_gam_CAI","tb_mv_kriging_CAI","tb_mv_gwr_CAI","tb_ms_gam_CAI","tb_ms_kriging_CAI","tb_ms_gwr_CAI",
        "tb_v_gam_fus","tb_v_kriging_fus","tb_v_gwr_fus","tb_s_gam_fus","tb_s_kriging_fus","tb_s_gwr_fus",
        "tb_mv_gam_fus","tb_mv_kriging_fus","tb_mv_gwr_fus","tb_ms_gam_fus","tb_ms_kriging_fus","tb_ms_gwr_fus")

#list_tb <-list(tb_v_gam_CAI,tb_v_kriging_CAI,tb_v_gwr_CAI,tb_s_gam_CAI,tb_s_kriging_CAI,tb_s_gwr_CAI,tb_mv_gam_CAI,tb_mv_kriging_CAI,tb_ms_gam_CAI,tb_ms_kriging_CAI,tb_ms_gwr_CAI,tb_ms_gwr_CAI #Add fusion here
#               tb_v_gam_fus,tb_v_kriging_fus,tb_s_gam_fus,tb_s_kriging_fus,tb_mv_gam_fus,tb_mv_kriging_fus,tb_ms_gam_fus,tb_ms_kriging_fus) #Add fusion here
#names(list_tb) <- c("tb_v_gam_CAI","tb_v_kriging_CAI","tb_v_gwr_CAI","tb_s_gam_CAI","tb_s_kriging_CAI","tb_s_gwr_CAI","tb_mv_gam_CAI","tb_mv_kriging_CAI","tb_ms_gam_CAI","tb_ms_kriging_CAI","tb_ms_gwr_CAI","tb_ms_gwr_CAI" #Add fusion here
#                   "tb_v_gam_fus","tb_v_kriging_fus","tb_s_gam_fus","tb_s_kriging_fus","tb_mv_gam_fus","tb_mv_kriging_fus","tb_ms_gam_fus","tb_ms_kriging_fus") #Add fusion here

##### DAILY AVERAGE ACCURACY : PLOT AND DIFFERENCES...Cd

for(i in 1:length(list_tb)){
  #i <- i+1
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
    
  p<- xyplot(as.formula(plot_formula),group=pred_mod,type="b",
          data=avg_tb,
          main=paste("rmse ",plot_name,sep=" "),
          pch=1:length(avg_tb$pred_mod),
          par.settings=list(superpose.symbol = list(
          pch=1:length(avg_tb$pred_mod))),
          auto.key=list(columns=5))
  print(p)
  
  dev.off()
  
}

#xyplot( rmse ~ prop_month | pred_mod,type="b",data=as.data.frame(avg_tb))

##### Calculate differences

metric_names <- c("mae","rmse","me","r")
diff_kriging_CAI <- diff_df(tb_s_kriging_CAI,tb_v_kriging_CAI,metric_names)
diff_gam_CAI <- diff_df(tb_s_gam_CAI[tb_s_gam_CAI$pred_mod!="mod_kr"],tb_v_gam_CAI,metric_names)
diff_gwr_CAI <- diff_df(tb_s_gwr_CAI,tb_v_gwr_CAI,metric_names)

layout_m<-c(1,1) #one row two columns
par(mfrow=layout_m)

png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
boxplot(diff_kriging_CAI$rmse,diff_gam_CAI$rmse,diff_gwr_CAI$rmse,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
        main="Difference between training and testing daily rmse")
dev.off()

#remove prop 0,
diff_kriging_CAI <- diff_df(tb_s_kriging_CAI[tb_s_kriging_CAI$prop_month!=0,],tb_v_kriging_CAI[tb_v_kriging_CAI$prop_month!=0,],metric_names)
diff_gam_CAI <- diff_df(tb_s_gam_CAI[tb_s_gam_CAI$prop_month!=0,],tb_v_gam_CAI[tb_v_gam_CAI$prop_month!=0,],metric_names)
diff_gwr_CAI <- diff_df(tb_s_gwr_CAI[tb_s_gwr_CAI$prop_month!=0,],tb_v_gwr_CAI[tb_v_gwr_CAI$prop_month!=0,],metric_names)
boxplot(diff_kriging_CAI$rmse,diff_gam_CAI$rmse,diff_gwr_CAI$rmse,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
        main="Difference between training and testing daily rmse")

#now monthly accuracy
metric_names <- c("mae","rmse","me","r")
diff_kriging_m_CAI <- diff_df(tb_ms_kriging_CAI[tb_ms_kriging_CAI$prop!=0,],tb_mv_kriging_CAI,metric_names)
diff_gam_m_CAI <- diff_df(tb_ms_gam_CAI[tb_ms_gam_CAI$prop!=0,],tb_mv_gam_CAI,metric_names)
diff_gwr_m_CAI <- diff_df(tb_ms_gwr_CAI[tb_ms_gwr_CAI$prop!=0,],tb_mv_gwr_CAI,metric_names)

layout_m<-c(1,1) #one row two columns
par(mfrow=layout_m)

png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
boxplot(diff_kriging_m_CAI$rmse,diff_gam_m_CAI$rmse,diff_gwr_m_CAI$rmse,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
        main="Difference between training and monhtly testing rmse")
dev.off()

#boxplot(diff_kriging_m_CAI$rmse,diff_gam_m_CAI$rmse,diff_gwr_CAI,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
#        main="Difference between training and monhtly testing rmse")

### For fusion

metric_names <- c("mae","rmse","me","r")
diff_kriging_fus <- diff_df(tb_s_kriging_fus,tb_v_kriging_fus,metric_names)
diff_gam_fus <- diff_df(tb_s_gam_fus,tb_v_gam_fus,metric_names)
diff_gwr_fus <- diff_df(tb_s_gwr_fus,tb_v_gwr_fus,metric_names)

layout_m<-c(1,1) #one row two columns
par(mfrow=layout_m)

png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
boxplot(diff_kriging_fus$rmse,diff_gam_fus$rmse,diff_gwr_fus$rmse,names=c("kriging_fus","gam_fus","gwr_fus"),
        main="Difference between training and testing daily rmse")
dev.off()

metric_names <- c("mae","rmse","me","r")
diff_kriging_m_fus <- diff_df(tb_ms_kriging_fus[tb_ms_kriging_fus$prop!=0,],tb_mv_kriging_fus[tb_mv_kriging_fus$prop!=0,],metric_names)
diff_gam_m_fus <- diff_df(tb_ms_gam_fus[tb_ms_gam_fus$prop!=0,],tb_mv_gam_fus[tb_mv_gam_fus$prop!=0,],metric_names)
diff_gwr_m_fus <- diff_df(tb_ms_gwr_fus[tb_ms_gwr_fus$prop!=0,],tb_mv_gwr_fus[tb_mv_gwr_fus$prop!=0,],metric_names)

layout_m<-c(1,1) #one row two columns
par(mfrow=layout_m)

png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
boxplot(diff_kriging_m_fus$rmse,diff_gam_m_fus$rmse,diff_gwr_m_fus$rmse, names=c("kriging_fus","gam_fus","gwr_fus"),
        main="Difference between training and testing FUS rmse")
dev.off()

### NOW PLOT OF COMPARISON BETWEEN Kriging and GAM

#Now get variance and range for holdout an dmethods.

tb_v_gam_CAI
tb_v_gam_fus
tb_v_kriging_CAI
tb_v_kriging_fus

methods_names <- c("tb_v_gam_CAI","tb_v_gam_fus","tb_v_kriging_CAI","tb_v_kriging_fus","tb_v_gwr_CAI","tb_v_gwr_fus")
list_prop_obj <- vector("list",length=length(methods_names))
for(i in 1:length(methods_names)){
  tb <- list_tb[[methods_names[i]]]
  names_id <- c("pred_mod","prop_month")
  names_mod <-unique(tb$pred_mod)
  list_prop_obj[[i]] <- calc_stat_prop_tb_diagnostic(names_mod,names_id,tb)
  names(list_prop_obj)[i] <- methods_names[i]
  #avg_tb <- prop_obj$avg_tb
}

ac_prop_tb_list <- extract_list_from_list_obj(list_prop_obj,"avg_tb")
nb_rows <- sapply(ac_prop_tb_list,FUN=nrow)
method_interp_names<-c("gam_CAI","gam_fus","kriging_CAI","kriging_fus","gwr_CAI","gwr_fus")
for(i in 1:length(methods_names)){
  avg_tb<-ac_prop_tb_list[[i]]
  avg_tb$method_interp <-rep(x=method_interp_names[i],times=nb_rows[i])
  ac_prop_tb_list[[i]] <- avg_tb  
}
#lapply(methods_names,function(i) {rep(x[i],nrows[i],times[i])},times=nb_rows)

#names(ac_prop_tb_list) <- names(list_prop_obj)

t44 <- do.call(rbind,ac_prop_tb_list) #contains all accuracy by method, proportion, model, sample etc.
View(t44)

t44[which.min(t44$rmse),] #Find the mimum rmse across all models and methods...

test <- t44[order(t44$rmse),]
test[1:24,]

test2<-test[test$method_interp%in% c("gam_fus","gam_CAI"),]
test2[1:24,]
#head(ac_prop_tb)
test3<-subset(test,prop_month==0 & method_interp%in%c("gam_CAI"))
#test3<-test[test$method_interp%in% c("gam_CAI"),]
test3[1:24,]

#list_prop_obj$avg_tb
#xyplot(as.formula(plot_formula),group=pred_mod,type="b",
#       data=avg_tb,
#       main=paste("rmse ",plot_name,sep=" "),
#       pch=1:length(avg_tb$pred_mod),
#       par.settings=list(superpose.symbol = list(
#         pch=1:length(avg_tb$pred_mod))),
#       auto.key=list(columns=5))

## DAILY DEVIATIONS WITH MODELS
### Examining results with models for daily deviation called 2: need to subset the models

#c("mod1.dev_mod1","mod1.dev_mod2","mod2.dev_mod1","mod2.dev_mod2","mod3.dev_mod1","mod3.dev_mod2","mod4.dev_mod1",  
#  "mod4.dev_mod2","mod5.dev_mod1","mod5.dev_mod2","mod6.dev_mod1","mod6.dev_mod2","mod7.dev_mod1","mod7.dev_mod2",  
#  "mod_kr.dev_mod1" "mod_kr.dev_mod2")

tb_s_gam_CAI_selected <-subset(tb_s_gam_CAI,prop_month==0 & pred_mod%in%c("mod1","mod4","mod5","mod6","mod7","mod8"))
tb_v_gam_CAI_selected <-subset(tb_v_gam_CAI,prop_month==0 & pred_mod%in%c("mod1","mod4","mod5","mod6","mod7","mod8"))
tb_s_gwr_CAI_selected <-subset(tb_s_gwr_CAI,prop_month==0 & pred_mod%in%c("mod1","mod4","mod5","mod6","mod7","mod8"))
tb_v_gwr_CAI_selected <-subset(tb_v_gwr_CAI,prop_month==0 & pred_mod%in%c("mod1","mod4","mod5","mod6","mod7","mod8"))
tb_s_kriging_CAI_selected <-subset(tb_s_kriging_CAI,prop_month==0 & pred_mod%in%c("mod1","mod4","mod5","mod6","mod7","mod8"))
tb_v_kriging_CAI_selected <-subset(tb_v_kriging_CAI,prop_month==0 & pred_mod%in%c("mod1","mod4","mod5","mod6","mod7","mod8"))

tb_s_gam_CAI2_selected <- subset(raster_prediction_obj16$tb_diagnostic_s,pred_mod%in%c("mod1.dev_mod1","mod1.dev_mod2","mod3.dev_mod1",
                                                                                       "mod3.dev_mod2","mod4.dev_mod1",  "mod4.dev_mod2",
                                                                                       "mod5.dev_mod1","mod5.dev_mod2","mod6.dev_mod1",
                                                                                       "mod6.dev_mod2","mod7.dev_mod1","mod7.dev_mod2"))
tb_v_gam_CAI2_selected <- subset(raster_prediction_obj16$tb_diagnostic_v,pred_mod%in%c("mod1.dev_mod1","mod1.dev_mod2","mod3.dev_mod1",
                                                                                       "mod3.dev_mod2","mod4.dev_mod1",  "mod4.dev_mod2",
                                                                                       "mod5.dev_mod1","mod5.dev_mod2","mod6.dev_mod1",
                                                                                       "mod6.dev_mod2","mod7.dev_mod1","mod7.dev_mod2"))

metric_names <- c("mae","rmse","me","r")

diff_gam_cai2 <- diff_df(tb_s_gam_CAI2_selected,tb_v_gam_CAI2_selected,metric_names)
diff_kriging_CAI_selected <- diff_df(tb_s_kriging_CAI_selected,tb_v_kriging_CAI_selected,metric_names)
diff_gam_CAI_selected <- diff_df(tb_s_gam_CAI_selected,tb_v_gam_CAI_selected,metric_names)
diff_gwr_CAI_selected <- diff_df(tb_s_gwr_CAI_selected,tb_v_gwr_CAI_selected,metric_names)

layout_m<-c(1,1) #one row two columns
par(mfrow=layout_m)

png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
boxplot(diff_gam_cai2$rmse,diff_gam_CAI_selected$rmse,
        diff_kriging_CAI_selected$rmse,diff_gwr_CAI_selected$rmse,names=c("gam_cai2","gam_CAI","kriging","gwr"))

dev.off()

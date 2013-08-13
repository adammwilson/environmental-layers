####################################  INTERPOLATION OF TEMPERATURES  #######################################
############################  Script for manuscript analyses,tables and figures #######################################
#This script reads information concerning the Oregon case study to adapt data for the revised 
# interpolation code.
#Figures and data for the contribution of covariate paper are also produced.
#AUTHOR: Benoit Parmentier                                                                      #
#DATE: 08/13/2013            
#Version: 1
#PROJECT: Environmental Layers project                                       #
#################################################################################################

###Loading R library and packages                                                      
library(gtools)                              # loading some useful tools 
library(mgcv)                                # GAM package by Simon Wood
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                              # NCAR Spatial Interpolation methods such as kriging, splines
library(raster)                              # Hijmans et al. package for raster processing
library(gdata)                               # various tools with xls reading
library(rasterVis)
library(parallel)
library(maptools)
library(maps)
library(reshape)
library(plotrix)
library(plyr)

#### FUNCTION USED IN SCRIPT

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

extract_list_from_list_obj<-function(obj_list,list_name){
  #Create a list of an object from a given list of object using a name prodived as input
  
  list_tmp<-vector("list",length(obj_list))
  for (i in 1:length(obj_list)){
    tmp<-obj_list[[i]][[list_name]] #double bracket to return data.frame
    list_tmp[[i]]<-tmp
  }
  return(list_tmp) #this is  a data.frame
}

#This extract a data.frame object from raster prediction obj and combine them in one data.frame 
extract_from_list_obj<-function(obj_list,list_name){
  #extract object from list of list. This useful for raster_prediction_obj
  library(ply)
  
  list_tmp<-vector("list",length(obj_list))
  for (i in 1:length(obj_list)){
    tmp<-obj_list[[i]][[list_name]] #double bracket to return data.frame
    list_tmp[[i]]<-tmp
  }
  tb_list_tmp<-do.call(rbind.fill,list_tmp) #long rownames
  #tb_list_tmp<-do.call(rbind,list_tmp) #long rownames
  
  return(tb_list_tmp) #this is  a data.frame
}

calc_stat_from_raster_prediction_obj <-function(raster_prediction_obj,stat){
  tb <-raster_prediction_obj$tb_diagnostic_v  #Kriging methods
  
  t<-melt(tb,
          measure=c("mae","rmse","r","m50"), 
          id=c("pred_mod"),
          na.rm=T)
  
  stat_tb<-cast(t,pred_mod~variable,stat)
  return(stat_tb)
}

## Produce data.frame with residuals for models and distance to closest fitting station
calc_dist_ref_data_point <- function(i,list_param){
  #This function creates a list of data.frame containing the distance to teh closest
  # reference point (e.g. fitting station) for a give data frame. 
  #Inputs:
  #data_s: given data.frame from wich distance is computed
  #data_v: reference data.frame, destination, often the fitting points used in analyses
  #i: index variable to operate on list
  #names_var: 
  #Outputs:
  #list_dstspat_er
  
  #Parsing input arguments
  data_s<-list_param$data_s[[i]]
  data_v<-list_param$data_v[[i]]
  
  names_var<-list_param$names_var
  
  ######
  
  names_var<-intersect(names_var,names(data_v)) #there may be missing columns
  #use columns that exists
  
  d_s_v<-matrix(0,nrow(data_v),nrow(data_s))
  for(k in 1:nrow(data_s)){
    pt<-data_s[k,]
    d_pt<-(spDistsN1(data_v,pt,longlat=FALSE))/1000  #Distance to station k in km
    d_s_v[,k]<-d_pt
  }
  
  #Create data.frame with position, ID, dst and residuals...
  
  pos<-vector("numeric",nrow(data_v))
  y<-vector("numeric",nrow(data_v))
  dst<-vector("numeric",nrow(data_v))
  
  for (k in 1:nrow(data_v)){
    pos[k]<-match(min(d_s_v[k,]),d_s_v[k,])
    dst[k]<-min(d_s_v[k,]) 
  }
  
  dstspat_er<-as.data.frame(cbind(v_id=as.vector(data_v$id),s_id=as.vector(data_s$id[pos]),
                                  pos=pos, lat=data_v$lat, lon=data_v$lon, x=data_v$x,y=data_v$y,
                                  dst=dst,
                                  as.data.frame(data_v[,names_var])))
  
  return(dstspat_er)  
}  

### Main function to call to obtain distance to closest fitting stations for valiation dataset
distance_to_closest_fitting_station<-function(raster_prediction_obj,names_mod,dist_classes=c(0)){
  #This function computes the distance between training and testing points and returns and data frame
  #with distance,residuals, ID of training and testing points
  #Input: raster_prediction_obj, names of residuals models to return, distance classes
  #Output: data frame
  
  #Functions used in the script
  
  mae_fun<-function(x){mean(abs(x))} #Mean Absolute Error give a residuals vector
  sd_abs_fun<-function(x){sd(abs(x))} #sd Absolute Error give a residuals vector
  
  ##BEGIN
  
  ##### PART I: generate data.frame with residuals in term of distance to closest fitting station
  
  #return list of training and testing data frames
  list_data_s <- extract_list_from_list_obj(raster_prediction_obj$validation_mod_obj,"data_s") #training
  list_data_v <- extract_list_from_list_obj(raster_prediction_obj$validation_mod_obj,"data_v") #testing (validation)
  
  i<-1
  names_var<-c(names_mod,"dates")
  list_param_dst<-list(i,list_data_s,list_data_v,names_mod)
  names(list_param_dst) <- c("index","data_s","data_v","names_var")
  
  #call function "calc_dist_ref_data_point" over 365 dates
  #note that this function depends on other functions !!! see script
  
  list_dstspat_er <-lapply(1:length(list_data_v),FUN=calc_dist_ref_data_point,list_param=list_param_dst)
  #now assemble in one data.frame
  dstspat_dat<-do.call(rbind.fill,list_dstspat_er)

  ########### PART II: generate distance classes and summary statistics
  
  if (length(dist_classes)==1){
    range_val<-range(dstspat_dat$dst)
    max_val<-round_any(range_val[2],10, f=ceiling) #round max value to nearest 10 (from plyr package)
    min_val<-0
    limit_val<-seq(min_val,max_val, by=10)
  }else{
    limit_val<-dist_classes
  }
 
  dstspat_dat$dst_cat1 <- cut(dstspat_dat$dst,include.lowest=TRUE,breaks=limit_val)
  
  names_var <- intersect(names_mod,names(dstspat_dat))
  t<-melt(dstspat_dat,
          measure=names_var, 
          id=c("dst_cat1"),
          na.rm=T)
  
  mae_tb<-cast(t,dst_cat1~variable,mae_fun)
  sd_abs_tb<-cast(t,dst_cat1~variable,sd_abs_fun)
  
  avg_tb<-cast(t,dst_cat1~variable,mean)
  sd_tb<-cast(t,dst_cat1~variable,sd)
  n_tb<-cast(t,dst_cat1~variable,length)
  #n_NA<-cast(t,dst_cat1~variable,is.na)
  
  #### prepare returning object
  dstspat_obj<-list(dstspat_dat,mae_tb,sd_abs_tb,avg_tb,sd_tb,n_tb)
  names(dstspat_obj) <-c("dstspat_dat","mae_tb","sd_abs_tb","avg_tb","sd_tb","n_tb")
  
  return(dstspat_obj)
  
}

# create plot of accury in term of distance to closest fitting station
plot_dst_spat_fun<-function(stat_tb,names_var,cat_val){
  
  range_y<-range(as.vector(unlist(stat_tb[,names_var])),na.rm=T) #flatten data.frame
  col_t<-rainbow(length(names_var))
  pch_t<- 1:length(names_var)
  plot(stat_tb[,names_var[1]], ylim=range_y,pch=pch_t[1],col=col_t[1],type="b",
       yla="MAE (in degree C)",xlab="",xaxt="n")
  #points((stat_tb[,names_var[k]], ylim=range_y,pch=pch_t[1],col=col_t[1]),type="p")
  for (k in 2:length(names_var)){
    lines(stat_tb[,names_var[k]], ylim=range_y,pch=pch_t[k],col=col_t[k],type="b",
          xlab="",axes=F)
    #points((stat_tb[,names_var[k]], ylim=range_y,pch=pch_t[k],col=col_t[k]),type="p")
  }
  legend("topleft",legend=names_var, 
         cex=1.2, pch=pch_t,col=col_t,lty=1,bty="n")
  axis(1,at=1:length(stat_tb[,1]),labels=stat_tb[,1])
}

plot_dst_MAE <-function(list_param){
  #
  #list_dist_obj: list of dist object 
  #col_t: list of color for each 
  #pch_t: symbol for line
  #legend_text: text for line and symbol
  #mod_name: selected models
  #
  ## BEGIN ##
  
  list_dist_obj<-list_param$list_dist_obj
  col_t<-list_param$col_t 
  pch_t<- list_param$pch_t 
  legend_text <- list_param$legend_text
  list_mod_name<-list_param$mod_name
  
  for (i in 1:length(list_dist_obj)){
    
    l<-list_dist_obj[[i]]
    mae_tb<-l$mae_tb
    n_tb<-l$n_tb
    sd_abs_tb<-l$sd_abs_tb
    
    mod_name<-list_mod_name[i]
    xlab_text<-"distance to fitting station"
    
    n <- unlist(n_tb[1:13,c(mod_name)])
    y <- unlist(mae_tb[1:13,c(mod_name)])
    
    x<- 1:length(y)
    y_sd <- unlist(sd_abs_tb[1:12,c(mod_name)])
    
    ciw <-y_sd
    #ciw2   <- qt(0.975, n) * y_sd2 / sqrt(n)
    
    #plotCI(y=y, x=x, uiw=ciw, col="red", main=paste(" MAE for ",mod_name,sep=""), barcol="blue", lwd=1,
    #       ylab="RMSE (C)", xlab=xlab_text)
    
    ciw   <- qt(0.975, n) * y_sd / sqrt(n)
    
    if(i==1){
      plotCI(y=y, x=x, uiw=ciw, col=col_t[i], main=paste(" Comparison of MAE in ",mod_name,sep=""), barcol="blue", lwd=1,
             ylab="RMSE (C)", xlab=xlab_text)
      lines(y~x, col=col_t[i])
      
    }else{
      lines(y~x, col=col_t[i])
    }
    
  }
  legend("topleft",legend=legend_text, 
         cex=1.2, pch=pch_t,col=col_t,lty=1,bty="n")
  #axis(1,at=1:length(stat_tb[,1]),labels=stat_tb[,1])
  
}

calc_stat_prop_tb <-function(names_mod,raster_prediction_obj,testing=TRUE){
  
  #add for testing??
  if (testing==TRUE){
    tb <-raster_prediction_obj$tb_diagnostic_v #use testing accuracy information
  }else{
    tb <-raster_prediction_obj$tb_diagnostic_s #use training accuracy information
  }
  
  t<-melt(subset(tb,pred_mod==names_mod),
          measure=c("mae","rmse","r","m50"), 
          id=c("pred_mod","prop"),
          na.rm=T)
  
  avg_tb<-cast(t,pred_mod+prop~variable,mean)
  sd_tb<-cast(t,pred_mod+prop~variable,sd)
  n_tb<-cast(t,pred_mod+prop~variable,length)
  #n_NA<-cast(t,dst_cat1~variable,is.na)
  
  #### prepare returning object
  prop_obj<-list(tb,avg_tb,sd_tb,n_tb)
  names(prop_obj) <-c("tb","avg_tb","sd_tb","n_tb")
  
  return(prop_obj)
}

#ploting 
plot_prop_metrics <-function(list_param){
  #
  #list_dist_obj: list of dist object 
  #col_t: list of color for each 
  #pch_t: symbol for line
  #legend_text: text for line and symbol
  #mod_name: selected models
  #
  ## BEGIN ##
  
  list_obj<-list_param$list_prop_obj
  col_t <-list_param$col_t 
  pch_t <- list_param$pch_t 
  legend_text <- list_param$legend_text
  list_mod_name<-list_param$mod_name
  metric_name<-list_param$metric_name
  
  for (i in 1:length(list_obj)){
    
    l<-list_obj[[i]]
    mod_name<-list_mod_name[i]
    avg_tb<-subset(l$avg_tb,pred_mod==mod_name,select=metric_name) #selecte relevant accuarcy metric
    n_tb<-subset(l$n_tb,pred_mod==mod_name,select=metric_name) 
    sd_tb<-subset(l$sd_tb,pred_mod==mod_name,select=metric_name) #l$sd_abs_tb[,metric_name]
    
    xlab_text<-"holdout proportion"
    
    no <- unlist(as.data.frame(n_tb))
    y <- unlist(as.data.frame(avg_tb))
    
    x<- l$avg_tb$prop
    y_sd <- unlist(as.data.frame(sd_tb)) #sd_tb
    
    ciw <-y_sd
    #ciw2   <- qt(0.975, n) * y_sd2 / sqrt(n)
    
    #plotCI(y=y, x=x, uiw=ciw, col="red", main=paste(" MAE for ",mod_name,sep=""), barcol="blue", lwd=1,
    #       ylab="RMSE (C)", xlab=xlab_text)
    
    ciw   <- qt(0.975, no) * y_sd / sqrt(no)
    
    if(i==1){
      plotCI(y=y, x=x, uiw=ciw, col=col_t[i], main=paste(" Comparison of ",metric_name," in ",mod_name,sep=""), barcol="blue", lwd=1,
             ylab="RMSE (C)", xlab=xlab_text)
      lines(y~x, col=col_t[i])
      
    }else{
      lines(y~x, col=col_t[i])
    }
    
  }
  legend("topleft",legend=legend_text, 
         cex=1.2, pch=pch_t,col=col_t,lty=1,bty="n")
  #axis(1,at=1:length(stat_tb[,1]),labels=stat_tb[,1])
  
}

plot_dst_spat_fun<-function(stat_tb,names_var,cat_val){
  
  range_y<-range(as.vector(unlist(stat_tb[,names_var])),na.rm=T) #flatten data.frame
  col_t<-rainbow(length(names_var))
  pch_t<- 1:length(names_var)
  plot(stat_tb[,names_var[1]], ylim=range_y,pch=pch_t[1],col=col_t[1],type="b",
       yla="MAE (in degree C)",xlab="",xaxt="n")
  #points((stat_tb[,names_var[k]], ylim=range_y,pch=pch_t[1],col=col_t[1]),type="p")
  for (k in 2:length(names_var)){
    lines(stat_tb[,names_var[k]], ylim=range_y,pch=pch_t[k],col=col_t[k],type="b",
          xlab="",axes=F)
    #points((stat_tb[,names_var[k]], ylim=range_y,pch=pch_t[k],col=col_t[k]),type="p")
  }
  legend("topleft",legend=names_var, 
         cex=1.2, pch=pch_t,col=col_t,lty=1,bty="n")
  axis(1,at=1:length(stat_tb[,1]),labels=stat_tb[,1])
}

plot_prop_metrics <-function(list_param){
  #
  #list_dist_obj: list of dist object 
  #col_t: list of color for each 
  #pch_t: symbol for line
  #legend_text: text for line and symbol
  #mod_name: selected models
  #
  ## BEGIN ##
  
  list_obj<-list_param$list_prop_obj
  col_t <-list_param$col_t 
  pch_t <- list_param$pch_t 
  legend_text <- list_param$legend_text
  list_mod_name<-list_param$mod_name
  metric_name<-list_param$metric_name
  
  for (i in 1:length(list_obj)){
    
    l<-list_obj[[i]]
    mod_name<-list_mod_name[i]
    avg_tb<-subset(l$avg_tb,pred_mod==mod_name,select=metric_name) #selecte relevant accuarcy metric
    n_tb<-subset(l$n_tb,pred_mod==mod_name,select=metric_name) 
    sd_tb<-subset(l$sd_tb,pred_mod==mod_name,select=metric_name) #l$sd_abs_tb[,metric_name]
    
    xlab_text<-"holdout proportion"
    
    no <- unlist(as.data.frame(n_tb))
    y <- unlist(as.data.frame(avg_tb))
    
    x<- l$avg_tb$prop
    y_sd <- unlist(as.data.frame(sd_tb)) #sd_tb
    
    ciw <-y_sd
    #ciw2   <- qt(0.975, n) * y_sd2 / sqrt(n)
    
    #plotCI(y=y, x=x, uiw=ciw, col="red", main=paste(" MAE for ",mod_name,sep=""), barcol="blue", lwd=1,
    #       ylab="RMSE (C)", xlab=xlab_text)
    
    ciw   <- qt(0.975, no) * y_sd / sqrt(no)
    
    if(i==1){
      plotCI(y=y, x=x, uiw=ciw, col=col_t[i], main=paste(" Comparison of ",metric_name," in ",mod_name,sep=""), barcol="blue", lwd=1,
             ylab="RMSE (C)", xlab=xlab_text)
      lines(y~x, col=col_t[i])
      
    }else{
      lines(y~x, col=col_t[i])
    }
    
  }
  legend("topleft",legend=legend_text, 
         cex=1.2, pch=pch_t,col=col_t,lty=1,bty="n")
  #axis(1,at=1:length(stat_tb[,1]),labels=stat_tb[,1])
  
}

create_s_and_p_table_term_models <-function(i,list_myModels){
  #Purpose:
  #Function to extract smooth term  table,parameter table and AIC from a list of models.
  #Originally created to processed GAM models run over a full year.
  #Inputs: 
  #1) list_myModels: list of fitted GAM models
  #2) i: index of list to run with lapply or mcapply
  #Outputs: list of 
  #1)s.table.term
  #2)p.table.term
  #3)AIC list
  #Authors: Benoit Parmentier
  #date: 08/142013
  
  ### Functions used in the scritp:
  
  #Remove models that were not fitted from the list
  #All modesl that are "try-error" are removed
  remove_errors_list<-function(list_items){
    #This function removes "error" items in a list
    list_tmp<-list_items
    for(i in 1:length(list_items)){
      
      if(inherits(list_items[[i]],"try-error")){
        list_tmp[[i]]<-0
      }else{
        list_tmp[[i]]<-1
      }
    }
    cnames<-names(list_tmp[list_tmp>0])
    x<-list_items[match(cnames,names(list_items))]
    return(x)
  }
  
  #turn term from list into data.frame
  name_col<-function(i,x){
    x_mat<-x[[i]]
    x_df<-as.data.frame(x_mat)
    x_df$mod_name<-rep(names(x)[i],nrow(x_df))
    x_df$term_name <-row.names(x_df)
    return(x_df)
  }
  
  ##BEGIN ###
  
  myModels <- list_myModels[[i]]
  myModels<-remove_errors_list(myModels)
  #could add AIC, GCV to the table as well as ME, RMSE...+dates...
  
  summary_list <- lapply(myModels, summary)
  s.table_list <- lapply(summary_list, `[[`, 's.table')
  p.table_list <- lapply(summary_list, `[[`, 'p.table')
  AIC_list <- lapply(myModels, AIC)
  
  #now put in one table
  
  s.table_list2<-lapply(1:length(myModels),name_col,s.table_list)
  p.table_list2<-lapply(1:length(myModels),name_col,p.table_list)
  s.table_term <-do.call(rbind,s.table_list2)
  p.table_term <-do.call(rbind,p.table_list2)
  
  #Now get AIC
  AIC_list2<-lapply(1:length(myModels),name_col,AIC_list)
  AIC_models <- do.call(rbind,AIC_list2)
  names(AIC_models)[1]<-"AIC"
  
  #Set up return object
  
  s_p_table_term_obj<-list(s.table_term,p.table_term,AIC_models)
  names(s_p_table_term_obj) <-c("s.table_term","p.table_term","AIC_models")
  return(s_p_table_term_obj)
  
}

convert_spdf_to_df_from_list <-function(obj_list,list_name){
  #extract object from list of list. This useful for raster_prediction_obj
  #output: data.frame formed by rbinding sp data.frame in the list
  library(plyr)
  
  list_tmp<-vector("list",length(obj_list))
  for (i in 1:length(obj_list)){
    #tmp<-obj_list[[i]][[list_name]] #double bracket to return data.frame
    list_tmp[[i]]<-as.data.frame(obj_list[[i]])
  }
  tb_list_tmp<-do.call(rbind.fill,list_tmp) #long rownames
  #tb_list_tmp<-do.call(rbind,list_tmp) #long rownames
  
  return(tb_list_tmp) #this is  a data.frame
}

##############################
#### Parameters and constants  


script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/"
#source(file.path(script_path,"interpolation_method_day_function_multisampling_06082013.R")) #Include GAM_day

#in_dir<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_GAM_fus_all_lst_05312013"
in_dir1 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb3_07092013/"
in_dir2 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb4_07152013/"

#kriging results:
in_dir3 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_kriging_day_lst_comb3_07112013"
#gwr results:
in_dir4 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gwr_day_lst_comb3_part1_07122013"
#multisampling results (gam)
in_dir5<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_daily_mults10_lst_comb3_08082013"
in_dir6<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_kriging_daily_mults10_lst_comb3_08062013"
in_dir7<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gwr_daily_mults10_lst_comb3_08072013"

out_dir<-"/home/parmentier/Data/IPLANT_project/paper_analyses_tables_fig_08032013"
setwd(out_dir)

y_var_name <- "dailyTmax"

out_prefix<-"analyses_08032013"

method_interpolation <- "gam_daily"
covar_obj_file1 <- "covar_obj__365d_gam_day_lst_comb3_07092013.RData"
met_obj_file_1 <- "met_stations_outfiles_obj_gam_daily__365d_gam_day_lst_comb3_07092013.RData"

#raster_prediciton object for baseline 1 () s(lat,lon) + s(elev)) and baseline 2 (slat,lon))
raster_obj_file_1 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_lst_comb3_07092013.RData" 
raster_obj_file_2 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_lst_comb4_07152013.RData"

raster_obj_file_3 <- "raster_prediction_obj_kriging_daily_dailyTmax_365d_kriging_day_lst_comb3_07112013.RData"
raster_obj_file_4 <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_day_lst_comb3_part1_07122013.RData"
#multisampling using baseline lat,lon + elev
raster_obj_file_5 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_daily_mults10_lst_comb3_08082013.RData"
raster_obj_file_6 <- "raster_prediction_obj_kriging_daily_dailyTmax_365d_kriging_daily_mults10_lst_comb3_08062013.RData"
raster_obj_file_7 <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_daily_mults10_lst_comb3_08072013.RData"
#raster_prediction_obj_gam_daily_dailyTmax_365d_gam_daily_mults10_lst_comb3_08082013.RData

#Load objects containing training, testing, models objects 

covar_obj <-load_obj(file.path(in_dir1,covar_obj_file1))
infile_covariates <- covar_obj$infile_covariates
infile_reg_outline <- covar_obj$infile_reg_outline
covar_names<- covar_obj$covar_names
#####
s_raster <- brick(file.path(in_dir1,infile_covariates))
names(s_raster)<-covar_names

raster_prediction_obj_1 <-load_obj(file.path(in_dir1,raster_obj_file_1)) #comb3
raster_prediction_obj_2 <-load_obj(file.path(in_dir2,raster_obj_file_2)) #comb4
raster_prediction_obj_3 <-load_obj(file.path(in_dir3,raster_obj_file_3))
raster_prediction_obj_4 <-load_obj(file.path(in_dir4,raster_obj_file_4)) 
raster_prediction_obj_5 <-load_obj(file.path(in_dir5,raster_obj_file_5)) #gam daily multisampling 10 to 70%
raster_prediction_obj_6 <-load_obj(file.path(in_dir6,raster_obj_file_6)) #kriging daily multisampling 
raster_prediction_obj_7 <-load_obj(file.path(in_dir7,raster_obj_file_7)) #kriging daily multisampling 

names(raster_prediction_obj_1) #list of two objects

### ACCURACY TABLE WITH BASELINES

#Check input covariates and model formula:
raster_prediction_obj_1$method_mod_obj[[1]]$formulas
raster_prediction_obj_2$method_mod_obj[[1]]$formulas

###baseline 2: s(lat,lon) + s(elev)

summary_metrics_v1<-raster_prediction_obj_1$summary_metrics_v
summary_metrics_v2<-raster_prediction_obj_2$summary_metrics_v

table_data1 <-summary_metrics_v1$avg[,c("mae","rmse","me","r")]
table_data2 <-summary_metrics_v2$avg[,c("mae","rmse","me","r")]

model_col<-c("Baseline2","Northness","Eastness","LST","DISTOC","Forest","CANHEIGHT","LST*Forest") # removed ,"LST*CANHEIGHT")
names_table_col<-c("DiffMAE","DiffRMSE","DiffME","Diffr","Model")

df1<- as.data.frame(sapply(table_data1,FUN=function(x) x-x[1]))
df1<- round(df1,digit=3) #roundto three digits teh differences
df1$Model <-model_col
names(df1)<- names_table_col
df1

###baseline 1: s(lat,lon) 

model_col<-c("Baseline1","Elevation","Northing","Easting","LST","DISTOC","Forest","CANHEIGHT") #,"LST*Forest") # removed ,"LST*CANHEIGHT")
df2<- as.data.frame(sapply(table_data2,FUN=function(x) x-x[1]))
df2<- round(df2,digit=3) #roundto three digits teh differences
df2$Model <-model_col
names(df2)<- names_table_col
df2

file_name<-paste("table3b_baseline2_paper","_",out_prefix,".txt",sep="")
write.table(df1,file=file_name,sep=",")

file_name<-paste("table3a_baseline1_paper","_",out_prefix,".txt",sep="")
write.table(df2,file=file_name,sep=",")

##Table 4: Interpolation methods comparison

#get sd for kriging, gam and gwr
tb1 <-raster_prediction_obj_1$tb_diagnostic_v  #Kriging methods
tb2 <-raster_prediction_obj_2$tb_diagnostic_v  #Kriging methods
tb3 <-raster_prediction_obj_3$tb_diagnostic_v  #Kriging methods
tb4 <-raster_prediction_obj_4$tb_diagnostic_v  #Kriging methods

names_mod<-c("mod1","mod2","mod3","mod4","mod5","mod6","mod7","mod8","mod9")

sd1 <- calc_stat_from_raster_prediction_obj(raster_prediction_obj_1,"sd")
sd2 <- calc_stat_from_raster_prediction_obj(raster_prediction_obj_2,"sd")
sd3 <- calc_stat_from_raster_prediction_obj(raster_prediction_obj_3,"sd")
sd4 <- calc_stat_from_raster_prediction_obj(raster_prediction_obj_4,"sd")

table_sd<-rbind(sd1[1,],sd3[1,])
table_sd<-rbind(table_sd,sd4[1,])

summary_metrics_v3<-raster_prediction_obj_3$summary_metrics_v  #Kriging methods
summary_metrics_v4<-raster_prediction_obj_4$summary_metrics_v  # GWR method

table_data3 <-summary_metrics_v3$avg[1,c("mae","rmse","me","r")] #first line mod1 (baseline)
table_data4 <-summary_metrics_v4$avg[1,c("mae","rmse","me","r")] #first line mod1 (baseline)
table_data1 <- table_data1[1,]

table<-rbind(table_data1,table_data3)
table<-rbind(table,table_data4)
table<- round(table,digit=3) #roundto three digits teh differences

model_col<-c("GAM","Kriging","GWR")
names_table_col<-c("MAE","RMSE","ME","R","Model")

table$Model <-model_col
names(table)<- names_table_col
table

file_name<-paste("table4_avg_paper","_",out_prefix,".txt",sep="")
write.table(table,file=file_name,sep=",")

file_name<-paste("table4_sd_paper","_",out_prefix,".txt",sep="")
write.table(table_sd,file=file_name,sep=",")

#for(i in nrow(table))
#mean_val<-table[i,j]
#sd_val<-table_sd[i,j]
#element<-paste(mean_val,"+-",sd_val,sep="")
#table__paper[i,j]<-element

####################################
####### Now create figures #############

#figure 1: study area
#figure 2: methodological worklfow
#figure 3:Figure 3. MAE/RMSE and distance to closest fitting station.
#Figure 4. RMSE and MAE, mulitisampling and hold out for FSS and GAM.
#Figure 5. Overtraining tendency
#Figure 6: Spatial pattern of prediction for one day

### Figure 1

### Figure 2: 

#not generated in R

################################################
################### Figure 3

#Analysis accuracy in term of distance to closest station
#Assign model's names

names_mod <- paste("res_mod",1:9,sep="")
names(raster_prediction_obj_1$validation_mod_obj[[1]])
limit_val<-seq(0,150, by=10)

l1 <- distance_to_closest_fitting_station(raster_prediction_obj_1,names_mod,dist_classes=limit_val)
l3 <- distance_to_closest_fitting_station(raster_prediction_obj_3,names_mod,dist_classes=limit_val)
l4 <- distance_to_closest_fitting_station(raster_prediction_obj_4,names_mod,dist_classes=limit_val)

list_dist_obj<-list(l1,l3,l4)
col_t<-c("red","blue","black")
pch_t<- 1:length(col_t)
legend_text <- c("GAM","Kriging","GWR")
mod_name<-c("res_mod1","res_mod1","res_mod1")#selected models

#png_names<- 
#png(file.path(out_path,paste("clim_surface_",y_var_name,"_",sampling_dat$date[i],"_",sampling_dat$prop[i],
#                             "_",sampling_dat$run_samp[i],out_prefix,".png", sep="")))

list_param_plot<-list(list_dist_obj,col_t,pch_t,legend_text,mod_name)
names(list_param_plot)<-c("list_dist_obj","col_t","pch_t","legend_text","mod_name")

#debug(plot_dst_MAE)
plot_dst_MAE(list_param_plot)

####################################################
#########Figure 4. RMSE and MAE, mulitisampling and hold out for FSS and GAM.

#Using baseline 2: lat,lon and elev

#Use run of 7 hold out proportions, 10 to 70% with 10 random samples and 12 dates...
#Use gam_day method
#Use comb3 i.e. using baseline s(lat,lon)+s(elev)

#names_mod<-c("mod1","mod2","mod3","mod4","mod5","mod6","mod7","mod8","mod9")
names_mod<-c("mod1")

debug(calc_stat_prop_tb)
prop_obj_gam<-calc_stat_prop_tb(names_mod,raster_prediction_obj_5)
prop_obj_kriging<-calc_stat_prop_tb(names_mod,raster_prediction_obj_6)
prop_obj_gwr<-calc_stat_prop_tb(names_mod,raster_prediction_obj_7)

list_prop_obj<-list(prop_obj_gam,prop_obj_kriging,prop_obj_gwr)
col_t<-c("red","blue","black")
pch_t<- 1:length(col_t)
legend_text <- c("GAM","Kriging","GWR")
mod_name<-c("mod1","mod1","mod1")#selected models
metric_name<-"rmse"
#png_names<- 
#png(file.path(out_path,paste("clim_surface_",y_var_name,"_",sampling_dat$date[i],"_",sampling_dat$prop[i],
#                             "_",sampling_dat$run_samp[i],out_prefix,".png", sep="")))

list_param_plot<-list(list_prop_obj,col_t,pch_t,legend_text,mod_name,metric_name)
names(list_param_plot)<-c("list_prop_obj","col_t","pch_t","legend_text","mod_name","metric_name")
#debug(plot_prop_metrics)
plot_prop_metrics(list_param_plot)

####################################################
#########Figure 5. Overtraining tendency

#read in relevant data:

tb5 <-raster_prediction_obj_5$tb_diagnostic_v  #gam dailycontains the accuracy metrics for each run...
tb6 <-raster_prediction_obj_6$tb_diagnostic_v  #Kriging daily methods
tb7 <-raster_prediction_obj_7$tb_diagnostic_v  #gwr daily methods

prop_obj_gam_s<-calc_stat_prop_tb(names_mod,raster_prediction_o  bj_5,testing=FALSE)
prop_obj_kriging_s<-calc_stat_prop_tb(names_mod,raster_prediction_obj_6,testing=FALSE)
prop_obj_gwr_s<-calc_stat_prop_tb(names_mod,raster_prediction_obj_7,testing=FALSE)

prop_obj_gam$avg_tb - prop_obj_gam_s$avg_tb
plot(prop_obj_gam_s$avg_tb$rmse ~ prop_obj_gam_s$avg_tb$prop, type="b",)

y_range<-range(prop_obj_gam$avg_tb$rmse,prop_obj_gam_s$avg_tb$rmse)
plot(prop_obj_gam$avg_tb$rmse ~ prop_obj_gam$avg_tb$prop, type="b",ylim=y_range)
lines(prop_obj_gam_s$avg_tb$rmse ~ prop_obj_gam_s$avg_tb$prop, type="b",ylim=y_range,col=c("red"))
lines(prop_obj_gam$avg_tb$rmse ~ prop_obj_gam$avg_tb$prop, type="b",ylim=y_range,col=c("red"),lty=2)
lines(prop_obj_gwr_s$avg_tb$rmse ~ prop_obj_gwr_s$avg_tb$prop, type="b",ylim=y_range,col=c("black"))
lines(prop_obj_gwr$avg_tb$rmse ~ prop_obj_gam$avg_tb$prop, type="b",ylim=y_range,col=c("black"),lty=2)

y_range<-range(prop_obj_kriging$avg_tb$rmse,prop_obj_kriging_s$avg_tb$rmse)
plot(prop_obj_kriging$avg_tb$rmse ~ prop_obj_kriging$avg_tb$prop, type="b",ylim=y_range,col=c("blue"),lty=2)
lines(prop_obj_kriging_s$avg_tb$rmse ~ prop_obj_kriging_s$avg_tb$prop, type="b",ylim=y_range,col=c("blue"))

## Calculate average difference for RMSE for all three methods
#read in relevant data:
tb1_s<-extract_from_list_obj(raster_prediction_obj_1$validation_mod_obj,"metrics_s")
rownames(tb1_s)<-NULL #remove row names
tb1_s$method_interp <- "gam_daily" #add type of interpolation...out_prefix too??

tb3_s<-extract_from_list_obj(raster_prediction_obj_4$validation_mod_obj,"metrics_s")
rownames(tb1_s)<-NULL #remove row names
tb3_s$method_interp <- "kriging_daily" #add type of interpolation...out_prefix too??

tb4_s<-extract_from_list_obj(raster_prediction_obj_3$validation_mod_obj,"metrics_s")
rownames(tb4_s)<-NULL #remove row names
tb4_s$method_interp <- "gwr_daily" #add type of interpolation...out_prefix too??

#tb1_s <-raster_prediction_obj_1$tb_diagnostic_s  #gam dailycontains the accuracy metrics for each run...
#tb3_s <-raster_prediction_obj_3$tb_diagnostic_s  #Kriging daily methods
#tb4_s <-raster_prediction_obj_4$tb_diagnostic_s  #gwr daily methods

tb1 <-raster_prediction_obj_1$tb_diagnostic_v  #gam dailycontains the accuracy metrics for each run...
tb3 <-raster_prediction_obj_3$tb_diagnostic_v  #Kriging daily methods
tb4 <-raster_prediction_obj_4$tb_diagnostic_v  #gwr daily methods

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

#debug(diff_df)
diff_tb1 <-diff_df(tb1_s[tb1_s$pred_mod=="mod1",],tb1[tb1$pred_mod=="mod1",],c("mae","rmse")) #select differences for mod1
diff_tb3 <-diff_df(tb3_s[tb3_s$pred_mod=="mod1",],tb3[tb3$pred_mod=="mod1",],c("mae","rmse"))
diff_tb4 <-diff_df(tb4_s[tb4_s$pred_mod=="mod1",],tb4[tb4$pred_mod=="mod1",],c("mae","rmse"))

x<-data.frame(gam=diff_tb1$mae,gwr=diff_tb3$mae,kriging=diff_tb4$mae)

boxplot(x) #plot differences in training and testing accuracies for three methods

mae_tmp<- data.frame(gam=tb1[tb1$pred_mod=="mod1",c("mae")],
                     kriging=tb3[tb3$pred_mod=="mod1",c("mae")],
                     gwr=tb4[tb4$pred_mod=="mod1",c("mae")])

plot(mae_tmp$gam,col=c("red"),type="b")
lines(mae_tmp$kriging,col=c("blue"),type="b")
lines(mae_tmp$gwr,col=c("black"),type="b")
legend("topleft",legend=legend_text, 
       cex=1.2, pch=pch_t,col=col_t,lty=1,bty="n")

max(mae_tmp$gam)

x2<-tb1[tb1$pred_mod=="mod1",c("mae","date")]
arrange(x2,desc(mae))

kriging=tb3[tb3$pred_mod=="mod1",c("mae")],
                     gwr=tb4[tb4$pred_mod=="mod1",c("mae")])

##### MONTHLY AVERAGES

tb1_month<-raster_prediction_obj_1$summary_month_metrics_v[[1]] #note that this is for model1
tb3_month<-raster_prediction_obj_3$summary_month_metrics_v[[1]]
tb4_month<- raster_prediction_obj_4$summary_month_metrics_v[[1]]

y_range<-range(tb1_month$mae,tb3_month$mae,tb4_month$mae)
plot(1:12,tb1_month$mae,col=c("red"),type="b",ylim=y_range)
lines(1:12,tb3_month$mae,col=c("blue"),type="b")
lines(1:12,tb4_month$mae,col=c("black"),type="b")

date<-strptime(tb1$date, "%Y%m%d")   # interpolation date being processed
month<-strftime(date, "%m")          # current month of the date being processed

tb1$month<-month
x3<-tb1[tb1$pred_mod=="mod1",]

(plot(x3[month=="01",c("mae")]))
median(x3[x3$month=="03",c("mae")],na.rm=T)
mean(x3[x3$month=="03",c("mae")],na.rm=T)


####### FIGURE 6 ######

y_var_name <-"dailyTmax"
index<-244 #index corresponding to January 1

lf1 <- raster_prediction_obj_1$method_mod_obj[[index]][[y_var_name]]
lf3 <- raster_prediction_obj_3$method_mod_obj[[index]][[y_var_name]]
lf4 <- raster_prediction_obj_4$method_mod_obj[[index]][[y_var_name]]

date_selected <- "20109101"
methods_names <-c("gam","kriging","gwr")
names_layers<-methods_names
lf <-list(lf1$mod1,lf3$mod1,lf4$mod1)
#lf <-lf[[1]]

pred_temp_s <-stack(lf)
#predictions<-mask(predictions,mask_rast)
names(pred_temp_s)<-names_layers
s.range <- c(min(minValue(pred_temp_s)), max(maxValue(pred_temp_s)))
#s.range <- s.range+c(5,-5)
col.breaks <- pretty(s.range, n=200)
lab.breaks <- pretty(s.range, n=100)
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
max_val<-s.range[2]
min_val <-s.range[1]
#max_val<- -10
min_val <- 0
layout_m<-c(1,3) #one row two columns

png(paste("spatial_pattern_tmax_prediction_levelplot_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])

levelplot(pred_temp_s,main="Interpolated Surfaces Method Comparison", ylab=NULL,xlab=NULL,
          par.settings = list(axis.text = list(font = 2, cex = 1.3),layout=layout_m,
                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
          names.attr=names_layers,col.regions=temp.colors,at=seq(max_val,min_val,by=0.01))
#col.regions=temp.colors(25))
dev.off()

## FIGURE COMPARISON OF  MODELS COVARRIATES

lf1 <- raster_prediction_obj_1$method_mod_obj[[index]][[y_var_name]]
lf1 #contains the models for gam

pred_temp_s <-stack(lf1$mod1,lf1$mod4)
date_selected <- "20109101"
#names_layers <-c("mod1=s(lat,long)+s(elev)","mod4=s(lat,long)+s(LST)","diff=mod1-mod4")
names_layers <-c("mod1=s(lat,long)+s(elev)","mod4=s(lat,long)+s(LST)")
names(pred_temp_s)<-names_layers
s.range <- c(min(minValue(pred_temp_s)), max(maxValue(pred_temp_s)))
#s.range <- s.range+c(5,-5)
col.breaks <- pretty(s.range, n=200)
lab.breaks <- pretty(s.range, n=100)
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
max_val<-s.range[2]
min_val <-s.range[1]
#max_val<- -10
min_val <- 0
layout_m<-c(1,2) #one row two columns

png(paste("spatial_pattern_tmax_prediction_models_gam_levelplot_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])

levelplot(pred_temp_s,main="Interpolated Surfaces Model Comparison", ylab=NULL,xlab=NULL,
          par.settings = list(axis.text = list(font = 2, cex = 1.3),layout=layout_m,
                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
          names.attr=names_layers,col.regions=temp.colors,at=seq(max_val,min_val,by=0.01))
#col.regions=temp.colors(25))
dev.off()

diff<-raster(lf1$mod1)-raster(lf1$mod4)
names_layers <- c("difference=mod1-mod4")
names(diff) <- names_layers
plot(diff,col=temp.colors(100),main=names_layers)
#levelplot(diff,main="Interpolated Surfaces Model Comparison", ylab=NULL,xlab=NULL,
#          par.settings = list(axis.text = list(font = 2, cex = 1.3),layout=c(1,1),
#                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
#          names.attr=names_layers,col.regions=temp.colors)

######## NOW GET A ACCUURAY BY STATIONS

list_data_v<-extract_list_from_list_obj(raster_prediction_obj_1$validation_mod_obj,"data_v")
data_v_test <- list_data_v[[1]]

#Convert sp data.frame and combined them in one unique df, see function define earlier
data_v_combined <-convert_spdf_to_df_from_list(list_data_v) #long rownames
names_var<-c("res_mod1","res_mod2","res_mod3","res_mod4","res_mod5","res_mod6","res_mod7","res_mod8")
t<-melt(data_v_combined,
        measure=names_var, 
        id=c("id"),
        na.rm=T)

mae_fun<-function(x){mean(abs(x))} #Mean Absolute Error give a residuals vector
sd_abs_fun<-function(x){sd(abs(x))} #sd Absolute Error give a residuals vector

mae_tb<-cast(t,id~variable,mae_fun) #join to station location...

sd_abs_tb<-cast(t,dst_cat1~variable,sd_abs_fun)

#avg_tb<-cast(t,dst_cat1~variable,mean)
#sd_tb<-cast(t,dst_cat1~variable,sd)
#n_tb<-cast(t,dst_cat1~variable,length)

met_obj <-load_obj(file.path(in_dir1,met_obj_file_1))
stat_loc<-readOGR(dsn=dirname(met_obj$loc_stations),layer=sub(".shp","",basename(met_obj$loc_stations)))

data_v_mae <-merge(mae_tb,stat_loc,by.x=c("id"),by.y=c("STAT_ID"))
hist(data_v_mae$res_mod1)
mean(data_v_mae$res_mod1)

coords<- data_v_mae[c('longitude','latitude')]              #Define coordinates in a data frame
CRS_interp<-proj4string(data_v_test)
coordinates(data_v_mae)<-coords                      #Assign coordinates to the data frame
proj4string(data_v_mae)<- proj4string(stat_loc)                #Assign coordinates reference system in PROJ4 format
data_v_mae<-spTransform(data_v_mae,CRS(CRS_interp))     #Project from WGS84 to new coord. system

p<-bubble(data_v_mae,"res_mod1",maxsize=4,col=c("red"),fill=FALSE)
#p<-bubble(data_v_mae,"res_mod1",maxsize=4,col=c("red"),fill=FALSE,key.entries=c(1,1.5,2,2.5,3,3.5,4,4.5))

p

infile_reg_outline <- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
reg_outline <- readOGR(dsn=dirname(infile_reg_outline),layer=sub(".shp","",basename(infile_reg_outline)))

p + layer(sp.polygons(reg_outline,lwd=0.9,col="darkgray"))

p4<-bubble(data_v_mae,"res_mod4",maxsize=4,col=c("red"),fill=FALSE)
p4 + layer(sp.polygons(reg_outline,lwd=0.9,col="darkgray"))

col_t <- colorRampPalette(c('blue', 'white', 'red'))

p_elev <-levelplot(subset(s_raster,"elev_s"),margin=FALSE)
p4 <-bubble(data_v_mae[data_v_mae$res_mod4>2.134,],"res_mod4",maxsize=4,col=c("blue"),fill=FALSE)
p_elev + p4 + layer(sp.polygons(reg_outline,lwd=0.9,col="green"))
title("mod4")

p_elev <-levelplot(subset(s_raster,"elev_s"))
p1 <-bubble(data_v_mae[data_v_mae$res_mod1>2.109,],"res_mod1",maxsize=4,col=c("blue"),fill=FALSE)
p_elev + p1 + layer(sp.polygons(reg_outline,lwd=0.9,col="green"))
#bubble(data_v_mae,"res_mod1")
#p<-spplot(data_v_mae,"res_mod1",maxsize=4,col=c("red"))
#p
#stations that are outliers in one but not the other...
id_setdiff<-setdiff(data_v_mae[data_v_mae$res_mod1>2.109,]$id,data_v_mae[data_v_mae$res_mod4>2.134,]$id)

data_id_setdiff <- data_v_mae[data_v_mae$id %in% id_setdiff,]

p_elev +layer(sp.polygons(reg_outline,lwd=0.9,col="green")) + layer(sp.points(data_id_setdiff,pch=4,cex=2,col="pink"))

#### ls()

#Now get p values and other things...

###baseline 2: s(lat,lon) + s(elev)

tb1_s
names_var <- c("mae","rmse","me","r")
#id_var <- 
t<-melt(tb1_s,
        measure=names_var, 
        id=c("pred_mod"),
        na.rm=T)

summary_metrics_s1$avg <-cast(t,pred_mod~variable,mean)
#sd_abs_tb<-cast(t,dst_cat1~variable,sd_abs_fun)

#summary_metrics_s1<-raster_prediction_obj_1$summary_metrics_s
#summary_metrics_s2<-raster_prediction_obj_2$summary_metrics_v

table_data1 <-summary_metrics_s1$avg[,c("mae","rmse","me","r")]
#table_data2 <-summary_metrics_v2$avg[,c("mae","rmse","me","r")]

model_col<-c("Baseline2","Northness","Eastness","LST","DISTOC","Forest","CANHEIGHT","LST*Forest") # removed ,"LST*CANHEIGHT")
names_table_col<-c("DiffMAE","DiffRMSE","DiffME","Diffr","Model")

df1<- as.data.frame(sapply(table_data1,FUN=function(x) x-x[1]))
df1<- round(df1,digit=3) #roundto three digits teh differences
df1$Model <-model_col
names(df1)<- names_table_col
df1

list_myModels <- extract_list_from_list_obj(raster_prediction_obj_1$method_mod_obj,"mod")

#for (i in 1:length(list_myModels)){
#  i<-1


list_models_info <-lapply(1:length(list_myModels),FUN=create_s_and_p_table_term_models,list_myModels)
#raster_prediction_obj_1$method_mod_obj[[i]]$sampling_dat$date
dates<-(extract_from_list_obj(raster_prediction_obj_1$method_mod_obj,"sampling_dat"))$date #get vector of dates
names(list_models_info)<-dates

#Add dates to the data.frame??

s.table_term_tb <-extract_from_list_obj(list_models_info,"s.table_term")
s.table_term_tb_t <-extract_list_from_list_obj(list_models_info,"s.table_term")

max_val<-round_any(range_val[2],10, f=ceiling) #round max value to nearest 10 (from plyr package)
min_val<-0
limit_val<-seq(min_val,max_val, by=10)
}else{
  limit_val<-dist_classes
}
threshold_val<-c(0.01,0.05,0.1)
s.table_term_tb$p_val_rec1 <- s.table_term_tb[["p-value"]] < threshold_val[1]
s.table_term_tb$p_val_rec2 <- s.table_term_tb[["p-value"]] < threshold_val[2]
s.table_term_tb$p_val_rec3 <- s.table_term_tb[["p-value"]] < threshold_val[3]

#dstspat_dat$dst_cat1 <- cut(dstspat_dat$dst,include.lowest=TRUE,breaks=limit_val)

#test<-do.call(rbind,s.table_term_tb_t)
#cut
s.table_term_tb
names_var <- c("p-value")
#id_var <- 
t<-melt(s.table_term_tb,
        measure=names_var, 
        id=c("mod_name","term_name"),
        na.rm=T)

summary_s.table_term <- cast(t,term_name+mod_name~variable,median)
summary_s.table_term

names_var <- c("p_val_rec1","p_val_rec2","p_val_rec3")
t2<-melt(s.table_term_tb,
        measure=names_var, 
        id=c("mod_name","term_name"),
        na.rm=T)

summary_s.table_term2 <- cast(t2,term_name+mod_name~variable,sum)
summary_s.table_term2

#Now combine tables and drop duplicate columns the combined table can be modified for the paper...
s.table_summary_tb <- cbind(summary_s.table_term,summary_s.table_term2[,-c("term_name","mod_name")]) 



################### END OF SCRIPT ###################



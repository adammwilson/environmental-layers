##VALIDATION-ACCURACY ASSESSMENT

#The interpolation is done first at the monthly add delta.
#AUTHOR: Benoit Parmentier                                                                        
#DATE: 05/06/2013                                                                                 

#Change this to allow explicitly arguments...
#Arguments: 
#1)list of climatology files for all models...(365*nb of models)
#2)data_s:training
#3)data_v:testing
#4)list of dates??: index
#5)stack of covariates: not needed at this this stage
#6)dst: data at the monthly time scale

#Functions used in the script
calc_val_metrics<-function(x,y){
  #This functions calculates accurayc metrics on given two vectors.
  #Arguments: list of fitted models, raster stack of covariates
  #Output: spatial grid data frame of the subset of tiles
  #s_sgdf<-as(r_stack,"SpatialGridDataFrame") #Conversion to spatial grid data frame
  
  residuals<-x-y
  mae<-mean(abs(residuals),na.rm=T)
  rmse<-sqrt(mean((residuals)^2,na.rm=T))
  me<-mean(residuals,na.rm=T)
  r<-cor(x,y,use="complete")
  m50<-median(residuals,na.rm=T)
  metrics_dat<-as.data.frame(cbind(mae,rmse,me,r,m50))
  names(metrics_dat)<-c("mae","rmse","me","r","m50")
  metrics_obj<-list(metrics_dat,as.data.frame(residuals))
  names(metrics_obj)<-c("metrics_dat","residuals")
  return(metrics_obj)
}

calc_val_metrics_rast <-function(df,y_ref,pred_names){
  #Input parameters:
  #1) df: data frame containing the observed and predicted variables (data_s or data_v)
  #2) y_ref: observed variable correspond to y_var_name??
  #3) pred_names: models run containig predicted values
  
  # library
  library(maptools)
  
  ## START SCRIPT
  
  list_metrics<-vector("list",length(pred_names))
  list_residuals<-vector("list",length(pred_names))
  names(list_metrics)<-pred_names
  names(list_residuals)<-pred_names
  for (j in 1:length(pred_names)){
    pred_var<-pred_names[j]
    metrics<-calc_val_metrics(df[[pred_var]],df[[y_ref]])
    list_metrics[[j]]<-metrics[[1]]
    list_residuals[[j]]<-metrics[[2]]
  }
  metrics_df<-do.call(rbind,list_metrics)
  metrics_df$pred_mod <- pred_names #adding name column
  residuals_df<-do.call(cbind,list_residuals) #creating data frame for residuals
  names(residuals_df)<-paste("res",pred_names,sep="_")
  
  accuracy_obj<-list(metrics_df,residuals_df) #output object
  names(accuracy_obj)<-c("metrics","residuals") 
  return(accuracy_obj)
}  

### Main function to compute training and testing accuracy statistics
calculate_accuracy_metrics<-function(i,list_param){
  library(plyr)
  ### Caculate accuracy metrics
  
  ############### BEGIN SCRIPT ###########
  
  #PARSING INPUT PARAMETERS
  out_path <- list_param$out_path
  day_list <- list_param$rast_day_year_list[[i]] #this is the list of raster files, may be daily or monthly predictions
  if(class(day_list[[1]])=="list"){
    day_list<-unlist(day_list)
  }
  names_mod <- names(day_list) #names of the predicted variables

  y_ref <- list_param$y_ref  #This is the reference variable from which resituals and accuracy metrics are created
  multi_time_scale <- list_param$multi_time_scale
  
  data_v <- list_param$list_data_v[[i]]
  data_s <- list_param$list_data_s[[i]]
  sampling_dat_day <- list_param$list_sampling_dat[[i]]
  
  ## Now create the stack
  
  rast_day_mod <- stack(day_list)
  names(rast_day_mod) <- names(day_list)
  #Change to handle cases in which data_v is NULL!!!
    
  ns <- nrow(data_s) # some loss of data might have happened because of the averaging...
  nv <- nrow(data_v)
  
  #add sampling dat info...
  N=length(names_mod)
  
  #Handle case of 0% hold out, monhtly or daily
  if (nv > 0){
    run_info<-cbind(sampling_dat_day,n=nv)
    run_info[rep(seq_len(nrow(run_info)), each=N),] #repeating same row n times
    
    extract_data_v<-extract(rast_day_mod,data_v,df=TRUE)
    data_v <-spCbind(data_v,extract_data_v) #should match IDs before joining for good practice    
    metrics_v_obj<-calc_val_metrics_rast(data_v,y_ref,names_mod)
    metrics_v_df<-cbind(metrics_v_obj$metrics,run_info)
    metrics_v_df["var_interp"]<-rep(y_ref,times=nrow(metrics_v_df)) 
    #Name of the variable interpolated, useful for cross-comparison between methods at later stages
    data_v<-spCbind(data_v,metrics_v_obj$residuals)
  }
  
  extract_data_s<-extract(rast_day_mod,data_s,df=TRUE)  
  
  data_s <-spCbind(data_s,extract_data_s)

  metrics_s_obj <- calc_val_metrics_rast(data_s,y_ref,names_mod)  
  
  run_info <- cbind(sampling_dat_day,n=ns)
  run_info[rep(seq_len(nrow(run_info)), each=N),]
  metrics_s_df <- cbind(metrics_s_obj$metrics,run_info)
  metrics_s_df["var_interp"] <- rep(y_ref,times=nrow(metrics_s_df)) 
  #Name of the variable interpolated, useful for cross-comparison between methods at later stages
  
  data_s <- spCbind(data_s,metrics_s_obj$residuals)
  
  #prepare output object
  
  if (nv > 0){
    validation_obj<-list(metrics_s_df,metrics_v_df,data_s,data_v)
    names(validation_obj)<-c("metrics_s","metrics_v","data_s","data_v")
  }else{
    validation_obj<-list(metrics_s_df,data_s)
    names(validation_obj)<-c("metrics_s","data_s")
  }
  
  return(validation_obj)

}

#### Function to create a data.frame from validation obj
extract_from_list_obj<-function(obj_list,list_name){
  list_tmp<-vector("list",length(obj_list))
  for (i in 1:length(obj_list)){
    tmp<-obj_list[[i]][[list_name]] #double bracket to return data.frame
    list_tmp[[i]]<-tmp
  }
  tb_list_tmp<-do.call(rbind,list_tmp) #long rownames
  return(tb_list_tmp) #this is  a data.frame
}

#### Function to create a list from a object made up of a list with names e.g. method_mod_obj or clim_method_mod_obj
extract_list_from_list_obj<-function(obj_list,list_name){
  #Create a list of an object from a given list of object using a name prodived as input
  
  list_tmp<-vector("list",length(obj_list))
  for (i in 1:length(obj_list)){
    tmp<-obj_list[[i]][[list_name]] #double bracket to return data.frame
    list_tmp[[i]]<-tmp
  }
  return(list_tmp) #this is  a data.frame
}

#### Function to plot boxplot from data.frame table of accuracy metrics

boxplot_from_tb <-function(tb_diagnostic,metric_names,out_prefix,out_path){
  #now boxplots and mean per models
  library(gdata) #Nesssary to use cbindX
  
  ### Start script
  y_var_name<-unique(tb_diagnostic$var_interp) #extract the name of interpolated variable: dailyTmax, dailyTmin
  
  mod_names<-sort(unique(tb_diagnostic$pred_mod)) #models that have accuracy metrics
  t<-melt(tb_diagnostic,
          #measure=mod_var, 
          id=c("date","pred_mod","prop"),
          na.rm=F)
  t$value<-as.numeric(t$value) #problem with char!!!
  avg_tb<-cast(t,pred_mod~variable,mean)
  avg_tb$var_interp<-rep(y_var_name,times=nrow(avg_tb))
  median_tb<-cast(t,pred_mod~variable,median)
  
  #avg_tb<-cast(t,pred_mod~variable,mean)
  tb<-tb_diagnostic
 
  #mod_names<-sort(unique(tb$pred_mod)) #kept for clarity
  tb_mod_list<-lapply(mod_names, function(k) subset(tb, pred_mod==k)) #this creates a list of 5 based on models names
  names(tb_mod_list)<-mod_names
  #mod_metrics<-do.call(cbind,tb_mod_list)
  #debug here
  if(length(tb_mod_list)>1){
    mod_metrics<-do.call(cbindX,tb_mod_list) #column bind the list??
  }else{
    mod_metrics<-tb_mod_list[[1]]
  }
  
  test_names<-lapply(1:length(mod_names),function(k) paste(names(tb_mod_list[[1]]),mod_names[k],sep="_"))
  #test names are used when plotting the boxplot for the different models
  names(mod_metrics)<-unlist(test_names)
  rows_total<-lapply(tb_mod_list,nrow)
  for (j in 1:length(metric_names)){
    metric_ac<-metric_names[j]
    mod_pat<-glob2rx(paste(metric_ac,"_*",sep=""))   
    mod_var<-grep(mod_pat,names(mod_metrics),value=TRUE) # using grep with "value" extracts the matching names     
    #browser()
    test<-mod_metrics[mod_var]
    png(file.path(out_path,paste("boxplot_metric_",metric_ac, out_prefix,".png", sep="")))
    #boxplot(test,outline=FALSE,horizontal=FALSE,cex=0.5,
    #        ylab=paste(metric_ac,"in degree C",sep=" "))
    
    boxplot(test,outline=FALSE,horizontal=FALSE,cex=0.5,
              ylab=paste(metric_ac,"in degree C",sep=" "),axisnames=FALSE,axes=FALSE)
    axis(1, labels = FALSE)
    ## Create some text labels
    labels <- labels<- names(test)
    ## Plot x axis labels at default tick marks
    text(1:ncol(test), par("usr")[3] - 0.25, srt = 45, adj = 1,
         labels = labels, xpd = TRUE)
    axis(2)
    box()
    #legend("bottomleft",legend=paste(names(rows_total),":",rows_total,sep=""),cex=0.7,bty="n")
    #title(as.character(t(paste(t(names(rows_total)),":",rows_total,sep=""))),cex=0.8)
    title(paste(metric_ac,"for",y_var_name,sep=" "),cex=0.8)
    dev.off()
  }
  
  avg_tb$n<-rows_total #total number of predictions on which the mean is based
  median_tb$n<-rows_total
  summary_obj<-list(avg_tb,median_tb)
  names(summary_obj)<-c("avg","median")
  return(summary_obj)  
}
#boxplot_month_from_tb(tb_diagnostic,metric_names,out_prefix,out_path)
## Function to display metrics by months/seasons
boxplot_month_from_tb <-function(tb_diagnostic,metric_names,out_prefix,out_path){
  
  #Generate boxplot per month for models and accuracy metrics
  #Input parameters:
  #1) df: data frame containing accurayc metrics (RMSE etc.) per day)
  #2) metric_names: metrics used for validation
  #3) out_prefix
  #
  
  #################
  ## BEGIN
  y_var_name<-unique(tb_diagnostic$var_interp) #extract the name of interpolated variable: dailyTmax, dailyTmin  
  date_f<-strptime(tb_diagnostic$date, "%Y%m%d")   # interpolation date being processed
  tb_diagnostic$month<-strftime(date_f, "%m")          # current month of the date being processed
  mod_names<-sort(unique(tb_diagnostic$pred_mod)) #models that have accuracy metrics
  tb_mod_list<-lapply(mod_names, function(k) subset(tb_diagnostic, pred_mod==k)) #this creates a list of 5 based on models names
  names(tb_mod_list)<-mod_names
  t<-melt(tb_diagnostic,
          #measure=mod_var, 
          id=c("date","pred_mod","prop","month"),
          na.rm=F)
  t$value<-as.numeric(t$value) #problem with char!!!
  tb_mod_m_avg <-cast(t,pred_mod+month~variable,mean) #monthly mean for every model
  tb_mod_m_avg$var_interp<-rep(y_var_name,times=nrow(tb_mod_m_avg))
  
  tb_mod_m_sd <-cast(t,pred_mod+month~variable,sd)   #monthly sd for every model
  
  tb_mod_m_list <-lapply(mod_names, function(k) subset(tb_mod_m_avg, pred_mod==k)) #this creates a list of 5 based on models names
  
  for (k in 1:length(mod_names)){
    mod_metrics <-tb_mod_list[[k]]
    current_mod_name<- mod_names[k]
    for (j in 1:length(metric_names)){    
      metric_ac<-metric_names[j]
      col_selected<-c(metric_ac,"month")
      test<-mod_metrics[col_selected]
      png(file.path(out_path,paste("boxplot_metric_",metric_ac,"_",current_mod_name,"_by_month_",out_prefix,".png", sep="")))
      boxplot(test[[metric_ac]]~test[[c("month")]],outline=FALSE,horizontal=FALSE,cex=0.5,
              ylab=paste(metric_ac,"in degree C",sep=" "),,axisnames=FALSE,axes=FALSE)
      #boxplot(test[[metric_ac]]~test[[c("month")]],outline=FALSE,horizontal=FALSE,cex=0.5,
      #        ylab=paste(metric_ac,"in degree C",sep=" "))
      axis(1, labels = FALSE)
      ## Create some text labels
      labels <- month.abb # abbreviated names for each month
      ## Plot x axis labels at default tick marks
      text(1:length(labels), par("usr")[3] - 0.25, srt = 45, adj = 1,
           labels = labels, xpd = TRUE)
      axis(2)
      box()
      #legend("bottomleft",legend=paste(names(rows_total),":",rows_total,sep=""),cex=0.7,bty="n")
      title(paste(metric_ac,"for",current_mod_name,"by month",sep=" "))
      dev.off()
    }  
    
  }
  summary_month_obj <-c(tb_mod_m_list,tb_mod_m_avg,tb_mod_m_sd)
  names(summary_month_obj)<-c("tb_list","metric_month_avg","metric_month_sd")
  return(summary_month_obj)  
}


####################################
############ END OF SCRIPT #########
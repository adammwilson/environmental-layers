##VALIDATION-ACCURACY ASSESSMENT

#The interpolation is done first at the monthly add delta.
#AUTHOR: Benoit Parmentier                                                                        
#DATE: 03/12/2013                                                                                 

#Change this to allow explicitly arguments...
#Arguments: 
#1)list of climatology files for all models...(365*nb of models)
#2)data_s:training
#3)data_v:testing
#4)list of dates??: index
#5)stack of covariates: not needed at this this stage
#6)dst: data at the monthly time scale

#Function used in the script

calculate_accuracy_metrics<-function(i,list_param){
  
  ### Caculate accuracy metrics
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
    #
    
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
  
  ## BEGIN ##
  
  #PARSING INPUT PARAMETERS
  day_list<- list_param$rast_day_year_list[[i]]
  #day_list <-rast_day_yearlist[[i]] #list of prediction for the current date...
  names_mod<-names(day_list)
  method_mod_obj<-list_param$method_mod_obj
  #Change to results_mod_obj[[i]]$data_s to make it less specific
  data_v <- method_mod_obj[[i]]$data_v
  data_s <- method_mod_obj[[i]]$data_s
  
  ## Now create the stack
  
  rast_day_mod <- stack(day_list)
  names(rast_day_mod) <- names(day_list)
  extract_data_v<-extract(rast_day_mod,data_v,df=TRUE)
  extract_data_s<-extract(rast_day_mod,data_s,df=TRUE)
  
  data_v <-spCbind(data_v,extract_data_v) #should match IDs before joining for good practice
  data_s <-spCbind(data_s,extract_data_s)
  
  ns<-nrow(data_s) # some loss of data might have happened because of the averaging...
  nv<-nrow(data_v)
  
  sampling_dat_day<-(method_mod_obj[[i]])$sampling_dat
   
  metrics_v_obj<-calc_val_metrics_rast(data_v,y_var_name,names_mod)
  metrics_s_obj<-calc_val_metrics_rast(data_s,y_var_name,names_mod)
  
  #add sampling dat info...
  N=length(names_mod)
  run_info<-cbind(sampling_dat_day,n=nv)
  run_info[rep(seq_len(nrow(run_info)), each=N),] #repeating same row n times
  metrics_v_df<-cbind(metrics_v_obj$metrics,run_info)
  
  run_info<-cbind(sampling_dat_day,n=ns)
  run_info[rep(seq_len(nrow(run_info)), each=N),]
  metrics_s_df<-cbind(metrics_s_obj$metrics,run_info)
  
  data_v<-spCbind(data_v,metrics_v_obj$residuals)
  data_s<-spCbind(data_s,metrics_s_obj$residuals)
    
  validation_obj<-list(metrics_s_df,metrics_v_df,data_s,data_v)
  names(validation_obj)<-c("metrics_s","metrics_v","data_s","data_v")
  
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

#### Function to plot boxplot from data.frame table of accuracy metrics

boxplot_from_tb <-function(tb_diagnostic,metric_names,out_prefix){
  #now boxplots and mean per models
  mod_names<-unique(tb_diagnostic$pred_mod) #models that have accuracy metrics
  t<-melt(tb_diagnostic,
          #measure=mod_var, 
          id=c("date","pred_mod","prop"),
          na.rm=F)
  avg_tb<-cast(t,pred_mod~variable,mean)
  
  median_tb<-cast(t,pred_mod~variable,median)
  tb<-tb_diagnostic
  tb_mod_list<-vector("list",length(mod_names))
  for(i in 1:length(mod_names)){            # Reorganizing information in terms of metrics 
    mod_name_tb<-paste("tb_",mod_names[i],sep="")
    tb_mod<-subset(tb, pred_mod==mod_names[i])
    assign(mod_name_tb,tb_mod)
    tb_mod_list[[i]]<-tb_mod
  }
  names(tb_mod_list)<-mod_names
  mod_metrics<-do.call(cbind,tb_mod_list)
  for (j in 1:length(metric_names)){
    metric_ac<-metric_names[j]
    mod_pat<-glob2rx(paste("*.",metric_ac,sep=""))   
    mod_var<-grep(mod_pat,names(mod_metrics),value=TRUE) # using grep with "value" extracts the matching names     
    #browser()
    test<-mod_metrics[mod_var]
    png(paste("boxplot_metric_",metric_ac, out_prefix,".png", sep=""))
    boxplot(test,outline=FALSE,horizontal=FALSE,cex=0.5,
            ylab=paste(metric_ac,"in degree C",sep=" "))
    dev.off()
  }
  summary_obj<-list(avg_tb,median_tb)
  return(summary_obj)  
}

## Function to display metrics by months/seasons
boxplot_month_from_tb <-function(tb_diagnostic,metric_names,out_prefix){
  #Add code here...
}

####################################
############ END OF SCRIPT #########
####################################  INTERPOLATION OF TEMPERATURES  #######################################
############################  Script for manuscript analyses,tables and figures #######################################
#This script reads information concerning the Oregon case study to adapt data for the revised 
# interpolation code.
#Functions used in the production of figures and data for the multi timescale paper are recorded.
#AUTHOR: Benoit Parmentier                                                                      #
#DATE CREATED: 11/25/2013            
#DATE MODIFIED: 12/16/2013            
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

function_analyses_paper <-"multi_timescales_paper_interpolation_functions_12092013.R"

plot_transect_m2<-function(list_trans,r_stack,title_plot,disp=FALSE,m_layers){
  #This function creates plot of transects for stack of raster images.
  #Arguments:
  #list_trans: list of files containing the transects lines in shapefile format
  #r_stack: raster stack containing the information to extect
  #title_plot: plot title
  #disp: display and save from X11 if TRUE or plot to png file if FALSE
  #m_layers: index for layerers containing alternate units to be drawned on a differnt scale
  #RETURN:
  #list containing transect information
  
  nb<-length(list_trans) #number of transects
  t_col<-rainbow(nb)
  t_col<-c("red","green","darkblue","black")
  lty_list<-c("dashed","solid","dotted","twodash")
  list_trans_data<-vector("list",nb)
  
  #For scale 1
  for (i in 1:nb){ #nb is the number of transects
    trans_file<-list_trans[[i]][1]
    filename<-sub(".shp","",trans_file)             #Removing the extension from file.
    transect<-readOGR(dirname(filename), basename(filename))                 #reading shapefile 
    trans_data<-extract(r_stack, transect)
    if (disp==FALSE){
      png(file=paste(list_trans[[i]][2],".png",sep=""),
          height=480*1,width=480*2)
    }
    
    y_comb <- trans_data[[1]][,-m_layers]
    y_comb <- as.vector(y_comb)
    y_range <- range(y_comb,na.rm=T)
    
    #Plot layer values for specific transect
    for (k in 1:ncol(trans_data[[1]])){
      y<-trans_data[[1]][,k]
      x<-1:length(y)
      m<-match(k,m_layers)
      
      if (k==1 & is.na(m)){
        plot(x,y,type="l",xlab="transect distance from coastal origin (km)", ylab=" maximum temperature (degree C)",
             ylim=y_range,cex=1.2,col=t_col[k])
        #axis(2)
      }
      if (k==1 & !is.na(m)){ #if layer sc (i.e. ellevation then plot on another scale)
        plot(x,y,type="l",col=t_col[k],lty="dotted",axes=F) #plotting fusion profile
        #axis(4,xlab="",ylab="elevation(m)")  
        axis(4,cex=1.2)
      }
      if (k!=1 & is.na(m)){
        #par(new=TRUE)              # new plot without erasing old
        lines(x,y,type="l",xlab="",ylab="",col=t_col[k],axes=F) #plotting fusion profile
        #axis(2,xlab="",ylab="tmax (in degree C)")
      }
      if (k!=1 & !is.na(m)){
        par(new=TRUE)              # key: ask for new plot without erasing old
        plot(x,y,type="l",col=t_col[k],xlab="",ylab="",lty="dotted",axes=F) #plotting fusion profile
        #axis(4,xlab="",ylab="elevation(m)")  
        axis(4,cex=1.2)
      } 
    }
    title(title_plot[i])
    legend("topleft",legend=names(r_stack)[1:(nlayers(r_stack)-1)], 
           cex=1.2, col=t_col,lty=1,bty="n")
    legend("topright",legend=names(r_stack)[m_layers], 
           cex=1.2, col=t_col[3],lty="dotted",bty="n")
    if (disp==TRUE){
      savePlot(file=paste(list_trans[[i]][2],".png",sep=""),type="png")
    }
    if (disp==FALSE){
      dev.off()
    }
    list_trans_data[[i]]<-trans_data
  }
  names(list_trans_data)<-names(list_trans)
  return(list_trans_data)
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

list_diff_df_fun <- function(i,list_tb_s,list_tb_v,list_of_list_metric_names){
  list_metric_names <- list_of_list_metric_names[[i]]
  tb_s <- list_tb_s[[i]]
  tb_v <- list_tb_v[[i]]
  tb_diff<-vector("list", length(list_metric_names))
  for (i in 1:length(list_metric_names)){
    metric_name<-list_metric_names[i]
    tb_diff[[i]] <-tb_s[,c(metric_name)] - tb_v[,c(metric_name)]
  }
  names(tb_diff)<-list_metric_names
  tb_diff<-as.data.frame(do.call(cbind,tb_diff))
  return(tb_diff)
}

### generate filter for Moran's I function in raster package
autocor_filter_fun <-function(no_lag=1,f_type="queen"){
  if(f_type=="queen"){
    no_rows <- 2*no_lag +1
    border_row <-rep(1,no_rows)
    other_row <- c(1,rep(0,no_rows-2),1)
    other_rows <- rep(other_row,no_rows-2)
    mat_data<- c(border_row,other_rows,border_row)
    autocor_filter<-matrix(mat_data,nrow=no_rows)
  }
  #if(f_type=="rook){} #add later
  return(autocor_filter)
}
#MODIFY: calculate for multiple dates and create averages...
#Now run Moran's I for raster image given a list of  filters for different lags and raster stack
moran_multiple_fun<-function(i,list_param){
  #Parameters:
  #list_filters: list of filters with different lags in the image
  #r_stack: stack of raster image, only the selected layer is used...
  list_filters <-list_param$list_filters
  r <- subset(list_param$r_stack,i)
  moran_list <- lapply(list_filters,FUN=Moran,x=r)
  moran_v <-as.data.frame(unlist(moran_list))
  names(moran_v)<-names(r)
  return(moran_v)
}

#Modfiy...to allow any stat: min, max, mean,sd etc.
stat_moran_std_raster_fun<-function(i,list_param){
  f <-list_param$filter
  lf_list <- list_param$lf_list[[i]]
  list_var_stat<-vector("list",length(lf_list))
  for (k in 1:length(lf_list)){
    
    raster_pred<-raster(unlist(lf_list[k])) 
    #tmp_rast<-mask(raster_pred,mask_rast)
    #tmp_rast<-raster_pred
    #raster_pred2<-tmp_rast
    
    t1<-cellStats(raster_pred,na.rm=TRUE,stat=sd)    #Calculating the standard deviation for the     
    m1 <- Moran(x=raster_pred,w=f) #Calculating Moran's I with window of 3 an default Queen's case
    #m1 <- Moran(raster_pred,w=3) #Calculating Moran's I with window of 3 an default Queen's case
    
    stat<-as.data.frame(t(c(m1,t1)))
    names(stat)<-c("moranI","std")
    list_var_stat[[k]]<-stat
  }
  dat_var_stat<-do.call(rbind,list_var_stat)
  dat_var_stat$lf_names<-names(lf_list)
  dat_var_stat$index<- i
  return(dat_var_stat)
}

plot_accuracy_by_holdout_fun <-function(list_tb,ac_metric){
  #
  list_plots <- vector("list",length=length(list_tb))
  for (i in 1:length(list_tb)){
    #i <- i+1
    tb <-list_tb[[i]]
    plot_name <- names(list_tb)[i]
    pat_str <- "tb_m"
    if(substr(plot_name,start=1,stop=4)== pat_str){
      names_id <- c("pred_mod","prop")
      plot_formula <- paste(ac_metric,"~prop",sep="",collapse="") 
    }else{
      names_id <- c("pred_mod","prop_month")
      plot_formula <- paste(ac_metric,"~prop_month",collapse="")
    }
    names_mod <-unique(tb$pred_mod)
    prop_obj <- calc_stat_prop_tb_diagnostic(names_mod,names_id,tb)
    avg_tb <- prop_obj$avg_tb
  
    layout_m<-c(1,1) #one row two columns
    par(mfrow=layout_m)
    
    #add option for plot title?
    png(paste("Figure__accuracy_",ac_metric,"_prop_month_",plot_name,"_",out_prefix,".png", sep=""),
      height=480*layout_m[1],width=480*layout_m[2])
    
    p <- xyplot(as.formula(plot_formula),group=pred_mod,type="b",
          data=avg_tb,
          main=paste(ac_metric,plot_name,sep=" "),
          pch=1:length(avg_tb$pred_mod),
          par.settings=list(superpose.symbol = list(
          pch=1:length(avg_tb$pred_mod))),
          auto.key=list(columns=5))
    print(p)
  
    dev.off()
    list_plots[[i]] <- p
  }
  names(list_plots) <- names(list_tb)
  return(list_plots)
  #end of function
}

diff_date_rast_pred_fun <- function(i,list_param){
  
  index <- i #index date
  list_raster_obj_files <- list_param$list_raster_obj_files
  methods_name <- list_param$methods_name
  y_var_name <- list_param$y_var_name
  ref_mod <- list_param$ref_mod
  alt_mod <- list_param$alt_mod
  NA_flag_val <- list_param$NA_flag_val
  file_format <- list_param$file_format
  out_dir <- list_param$out_dir
  out_prefix <- list_param$out_prefix
  
  #index<-244 #index corresponding to Sept 1

  #lf_list<-lapply(list_raster_obj_files[c("gam_daily","gam_CAI","gam_fss")],
  #                             FUN=function(x){x<-load_obj(x);x$method_mod_obj[[index]][[y_var_name]]})  
  lf_list<-lapply(list_raster_obj_files[methods_name],
                               FUN=function(x){x<-load_obj(x);x$method_mod_obj[[index]][[y_var_name]]})  
  diff_pred_list <- vector("list",length=length(lf_list))
  for (i in 1:length(lf_list)){
    interpolation_method <- methods_name[i]
    r_diff <- raster(lf_list[[i]][[ref_mod]]) - raster(lf_list[[i]][[alt_mod]])
    data_name <-paste(index,"_",y_var_name,"_diff_",ref_mod,"_",alt_mod,sep="")
    raster_name <-file.path(out_dir,paste(interpolation_method,"_",data_name,out_prefix,file_format, sep=""))
    diff_pred_list[[i]] <-raster_name 
    writeRaster(r_diff, filename=raster_name,NAflag=NA_flag_val,bylayer=FALSE,bandorder="BSQ",overwrite=TRUE)  #Writing the data in a raster file format...  

  }
  return(unlist(diff_pred_list))
}

#Extract statistic by zones...make this general by changing names?
extract_diff_by_landcover <- function(r_stack_diff,s_raster,LC_subset,LC_names,avl){
  
  rclmat<-matrix(avl,ncol=3,byrow=TRUE)
  
  LC_s <- subset(s_raster,LC_subset)
  LC_s_rec <-reclassify(LC_s,rclmat)
  names(LC_s_rec)<- LC_names
  #plot(LC_s)  
  #plot average difference per class of forest and LC2
  list_avg_diff <- vector("list",length=nlayers(r_stack_diff))
  list_sd_diff <- vector("list",length=nlayers(r_stack_diff))

  for (i in 1:nlayers(r_stack_diff)){
    rast_diff  <- subset(r_stack_diff,i)
    list_avg <- vector("list",length=nlayers(LC_s_rec))
    list_sd <- vector("list",length=nlayers(LC_s_rec))
    for(k in 1:nlayers(LC_s_rec)){
      LC_rec <- subset(LC_s_rec,k)
      list_avg[[k]] <- zonal(rast_diff,z=LC_rec,stat="mean",na.rm=TRUE)[,2]
      list_sd[[k]]  <- zonal(rast_diff,z=LC_rec,stat="sd",na.rm=TRUE)[,2]
    }
    zones_avg <- do.call(cbind,list_avg) 
    zones_sd <- do.call(cbind,list_sd) 
    colnames(zones_avg)<- LC_names
    colnames(zones_sd)<-LC_names
    list_avg_diff[[i]] <- zones_avg
    list_sd_diff[[i]] <- zones_sd    
  }
  
  
  list_zones <- list(avg=list_avg_diff,sd=list_sd_diff)
  return(list_zones)
}
   
## Utility function to quickly write out a stack or brick of rasterlayer to disk using names of layers
# Note that each layers are written individually, default NA value and format is provided
write_out_raster_fun <-function(r_stack,out_suffix,out_dir,NA_flag_val=-9999,file_format=".rst"){
  for(i in 1:nlayers(r_stack)){
    list_raster_name <- vector("list",length=nlayers(r_stack))
    r<-subset(r_stack,i)
    raster_name <- paste(names(r_stack)[i],"_",out_suffix,file_format,sep="")
    writeRaster(r, NAflag=NA_flag_val,filename=file.path(out_dir,raster_name)
                   ,bylayer=FALSE,bandorder="BSQ",overwrite=TRUE)
    list_raster_name[[i]] <- file.path(out_dir,raster_name) 
  }
  return(unlist(list_raster_name))
}

################### END OF SCRIPT ###################



####################################  INTERPOLATION OF TEMPERATURES  #######################################
############################  Script for manuscript analyses,tables and figures #######################################
#This script reads information concerning the Oregon case study to adapt data for the revised 
# interpolation code.
#Functions used in the production of figures and data for the multi timescale paper are recorded.
#AUTHOR: Benoit Parmentier                                                                      #
#DATE CREATED: 11/25/2013            
#DATE MODIFIED: 12/09/2013            
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
  t_col<-c("red","green","black")
  lty_list<-c("dashed","solid","dotted")
  list_trans_data<-vector("list",nb)
  
  #For scale 1
  for (i in 1:nb){
    trans_file<-list_trans[[i]][1]
    filename<-sub(".shp","",trans_file)             #Removing the extension from file.
    transect<-readOGR(dirname(filename), basename(filename))                 #reading shapefile 
    trans_data<-extract(r_stack, transect)
    if (disp==FALSE){
      png(file=paste(list_trans[[i]][2],".png",sep=""),
          height=480*1,width=480*2)
    }
    #Plot layer values for specific transect
    for (k in 1:ncol(trans_data[[1]])){
      y<-trans_data[[1]][,k]
      x<-1:length(y)
      m<-match(k,m_layers)
      
      if (k==1 & is.na(m)){
        plot(x,y,type="l",xlab="transect distance from coastal origin (km)", ylab=" maximum temperature (degree C)",
             ,cex=1.2,col=t_col[k])
        #axis(2)
      }
      if (k==1 & !is.na(m)){
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
    legend("topleft",legend=names(r_stack)[1:2], 
           cex=1.2, col=t_col,lty=1,bty="n")
    legend("topright",legend=names(r_stack)[3], 
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

#Modfiy...temporal plot for 1,10,20
stat_moran_std_raster_fun<-function(i){
  list_var_stat<-vector("list",ncol(lf_list))
  for (k in 1:length(lf_list)){
    
    raster_pred<-raster(lf_list[i,k]) 
    tmp_rast<-mask(raster_pred,mask_rast)
    #tmp_rast<-raster_pred
    raster_pred2<-tmp_rast
    
    t1<-cellStats(raster_pred,na.rm=TRUE,stat=sd)    #Calculating the standard deviation for the 
    m1<-Moran(raster_pred,w=3) #Calculating Moran's I with window of 3 an default Queen's case
    stat<-as.data.frame(t(c(m1,t1)))
    names(stat)<-c("moranI","std")
    list_var_stat[[k]]<-stat
  }
  dat_var_stat<-do.call(rbind,list_var_stat)
  dat_var_stat$lf_names<-names(lf_list)
  dat_var_stat$dates<-dates[i]
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
  names(list_plots) <- names
  return(list_plots)
  #end of function
}

################### END OF SCRIPT ###################



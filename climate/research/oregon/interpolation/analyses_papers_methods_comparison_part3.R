######################################## Paper Methods_comparison #######################################
############################ Scripts for figures and analyses for the the IBS poster #####################################
#This script performs analyses and create figures for the FSS paper.
#It uses inputs from interpolation objects created at earlier stages...     
#Note that this is exploratory code i.e. not part of the worklfow.
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 08/02/2013                                                                                #
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

extract_list_from_list_obj<-function(obj_list,list_name){
  #Create a list of an object from a given list of object using a name prodived as input
  
  list_tmp<-vector("list",length(obj_list))
  for (i in 1:length(obj_list)){
    tmp<-obj_list[[i]][[list_name]] #double bracket to return data.frame
    list_tmp[[i]]<-tmp
  }
  return(list_tmp) #this is  a data.frame
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

################## PARAMETERS ##########

#in_dir<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_GAM_fus_all_lst_05312013"
#in_dir<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb3_07092013/"
in_dir<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb4_07152013/"
in_dir2<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_mult_lst_comb3_07232013"

out_dir<-""
setwd(in_dir)

out_prefix<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_GAM_fus_all_lst_05312013"
method_interpolation <- "gam_daily"
covar_obj_file <- "covar_obj__365d_gam_day_lst_comb3_07092013.RData"

raster_obj_file <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_lst_comb3_07092013.RData"
raster_obj_file2 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_mults15_lst_comb3_07232013.RData"

raster_prediction_obj <-load_obj(raster_obj_file)
names(raster_prediction_obj) #list of two objects

raster_prediction_obj$summary_metrics_v

#################################################################################
############ ANALYSES 1: ACCURACY IN TERMS OF DISTANCE TO CLOSEST STATIONS... #######

names(raster_prediction_obj$validation_mod_obj[[1]])

#First extract list of training and testing spdf from interpolation object

list_data_s <- extract_list_from_list_obj(raster_prediction_obj$validation_mod_obj,"data_s")
list_data_v <- extract_list_from_list_obj(raster_prediction_obj$validation_mod_obj,"data_v")

#Assign model's names
names_mod <- paste("res_mod",1:9,sep="")
i<-1
names_mod<-c("res_mod1","res_mod2","res_mod3","res_mod4","res_mod5","res_mod6","res_mod7","res_mod8","res_mod9")

## Generate data frame with residuals in term of distance to closest stations

names_var<-c(names_mod,"dates")
list_param_dst<-list(i,list_data_s,list_data_v,names_mod)
names(list_param_dst) <- c("index","data_s","data_v","names_var")

#call function over 365 dates
list_dstspat_er <-lapply(1:length(list_data_v),FUN=calc_dist_ref_data_point,list_param=list_param_dst)
#now assemble in one data.frame
dstspat_dat<-do.call(rbind.fill,list_dstspat_er)

###

# Plot results: accuracy in term of distance to closest fitting stations...

plot(dstspat_dat$dst,abs(dstspat_dat$res_mod1))
limit_val<-seq(0,150, by=10)

limit_val<-seq(5,155, by=10)

dstspat_dat$dst_cat1 <- cut(dstspat_dat$dst,breaks=limit_val)
mae_fun<-function(x){mean(abs(x))}
sd_abs_fun<-function(x){sd(abs(x))}
t<-melt(dstspat_dat,
        measure=c("res_mod1","res_mod2","res_mod3","res_mod4","res_mod5","res_mod6","res_mod8"), 
        id=c("dst_cat1"),
        na.rm=T)
mae_tb<-cast(t,dst_cat1~variable,mae_fun)
sd_abs_tb<-cast(t,dst_cat1~variable,sd_abs_fun)

avg_tb<-cast(t,dst_cat1~variable,mean)
sd_tb<-cast(t,dst_cat1~variable,sd)
n_tb<-cast(t,dst_cat1~variable,length)
n_NA<-cast(t,dst_cat1~variable,is.na)

mod_name<-"mod1"
mod_name<-"mod4"
xlab_text<-"distance to fitting station"

n <- unlist(n_tb[1:12,c(mod_name)])
y <- unlist(mae_tb[1:12,c(mod_name)])

x<- 1:length(y)
y_sd <- unlist(sd_abs_tb[1:12,c(mod_name)])

ciw <-y_sd
#ciw2   <- qt(0.975, n) * y_sd2 / sqrt(n)

plotCI(y=y, x=x, uiw=ciw, col="red", main=paste(" MAE for ",mod_name,sep=""), barcol="blue", lwd=1,
       ylab="RMSE (C)", xlab=xlab_text)

ciw   <- qt(0.975, n) * y_sd / sqrt(n)

plotCI(y=y, x=x, uiw=ciw, col="red", main=paste(" CI MAE for for ",mod_name,sep=""), barcol="blue", lwd=1,
       ylab="RMSE (C)", xlab=xlab_text)

names_var<-intersect(names_var,names(dstspat_dat)) #there may be missing columns
range_y<-range(as.vector(dstspat_dat[,names_var]),na.rm=T)
dst_dat<- vector("list",length(names_var))

#####################################################
### PART II MULTISAMPLING COMPARISON ###############

#Use run of 7 hold out proportions, 10 to 70% with 10 random samples and 12 dates...
#Use gam_day method
#Use comb3 i.e. using baseline s(lat,lon)+s(elev)

#read in relevant data:
raster_prediction_obj <-load_obj(file.path(in_dir2,raster_obj_file2))
tb <-raster_prediction_obj$tb_diagnostic_v  #contains the accuracy metrics for each run...

names_mod<-c("mod1","mod2","mod3","mod4","mod5","mod6","mod7","mod8","mod9")

t<-melt(tb,
        measure=c("mae","rmse","r","m50"), 
        id=c("pred_mod","prop"),
        na.rm=T)
tbp<- subset(tb,prop!=70) #remove 70% hold out because it is only predicted for mod1 (baseline)

tp<-melt(tbp,
        measure=c("mae","rmse","r","m50"), 
        id=c("pred_mod","prop"),
        na.rm=T)

avg_tp<-cast(tp,pred_mod~variable,mean)

avg_tb<-cast(t,pred_mod+prop~variable,mean)
sd_tb<-cast(t,pred_mod+prop~variable,sd)

n_tb<-cast(t,pred_mod+prop~variable,length)

xyplot(avg_tb$rmse~avg_tb$prop,type="b",group=pred_mod,
       data=avg_tb,
       pch=1:length(avg_tb$pred_mod),
       par.settings=list(superpose.symbol = list(
         pch=1:length(avg_tb$pred_mod))),
       auto.key=list(columns=5))

mod_name<-"mod1"
mod_name<-"mod4"
xlab_text<-"proportion of hold out"

n <- unlist(subset(n_tb,pred_mod==mod_name,select=c(rmse)))
y <- unlist(subset(avg_tb,pred_mod==mod_name,select=c(rmse)))

x<- 1:length(y)
y_sd <- unlist(subset(sd_tb,pred_mod==mod_name,select=c(rmse)))

ciw <-y_sd
#ciw2   <- qt(0.975, n) * y_sd2 / sqrt(n)

plotCI(y=y, x=x, uiw=ciw, col="red", main=paste(" Mean and Std_dev RMSE for ",mod_name,sep=""), barcol="blue", lwd=1,
       ylab="RMSE (C)", xlab=xlab_text)

ciw   <- qt(0.975, n) * y_sd / sqrt(n)

plotCI(y=y, x=x, uiw=ciw, col="red", main=paste(" Mean and CI RMSE for ",mod_name,sep=""), barcol="blue", lwd=1,
       ylab="RMSE (C)", xlab=xlab_text)

n=150
ciw   <- qt(0.975, n) * y_sd / sqrt(n)
ciw2   <- qt(0.975, n) * y_sd2 / sqrt(n)

#Comparison of MAE for different proportions for FUSION and CAI using CI
X11()
plotCI(y=y, x=x, uiw=ciw, col="red", main=" FUS: RMSE proportion of validation hold out", barcol="blue", lwd=1,
       ylab="RMSE (C)", xlab="Proportions of validation hold out (in %)")
lines(x,y,col="red")
legend("bottomright",legend=c("fus"), cex=1.2, col=c("red"),
       lty=1, title="RMSE")
savePlot(paste("Comparison_multisampling_fus_RMSE_CI",out_prefix,".png", sep=""), type="png")

plotCI(y=y2, x=x2, uiw=ciw2, col="black", main=" CAI: RMSE proportion of validation hold out", barcol="blue", lwd=1,
       ylab="RMSE (C)", xlab="Proportions of validation hold out (in %)")
lines(x2,y2,col="grey")
legend("bottomright",legend=c("CAI"), cex=1.2, col=c("grey"),
       lty=1, title="RMSE")
savePlot(paste("Comparison_multisampling_CAI_RMSE_CI",out_prefix,".png", sep=""), type="png")
dev.off()

#Comparison of MAE for different proportions for FUSION and CAI
X11()
plot(x,y,col="red",type="b", ylab="RMSE (C)", xlab="Proportions of validation hold out (in %)")
lines(x2,y2,col="grey")
points(x2,y2,col="grey")
title("MAE in terms of proportions and random sampling")
legend("bottomright",legend=c("fus","CAI"), cex=1.2, col=c("red","grey"),
       lty=1, title="RMSE")
savePlot(paste("Comparison_multisampling_fus_CAI_RMSE_averages",out_prefix,".png", sep=""), type="png")
dev.off()

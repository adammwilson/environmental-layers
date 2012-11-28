#####################################  METHODS COMPARISON part 7 ##########################################
#################################### Spatial Analysis: validation CAI-fusion  ############################################
#This script utilizes the R ojbects created during the interpolation phase.                       #
#We use the SNOTEL dataset and the 
#This scripts focuses on a detailed studay of differences in the predictions of CAI_kr and FUsion_Kr                              #
#AUTHOR: Benoit Parmentier                                                                        #
#DATE: 12/03/2012                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#491 --                                  #
###################################################################################################

###Loading R library and packages                                                      
library(gtools)                                        # loading some useful tools such as mixedsort
library(mgcv)                                           # GAM package by Wood 2006 (version 2012)
library(sp)                                             # Spatial pacakge with class definition by Bivand et al. 2008
library(spdep)                                          # Spatial package with methods and spatial stat. by Bivand et al. 2012
library(rgdal)                                          # GDAL wrapper for R, spatial utilities (Keitt et al. 2012)
library(gstat)                                          # Kriging and co-kriging by Pebesma et al. 2004
library(automap)                                        # Automated Kriging based on gstat module by Hiemstra et al. 2008
library(spgwr)
library(gpclib)
library(maptools)
library(graphics)
library(parallel)                            # Urbanek S. and Ripley B., package for multi cores & parralel processing
library(raster)
library(rasterVis)
library(plotrix)   #Draw circle on graph
library(reshape)
library(RCurl)
######### Functions used in the script
#loading R objects that might have similar names

out_prefix<-"_method_comp7_12042012_"
dates_input<-c("20100103","20100901")
i=2
##### LOAD USEFUL DATA

#obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path_wd<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison_10242012" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
#path<-"/Users/benoitparmentier/Dropbox/Data/NCEAS/Oregon_covariates/"            #Local dropbox folder on Benoit's laptop
setwd(path_wd) 
path_data_cai<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_CAI"  #Change to constant
path_data_fus<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_GAM"
#list files that contain model objects and ratingin-testing information for CAI and Fusion
obj_mod_fus_name<-"results_mod_obj__365d_GAM_fusion_const_all_lstd_11022012.RData"
obj_mod_cai_name<-"results_mod_obj__365d_GAM_CAI2_const_all_10312012.RData"

gam_fus<-load_obj(file.path(path_data_fus,obj_mod_fus_name))
gam_cai<-load_obj(file.path(path_data_cai,obj_mod_cai_name))  #This contains all the info
sampling_date_list<-gam_fus$sampling_obj$sampling_dat$date

### Projection for the current region
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
#User defined output prefix

#CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)
lines<-read.table(paste(path,"/",inlistf,sep=""), sep="")                      #Column 1 contains the names of raster files
inlistvar<-lines[,1]
inlistvar<-paste(path,"/",as.character(inlistvar),sep="")
covar_names<-as.character(lines[,2])                                         #Column two contains short names for covaraites

s_raster<- stack(inlistvar)                                                  #Creating a stack of raster images from the list of variables.
layerNames(s_raster)<-covar_names                                            #Assigning names to the raster layers
projection(s_raster)<-proj_str


########## Load Snotel data 
infile_snotname<-"snot_OR_2010_sp2_methods_11012012_.shp" #load Snotel data
snot_OR_2010_sp<-readOGR(".",sub(".shp","",infile_snotname))
snot_OR_2010_sp$date<-as.character(snot_OR_2010_sp$date)
tmp_date<-snot_OR_2010_sp$date[1]
#date<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed

#change format of dates...
#date_test<-strptime(tmp_date, "%e%m%y")   # interpolation date being processed
#date_test<-strptime(tmp_date, "%D")   # interpolation date being processed
#month<-strftime(date, "%m")          # current month of the date being processed
#LST_month<-paste("mm_",month,sep="") # name of LST month to be matched


#Load GHCN data used in modeling: training and validation site

### load specific date...and plot: make a function to extract the diff and prediction...
rast_diff_fc<-rast_fus_pred-rast_cai_pred
layerNames(rast_diff)<-paste("diff",date_selected,sep="_")


####COMPARE WITH LOCATION OF GHCN and SNOTEL NETWORK

X11(width=12,height=12)
plot(rast_diff_fc)
plot(snot_OR_2010_sp,pch=2,col="red",add=T)
plot(data_stat,add=T) #This is the GHCN network
legend("bottom",legend=c("SNOTEL", "GHCN"), 
       cex=0.8, col=c("red","black"),
       pch=c(2,1))
title(paste("SNOTEL and GHCN networks on ", date_selected, sep=""))
savePlot(paste("fig1a_map_SNOT_GHCN_network_diff_bckgd",date_selected,out_prefix,".png", sep=""), type="png")

plot(ELEV_SRTM)
plot(snot_OR_2010_sp,pch=2,col="red",add=T)
plot(data_stat,add=T)
legend("bottom",legend=c("SNOTEL", "GHCN"), 
       cex=0.8, col=c("red","black"),
       pch=c(2,1))
title(paste("SNOTEL and GHCN networks on ", date_selected, sep=""))
savePlot(paste("fig1b_map_SNOT_GHCN_network_elev_bckgd",date_selected,out_prefix,".png", sep=""), type="png")

## Select date from SNOT
#not_selected<-subset(snot_OR_2010_sp, date=="90110" )

dates<-c("20100103","20100901")
dates_snot<-c("10310","90110")
i=2

for(i in 1:length(dates)){
  
  date_selected<-dates[i]
  date_selected_snot<-as.integer(dates_snot[i])  #Change format of date at a later stage...
  snot_selected <-snot_OR_2010_sp[snot_OR_2010_sp$date==date_selected_snot,]
  #snot_selected<-na.omit(as.data.frame(snot_OR_2010_sp[snot_OR_2010_sp$date==90110,]))
  
  LC_stack<-stack(LC1,LC2,LC3,LC4,LC6,LC7)
  rast_pred3<-stack(rast_diff_fc,rast_pred2,LC_stack)
  layerNames(rast_pred3)<-c("diff_fc","fus","CAI","ELEV_SRTM","LC1","LC2","LC3","LC4","LC6","LC7")   #extract amount of veg...
  
  #extract info
  extract_snot<-extract(rast_pred3,snot_selected)  #
  snot_data_selected<-cbind(as.data.frame(snot_selected),extract_snot)
  snot_data_selected$res_f<-snot_data_selected$fus-snot_data_selected$tmax
  snot_data_selected$res_c<-snot_data_selected$CAI-snot_data_selected$tmax
  snot_data_selected<-(na.omit(as.data.frame(snot_data_selected)))
  
  #Plot predicted vs observed
  y_range<-range(c(data_vf$pred_mod7,snot_data_selected$fus),na.rm=T)
  x_range<-range(c(data_vf$dailyTmax,snot_data_selected$tmax),na.rm=T)
  plot(data_vf$pred_mod7,data_vf$dailyTmax, ylab="Observed daily tmax (C)", xlab="Predicted daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  text(data_vf$pred_mod7,data_vf$dailyTmax,labels=data_vf$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5,col="black")
  points(snot_data_selected$fus,snot_data_selected$tmax,pch=2,co="red")
  title(paste("Testing stations tmax fusion vs daily tmax",date_selected,sep=" "))
  savePlot(paste("fig2_testing_scatterplot_pred_fus_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png")
  
  ###### CAI
  y_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  x_range<-range(c(data_vc$dailyTmax,snot_data_selected$tmax),na.rm=T)
  plot(data_vc$pred_mod9,data_vc$dailyTmax, ylab="Observed daily tmax (C)", xlab="Predicted daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  text(data_vc$pred_mod9,data_vc$dailyTmax,labels=data_vf$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5,col="black")
  points(snot_data_selected$CAI,snot_data_selected$tmax,pch=2,co="red")
  title(paste("Testing stations tmax CAI vs daily tmax",date_selected,sep=" "))
  savePlot(paste("fig3_testing_scatterplot_pred_CAI_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png")
  
  ##### ELEV CAI
  y_range<-range(c(data_vc$dailyTmax,snot_data_selected$tmax),na.rm=T)
  y_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  x_range<-range(c(data_vc$ELEV_SRTM,snot_data_selected$ELEV_SRTM),na.rm=T)
  
  plot(data_vc$ELEV_SRTM,data_vc$pred_mod9,ylab="Observed daily tmax (C)", xlab="Predicted daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  text(data_vc$ELEV_SRTM,data_vc$pred_mod9,labels=data_vc$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5, col="black")
  points(snot_data_selected$ELEV_SRTM,snot_data_selected$CAI,pch=2,co="red")
  title(paste("Testing stations tmax CAI vs daily tmax",date_selected,sep=" "))
  savePlot(paste("fig4_testing_scatterplot_pred_CAI_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png")
  
  ##### ELEV CAI
  y_range<-range(c(data_vc$dailyTmax,snot_data_selected$tmax),na.rm=T)
  #y_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  x_range<-range(c(data_vc$ELEV_SRTM,snot_data_selected$ELEV_SRTM),na.rm=T)
  
  plot(data_vc$ELEV_SRTM,data_vc$dailyTmax,ylab="Observed daily tmax (C)", xlab="Elevation (m)", 
       ylim=y_range,xlim=x_range)
  text(data_vc$ELEV_SRTM,data_vc$dailyTmax,labels=data_vc$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5, col="black")
  points(snot_data_selected$ELEV_SRTM,snot_data_selected$tmax,pch=2,co="red")
  title(paste("Testing stations tmax CAI vs daily tmax",date_selected,sep=" "))
  savePlot(paste("fig4_testing_scatterplot_pred_CAI_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png")
  
  brks<-seq(0,100,10)
  lab_brks<-seq(1,10,1)
  lc1_rcstat<-cut(snot_data_selected$LC1,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$LC1_rec<-lc1_rcstat
  lc_rcstat<-cut(snot_data_selected$LC3,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$LC3_rec<-lc_rcstat
  lc_rcstat<-cut(snot_data_selected$LC2,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$LC2_rec<-lc_rcstat
  lc_rcstat<-cut(snot_data_selected$LC4,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$LC4_rec<-lc_rcstat
  lc_rcstat<-cut(snot_data_selected$LC6,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$LC6_rec<-lc_rcstat
  lc_rcstat<-cut(snot_data_selected$LC7,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$LC7_rec<-lc_rcstat
  
  tmp<-aggregate(diff_fc~LC1_rec,data=snot_data_selected,FUN=mean)
  plot(tmp, type="l")
  tmp<-aggregate(diff_fc~LC2_rec,data=snot_data_selected,FUN=mean)
  plot(tmp)
  tmp<-aggregate(diff_fc~LC3_rec,data=snot_data_selected,FUN=mean)
  plot(tmp)
  tmp<-aggregate(diff_fc~LC4_rec,data=snot_data_selected,FUN=mean)
  plot(tmp)
  tmp<-aggregate(diff_fc~LC6_rec,data=snot_data_selected,FUN=mean)
  plot(tmp)
  plot(snot_data_selected$LC2_rec,snot_data_selected$diff_fc)
  table(snot_data_selected$LC7_rec)
  scatterplot(diff_fc~LC1|LC1_rec)
  mod_diff_fc_LC2<-lm(diff_fc~LC_rec,data=snot_data_selected)
  lc_melt<-melt(snot_data_selected[c("diff_fc","LC1_rec","LC2_rec","LC3_rec","LC4_rec","LC6_rec")],
                measure=c("diff_fc"), 
                id=c("LC1_rec","LC2_rec","LC3_rec","LC4_rec","LC6_rec"),
                na.rm=F)
  lc_cast<-cast(lc_melt,value~variable,mean)
  tabf_resmod9_locs<-as.data.frame(tabf_cast[,,1])
  
  ### PLOT LAND COVER
  X11()
  plot(zones_stat$zones,zones_stat$LC1_forest,type="b",ylim=c(-4.5,4.5),
       ylab="difference between CAI and fusion",xlab="land cover percent class/10")
  lines(lab_brks,snot_data_selected,col="red",type="b")
  lines(zones_stat$zones,zones_stat[,4],col="blue",type="b")
  lines(zones_stat$zones,zones_stat[,5],col="darkgreen",type="b")
  lines(zones_stat$zones,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1)
  title(paste("Prediction tmax difference and land cover ",sep=""))
  
  savePlot(paste("fig6_diff_prediction_tmax_difference_land cover",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  snot_data_selected$LC2_rec<-lc2_rcstat
  
  ############ ACCURACY METRICS AND RESIDUALS ####
  #snot_data_selected<-
  calc_accuracy_metrics<-function(x,y){
    residuals<-x-y
    mae<-mean(abs(residuals),na.rm=T)
    rmse<-sqrt(mean((residuals)^2,na.rm=T))
    me<-mean(residuals,na.rm=T)
    #r2<-cor(x,y,na.rm=T)
    metrics_dat<-list(mae,rmse,me)
    names(metrics_dat)<-c("mae","rmse","me")
    return(metrics_dat)
  }
  
  #change to tmax when fixed problem...
  ac_metrics_fus<-calc_accuracy_metrics(snot_data_selected$tmax,snot_data_selected$fus) 
  ac_metrics_cai<-calc_accuracy_metrics(snot_data_selected$tmax,snot_data_selected$CAI) 
  
  #print(ac_metrics_fus,ac_metrics_cai)
  ac_metrics_fus
  ac_metrics_cai
  
  plot(snot_data_selected$E_SRTM,snot_data_selected$diff_fc)
  
  #DO MAE IN TERM OF ELEVATION CLASSES and LAND COVER CLASSES as well as diff...
  #DO diff IN TERM OF ELEVATION CLASSES as well as diff..
  
  brks<-c(0,500,1000,1500,2000,2500,4000)
  lab_brks<-1:6
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$elev_rec<-elev_rcstat
  y_range<-range(c(snot_data_selected$diff_fc))
  x_range<-range(c(elev_rcstat))
  plot(elev_rcstat,snot_data_selected$diff_fc, ylab="diff_fc", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations diff f vs Elevation",date_selected,sep=" "))
  
  brks<-c(0,500,1000,1500,2000,2500,4000)
  lab_brks<-1:6
  elev_rcstat<-cut(snot_data_selected$E_SRTM,breaks=brks,labels=lab_brks,right=F)
  
  y_range<-range(c(snot_data_selected$res_f),na.rm=T)
  x_range<-range(c(elev_rcstat))
  plot(elev_rcstat,snot_data_selected$res_f, ylab="res_f", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations residuals fusion vs Elevation",date_selected,sep=" "))
  
  brks<-c(0,500,1000,1500,2000,2500,4000)
  lab_brks<-1:6
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$elev_rec<-elev_rcstat
  y_range<-range(c(snot_data_selected$res_c),na.rm=T)
  x_range<-range(c(elev_rcstat))
  plot(elev_rcstat,snot_data_selected$res_c, ylab="res_c", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations residuals CAI vs Elevation",date_selected,sep=" "))
  
  #ADD BOTH
  plot(elev_rcstat,snot_data_selected$res_c, ylab="res_c", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  
  #CORRELATION BETWEEN RESIDUALS FUS and CAI
  
  y_range<-range(c(snot_data_selected$res_f,snot_data_selected$res_c),na.rm=T)
  x_range<-range(c(snot_data_selected$res_c,snot_data_selected$res_f),na.rm=T)
  plot(snot_data_selected$res_f,snot_data_selected$res_c, ylab="res_c", xlab="res_f ", 
       ylim=y_range, xlim=x_range)
  abline(0,1)
  #
  
  #CORRELATION BETWEEN PREDICTION FUS and CAI
  
  y_range<-range(c(snot_data_selected$fus),na.rm=T)
  x_range<-range(c(snot_data_selected$CAI),na.rm=T)
  plot(snot_data_selected$fus,snot_data_selected$CAI, ylab="CAI", xlab="FUS", 
       ylim=y_range, xlim=x_range)
  ####
  mae<-function(residuals){
    mean(abs(residuals),na.rm=T)
  }
  
  mean_diff_fc<-aggregate(diff_fc~elev_rec,data=snot_data_selected,mean)
  mean_mae_c<-aggregate(res_c~elev_rec,data=snot_data_selected,mae)
  mean_mae_f<-aggregate(res_c~elev_rec,data=snot_data_selected,mae)
  plot(mean_fus,type="o")
  plot(as.integer(as.character(mean_fus$elev_rec)),mean_fus$diff_fc,type="b")
  plot(as.integer(as.character(mean_fus$elev_rec)),mean_fus$diff_fc,type="b")
  hist(snot_data_selected$E_SRTM)
  hist(data_vf$ELEV_SRTM)
  
  diffelev_mod<-lm(diff_fc~elev_rec,data=snot_data_selected)
  summary(diffelev_mod)
  diffelev_mod<-lm(res_c~elev_rec,data=snot_data_selected)
  summary(diffelev_mod)
  diffelev_mod$fit
  table(snot_data_selected$elev_rec) #Number of observation per class
  max(snot_data_selected$E_STRM)
  
  avl<-c(0,10,1,10,20,2,20,30,3,30,40,4,40,50,5,50,60,6,60,70,7,70,80,8,80,90,9,90,100,10)#Note that category 1 does not include 0!!
  rclmat<-matrix(avl,ncol=3,byrow=TRUE)
  
  
  #### DO THIS FOR IMAGE STACK...DIFF and LAND COVER...
  dat_stack<-stack(rast_diff,rast_fus_pred,rast_cai_pred, ELEV_SRTM)
  dat_analysis<-as(dat_stack,"SpatialGridDataFrame")
  names(dat_analysis)<-c("diff_fc","pred_fus","pred_cai","E_SRTM")
  brks<-c(0,500,1000,1500,2000,2500,4000)
  lab_brks<-1:6
  elev_rcstat<-cut(dat_analysis$E_SRTM,breaks=brks,labels=lab_brks,right=F)
  dat_analysis$elev_rec<-elev_rcstat
  
  spplot(dat_analysis,"elev_rec")
  spplot(dat_analysis,"diff_fc")
  mean_diff_fc<-aggregate(diff_fc~elev_rec,data=dat_analysis,mean)
  table(dat_analysis$elev_rec) #Number of observation per class
  
  diffelev_mod<-lm(diff_fc~elev_rec,data=dat_analysis)
  summary(diffelev_mod)
  mean_rec6_val<-0.65993+(-8.56327)
  mean_diff_fc
  
  brks<-c(0,500,1000,1500,2000,2500,4000)
  lab_brks<-1:6
  elev_rcstat<-cut(data_vf$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  y_range<-range(c(diff_fc))
  x_range<-range(c(elev_rcstat))
  plot(elev_rcstat,diff_fc, ylab="diff_cf", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals fusion vs Elevation",date_selected,sep=" "))
  
  # Combine both training and testing
  pred_fus<-c(data_vf$pred_mod7,data_sf$pred_mod7)
  pred_cai<-c(data_vc$pred_mod9,data_sc$pred_mod9)
  elev_station<-c(data_vf$ELEV_SRTM,data_sf$ELEV_SRTM)
  diff_fc<-pred_fus-pred_cai
  
  elev_rcstat<-cut(elev_station,breaks=brks,labels=lab_brks,right=F)
  y_range<-range(diff_fc)
  x_range<-range(elev_station)
  plot(elev_station,diff_fc, ylab="diff_fc", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  text(elev_rcstat,diff_fc,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals fusion vs Elevation",date_selected,sep=" "))
  
  #USING BOTH validation and training
} 
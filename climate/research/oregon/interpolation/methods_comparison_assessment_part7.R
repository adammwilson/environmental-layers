#####################################  METHODS COMPARISON part 7 ##########################################
#################################### Spatial Analysis: validation CAI-fusion  ############################################
#This script utilizes the R ojbects created during the interpolation phase.                       #
#We use the SNOTEL dataset and the GHCN network to assess the prediction accuracy.
#This scripts focuses on a detailed study of differences in the predictions of CAI_kr and FUsion_Kr                              #
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

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

format_padding_month<-function(date_str){
  date_trans<-character(length=length(date_str))
  for (i in 1:length(date_str)){
    tmp_date<-date_str[i]
    nc<-nchar(tmp_date)
    nstart<-nc-1
    year<-substr(tmp_date,start=nstart,stop=nc)
    md<-substr(tmp_date,start=1,stop=(nstart-1))
    if (nchar(md)==3){
      md<-paste("0",md,sep="")
    }
    date_trans[i]<-paste(md,year,sep="")
  }  
  return(date_trans)
}

merge_multiple_df<-function(df_list,by_name){
  for (i in 1:(length(df_list)-1)){
    if (i==1){
      df1=df_list[[i]]
    }
    if (i!=1){
      df1=df_m
    }
    df2<-df_list[[i+1]]
    df_m<-merge(df1,df2,by=by_name,all=T)
  }
  return(df_m)
}

reclassify_df<-function(df,var_name,brks,lab_brks,suffix,summary_var){
  var_tab<-vector("list",length(var_name))
  for (i in 1:length(var_name)){
    var_rec_name<-paste(var_name[i],suffix,sep="_")
    var_rcstat<-cut(df[[var_name[i]]],breaks=brks,labels=lab_brks,right=T)
    df[[var_rec_name]]<-var_rcstat
    tmp<-aggregate(df[[summary_var]]~df[[var_rec_name]],data=df,FUN=mean)
    names(tmp)<-c(suffix,var_rec_name)
    var_tab[[i]]<-tmp
  }
  obj<-list(var_tab,df)
  names(obj)<-c("agg_df","df")
  return(list(var_tab,df))
}

station_data_interp<-function(date_str,obj_mod_interp_str,training=TRUE,testing=TRUE){
  date_selected<-date_str
  #load interpolation object
  obj_mod_interp<-load_obj(obj_mod_interp_str)
  sampling_date_list<-obj_mod_interp$sampling_obj$sampling_dat$date
  k<-match(date_selected,sampling_date_list)
  names(obj_mod_interp[[1]][[k]])               #Show the name structure of the object/list
  
  #Extract the training and testing information for the given date...
  data_s<-obj_mod_interp[[1]][[k]]$data_s #object for the first date...20100103                  
  data_v<-obj_mod_interp[[1]][[k]]$data_v #object for the first date...20100103                  
  if (testing==TRUE & training==FALSE){
    return(data_v)
  }
  if (training==TRUE & testing==FALSE){
    return(data_s)
  }
  if (training==TRUE & testing==TRUE ){
    dataset_stat<-list(data_v,data_s)
    names(dataset_stat)<-c("testing","training")
    return(dataset_stat)
  }
}

### Caculate accuracy metrics
calc_accuracy_metrics<-function(x,y){
  residuals<-x-y
  mae<-mean(abs(residuals),na.rm=T)
  rmse<-sqrt(mean((residuals)^2,na.rm=T))
  me<-mean(residuals,na.rm=T)
  r<-cor(x,y,use="complete")
  avg<-mean(residuals,na.rm=T)
  m50<-median(residuals,na.rm=T)
  metrics_dat<-as.data.frame(cbind(mae,rmse,me,r,avg,m50))
  names(metrics_dat)<-c("mae","rmse","me","r","avg","m50")
  return(metrics_dat)
}

#MODIFY LATER
# raster_pred_interp<-function(date_str,rast_file_name_list,path_data,data_sp){
#   date_selected<-date_str
#   #load interpolation object
#   setwd(path_data)
#   file_pat<-glob2rx(paste("*tmax_predicted*",date_selected,"*_365d_GAM_CAI2_const_all_10312012.rst",sep="")) #Search for files in relation to fusion                  
#   lf_pred<-list.files(pattern=file_pat) #Search for files in relation to fusion                  
#   
#   rast_cai2c<-stack(lf_cai2c)                   #lf_cai2c CAI results with constant sampling over 365 dates
#   rast_cai2c<-mask(rast_cai2c,mask_ELEV_SRTM)
#   
#   obj_mod_interp<-load_obj(obj_mod_interp_str)
#   sampling_date_list<-obj_mod_interp$sampling_obj$sampling_dat$date
#   k<-match(date_selected,sampling_date_list)
#   names(obj_mod_interp[[1]][[k]])               #Show the name structure of the object/list
#   
#   #Extract the training and testing information for the given date...
#   data_s<-obj_mod_interp[[1]][[k]]$data_s #object for the first date...20100103                  
#   data_v<-obj_mod_interp[[1]][[k]]$data_v #object for the first date...20100103                  
#   if (testing==TRUE & training==FALSE){
#     return(data_v)
#   }
#   if (training==TRUE & testing==FALSE){
#     return(data_s)
#   }
#   if (training==TRUE & testing==TRUE ){
#     dataset_stat<-list(data_v,data_s)
#     names(dataset_stat)<-c("testing","training")
#     return(dataset_stat)
#   }
# }

#########
#loading R objects that might have similar names

out_prefix<-"_method_comp7_12042012_"
infile2<-"list_365_dates_04212012.txt"

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

#dates<-c("20100103","20100901")
#dates_snot<-c("10310","90110")
#dates<-c("20100101","20100103","20100301","20100302","20100501","20100502","20100801","20100802","20100901","20100902")
#dates_snot<-c("10110","10310","30110","30210","50110","50210","80110","80210","90110","90210")

#Use file with date
dates<-readLines(file.path(path,infile2))
#Or use list of date in string
#dates<-c("20100103","20100901")

dates_snot_tmp<-snot_OR_2010_sp$date
dates_snot_formatted<-format_padding_month(dates_snot_tmp)
date_test<-strptime(dates_snot_formatted, "%m%d%y")   # interpolation date being processed
snot_OR_2010_sp$date_formatted<-date_test
#Load GHCN data used in modeling: training and validation site

### load specific date...and plot: make a function to extract the diff and prediction...
rast_diff_fc<-rast_fus_pred-rast_cai_pred
layerNames(rast_diff)<-paste("diff",date_selected,sep="_")

####COMPARE WITH LOCATION OF GHCN and SNOTEL NETWORK


i=1
date_selected<-dates[i]

X11(width=16,height=9)
par(mfrow=c(1,2))

plot(rast_diff_fc)
plot(snot_OR_2010_sp,pch=2,col="red",add=T)
plot(data_stat,add=T) #This is the GHCN network
legend("bottom",legend=c("SNOTEL", "GHCN"), 
       cex=0.8, col=c("red","black"),
       pch=c(2,1))
title(paste("SNOTEL and GHCN networks on ", date_selected, sep=""))

plot(ELEV_SRTM)
plot(snot_OR_2010_sp,pch=2,col="red",add=T)
plot(data_stat,add=T)
legend("bottom",legend=c("SNOTEL", "GHCN"), 
     cex=0.8, col=c("red","black"),
      pch=c(2,1))
title(paste("SNOTEL and GHCN networks", sep=""))
savePlot(paste("fig1_map_SNOT_GHCN_network_diff_elev_bckgd",date_selected,out_prefix,".png", sep=""), type="png")

#add histogram of elev for SNOT and GHCN
hist(snot_data_selected$ELEV_SRTM,main="")
title(paste("SNOT stations and Elevation",date_selected,sep=" "))
hist(data_vc$ELEV_SRTM,main="")
title(paste("GHCN stations and Elevation",date_selected,sep=" "))
savePlot(paste("fig2_hist_elev_SNOT_GHCN_",out_prefix,".png", sep=""), type="png")
dev.off()
## Select date from SNOT
#not_selected<-subset(snot_OR_2010_sp, date=="90110" )
list_ac_tab <-vector("list", length(dates))  #storing the accuracy metric data.frame in a list...
names(list_ac_tab)<-paste("date",1:length(dates),sep="")
X11(width=16,height=9)
par(mfrow=c(1,2))
#for(i in 1:length(dates)){
for(i in 163:length(dates)){
  date_selected<-dates[i]
  
  ## Get the relevant raster layers with prediction for fusion and CAI
  oldpath<-getwd()
  setwd(path_data_cai)
  file_pat<-glob2rx(paste("*tmax_predicted*",date_selected,"*_365d_GAM_CAI2_const_all_10312012.rst",sep="")) #Search for files in relation to fusion                  
  lf_cai2c<-list.files(pattern=file_pat) #Search for files in relation to fusion                  
  rast_cai2c<-stack(lf_cai2c)                   #lf_cai2c CAI results with constant sampling over 365 dates
  rast_cai2c<-mask(rast_cai2c,mask_ELEV_SRTM)
  
  oldpath<-getwd()
  setwd(path_data_fus)
  file_pat<-glob2rx(paste("*tmax_predicted*",date_selected,"*_365d_GAM_fusion_const_all_lstd_11022012.rst",sep="")) #Search for files in relation to fusion                  
  lf_fus1c<-list.files(pattern=file_pat) #Search for files in relation to fusion                        
  rast_fus1c<-stack(lf_fus1c)
  rast_fus1c<-mask(rast_fus1c,mask_ELEV_SRTM)
  
  #PLOT ALL MODELS
  #Prepare for plotting
  
  setwd(path) #set path to the output path
  
  rast_fus_pred<-raster(rast_fus1c,1)  # Select the first model from the stack i.e fusion with kriging for both steps
  rast_cai_pred<-raster(rast_cai2c,1)  
  layerNames(rast_cai_pred)<-paste("cai",date_selected,sep="_")
  layerNames(rast_fus_pred)<-paste("fus",date_selected,sep="_")
  rast_pred2<-stack(rast_fus_pred,rast_cai_pred)
  #function to extract training and test from object from object models created earlier during interpolation...
 
  #load training and testing date for the specified date for fusion and CAI
  data_vf<-station_data_interp(date_selected,file.path(path_data_fus,obj_mod_fus_name),training=FALSE,testing=TRUE)
  #data_sf<-station_data_interp(date_selected,file.path(path_data_fus,obj_mod_fus_name),training=TRUE,testing=FALSE)
  data_vc<-station_data_interp(date_selected,file.path(path_data_cai,obj_mod_cai_name),training=FALSE,testing=TRUE)
  #data_sc<-station_data_interp(date_selected,file.path(path_data_cai,obj_mod_cai_name),training=TRUE,testing=FALSE)
  
  date_selected_snot<-strptime(date_selected,"%Y%m%d")
  snot_selected <-snot_OR_2010_sp[snot_OR_2010_sp$date_formatted==date_selected_snot,]
  #snot_selected<-na.omit(as.data.frame(snot_OR_2010_sp[snot_OR_2010_sp$date==90110,]))
  rast_diff_fc<-rast_fus_pred-rast_cai_pred
  LC_stack<-stack(LC1,LC2,LC3,LC4,LC6,LC7)
  rast_pred3<-stack(rast_diff_fc,rast_pred2,ELEV_SRTM,LC_stack)
  layerNames(rast_pred3)<-c("diff_fc","fus","CAI","ELEV_SRTM","LC1","LC2","LC3","LC4","LC6","LC7")   #extract amount of veg...
  
  #extract predicted tmax corresponding to 
  extract_snot<-extract(rast_pred3,snot_selected)  #return value from extract is a matrix (with input SPDF)
  snot_data_selected<-cbind(as.data.frame(snot_selected),extract_snot)  #bind data together
  snot_data_selected$res_f<-snot_data_selected$fus-snot_data_selected$tmax #calculate the residuals for Fusion
  snot_data_selected$res_c<-snot_data_selected$CAI-snot_data_selected$tmax #calculate the residuals for CAI
  #snot_data_selected<-(na.omit(as.data.frame(snot_data_selected))) #remove rows containing NA, this may need to be modified later.
  
  ###fig3: Plot predicted vs observed tmax
  #fig3a: FUS
  x_range<-range(c(data_vf$pred_mod7,snot_data_selected$fus,data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  y_range<-range(c(data_vf$dailyTmax,snot_data_selected$tmax,data_vc$dailyTmax,snot_data_selected$tmax),na.rm=T)
  plot(data_vf$pred_mod7,data_vf$dailyTmax, ylab="Observed daily tmax (C)", xlab="Fusion predicted daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  #text(data_vf$pred_mod7,data_vf$dailyTmax,labels=data_vf$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5,col="black")
  points(snot_data_selected$fus,snot_data_selected$tmax,pch=2,co="red")
  title(paste("Testing stations tmax fusion vs daily tmax",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))  
  #fig 3b: CAI
  #x_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI))
  #y_range<-range(c(data_vc$dailyTmax,snot_data_selected$tmax))
  plot(data_vc$pred_mod9,data_vc$dailyTmax, ylab="Observed daily tmax (C)", xlab="CAI predicted daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  #text(data_vc$pred_mod9,data_vc$dailyTmax,labels=data_vf$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5,col="black")
  points(snot_data_selected$CAI,snot_data_selected$tmax,pch=2,co="red") 
  #text(snot_data_selected$CAI,snot_data_selected$tmax,labels=1:nrow(snot_data_selected),pos=3)
  #title(paste("Testing stations tmax CAI vs daily tmax",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  savePlot(paste("fig3_testing_scatterplot_pred_fus_CAI_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png")
    
  ##### Fig4a: ELEV-CAI
  y_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  #y_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  x_range<-range(c(data_vc$ELEV_SRTM,snot_data_selected$ELEV_SRTM),na.rm=T)
  lm_mod1<-lm(data_vc$pred_mod9~data_vc$ELEV_SRTM)
  lm_mod2<-lm(snot_data_selected$CAI~snot_data_selected$ELEV_SRTM)
  plot(data_vc$ELEV_SRTM,data_vc$pred_mod9,ylab="Observed daily tmax (C)", xlab="Elevation (m)", 
       ylim=y_range,xlim=x_range)
  #text(data_vc$ELEV_SRTM,data_vc$pred_mod9,labels=data_vc$idx,pos=3)
  abline(lm_mod1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  abline(lm_mod2,col="red") #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5, col="black")
  points(snot_data_selected$ELEV_SRTM,snot_data_selected$CAI,pch=2,co="red")
  title(paste("Testing stations tmax CAI vs elevation",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  
  #Fig4bELEV-FUS
  y_range<-range(c(data_vf$pred_mod7,snot_data_selected$fus),na.rm=T)
  x_range<-range(c(data_vf$ELEV_SRTM,snot_data_selected$ELEV_SRTM),na.rm=T)
  lm_mod1<-lm(data_vf$pred_mod7~data_vf$ELEV_SRTM)
  lm_mod2<-lm(snot_data_selected$fus~snot_data_selected$ELEV_SRTM)
  plot(data_vf$ELEV_SRTM,data_vf$pred_mod7,ylab="Observed daily tmax (C)", xlab="Elevation (m)", 
       ylim=y_range,xlim=x_range)
  #text(data_vc$ELEV_SRTM,data_vc$pred_mod9,labels=data_vc$idx,pos=3)
  abline(lm_mod1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  abline(lm_mod2,col="red") #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5, col="black")
  points(snot_data_selected$ELEV_SRTM,snot_data_selected$fus,pch=2,co="red")
  title(paste("Testing stations tmax  vs elevation",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  savePlot(paste("fig4_testing_scatterplot_pred_fus_CIA_elev_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png") 
  
  ############ ACCURACY METRICS AND RESIDUALS #############
  
  #START FIG 5
  #####Fig5a: CAI vs FUSION: difference by plotting on in terms of the other
  lm_mod<-lm(snot_data_selected$CAI~snot_data_selected$fus)
  y_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  x_range<-range(c(data_vf$pred_mod7,snot_data_selected$fus),na.rm=T)
  
  plot(data_vf$pred_mod7,data_vc$pred_mod9,ylab="Predicted CAI daily tmax (C)", xlab="Predicted fusion daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  #text(data_vc$ELEV_SRTM,data_vc$dailyTmax,labels=data_vc$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  abline(lm_mod,col="red")
  grid(lwd=0.5, col="black")
  points(snot_data_selected$fus,snot_data_selected$CAI,pch=2,co="red")
  title(paste("Testing stations predicted tmax fusion vs CAI tmax",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  ####Fig5b: diff vs elev: difference by plotting on in terms of elev
  diff_fc<-data_vf$pred_mod7-data_vc$pred_mod9
  plot(snot_data_selected$ELEV_SRTM,snot_data_selected$diff_fc,pch=2,col="red")
  lm_mod<-lm(snot_data_selected$diff_fc~snot_data_selected$ELEV_SRTM)
  abline(lm_mod,col="red")
  points(data_vf$ELEV_SRTM,diff_fc)
  lm_mod<-lm(diff_fc~data_vf$ELEV_SRTM)
  abline(lm_mod)
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  title(paste("Prediction tmax difference and elevation ",sep=""))
  savePlot(paste("fig5_testing_scatterplot_pred_fus_CAI_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png")

  #DO diff IN TERM OF ELEVATION CLASSES as well as diff..
    
  #### START FIG 6: difference fc vs elev
  #fig6a
  brks<-c(0,500,1000,1500,2000,2500,4000)
  lab_brks<-1:6
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$elev_rec<-elev_rcstat
  y_range<-range(c(snot_data_selected$diff_fc),na.rm=T)
  x_range<-range(c(elev_rcstat),na.rm=T)
  plot(elev_rcstat,snot_data_selected$diff_fc, ylab="diff_fc", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations diff f vs Elevation",date_selected,sep=" "))
  
  ###With fewer classes...fig6b
  brks<-c(0,1000,2000,3000,4000)
  lab_brks<-1:4
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$elev_rec<-elev_rcstat
  y_range<-range(c(snot_data_selected$diff_fc),na.rm=T)
  x_range<-range(c(elev_rcstat),na.rm=T)
  plot(elev_rcstat,snot_data_selected$diff_fc, ylab="diff_fc", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations diff f vs Elevation",date_selected,sep=" "))
  savePlot(paste("fig6_elevation_classes_diff_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""), type="png")
  
  #START FIG 7 with residuals
  #fig 7a
  brks<-c(0,1000,2000,3000,4000)
  lab_brks<-1:4
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$elev_rec<-elev_rcstat
  y_range<-range(c(snot_data_selected$res_f,snot_data_selected$res_c),na.rm=T)
  x_range<-range(c(elev_rcstat),na.rm=T)
  plot(elev_rcstat,snot_data_selected$res_f, ylab="res_f", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations residuals fusion vs Elevation",date_selected,sep=" "))
  #fig 7b
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  y_range<-range(c(snot_data_selected$res_c,snot_data_selected$res_f),na.rm=T)
  x_range<-range(c(elev_rcstat))
  plot(elev_rcstat,snot_data_selected$res_c, ylab="res_c", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations residuals CAI vs Elevation",date_selected,sep=" "))
  savePlot(paste("fig7_elevation_classes_residuals_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""), type="png")
  
  ####### COMPARE CAI FUSION USING SNOTEL DATA WITH ACCURACY METRICS###############
  ################ RESIDUALS and MAE etc.  #####################
  
  ### Run for full list of date? --365
  ac_tab_snot_fus<-calc_accuracy_metrics(snot_data_selected$tmax,snot_data_selected$fus) 
  ac_tab_snot_cai<-calc_accuracy_metrics(snot_data_selected$tmax,snot_data_selected$CAI) 
  ac_tab_ghcn_fus<-calc_accuracy_metrics(data_vf$dailyTmax,data_vf$pred_mod7) 
  ac_tab_ghcn_cai<-calc_accuracy_metrics(data_vc$dailyTmax,data_vc$pred_mod9)
  
  ac_tab<-do.call(rbind,list(ac_tab_snot_fus,ac_tab_snot_cai,ac_tab_ghcn_fus,ac_tab_ghcn_cai))
  rownames(ac_tab)<-c("snot_fus","snot_cai","ghcn_fus","ghcn_cai")
  ac_tab$date<-date_selected
  list_ac_tab[[i]]<-ac_tab  #storing the accuracy metric data.frame in a list...
  #save(list_ac_tab,)
  save(list_ac_tab,file= paste("list_ac_tab_", date_selected,out_prefix,".RData",sep=""))

  #FIG8: boxplot of residuals for methods (fus, cai) using SNOT and GHCN data
  #fig8a
  y_range<-range(c(snot_data_selected$res_f,snot_data_selected$res_c,data_vf$res_mod7,data_vc$res_mod9),na.rm=T)
  boxplot(snot_data_selected$res_f,snot_data_selected$res_c,names=c("FUS","CAI"),ylim=y_range,ylab="Residuals tmax degree C")
  title(paste("Residuals for fusion and CAI methods for SNOT data ",date_selected,sep=" "))
  #fig8b
  boxplot(data_vf$res_mod7,data_vc$res_mod9,names=c("FUS","CAI"),ylim=y_range,ylab="Residuals tmax degree C")
  title(paste("Residuals for fusion and CAI methods for GHCN data ",date_selected,sep=" "))
  savePlot(paste("fig8_residuals_boxplot_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""), type="png")
  
  mae_fun<-function(residuals){
    mean(abs(residuals),na.rm=T)
  }
  
  mean_diff_fc<-aggregate(diff_fc~elev_rec,data=snot_data_selected,mean)
  mean_mae_c<-aggregate(res_c~elev_rec,data=snot_data_selected,mae_fun)
  mean_mae_f<-aggregate(res_f~elev_rec,data=snot_data_selected,mae_fun)
  
  ####FIG 9: plot MAE for fusion and CAI as well as boxplots of both thechnique
  #fig 9a: boxplot of residuals for MAE and CAI
  height<-cbind(snot_data_selected$res_f,snot_data_selected$res_c)
  boxplot(height,names=c("FUS","CAI"),ylab="Residuals tmax degree C")
  title(paste("Residuals for fusion and CAI methods for SNOT data ",date_selected,sep=" "))
  #par(new=TRUE)
  #abline(h=ac_tab[1,1],col="red")
  points(1,ac_tab[1,1],pch=5,col="red")
  points(2,ac_tab[2,1],pch=5,col="black")
  legend("bottom",legend=c("FUS_MAE", "CAI_MAE"), 
         cex=0.8, col=c("red","black"),
         pch=c(2,1))
  #fig 9b: MAE per 3 elevation classes:0-1000,1000-2000,2000-3000,3000-4000
  y_range<-c(0,max(c(mean_mae_c[,2],mean_mae_f[,2]),na.rm=T))
  plot(1:3,mean_mae_c[,2],ylim=y_range,type="n",ylab="MAE in degree C",xlab="elevation classes")
  points(mean_mae_c,ylim=y_range)
  lines(1:3,mean_mae_c[,2],col="black")
  par(new=TRUE)              # key: ask for new plot without erasing old
  points(mean_mae_f,ylim=y_range)
  lines(1:3,mean_mae_f[,2],col="red")
  legend("bottom",legend=c("FUS_MAE", "CAI_MAE"), 
         cex=0.8, col=c("red","black"),
         pch=c(2,1))
  title(paste("MAE per elevation classes for SNOT data ",date_selected,sep=" "))
  savePlot(paste("fig9_residuals_boxplot_MAE_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""), type="png")
  
  ### LM MODELS for difference and elevation categories
  ## Are the differences plotted on fig 9 significant??
  diffelev_mod<-lm(diff_fc~elev_rec,data=snot_data_selected)
  summary(diffelev_mod)
  ##LM MODEL MAE PER ELEVATION CLASS: residuals for CAI
  diffelev_mod<-lm(res_c~elev_rec,data=snot_data_selected)
  summary(diffelev_mod)
  ##LM MODEL MAE PER ELEVATION CLASS: residuals for Fusions
  diffelev_mod<-lm(res_f~elev_rec,data=snot_data_selected)
  summary(diffelev_mod)
  
  ### LM MODELS for RESIDUALS BETWEEN CAI AND FUSION
  ## Are the differences plotted on fig 9 significant??
  ## STORE THE p values...?? overall and per cat?
  
  #diffelev_mod<-lm(res_f~elev_rec,data=snot_data_selected)
  #table(snot_data_selected$elev_rec) #Number of observation per class
  #max(snot_data_selected$E_STRM)
  
  #res
  
  #############################################
  #USING BOTH validation and training
  #This part is exploratory....
  ################## EXAMINING RESIDUALS AND DIFFERENCES IN LAND COVER......############
  ######

  #LC_names<-c("LC1_rec","LC2_rec","LC3_rec","LC4_rec","LC6_rec")
  suf_name<-c("rec1")
  sum_var<-c("diff_fc")
  LC_names<-c("LC1","LC2","LC3","LC4","LC6")
  brks<-c(-1,20,40,60,80,101)
  lab_brks<-seq(1,5,1)
  #var_name<-LC_names; suffix<-"rec1"; s_function<-"mean";df<-snot_data_selected;summary_var<-"diff_fc"
  #reclassify_df(snot_data_selected,LC_names,var_name,brks,lab_brks,suffix,summary_var)
  
  #Calculate mean per land cover percentage
  data_agg<-reclassify_df(snot_data_selected,LC_names,brks,lab_brks,suf_name,sum_var)
  data_lc<-data_agg[[1]]
  snot_data_selected<-data_agg[[2]]
  
  by_name<-"rec1"
  df_lc_diff_fc<-merge_multiple_df(data_lc,by_name)
  
  ###### FIG10: PLOT LAND COVER
  zones_stat<-df_lc_diff_fc #first land cover
  #names(zones_stat)<-c("lab_brks","LC")
  y_range<-range(as.vector(t(zones_stat[,-1])),na.rm=T)
  lab_brks_mid<-c(10,30,50,70,90)
  plot(lab_brks_mid,zones_stat[,2],type="b",ylim=y_range,col="black", lwd=2,
       ylab="difference between fusion and CAI",xlab="land cover percent classes")
  lines(lab_brks_mid,zones_stat[,3],col="red",type="b")
  lines(lab_brks_mid,zones_stat[,4],col="blue",type="b")
  lines(lab_brks_mid,zones_stat[,5],col="darkgreen",type="b")
  lines(lab_brks_mid,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1,lwd=1.8)
  title(paste("Prediction tmax difference and land cover ",date_selected,sep=""))

  ###NOW USE RESIDUALS FOR FUSION
  sum_var<-"res_f"
  suf_name<-"rec2"
  data_agg2<-reclassify_df(snot_data_selected,LC_names,brks,lab_brks,suf_name,sum_var)
  data_resf_lc<-data_agg2[[1]]
  #snot_data_selected<-data_agg[[2]]
  
  by_name<-"rec2"
  df_lc_resf<-merge_multiple_df(data_resf_lc,by_name)
  
  zones_stat<-df_lc_resf #first land cover
  #names(zones_stat)<-c("lab_brks","LC")
  lab_brks_mid<-c(10,30,50,70,90)
  plot(lab_brks_mid,zones_stat[,2],type="b",ylim=y_range,col="black",lwd=2,
       ylab="tmax residuals fusion ",xlab="land cover percent classes")
  lines(lab_brks_mid,zones_stat[,3],col="red",type="b")
  lines(lab_brks_mid,zones_stat[,4],col="blue",type="b")
  lines(lab_brks_mid,zones_stat[,5],col="darkgreen",type="b")
  lines(lab_brks_mid,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1,lwd=1.2)
  title(paste("Prediction tmax residuals and land cover ",date_selected,sep=""))
  savePlot(paste("fig10_diff_prediction_tmax_diff_res_f_land cover",date_selected,out_prefix,".png", sep=""), type="png")
  
  #### FIGURE11: res_f and res_c per land cover
  
  sum_var<-"res_c"
  suf_name<-"rec3"
  data_agg3<-reclassify_df(snot_data_selected,LC_names,brks,lab_brks,suf_name,sum_var)
  data_resc_lc<-data_agg3[[1]]
  snot_data_selected<-data_agg3[[2]]
  
  by_name<-"rec3"
  df_lc_resc<-merge_multiple_df(data_resc_lc,by_name)
  
  zones_stat<-df_lc_resc #first land cover
  #names(zones_stat)<-c("lab_brks","LC")
  y_range<-range(as.vector(t(zones_stat[,-1])),na.rm=T)
  lab_brks_mid<-c(10,30,50,70,90)
  plot(lab_brks_mid,zones_stat[,2],type="b",ylim=y_range,col="black",lwd=2,
       ylab="tmax residuals CAI",xlab="land cover percent classes")
  lines(lab_brks_mid,zones_stat[,3],col="red",type="b")
  lines(lab_brks_mid,zones_stat[,4],col="blue",type="b")
  lines(lab_brks_mid,zones_stat[,5],col="darkgreen",type="b")
  lines(lab_brks_mid,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1,lwd=1.2)
  title(paste("Prediction tmax residuals CAI and land cover ",date_selected,sep=""))
  
  #fig11b
  zones_stat<-df_lc_resf #first land cover
  #names(zones_stat)<-c("lab_brks","LC")
  y_range<-range(as.vector(t(zones_stat[,-1])),na.rm=T)
  lab_brks_mid<-c(10,30,50,70,90)
  plot(lab_brks_mid,zones_stat[,2],type="b",ylim=y_range,col="black",lwd=2,
       ylab="tmax residuals fusion ",xlab="land cover percent classes")
  lines(lab_brks_mid,zones_stat[,3],col="red",type="b")
  lines(lab_brks_mid,zones_stat[,4],col="blue",type="b")
  lines(lab_brks_mid,zones_stat[,5],col="darkgreen",type="b")
  lines(lab_brks_mid,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1,lwd=1.2)
  title(paste("Prediction tmax residuals and land cover ",date_selected,sep=""))
  #savePlot(paste("fig10_diff_prediction_tmax_diff_res_f_land cover",date_selected,out_prefix,".png", sep=""), type="png")
  savePlot(paste("fig11_prediction_tmax_res_f_res_c_land cover",date_selected,out_prefix,".png", sep=""), type="png")
    
} 

#Collect accuracy information for different dates
ac_data_xdates<-do.call(rbind,list_ac_tab)

ac_data_xdates$mod_id<-rownames(ac_data_xdates)

tmp_rownames<-rownames(ac_data_xdates)
rowstr<-strsplit(tmp_rownames,"\\.")
for (i in 1:length(rowstr)){
  ac_data_xdates$mod_id[i]<-rowstr[[i]][[2]]
}
##Now subset for each model...

mod_names<-unique(ac_data_xdates$mod_id)
for (i in 1:length(rowstr)){
  data_ac<-subset(ac_data_xdates,mod_id==mod_names[i])
  data_name<-paste("data_ac_",mod_names[i],sep="")
  assign(data_name,data_ac)
}

X11(12,12)
boxplot(data_ac_ghcn_fus$mae)
boxplot(data_ac_snot_fus$mae)
boxplot(data_ac_ghcn_cai$mae)
boxplot(data_ac_snot_cai$mae)
boxplot(data_ac_snot_fus$mae,data_ac_snot_cai$mae,names=c("fus","CAI"))
boxplot(data_ac_ghcn_fus$mae,data_ac_ghcn_cai$mae,names=c("fus","CAI"))
boxplot(data_ac_ghcn_fus$mae,data_ac_ghcn_cai$mae,data_ac_snot_fus$mae,data_ac_snot_cai$mae,names=c("fus_SNOT","CAI_SNOT","fus_GHCN","CAI_GHCN"))
savePlot(paste("fig12_prediction_tmax_MAE_boxplot_fus_CAI_GHCN_SNOT_",date_selected,out_prefix,".png", sep=""), type="png")

boxplot(data_ac_ghcn_fus$rmse,data_ac_ghcn_cai$rmse,data_ac_snot_fus$rmse,data_ac_snot_cai$rmse,names=c("fus_SNOT","CAI_SNOT","fus_GHCN","CAI_GHCN"))
savePlot(paste("fig12_prediction_tmax_RMSE_boxplot_fus_CAI_GHCN_SNOT_",date_selected,out_prefix,".png", sep=""), type="png")

filename<-paste("accuracy_table_GHCN_SNOT_", date_selected,out_prefix,".RData",sep="")
save(ac_data_xdates,file=filename)

mean(data_ac_snot_fus)
mean(data_ac_snot_cai)
mean(data_ac_ghcn_fus)
mean(data_ac_ghcn_cai)

### END OF CODE
#Write a part to caculate MAE per date...
#ac_table_metrics<-do.call(rbind,ac_tab_list)

#Subset and present the average MAE and RMSE for the dataset...

#calculate average per month, extract LST too...?

####################################################################
#From this line on: code is exploratory...
####################################################################
#### DO THIS FOR IMAGE STACK...DIFF and LAND COVER...#### RESIDUALS AND LAND COVER...
# 
# dat_stack<-stack(rast_diff,rast_fus_pred,rast_cai_pred, ELEV_SRTM)
# dat_analysis<-as(dat_stack,"SpatialGridDataFrame")
# names(dat_analysis)<-c("diff_fc","pred_fus","pred_cai","E_SRTM")
# brks<-c(0,500,1000,1500,2000,2500,4000)
# lab_brks<-1:6
# elev_rcstat<-cut(dat_analysis$E_SRTM,breaks=brks,labels=lab_brks,right=F)
# dat_analysis$elev_rec<-elev_rcstat
# 
# spplot(dat_analysis,"elev_rec")
# spplot(dat_analysis,"diff_fc")
# mean_diff_fc<-aggregate(diff_fc~elev_rec,data=dat_analysis,mean)
# table(dat_analysis$elev_rec) #Number of observation per class
# 
# diffelev_mod<-lm(diff_fc~elev_rec,data=dat_analysis)
# summary(diffelev_mod)
# mean_rec6_val<-0.65993+(-8.56327)
# mean_diff_fc
# 
# brks<-c(0,500,1000,1500,2000,2500,4000)
# lab_brks<-1:6
# elev_rcstat<-cut(data_vf$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
# y_range<-range(c(diff_fc))
# x_range<-range(c(elev_rcstat))
# plot(elev_rcstat,diff_fc, ylab="diff_cf", xlab="ELEV_SRTM (m) ", 
#      ylim=y_range, xlim=x_range)
# text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
# grid(lwd=0.5,col="black")
# title(paste("Testing stations residuals fusion vs Elevation",date_selected,sep=" "))
# 
# # Combine both training and testing
# pred_fus<-c(data_vf$pred_mod7,data_sf$pred_mod7)
# pred_cai<-c(data_vc$pred_mod9,data_sc$pred_mod9)
# elev_station<-c(data_vf$ELEV_SRTM,data_sf$ELEV_SRTM)
# diff_fc<-pred_fus-pred_cai
# 
# elev_rcstat<-cut(elev_station,breaks=brks,labels=lab_brks,right=F)
# y_range<-range(diff_fc)
# x_range<-range(elev_station)
# plot(elev_station,diff_fc, ylab="diff_fc", xlab="ELEV_SRTM (m) ", 
#      ylim=y_range, xlim=x_range)
# text(elev_rcstat,diff_fc,labels=data_vf$idx,pos=3)
# grid(lwd=0.5,col="black")
# title(paste("Testing stations residuals fusion vs Elevation",date_selected,sep=" "))
# 

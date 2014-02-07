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
#

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

out_prefix<-"_method_comp7_12102012b_"
infile2<-"list_365_dates_04212012.txt"
infile1<- "ghcn_or_tmax_covariates_06262012_OR83M.shp"    #GHCN shapefile containing variables for modeling 2010                 
#infile2<-"list_10_dates_04212012.txt"                    #List of 10 dates for the regression
infile2<-"list_365_dates_04212012.txt"                    #list of dates
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       #mean LST for day 244
inlistf<-"list_files_05032012.txt"                        #list of raster images containing the Covariates
infile6<-"OR83M_state_outline.shp"
#stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)

i=2
##### LOAD USEFUL DATA

#obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison_10242012" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
path_wd<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison_10242012" #Jupiter LOCATION on Atlas for kriging
#path<-"/Users/benoitparmentier/Dropbox/Data/NCEAS/Oregon_covariates/"            #Local dropbox folder on Benoit's laptop
setwd(path) 
path_data_cai<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_CAI"  #Change to constant
path_data_fus<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_GAM"
#list files that contain model objects and ratingin-testing information for CAI and Fusion
obj_mod_fus_name<-"results_mod_obj__365d_GAM_fusion_const_all_lstd_11022012.RData"
obj_mod_cai_name<-"results_mod_obj__365d_GAM_CAI2_const_all_10312012.RData"

#external function
source("function_methods_comparison_assessment_part7_12102012.R")

### Projection for the current region
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
#User defined output prefix

### MAKE THIS A FUNCTION TO LOAD STACK AND DEFINE VALID RANGE...
#CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)
lines<-read.table(paste(path,"/",inlistf,sep=""), sep="")                      #Column 1 contains the names of raster files
inlistvar<-lines[,1]
inlistvar<-paste(path,"/",as.character(inlistvar),sep="")
covar_names<-as.character(lines[,2])                                         #Column two contains short names for covaraites

s_raster<- stack(inlistvar)                                                  #Creating a stack of raster images from the list of variables.
layerNames(s_raster)<-covar_names                                            #Assigning names to the raster layers
projection(s_raster)<-proj_str

#Create mask using land cover data
pos<-match("LC10",layerNames(s_raster))            #Find the layer which contains water bodies
LC10<-subset(s_raster,pos)
LC10[is.na(LC10)]<-0                               #Since NA values are 0, we assign all zero to NA
mask_land<-LC10<100                                #All values below 100% water are assigned the value 1, value 0 is "water"
mask_land_NA<-mask_land                            
mask_land_NA[mask_land_NA==0]<-NA                  #Water bodies are assigned value 1

data_name<-"mask_land_OR"
raster_name<-paste(data_name,".rst", sep="")
writeRaster(mask_land, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
#writeRaster(r2, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)

pos<-match("ELEV_SRTM",layerNames(s_raster)) #Find column with name "ELEV_SRTM"
ELEV_SRTM<-raster(s_raster,layer=pos)             #Select layer from stack on 10/30
s_raster<-dropLayer(s_raster,pos)
ELEV_SRTM[ELEV_SRTM <0]<-NA
mask_ELEV_SRTM<-ELEV_SRTM>0

#Change this a in loop...
pos<-match("LC1",layerNames(s_raster)) #Find column with name "value"
LC1<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC1[is.na(LC1)]<-0
pos<-match("LC2",layerNames(s_raster)) #Find column with name "value"
LC2<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC2[is.na(LC2)]<-0
pos<-match("LC3",layerNames(s_raster)) #Find column with name "value"
LC3<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC3[is.na(LC3)]<-0
pos<-match("LC4",layerNames(s_raster)) #Find column with name "value"
LC4<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC4[is.na(LC4)]<-0
pos<-match("LC6",layerNames(s_raster)) #Find column with name "value"
LC6<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC6[is.na(LC6)]<-0
pos<-match("LC7",layerNames(s_raster)) #Find column with name "value"
LC7<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC7[is.na(LC7)]<-0
pos<-match("LC9",layerNames(s_raster)) #Find column with name "LC9", this is wetland...
LC9<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC9[is.na(LC9)]<-0

LC_s<-stack(LC1,LC2,LC3,LC4,LC6,LC7)
layerNames(LC_s)<-c("LC1_forest","LC2_shrub","LC3_grass","LC4_crop","LC6_urban","LC7_barren")
LC_s <-mask(LC_s,mask_ELEV_SRTM)
plot(LC_s)

s_raster<-addLayer(s_raster, LC_s)

#mention this is the last... files

#Read region outline...
filename<-sub(".shp","",infile6)             #Removing the extension from file.
reg_outline<-readOGR(".", filename)                 #reading shapefile 

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
#rast_diff_fc<-rast_fus_pred-rast_cai_pred
#layerNames(rast_diff)<-paste("diff",date_selected,sep="_")

####COMPARE WITH LOCATION OF GHCN and SNOTEL NETWORK


i=1
date_selected<-dates[i]

X11(12,12)
# #plot(rast_diff_fc)
# plot(snot_OR_2010_sp,pch=2,col="red",add=T)
# plot(data_stat,add=T) #This is the GHCN network
# legend("bottom",legend=c("SNOTEL", "GHCN"), 
#        cex=0.8, col=c("red","black"),
#        pch=c(2,1))
# title(paste("SNOTEL and GHCN networks on ", date_selected, sep=""))

plot(ELEV_SRTM)
plot(snot_OR_2010_sp,pch=2,col="red",add=T)
#plot(data_stat,add=T)
legend("bottom",legend=c("SNOTEL", "GHCN"), 
       cex=0.8, col=c("red","black"),
       pch=c(2,1))
title(paste("SNOTEL and GHCN networks", sep=""))
savePlot(paste("fig1_map_SNOT_GHCN_network_diff_elev_bckgd",date_selected,out_prefix,".png", sep=""), type="png")
dev.off()

#add histogram of elev for SNOT and GHCN
#X11(width=16,height=9)
#par(mfrow=c(1,2))
#hist(snot_data_selected$ELEV_SRTM,main="")
#title(paste("SNOT stations and Elevation",date_selected,sep=" "))
#hist(data_vc$ELEV_SRTM,main="")
#title(paste("GHCN stations and Elevation",date_selected,sep=" "))
#savePlot(paste("fig2_hist_elev_SNOT_GHCN_",out_prefix,".png", sep=""), type="png")
#dev.off()
## Select date from SNOT
#not_selected<-subset(snot_OR_2010_sp, date=="90110" )
list_ac_tab <-vector("list", length(dates))  #storing the accuracy metric data.frame in a list...
names(list_ac_tab)<-paste("date",1:length(dates),sep="")


#ac_mod<-mclapply(1:length(dates), accuracy_comp_CAI_fus_function,mc.preschedule=FALSE,mc.cores = 8) #This is the end bracket from mclapply(...) statement
source("function_methods_comparison_assessment_part7_12102012.R")
#Use mcMap or mappply for function with multiple arguments...
#ac_mod<-mclapply(1:6, accuracy_comp_CAI_fus_function,mc.preschedule=FALSE,mc.cores = 1) #This is the end bracket from mclapply(...) statement
ac_mod<-mclapply(1:length(dates), accuracy_comp_CAI_fus_function,mc.preschedule=FALSE,mc.cores = 8) #This is the end bracket from mclapply(...) statement

tb<-ac_mod[[1]][[4]][0,]  #empty data frame with metric table structure that can be used in rbinding...
tb_tmp<-ac_mod #copy

for (i in 1:length(tb_tmp)){
  tmp<-tb_tmp[[i]][[4]]
  tb<-rbind(tb,tmp)
}
rm(tb_tmp)
#Collect accuracy information for different dates
#ac_data_xdates<-do.call(rbind,tb)
ac_data_xdates<-tb
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

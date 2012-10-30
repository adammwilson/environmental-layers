#####################################  METHODS COMPARISON part 3 ##########################################
#################################### Spatial Analysis ############################################
#This script utilizes the R ojbects created during the interpolation phase.                       #
#At this stage the script produces figures of various accuracy metrics and compare methods:       #
#- rank station in terms of residuals and metrics (MAE and RMSE)                                  #
#- calculate accuary metrics for climtology predictions...                                        #
#- spatial density of station network and accuracy metric                                         #
#- visualization of maps of prediction and difference for comparison                              #
#AUTHOR: Benoit Parmentier                                                                        #
#DATE: 10/23/2012                                                                                 #
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
## Functions
#loading R objects that might have similar names
load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

###Parameters and arguments

infile1<- "ghcn_or_tmax_covariates_06262012_OR83M.shp"    #GHCN shapefile containing variables for modeling 2010                 
#infile2<-"list_10_dates_04212012.txt"                    #List of 10 dates for the regression
infile2<-"list_365_dates_04212012.txt"                    #list of dates
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       #mean LST for day 244
inlistf<-"list_files_05032012.txt"                        #list of raster images containing the Covariates
infile6<-"OR83M_state_outline.shp"
#stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)

out_prefix<-"methods_09262012_"
#output path
path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_07192012_GAM"
setwd(path)

##### LOAD USEFUL DATA

obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
#path<-"/Users/benoitparmentier/Dropbox/Data/NCEAS/Oregon_covariates/"            #Local dropbox folder on Benoit's laptop
setwd(path) 
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
                                              #User defined output prefix

sampling_CAI<-load_obj("results2_CAI_sampling_obj_09132012_365d_GAM_CAI2_multisampling2.RData")
sampling_fus<-load_obj("results2_fusion_sampling_obj_10d_GAM_fusion_multisamp4_09192012.RData")
fus_CAI_mod<-load_obj("results2_CAI_Assessment_measure_all_09132012_365d_GAM_CAI2_multisampling2.RData")
gam_fus_mod1<-load_obj("results2_fusion_Assessment_measure_all_10d_GAM_fusion_multisamp4_09192012.RData")

filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

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
#LC10_mask[LC10_mask<100]<-1
mask_land<-LC10==100                               #All values below 100% water are assigned the value 1, value 0 is "water"
mask_land_NA<-mask_land                            
mask_land_NA[mask_land_NA==0]<-NA                  #Water bodies are assigned value 1

data_name<-"mask_land_OR"
raster_name<-paste(data_name,".rst", sep="")
writeRaster(mask_land, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
#writeRaster(r2, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)

pos<-match("mm_01",layerNames(s_raster))
mm_01<-subset(s_raster,pos)
mm_01<-mm_01-273.15
mm_01<-mask(mm_01,mask_land_NA)  #Raster image used as backround
#mention this is the last... files

################# PART 1: COMPARING RESULTS USING ALL MONTHNLY DATA AND SAMPLED FROM DAILY###################
######## Visualize data: USING ALL MONTHLY DATA...

file_pat<-glob2rx("GAMCAI_tmax_pred_predicted_mod*_365d_GAM_CAI2_all_lstd_10262012.rst")   
lf_cai_all<-list.files(pattern=file_pat)
image_file<-grep(file_pat,lf_ck,value=TRUE) # using grep with "value" extracts the matching names         
raster_pred_k<-raster(image_file)

#### CHECKING RESULTS ON 10/28 BY VISUALIZING PREDICTIONS... DO THAT FOR ALL MODELS AND TWO DATES...

date_selected<-"20100103"
lf_fus<-list.files(pattern=paste("GAM_bias_tmax_predicted_mod8_",date_selected,"_30_1_365d_GAM_fusion_all_lstd_10242012.rst$",sep="")) #Search for files in relation to fusion
lf_cai<-list.files(pattern=paste("*CAI_tmax_pred.*",date_selected,"*.08072012_365d_GAM_CAI2.rst$",sep="")) #Search for files in relation to fusion

lf_cai<-list.files(pattern=paste("GAMCAI_tmax_pred_predicted_mod8_20100103_30_1_365d_GAM_CAI2_all_lstd_10262012.rst",sep=""))

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_GAM"
setwd(path)
tmax_predicted_mod8<-raster("GAM_bias_tmax_predicted_mod8_20100103_30_1_365d_GAM_fusion_all_lstd_10242012.rst")
tmax_bias_mod8<-raster("GAM_bias_predicted_mod8_20100103_30_1_365d_GAM_fusion_all_lstd_10242012.rst")
daily_delta_tmax<-raster("fusion_daily_delta_20100103_30_1_365d_GAM_fusion_all_lstd_10242012.rst")
fusion<-raster("fusion_tmax_predicted_20100103_30_1_365d_GAM_fusion_all_lstd_10242012.rst")
LST<-(raster("mean_month1_rescaled.rst"))-273.16

tmp_rast<-LST+daily_delta_tmax-tmax_bias_mod8
dif_rast<-tmp_rast-tmax_predicted_mod8
eq<-tmp_rast==tmax_predicted_mod8
plot(tmax_predicted_mod8)

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_CAI"
setwd(path)
cai_tmax_mod8_rast<-raster("GAMCAI_tmax_pred_predicted_mod8_20100103_30_1_365d_GAM_CAI2_all_lstd_10262012.rst")
cai_delta_rast<-raster("CAI_deltaclim_20100103_30_1_365d_GAM_CAI2_all_lstd_10262012.rst")
cai_clim_rast<-raster("CAI_clim_20100103_30_1_365d_GAM_CAI2_all_lstd_10262012.rst")
cai_climod8_rast<-raster("GAMCAI_predicted_mod8_20100103_30_1_365d_GAM_CAI2_all_lstd_10262012.rst")

# s.range <- c(min(minValue(diff)), max(maxValue(diff)))
# col.breaks <- pretty(s.range, n=50)
# lab.breaks <- pretty(s.range, n=5)
 temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
# plot(diff, breaks=col.breaks, col=temp.colors(length(col.breaks)-1)
#      
pred_cai_mod8<-stack(cai_climod8_rast,cai_delta_rast,cai_tmax_mod8_rast)
layerNames(pred_cai_mod8)<-paste(c("cai_climod8_rast","cai_delta_rast","cai_tmax_mod8_rast"),"20100103",sep=" ")
pred_fus_mod8<-stack(tmax_bias_mod8,daily_delta_tmax,LST,tmax_predicted_mod8)
layerNames(pred_fus_mod8)<-paste(c("tmax_bias_mod8","daily_delta_tmax","LST","tmax_predicted_mod8"),"20100103",sep=" ")
pred_fus_cai_comp<-stack(pred_fus_mod8,pred_cai_mod8)

X11(12,12)
#for (i in 1:)
#plot(pred_cai_mod8,col=temp.colors)
plot(pred_cai_mod8,col=temp.colors(25))
#title("prediction for 20100103")
plot(pred_fus_mod8,col=temp.colors(25))
plot(pred_fus_cai_comp,col=temp.colors(25))
dev.off
tmp_rast2<-cai_delta_rast + cai_climod8_rast
dif2_rast<-tmp_rast2-cai_tmax_mod8_rast

dif2<-cai_rast-tmax_predicted_mod8 #This seems to be the same!!!
plot(cai_climod8_rast) #THe bias surface is different when compared to the climatology based on mod8
plot(tmax_bias_mod8)
plot(tmax_predicted_mod8)
plot(cai_tmax_rast)

cellStats(pred_fus_cai_comp)

#t<-"results2_CAI_Assessment_measure_all_08072012_365d_GAM_CAI2.RData" 

## Examining data with simplified CAI3 models in comparison to other: 11/03/2012

oldpath<-getwd()
path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_CAI"
setwd(path)

date_selected<-"20100103"
#lf_fus<-list.files(pattern=paste("GAM_bias_tmax_predicted_mod8_",date_selected,"_30_1_365d_GAM_fusion_all_lstd_10242012.rst$",sep="")) #Search for files in relation to fusion
file_pat<-glob2rx(paste("*tmax_pred*",date_selected,"*_365d_GAM_CAI3_all_10302012.rst",sep="")) #Search for files in relation to fusion
lf_cai3<-list.files(pattern=file_pat) #Search for files in relation to fusion

cai3_mod<-load_obj("results_mod_obj__365d_GAM_CAI3_all_10302012.RData")
names(cai3_mod$gam_CAI_mod[[1]]) #check names within object
formulas<-as.character(cai3_mod$gam_CAI_mod[[1]]$formulas)
rmse<-(cai3_mod$gam_CAI_mod[[1]]$tb_metrics1)
nmodels<-6
for(i in 4:(nmodels+3)){            # start of the for loop #1
  rmse[,i]<-as.numeric(as.character(rmse[,i]))  
}
rmse_val<-as.numeric(c(rmse[1,9],rmse[1,4:8]))
rmse_val<-format(rmse_val,digits=3)
rmse_val<-paste("(",rmse_val,")",sep="")
rname<-c("CAI_Kr",formulas)
rname<-paste(rname,rmse_val,sep=" ")

rast_cai3<-stack(lf_cai3)
rast_cai3<-mask(rast_cai3,mask_ELEV_SRTM)
#s_range <- c(min(minValue(diff)), max(maxValue(diff)))
s_range<-c(minValue(rast_cai3),maxValue(rast_cai3)) #stack min and max
s_range<-c(min(s_range),max(s_range))
col_breaks <- pretty(s_range, n=50)
lab_breaks <- pretty(s_range, n=5)
temp_colors <- colorRampPalette(c('blue', 'white', 'red'))
par(mfrow=c(2,3))
for (k in 1:length(lf_cai3)){
  cai3_r<-raster(rast_cai3,k)
  plot(cai3_r, breaks=col_breaks, col=temp_colors(length(col_breaks)-1),   
       axis=list(at=lab_breaks, labels=lab_breaks),main=rname[k])
}

#Wihtout setting range
s_range<-c(-12,18)
#col_breaks <- pretty(s_range, n=50)
#lab_breaks <- pretty(s_range, n=5)
col_breaks<-49
temp_colors <- colorRampPalette(c('blue', 'white', 'red'))
par(mfrow=c(2,3))
for (k in 1:length(lf_cai3)){
  cai3_r<-raster(rast_cai3,k)
  plot(cai3_r, col=temp_colors(col_breaks),   
       ,main=rname[k])
}

###### PART 2: ################
#### EXAMINING LST AND ELEVATION DATASETS TO CHECK FOR EXTREMES AND SELECT SCREENING VALUES....

# Examining ELEV_SRTM and screening out...

pos<-match("ELEV_SRTM",layerNames(s_raster))
ELEV_SRTM<-raster(s_raster,pos)
head(freq(ELEV_SRTM))  #Show how many pixels are below 0

# mask out values below zero
elev<-ELEV_SRTM
elev[0<elev]<-NA  #Remove all negative elevation
#Show how many were NA...

mask_ELEV_SRTM<-elev >0
quartz()
plot(elev, main="Elevation in Oregon")
savePlot(paste("elevation_Oregon",out_prefix,".png", sep=""), type="png")

# Examining LST data: extremes values and number of valid observation              

l<-list.files(pattern="mean_month.*rescaled.rst")
ln<-list.files(pattern="mean_month_valid_obs_.*_Sum.rst") #number of observation missing
l<-mixedsort(l)
ln<-mixedsort(ln)

#l <-readLines(paste(path,"/",infile6, sep=""))
molst<-stack(l)  #Creating a raster stack...
nobslst<-stack(ln)  #Creating a raster stack...

#setwd(old)
molst<-molst-273.16  #K->C          #LST stack of monthly average...
idx <- seq(as.Date('2010-01-15'), as.Date('2010-12-15'), 'month')
molst <- setZ(molst, idx)
layerNames(molst) <- month.abb
layerNames(nobslst)<-month.abb

X11(width=18,height=12)
#PLOT THE STACK AND SAVE IMAGE
plot(molst,col=temp.colors(25))
savePlot("lst_climatology_OR.png",type="png")
levelplot(molst,col.regions=temp.colors(25))
plot(nobslst)
savePlot("lst_climatology_OR_nobs.png",type="png")
dev.off)

#note differnces in patternin agricultural areas and 
extremeValues(molst) #not yet implemented...
min_values<-cellStats(molst,"min")
max_values<-cellStats(molst,"max")
mean_values<-cellStats(molst,"mean")
sd_values<-cellStats(molst,"sd")
#median_values<-cellStats(molst,"median") Does not extist
statistics_LST_s<-cbind(min_values,max_values,mean_values,sd_values) #This shows that some values are extremes...especially in October
LST_stat_data<-as.data.frame(statistics_LST_s)
names(LST_stat_data)<-c("min","max","mean","sd")
# Statistics for number of valid observation stack
min_values<-cellStats(nobslst,"min")
max_values<-cellStats(nobslst,"max")
mean_values<-cellStats(nobslst,"mean")
sd_values<-cellStats(nobslst,"sd")
statistics_LSTnobs_s<-cbind(min_values,max_values,mean_values,sd_values) #This shows that some values are extremes...especially in October
LSTnobs_stat_data<-as.data.frame(statistics_LSTnobs_s)

X11(width=12,height=12)
#Plot statiscs (mean,min,max) for monthly LST images
plot(1:12,LST_stat_data$mean,type="b",ylim=c(-15,60),col="black",xlab="month",ylab="tmax (degree C)")
lines(1:12,LST_stat_data$min,type="b",col="blue")
lines(1:12,LST_stat_data$max,type="b",col="red")
text(1:12,LST_stat_data$mean,rownames(LST_stat_data),cex=1,pos=2)

legend("topleft",legend=c("min","mean","max"), cex=1.5, col=c("blue","black","red"),
       lty=1)
title(paste("LST statistics for Oregon", "2010",sep=" "))
savePlot("lst_statistics_OR.png",type="png")

#Plot number of valid observations for LST
plot(1:12,LSTnobs_stat_data$mean,type="b",ylim=c(0,280),col="black",xlab="month",ylab="tmax (degree C)")
lines(1:12,LSTnobs_stat_data$min,type="b",col="blue")
lines(1:12,LSTnobs_stat_data$max,type="b",col="red")
text(1:12,LSTnobs_stat_data$mean,rownames(LSTnobs_stat_data),cex=1,pos=2)

legend("topleft",legend=c("min","mean","max"), cex=1.5, col=c("blue","black","red"),
       lty=1)
title(paste("LST number of valid observations for Oregon", "2010",sep=" "))
savePlot("lst_nobs_OR.png",type="png")

mm_10<-raster(molst,10)
tab_mm_10<-freq(mm_10)  #This shows that there are only 6 pixels with values between  -86 and -89.. and 8 pixels with values between 38 and 31
head(tab_mm_10)
#screening maybe good...?

mm_01<-raster(molst,1)
tab_mm_10<-freq(mm_01)  #This shows that there are only 6 pixels with values between  -86 and -89.. and 8 pixels with values between 38 and 31
#head(tab_mm_10)

mm_07<-raster(molst,7)
tab_mm_10<-freq(mm_07)  #This shows that there are only 6 pixels with values between  -86 and -89.. and 8 pixels with values between 38 and 31
#head(tab_mm_10)

LST_comp<-stack(mm_01,mm_07)

X11(width=18,height=10)
#PLOT THE STACK AND SAVE IMAGE
plot(LST_comp,col=temp.colors(25))
#title("LST spatial pattern in January and August")
savePlot("lst_climatology_OR.png",type="png")


###### PART 3: EXAMANING MODEL OUTPUTS USING MOD OBJECTS TO CHECK AND COMPARE RESULTS ############
############ LOOKING AT SPECIFIC STATIONS...
path_data_cai<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_CAI"
path_data_fus<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_GAM"
setwd(path_data) #output data
gam_fus<-load_obj(file.path(path_data_fus,
                            "results2_fusion_Assessment_measure_all_365d_GAM_fusion_all_lstd_10242012.RData"))
gam_cai<-load_obj(file.path(path_dat_cai,
                            "results2_CAI_Assessment_measure_all_365d_GAM_CAI2_all_lstd_10262012.RData")  #This contains all the info
sampling_obj_cai<-load_obj(file.path(path_data_cai,
                                     "results2_CAI_sampling_obj_365d_GAM_CAI2_all_lstd_10262012.RData"))
sampling_date_list<-sampling_obj_cai$sampling_dat$date
date_selected="20100103"

k<-match(date_selected,sampling_date_list)

fus_d<-gam_fus[[k]] #object for the first date...20100103
fus_d$tb_metrics1   #This is a summary for validation metrics...(RMSE and MAE)

cai_d<-gam_cai[[k]] #object for the first date...20100103
cai_d$tb_metrics1   #This is a summary for validation metrics...(RMSE and MAE)
                  
names(fus_d$mod_obj) #list models, nine models: mod1, mod2,...,mod9a, mod9b
names(cai_d$mod_obj)
                  
mod1_f<-fus_d$mod_obj$mod1 # extract model 1 for fusion on 
mod4_f<-fus_d$mod_obj$mod4
mod7_f<-fus_d$mod_obj$mod7
data_sf<-fus_d$data_s
data_vf<-fus_d$data_v

plot(mod1_f)  #Examining the fit
plot(mod1)    #Examing the fit
#data_monthf<-fus_d1$data_month

mod1<-cai_d1$mod_obj$mod1
mod4<-cai_d1$mod_obj$mod4
mod7<-cai_d1$mod_obj$mod7
data_s<-cai_d1$data_s
data_v<-cai_d1$data_v
data_month<-cai_d1$data_month

#### COMPARE  LST AND TMAX averages over the year...  ###############
#### Screening necessary?
                  							
LSTD_bias_month<-vector("list",length(gam_cai))  # list to store bias information
data_month_list<-vector("list",length(gam_cai))  # list to store data_month used for training

for (i in 1:length(gam_cai)){
  data_month_list[[i]]<-gam_cai[[i]]$data_month
  #LSTD_bias_month[[i]]<-aggregate(LSTD_bias~month,data=data_month_list[[i]],mean)
}

#tmp<-do.call(rbind,LSTD_bias_month)
data_month_all<-do.call(rbind,data_month_list)

data_month_all$LST<-data_month_all$LST-273.16
LSTD_bias_avgm<-aggregate(LSTD_bias~month,data=data_month_all,mean)
LSTD_bias_sdm<-aggregate(LSTD_bias~month,data=data_month_all,sd)

LST_avgm<-aggregate(LST~month,data=data_month_all,mean)
TMax_avgm<-aggregate(TMax~month,data=data_month_all,mean)

#plot(LST_avgm,type="b")
#lines(TMax_avgm,col="blue",add=T)
plot(data_month_all$month,data_month_all$LST, xlab="month",ylab="LST at station")
title(paste("Monthly LST at stations in Oregon", "2010",sep=" "))
savePlot(paste("LST_at_stations_per_month_",out_prefix,".png", sep=""), type="png")
                  
tab_freq<-cbind(hist_LST$breaks,hist_LST$counts)               
median(data_month_all$LST,na.rm=T)
median(data_month_all$TMax,na.rm=T)
                  
statistics_LST_s<-as.data.frame(statistics_LST_s)
plot(TMax_avgm,type="b",ylim=c(0,35),col="red",xlab="month",ylab="tmax (degree C)")
lines(1:12,statistics_LST_s$mean_values,type="b",col="blue")
text(TMax_avgm[,1],TMax_avgm[,2],rownames(statistics_LST_s),cex=0.8,pos=2)
                  
legend("topleft",legend=c("TMax","LST"), cex=1, col=c("red","blue"),
              lty=1)
title(paste("Monthly mean tmax and LST at stations in Oregon", "2010",sep=" "))           
savePlot(paste("Temporal_profile_res",id[i],out_prefix,".png", sep=""), type="png")
                  
#Account for forest
data_month_all_forest<-data_month_all[data_month_all$LC1>=50,]
data_month_all_grass<-data_month_all[data_month_all$LC3>=10,]
LST_avgm_forest<-aggregate(LST~month,data=data_month_all_forest,mean)
LST_avgm_grass<-aggregate(LST~month,data=data_month_all_grass,mean)

plot(TMax_avgm,type="b",ylim=c(-7,42))
lines(LST_avgm,col="blue",add=T)
lines(LST_avgm_forest,col="green",add=T)
lines(LST_avgm_grass,col="red",add=T)
legend("topleft",legend=c("TMax","LST","LST_forest","LST_grass"), cex=0.8, col=c("black","blue","green","red"),
       lty=1)
title(paste("Monthly average tmax for stations in Oregon ", "2010",sep=""))

savePlot(paste("Temporal_profile_res",id[i],out_prefix,".png", sep=""), type="png")  
                  
#Check where forest >50 and grass>10
#Create mask using land cover data
pos<-match("LC1",layerNames(s_raster))            #Find the layer which contains water bodies
LC1<-subset(s_raster,pos)
LC1[is.na(LC1)]<-0                               #Since NA values are 0, we assign all zero to NA
LC1_50<-LC1>= 50
plot(LC1_50)
pos<-match("LC3",layerNames(s_raster))            #Find the layer which contains water bodies
LC3<-subset(s_raster,pos)
LC3[is.na(LC3)]<-0                               #Since NA values are 0, we assign all zero to NA
LC3_10<-LC3>= 10

LC1_mean <- cellStats(LC3, mean)
LC1_mean <- cellStats(LC3, mean)
#
avl<-c(0,10,1,10,20,2,20,30,3,30,40,4,40,50,5,50,60,6,60,70,7,70,80,8,80,90,9,90,100,10)
rclmat<-matrix(avl,ncol=3,byrow=TRUE)
LC3_rec<-reclass(LC3,rclmat)  #Loss of layer names when using reclass
tab_freq<-freq(LC3_rec)     

crosstab(LC1_50,LC3_10) #Very little overalp between classes...
X11(12,12)                  
LC_rec<-stack(LC1_50,LC3_10)    
layerNames(LC_rec)<-c("forest: LC1>50%","grass: LC3>10%")
plot(LC_rec,col=c("black","red"))
dev.off()
#legend("topleft",legend=c("fo","LST","LST_forest","LST_grass"), cex=0.8, col=c("black","blue","green","red"),
              #lty=1)
                  
############ PART 4: RESIDUALS ANALYSIS: ranking, plots, focus regions  ##################
############## EXAMINING STATION RESIDUALS ###########
########### CONSTANT OVER 365 AND SAMPLING OVER 365
#Plot daily_deltaclim_rast, bias_rast,add data_s and data_v
                  
# RANK STATION by average or median RMSE
# Count the number of times a station is in the extremum group of outliers...
# LOOK at specific date...
                  
#Examine residuals for a spciefic date...Jan, 1 using run of const_all i.e. same training over 365 dates
path_data_cai<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_CAI"  #Change to constant
path_data_fus<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_GAM"
setwd(path_data) #output data

#list files that contain raster tmax prediction (maps) for CAI and Fusion
#lf_c<-"GAM_predicted_mod7_20101231_30_1_365d_GAM_fusion_const_10172012.rst"
lf_cg<-list.files(pattern="GAM_predicted_mod2_.*_30_1_365d_GAM_fusion_const_10172012.rst$")    #changee to constant 
lf_ck<-list.files(pattern="fusion_tmax_predicted_.*_30_1_365d_GAM_fusion_const_10172012.rst$") #Fusion+Kriging
                  
#list files that contain model objects and ratingin-testing information for CAI and Fusion
fus1_c<-load_obj("results2_fusion_Assessment_measure_all_365d_GAM_fusion_const_10172012.RData")
#fus1_c<-load_obj("results2_fusion_Assessment_measure_all_365d_GAM_fusion_const_10172012.RData")
                  
gam_fus<-load_obj(file.path(path_data_fus,
                          "results2_fusion_Assessment_measure_all_365d_GAM_fusion_all_lstd_10242012.RData"))
gam_cai<-load_obj(file.path(path_dat_cai,
                          "results2_CAI_Assessment_measure_all_365d_GAM_CAI2_all_lstd_10262012.RData")  #This contains all the info
sampling_obj_cai<-load_obj(file.path(path_data_cai,
                                          "results2_CAI_sampling_obj_365d_GAM_CAI2_all_lstd_10262012.RData"))
sampling_date_list<-sampling_obj_cai$sampling_dat$date
                                    
date_selected<-"20100103"
                  
k<-match(date_selected,sampling_date_list)
                  
data_sf<-gam_fus[[k]]$data_s #object for the first date...20100103                  
data_vf<-gam_fus[[k]]$data_v #object for the first date...20100103                  
data_sc<-gam_fus[[k]]$data_s #object for the first date...20100103                  
data_vc<-gam_fus[[k]]$data_v #object for the first date...20100103                  

#Select background image for plotting
mod_pat<-glob2rx(paste("*",date_selected,"*",sep=""))   
image_file<-grep(mod_pat,lf_ck,value=TRUE) # using grep with "value" extracts the matching names         
raster_pred_k<-raster(image_file)
image_file<-grep(mod_pat,lf_cg,value=TRUE) # using grep with "value" extracts the matching names         
raster_pred_g<-raster(image_file)
plot(data_sf,add=TRUE)
plot(data_vf,pch=1,add=TRUE)
                  
#Create a residual table...
res_mod9_list<-vector("list",365)
res_mod2_list<-vector("list",365)
                  
tab_nv<-matrix(NA,365,1)
for(k in 1:365){
      tab_nv[k]<-length(fus1_c[[k]]$data_v$res_mod9)
}
#note that there might be some variation in the number!!!
                  
for(k in 1:365){
   res_mod9<-fus1_c[[k]]$data_v$res_mod9
   res_mod2<-fus1_c[[k]]$data_v$res_mod2
   res_mod9_list[[k]]<-res_mod9
   res_mod2_list[[k]]<-res_mod2
   #subset data frame? or rbind them...and reshape?? think about it
}
tab_resmod9<-do.call(rbind,res_mod9_list)
                  
data_v_list<-vector("list",365)
                  
for(k in 1:365){
      data_v<-as.data.frame(fus1_c[[k]]$data_v)
      data_v<-data_v[,c("id","res_mod9")]
      #subset data frame? or rbind them...and reshape?? think about it
      data_v_list[[k]]<-data_v
}
tab_resmod9<-do.call(rbind,data_v_list)
                  
#NOW USE RESHAPE TO CREATE TABLE....

#updated the analysis
                  
"tmax_predicted*20100103_30_1_365d_GAM_fusion_all_lstd_10242012.rst"
oldpath<-getwd()
path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_GAM"
setwd(path)
date_selected<-"20100103"
#lf_fus<-list.files(pattern=paste("GAM_bias_tmax_predicted_mod8_",date_selected,"_30_1_365d_GAM_fusion_all_lstd_10242012.rst$",sep="")) #Search for files in relation to fusion
file_pat<-glob2rx(paste("*tmax_pred*",date_selected,"*_365d_GAM_fusion_all_lstd_10242012.rst",sep="")) #Search for files in relation to fusion
lf_fus1a<-list.files(pattern=file_pat) #Search for files in relation to fusion
                                    
rast_fus1a<-stack(lf_fus1a)
rast_fus1a<-mask(rast_fus1a,mask_ELEV_SRTM)

path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_07192012_GAM"
setwd(path)
date_selected<-"20100103"
#lf_fus<-list.files(pattern=paste("GAM_bias_tmax_predicted_mod8_",date_selected,"_30_1_365d_GAM_fusion_all_lstd_10242012.rst$",sep="")) #Search for files in relation to fusion
#lf_fus1<-list.files(pattern=paste("*.tmax_predicted.*.",date_selected,".*._365d_GAM_fusion_lstd_10062012.rst$",sep=""))                  
file_pat<-glob2rx(paste("*tmax_predicted*",date_selected,"*_365d_GAM_fusion_lstd_10062012.rst",sep="")) #Search for files in relation to fusion
lf_fus1s<-list.files(pattern=file_pat) #Search for files in relation to fusion                  
 
rast_fus1s<-stack(lf_fus1s)
rast_fus1s<-mask(rast_fus1s,mask_ELEV_SRTM)
                  
s_range<-c(minValue(rast_fusa1),maxValue(rast_fusa1)) #stack min and max
s_range<-c(min(s_range),max(s_range))
col_breaks <- pretty(s_range, n=50)
lab_breaks <- pretty(s_range, n=5)
temp_colors <- colorRampPalette(c('blue', 'white', 'red'))
X11(width=18,height=12)
par(mfrow=c(3,3))
for (k in 1:length(lf_fus1a)){
    fus1a_r<-raster(rast_fus1a,k)
    plot(fus1a_r, breaks=col_breaks, col=temp_colors(length(col_breaks)-1),   
                axis=list(at=lab_breaks, labels=lab_breaks))
}
                  
#Wihtout setting range
s_range<-c(-12,18)
#col_breaks <- pretty(s_range, n=50)
#lab_breaks <- pretty(s_range, n=5)
col_breaks<-49
temp_colors <- colorRampPalette(c('blue', 'white', 'red'))
lab_breaks <- pretty(s_range, n=5)  
X11(width=18,height=12)
par(mfrow=c(3,3))
mod_name<-paste("mod",1:8, sep="")
rname<-c("FUS_kr",mod_name)
for (k in 1:length(lf_fus1a)){
    fus1a_r<-raster(rast_fus1a,k)
    plot(fus1a_r, breaks=col_breaks, col=temp_colors(col_breaks-1),   
                axis=list(at=lab_breaks, labels=lab_breaks),main=rname[k])
}
                  
#Wihtout setting range
s_range<-c(-12,23)
#col_breaks <- pretty(s_range, n=50)
#lab_breaks <- pretty(s_range, n=5)
col_breaks<-49
temp_colors <- colorRampPalette(c('blue', 'white', 'red'))
lab_breaks <- pretty(s_range, n=5)  
X11(width=18,height=12)
par(mfrow=c(3,3))
mod_name<-paste("mod",1:8, sep="")
rname<-c("FUS_kr",mod_name)
for (k in 1:length(lf_fus1s)){
    fus1s_r<-raster(rast_fus1s,k)
    plot(fus1s_r, breaks=col_breaks, col=temp_colors(col_breaks-1),   
                axis=list(at=lab_breaks, labels=lab_breaks),main=rname[k])
}
                  
                  
                  
                  
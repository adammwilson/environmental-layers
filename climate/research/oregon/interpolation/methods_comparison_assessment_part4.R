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

out_prefix<-"methods_11092012_"

##### LOAD USEFUL DATA

#obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison_10242012" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
#path<-"/Users/benoitparmentier/Dropbox/Data/NCEAS/Oregon_covariates/"            #Local dropbox folder on Benoit's laptop
setwd(path) 
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
#User defined output prefix

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

date_selected<-"20100103"

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

rast_fus_pred<-raster(rast_fus1c,1)  # Select the first model from the stack i.e fusion with kriging for both steps
rast_cai_pred<-raster(rast_cai2c,1)                  

#list files that contain model objects and ratingin-testing information for CAI and Fusion
fus1_c<-load_obj("results2_fusion_Assessment_measure_all_365d_GAM_fusion_const_10172012.RData")

gam_fus<-load_obj(file.path(path_data_fus,
                            "results_mod_obj__365d_GAM_fusion_const_all_lstd_11022012.RData"))
gam_cai<-load_obj(file.path(path_data_cai,
                            "results_mod_obj__365d_GAM_CAI2_const_all_10312012.RData"))  #This contains all the info
sampling_date_list<-gam_fus$sampling_obj$sampling_dat$date

#Create a residual table...
res_mod9_list<-vector("list",365)
res_mod2_list<-vector("list",365)

tab_nv<-matrix(NA,365,1)
for(k in 1:365){
  tab_nv[k]<-length(fus1_c[[k]]$data_v$res_mod9)
}
#note that there might be some variation in the number!!!

for(k in 1:365){
  res_mod9<-gam_fus$gam_fus_mod[[k]]$data_v$res_mod7
  res_mod2<-gam_fus$gam_fus_mod[[k]]$data_v$res_mod2
  res_mod9_list[[k]]<-res_mod9
  res_mod2_list[[k]]<-res_mod2
  #subset data frame? or rbind them...and reshape?? think about it
}
tab_resmod9<-do.call(rbind,res_mod9_list)

data_v_list<-vector("list",365)

for(k in 1:365){
  data_v<-as.data.frame(gam_fus$gam_fus_mod[[k]]$data_v)
  data_v<-data_v[,c("id","date","res_mod7","x_OR83M", "y_OR83M")]
  #subset data frame? or rbind them...and reshape?? think about it
  data_v_list[[k]]<-data_v
}
tab_resmod9<-do.call(rbind,data_v_list)
tab_locs<-melt(tab_resmod9,
               measure=c("x_OR83M","y_OR83M"),
               id=c("id"),
               na.rm=F)
tab_locs_cast<-cast(tab_locs,id~variable,mean)
tab_locs<-as.data.frame(tab_locs_cast)
coords<- tab_locs[,c('x_OR83M','y_OR83M')]
coordinates(tab_locs)<-coords
proj4string(tab_locs)<-proj_str  #Need to assign coordinates...

tab_melt<-melt(tab_resmod9[c("id","date","res_mod7")],
               measure=c("res_mod7"), 
               id=c("id","date"),
               na.rm=F)

tab_cast<-cast(tab_melt,id~date~variable,mean)
tab_resmod9_locs<-as.data.frame(tab_cast[,,1])
sd_v<-sapply(tab_resmod9_locs,sd,na.rm=T)
mean_v<-sapply(tab_resmod9_locs,mean,na.rm=T)
tab_res_rec<-tab_resmod9_locs

for (k in 1:365){
  mean<-mean_v[k]
  sd<-sd_v[k]
  y<-tab_resmod9_locs[,k]
  breaks<-c("-inf",mean-2*sd,mean+2*sd,"+inf")
  rec<-cut(y,breaks,labels=c(-1,0,1))
  rec<-as.numeric(as.character(rec))
  tab_res_rec[,k]<-rec
}


tab_res_rec<-tab_res_rec[,1:365]                  #mae_fun<-function (x){mean(abs(x),na.rm=T)}                                    
for (i in 1:nrow(tab_resmod9_locs)){
  rec<-as.numeric(tab_res_rec[i,])
  tmp<-table(rec)
  tab_res_rec$c1[i]<-as.numeric(tmp[1])
  tab_res_rec$c2[i]<-as.numeric(tmp[2])
  tab_res_rec$c3[i]<-as.numeric(tmp[3])                  
}

tab_res_rec$id<-as.character(rownames(tab_res_rec))
#mae_fun<-function (x){mean(abs(x),na.rm=T)}                                    
for (i in 1:nrow(tab_resmod9_locs)){
  x<-tab_resmod9_locs[i,]
  tab_locs$mae[i]<-mean(abs(x),na.rm=T)
  #tab_locs$mae2[i]<-mae_fun(x)
  rec<-as.numeric(tab_res_rec[i,])
  tmp<-table(rec)
  tab_res_rec$c1<-as.numeric(tmp[1])
  tab_res_rec$c2<-as.numeric(tmp[2])
  tab_res_rec$c3<-as.numeric(tmp[3])
  
}
tab_resmod9_locs$id<-rownames(tab_resmod9_locs)
tab_resmod9_locs[is.na(tab_resmod9_locs)]<-NA  #replace NaN by NA
tab_locs$id<-as.character(tab_locs$id)
data_v_res<-merge(tab_locs,tab_resmod9_locs,by="id")
data_v_res<-merge(tab_res_rec[,c("id","c1","c2","c3")],data_v_res,by="id")
coords<- data_v_res[,c('x_OR83M','y_OR83M')]
coordinates(data_v_res)<-coords
proj4string(data_v_res)<-proj_str  #Need to assign coordinates...
tmp<-subset(data_v_res,select=date_selected)

data_v_res$idx<-1:length(data_v_res)
#Plotting results...

data_v_plot<-subset(data_v_res,!is.na(mae),"mae")      
bubble(data_v_plot,"mae",add=TRUE)
spplot(data_v_res,"mae")

data_v_plot<-subset(data_v_res,!is.na(c1),"c1")
bubble(data_v_res,"c3")
plot(data_v_res[5,])
plot(reg_outline,add=TRUE)
#Figure on 11/07
s_range<-c(minValue(rast_fus_pred),maxValue(rast_fus_pred)) #stack min and max
s_range<-c(min(s_range),max(s_range))
col_breaks <- pretty(s_range, n=50)
lab_breaks <- pretty(s_range, n=5)
temp_colors <- colorRampPalette(c('blue', 'white', 'red'))
plot(rast_fus_pred, breaks=col_breaks, col=temp_colors(49),   
     axis=list(at=lab_breaks, labels=lab_breaks))              
plot(reg_outline,add=TRUE)

plot(data_v_res, pch=1,cex=sqrt(data_v_res$c1)/5,add=TRUE)

plot(data_v_plot, pch=1,cex=sqrt(data_v_plot$mae),add=TRUE)
# Calculate RMSE and MAE for each station over 365 dates and plot it on a map as well as on a scatterplot
#Look at the number of observation.


#1) Digitize features from high resolution imagery and see if you can see differences in the way it is detected in CAI and fusion method...
#in particular how much variation there is in such polygons...
#Polygon to digitize: valley, crop area, mountain...Show that CAI does not capture crop as well as Fusion
#2) Select transect with slope changes and examining variation in temperatures...: calculate average MAE on the transect for examle river
#3) Land cover: examine MAE per land cover...for CAI and Fusion, LST bias and ecoregions...
#4) Look at differences...regions
#5) Summarize by season/month
#6) PCA on differences and CAI-Fusion
#7) PCA on residuals...
#8) Plot residuals at station by buble plot and kriging...
#9) PCA MAE and other variables ...                  

### RESIDUALS FOR SPECIFIC DATE

date_selected<-"20100103"

k<-match(date_selected,sampling_date_list)
names(gam_fus$gam_fus_mod[[k]])               #Show the name structure of the object/list

data_sf<-gam_fus$gam_fus_mod[[k]]$data_s #object for the first date...20100103                  
data_vf<-gam_fus$gam_fus_mod[[k]]$data_v #object for the first date...20100103                  
data_sc<-gam_cai$gam_CAI_mod[[k]]$data_s #object for the first date...20100103                  
data_vc<-gam_cai$gam_CAI_mod[[k]]$data_v #object for the first date...20100103                  

id_selected<-intersect(data_v_res$id,data_vf$id)
pos<-match(id_selected,data_v_res$id)
tmp<-as.data.frame(data_v_res[pos,c("id","idx","mae","c1","c2","c3")])
tmp<-tmp[,c("id","idx","mae","c1","c2","c3")]
data_vf<-merge(data_vf,tmp,by="id")

coords<- data_vf[,c('x_OR83M','y_OR83M')]
coordinates(data_vf)<-coords
proj4string(data_vf)<-proj_str  #Need to assign coordinates...

#Select background image for plotting and Plot validation residuals for fusion on 20100103
s.range <- c(min(minValue(rast_fus_pred)), max(maxValue(rast_fus_pred)))
col.breaks <- pretty(s.range, n=50)
lab.breaks <- pretty(s.range, n=5)
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))

#Training plot
plot(rast_fus_pred, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
     axis=list(at=lab.breaks, labels=lab.breaks))
plot(data_sf,cex=1,main="Training stations", add=TRUE)
plot(reg_outline,add=TRUE)

#savePlot...

#Testing plot
plot(rast_fus_pred, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
     axis=list(at=lab.breaks, labels=lab.breaks))
plot(data_vf,cex=1,pch=1,add=TRUE)
plot(reg_outline,add=TRUE)
text(data_vf,labels=data_vf$idx,pos=3)
#savePlot...

#Plot residuals
res_modvf<-res_mod7

plot(data_vf$res_mod7)
text(data_vf$res_mod7,labels=data_vf$idx,pos=3)

#Plot residuals
res_modvf<-res_mod7

plot(data_vf$res_mod7)
text(data_vf$res_mod7,labels=data_vf$idx,pos=3)





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




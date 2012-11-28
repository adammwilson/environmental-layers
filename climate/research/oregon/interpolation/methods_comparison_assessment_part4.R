#####################################  METHODS COMPARISON part 4 ##########################################
#################################### Spatial Analysis ############################################
#This script utilizes the R ojbects created during the interpolation phase.                       #
#At this stage the script produces figures of various accuracy metrics and compare methods:       #
#- rank station in terms of residuals and metrics (MAE and RMSE)                                  #
#- calculate accuary metrics for climtology predictions...                                        #
#- spatial density of station network and accuracy metric                                         #
#- visualization of maps of prediction and difference for comparison                              #
#AUTHOR: Benoit Parmentier                                                                        #
#DATE: 11/23/2012                                                                                 #
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

gam_fus<-load_obj(file.path(path_data_fus,
                            "results_mod_obj__365d_GAM_fusion_const_all_lstd_11022012.RData"))
gam_cai<-load_obj(file.path(path_data_cai,
                            "results_mod_obj__365d_GAM_CAI2_const_all_10312012.RData"))  #This contains all the info
sampling_date_list<-gam_fus$sampling_obj$sampling_dat$date

#Create a residual table...
res_mod9f_list<-vector("list",365)
res_mod9c_list<-vector("list",365)
data_vf_list<-vector("list",365)
data_vc_list<-vector("list",365)

for(k in 1:365){
  data_vf<-as.data.frame(c)
  data_vc<-as.data.frame(gam_cai$gam_CAI_mod[[k]]$data_v)
  data_vf<-data_vf[,c("id","date","res_mod7","x_OR83M", "y_OR83M")]
  data_vc<-data_vc[,c("id","date","res_mod9","x_OR83M", "y_OR83M")]
  #subset data frame? or rbind them...and reshape?? think about it
  data_vf_list[[k]]<-data_vf
  data_vc_list[[k]]<-data_vc
}
tab_resmod9f<-do.call(rbind,data_vf_list)
tab_resmod9c<-do.call(rbind,data_vc_list)

#Get the unique location information...
tab_locsc<-melt(tab_resmod9c,
               measure=c("x_OR83M","y_OR83M"),
               id=c("id"),
               na.rm=F)
tab_locsc_cast<-cast(tab_locsc,id~variable,mean)
tab_locsc<-as.data.frame(tab_locsc_cast)
coords<- tab_locsc[,c('x_OR83M','y_OR83M')]
coordinates(tab_locsc)<-coords
proj4string(tab_locsc)<-proj_str  #Need to assign coordinates...

tab_locsf<-melt(tab_resmod9f,
                measure=c("x_OR83M","y_OR83M"),
                id=c("id"),
                na.rm=F)
tab_locsf_cast<-cast(tab_locsf,id~variable,mean)
tab_locsf<-as.data.frame(tab_locsf_cast)
coords<- tab_locsf[,c('x_OR83M','y_OR83M')]
coordinates(tab_locsf)<-coords
proj4string(tab_locsf)<-proj_str  #Need to assign coordinates..

all.equal(tab_locsc,tab_locsf) #Checking that stations used over the year are equal!!!
#since all equal we can use one object, it will be sufficient...
tab_locs<-tab_locsf

####### Now join and summarize objects together...

tabf_melt<-melt(tab_resmod9f[c("id","date","res_mod7")],
               measure=c("res_mod7"), 
               id=c("id","date"),
               na.rm=F)
tabc_melt<-melt(tab_resmod9c[c("id","date","res_mod9")],
               measure=c("res_mod9"), 
               id=c("id","date"),
               na.rm=F)
tabf_cast<-cast(tabf_melt,id~date~variable,mean)
tabf_resmod9_locs<-as.data.frame(tabf_cast[,,1])
tabc_cast<-cast(tabc_melt,id~date~variable,mean)
tabc_resmod9_locs<-as.data.frame(tabc_cast[,,1])


#Now reclass the data into outliers: Class with -1 (negative exteme),class 0 (not extreme), class 1 (positive extre)
calculate_extremes_stat <-function (tab_resmod9_locs,tab_locs){
  
  #tab_resmod9_locs<-tabf_resmod9_locs #comment out when running function
  #tab_locs<-tab_locsf
  #tab_res_rec<-tabf_resmod9_locs
  
  #start here
  sd_v<-sapply(tab_resmod9_locs,sd,na.rm=T) #This the sandard deviation of residuals for every day for the fusion prediction
  mean_v<-sapply(tab_resmod9_locs,mean,na.rm=T)  #This the mean of residuals for every day for the fusion prediction
  
  for (k in 1:365){
    mean_val<-mean_v[k]
    sd_val<-sd_v[k]
    y<-tab_resmod9_locs[,k]
    breaks<-c("-inf",mean_val-2*sd_val,mean_val+2*sd_val,"+inf")
    rec<-cut(y,breaks,labels=c(-1,0,1))
    rec<-as.numeric(as.character(rec))
    tab_res_rec[,k]<-rec
  }
  
  tab_res_rec<-tab_res_rec[,1:365]                  #mae_fun<-function (x){mean(abs(x),na.rm=T)}                                    
  
  tab_res_rec$id<-as.character(rownames(tab_res_rec))
  #mae_fun<-function (x){mean(abs(x),na.rm=T)}                                    
  for (i in 1:nrow(tab_resmod9_locs)){
    x<-tab_resmod9_locs[i,]
    tab_locs$mae[i]<-mean(abs(x),na.rm=T)
    #tab_locs$mae2[i]<-mae_fun(x)
    rec<-as.numeric(tab_res_rec[i,])
    tmp<-table(rec)
    tab_res_rec$c1[i]<-as.numeric(tmp[1])
    tab_res_rec$c2[i]<-as.numeric(tmp[2])
    tab_res_rec$c3[i]<-as.numeric(tmp[3])  
  }
  
  projstr_info<-proj4string(tab_locs)
  tab_resmod9_locs$id<-rownames(tab_resmod9_locs)
  tab_resmod9_locs[is.na(tab_resmod9_locs)]<-NA  #replace NaN by NA
  tab_locs$id<-as.character(tab_locs$id)
  data_v_res<-merge(tab_locs,tab_resmod9_locs,by="id")
  data_v_res<-merge(tab_res_rec[,c("id","c1","c2","c3")],data_v_res,by="id")

  coords<- data_v_res[,c('x_OR83M','y_OR83M')]
  coordinates(data_v_res)<-coords
  proj4string(data_v_res)<-projstr_info  #Need to assign coordinates...
  
  #tmp<-subset(data_v_res,select=date_selected)
  data_v_res$idx<-1:length(data_v_res)
  #Plotting results...
  return (data_v_res)
}

### usef the function...
data_v_resf<-calculate_extremes_stat(tabf_resmod9_locs,tab_locs) # call function
data_v_resc<-calculate_extremes_stat(tabc_resmod9_locs,tab_locs) # call functio

#Some plotting of results...
data_v_plot<-subset(data_v_resf,!is.na(data_v_resf$mae),"mae")      
bubble(data_v_plot,"mae",add=TRUE)
spplot(data_v_resf,"mae")

data_v_plot<-subset(data_v_res,!is.na(c1),"c1")
bubble(data_v_res,"c1")
plot(data_v_res[5,])
plot(reg_outline,add=TRUE)
#Figure on 11/07
s.range <- c(min(minValue(rast_fus_pred)), max(maxValue(rast_fus_pred)))
col.breaks <- pretty(s.range, n=50)
lab.breaks <- pretty(s.range, n=5)
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))

#Training plot
plot(rast_fus_pred, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
     axis=list(at=lab.breaks, labels=lab.breaks))
plot(reg_outline,add=TRUE)

plot(data_v_res, pch=1,cex=sqrt(data_v_res$c1)/5,add=TRUE)

plot(data_v_plot, pch=1,cex=sqrt(data_v_plot$mae),add=TRUE)
# Calculate RMSE and MAE for each station over 365 dates and plot it on a map as well as on a scatterplot
#Look at the number of observation.

#bubble(data_v_res,"X20101230",na.rm=T,do.sqrt=TRUE)

#1) Digitize features from high resolution imagery and see if you can see differences in the way it is detected in CAI and fusion method...
#in particular how much variation there is in such polygons...
#Polygon to digitize: valley, crop area, mountain...Show that CAI does not capture crop as well as Fusion
#2) Transect with slope changes and examining variation in temperatures...: calculate average MAE on the transect for examle river
#3) X Land cover: examine MAE per land cover...for CAI and Fusion, LST bias and ecoregions...: boxplots at 
#4) Look at differences...regions with box plot of MAE inside and outside regions of differences...
#5) X Summarize by season/month
#6) PCA on differences and CAI-Fusion
#7) X PCA on residuals...
#8) X Plot residuals at station by buble plot and kriging...
#9) PCA MAE and other variables ...                  

### PLOTS OF RESIDUALS AND COVARIATES FOR SPECIFIC DATE

#This should be in a loop...
setwd(path)
date_selected<-"20100103"
dates<-c("20100103","20100901")

for (i in 1:length(dates)){
  date_selected<-dates[i]
  k<-match(date_selected,sampling_date_list)
  names(gam_fus$gam_fus_mod[[k]])               #Show the name structure of the object/list
  
  #Extract the training and testing information for the given date...
  data_sf<-gam_fus$gam_fus_mod[[k]]$data_s #object for the first date...20100103                  
  data_vf<-gam_fus$gam_fus_mod[[k]]$data_v #object for the first date...20100103                  
  data_sc<-gam_cai$gam_CAI_mod[[k]]$data_s #object for the first date...20100103                  
  data_vc<-gam_cai$gam_CAI_mod[[k]]$data_v #object for the first date...20100103                  
  
  #Join information extracted using the function previously
  id_selected<-intersect(data_v_resf$id,data_vf$id) #Match station using their official IDSs
  pos<-match(id_selected,data_v_resf$id)
  tmp<-as.data.frame(data_v_resf[pos,c("id","idx","mae","c1","c2","c3")])
  tmp<-tmp[,c("id","idx","mae","c1","c2","c3")]
  data_vf<-merge(data_vf,tmp,by="id")
  id_selected<-intersect(data_v_resc$id,data_vc$id) #Match station using their official IDSs
  pos<-match(id_selected,data_v_resc$id)
  tmp<-as.data.frame(data_v_resc[pos,c("id","idx","mae","c1","c2","c3")])
  tmp<-tmp[,c("id","idx","mae","c1","c2","c3")]
  data_vc<-merge(data_vc,tmp,by="id")
  #Turn the data frame back into a spdf
  coords<- data_vf[,c('x_OR83M','y_OR83M')]
  coordinates(data_vf)<-coords
  proj4string(data_vf)<-proj_str  #Need to assign coordinates...
  coords<- data_vc[,c('x_OR83M','y_OR83M')]
  coordinates(data_vc)<-coords
  proj4string(data_vc)<-proj_str  #Need to assign coordinates...
  
  #Prepare for plotting
  X11(width=16,height=9)
  par(mfrow=c(1,2))
  
  #Select background image for plotting and Plot validation residuals for fusion on 20100103
  s.range <- c(min(minValue(rast_fus_pred)), max(maxValue(rast_fus_pred)))
  col.breaks <- pretty(s.range, n=50)
  lab.breaks <- pretty(s.range, n=5)
  temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
  
  #Training plot
  plot(rast_fus_pred, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
       axis=list(at=lab.breaks, labels=lab.breaks))
  plot(data_sf,cex=1, add=TRUE)
  plot(reg_outline,add=TRUE)
  title(paste("Training stations",date_selected,sep=" "))
  #Testing plot
  plot(rast_fus_pred, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
       axis=list(at=lab.breaks, labels=lab.breaks))
  plot(data_vf,cex=1,pch=1,add=TRUE)
  plot(reg_outline,add=TRUE)
  text(data_vf,labels=data_vf$idx,pos=3,cex=1.3)
  title(paste("Testing stations",date_selected,sep=" "))
  
  #savePlot here...
  savePlot(paste("fig1_training_testing_",date_selected,out_prefix,".png", sep=""), type="png")
  
  #plot(rast_fus_pred, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
  #     axis=list(at=lab.breakts, labels=lab.breaks))
  #plot(data_vf,cex=sqrt(data_vf$res_mod7),pch=1,add=TRUE)
  #plot(reg_outline,add=TRUE)
  #text(data_vf,labels=data_vf$idx,pos=3)
  
  #X11(width=16,height=9)
  #par(mfrow=c(1,2))
  
  # CREATE A FUNCTION TO AUTOMATE THE PLOT: improve code here
  #Plot residuals
  y_range<-range(c(data_vf$res_mod7,data_vc$res_mod9))
  plot(data_vf$idx,data_vf$res_mod7, ylab="Residuals", xlab="Index", ylim=y_range)
  text(data_vf$idx,data_vf$res_mod7,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals fusion",date_selected,sep=" "))
  plot(data_vc$idx,data_vc$res_mod9,ylab="Residuals", xlab="Index", ylim=y_range)
  text(data_vc$idx,data_vc$res_mod9,data_vc$idx,labels=data_vc$idx,pos=3)
  grid(lwd=0.5, col="black")
  title(paste("Testing stations residuals CAI",date_selected,sep=" "))
  #savePlot here...
  savePlot(paste("fig2_testing_residuals_fusion_CAI_",date_selected,out_prefix,".png", sep=""), type="png")
  
  #Plot predicted vs observed
  y_range<-range(c(data_vf$pred_mod7,data_vc$pred_mod9))
  x_range<-range(c(data_vf$dailyTmax,data_vc$dailyTamx))
  plot(data_vf$pred_mod7,data_vf$dailyTmax, ylab="Observed daily tmax (C)", xlab="Predicted daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  text(data_vf$pred_mod7,data_vf$dailyTmax,labels=data_vf$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5,col="black")
  title(paste("Testing stations tmax fusion vs daily tmax",date_selected,sep=" "))
  plot(data_vc$pred_mod9,data_vc$dailyTmax,ylab="Observed daily tmax (C)", xlab="Predicted daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  text(data_vc$pred_mod9,data_vc$dailyTmax,labels=data_vc$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5, col="black")
  title(paste("Testing stations tmax CAI vs daily tmax",date_selected,sep=" "))
  #savePlot here...
  savePlot(paste("fig3_testing_predicted_observed_fusion_CAI_",date_selected,out_prefix,".png", sep=""), type="png")
 
  ###########Plot residuals and covariates
  ##Elevation and residulas
  y_range<-range(c(data_vf$res_mod7,data_vc$res_mod9))
  x_range<-range(c(data_vf$ELEV_SRTM,data_vc$ELEV_SRTM))
  plot(data_vf$ELEV_SRTM,data_vf$res_mod7, ylab="Residuals", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  text(data_vf$ELEV_SRTM,data_vf$res_mod7,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals fusion vs Elevation",date_selected,sep=" "))
  plot(data_vc$ELEV_SRTM,data_vc$res_mod9, ylab="Residuals", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  text(data_vc$ELEV_SRTM,data_vc$res_mod9,labels=data_vc$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals CAI vs Elevation",date_selected,sep=" "))
  savePlot(paste("fig4_testing_residuals_fusion_CAI_Elev_",date_selected,out_prefix,".png", sep=""), type="png")
  
  ##Forest (LC1) and residuals
  y_range<-range(c(data_vf$res_mod7,data_vc$res_mod9))
  x_range<-range(c(data_vf$LC1,data_vc$LC1))
  plot(data_vf$LC1,data_vf$res_mod7,ylab="Residuals", xlab="LC1 (%)", 
       ylim=y_range, xlim=x_range)
  text(data_vf$LC1,data_vf$res_mod7,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals CAI vs LC1 (forest)",date_selected,sep=" "))
  plot(data_vc$LC1,data_vc$res_mod9,ylab="Residuals", xlab="LC1 (%)", 
       ylim=y_range, xlim=x_range)
  text(data_vc$LC1,data_vc$res_mod9,labels=data_vc$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals CAI vs LC1(forest)",date_selected,sep=" "))
  savePlot(paste("fig5_testing_residuals_fusion_CAI_LC1_",date_selected,out_prefix,".png", sep=""), type="png")
  
  ##Grass (LC3)  and residuals
  y_range<-range(c(data_vf$res_mod7,data_vc$res_mod9))
  x_range<-range(c(data_vf$LC3,data_vc$LC3))
  plot(data_vf$LC3,data_vf$res_mod7,ylab="Residuals", xlab="LC3 (%)", 
       ylim=y_range, xlim=x_range)
  text(data_vf$LC3,data_vf$res_mod7,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals CAI vs LC3 (grass)",date_selected,sep=" "))
  plot(data_vc$LC3,data_vc$res_mod9,ylab="Residuals", xlab="LC3 (%)", 
       ylim=y_range, xlim=x_range)
  text(data_vc$LC3,data_vc$res_mod9,labels=data_vc$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals CAI vs LC3(grass)",date_selected,sep=" "))
  savePlot(paste("fig6_testing_residuals_fusion_CAI_LC3_",date_selected,out_prefix,".png", sep=""), type="png")
  
  #LSTD_bias and residuals
  y_range<-range(c(data_vf$res_mod7,data_vc$res_mod9))
  x_range<-range(c(data_vf$LSTD_bias,data_vc$LSTD_bias))
  plot(data_vf$LSTD_bias,data_vf$res_mod7)
  text(data_vf$LSTD_bias,data_vf$res_mod7,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations LST bias vs residuals",date_selected,sep=" "))
  plot(data_sf$LSTD_bias,data_sf$res_mod7)
  #text(data_sf$LSTD_bias,data_sf$res_mod7,labels=data_vf$idx,pos=3) # Also labels for training?
  grid(lwd=0.5,col="black")
  title(paste("Training stations LST bias vs residuals",date_selected,sep=" "))
  savePlot(paste("fig7_testing_training_residuals_fusion_LSTD_bias_",date_selected,out_prefix,".png", sep=""), type="png")
   
  #LSTD vs TMax and residuals
  y_range<-range(c(data_vf$TMax,data_vc$TMax))
  x_range<-range(c(data_vf$LSTD_bias,data_vc$LSTD_bias))
  plot(data_vf$LSTD_bias,data_vf$res_mod7,xlab="LST bias", ylab="Residuals")
  text(data_vf$LSTD_bias,data_vf$res_mod7,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations LST bias vs fusion residuals",date_selected,sep=" "))
  plot((data_vf$LST-273.16),data_vf$TMax,xlab="LST", ylab="TMax (monthly tmax in C)")
  text((data_vf$LST-273.16),data_vf$TMax,labels=data_vf$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5,col="black")
  title(paste("Testing stations LST vs TMax ",date_selected,sep=" "))
  savePlot(paste("fig8_testing_TMax_fusion_",date_selected,out_prefix,".png", sep=""), type="png")
  
  ##Elevation and residuals of temperature prediction
  diff_fc<-data_vf$pred_mod7-data_vc$pred_mod9
  
 
  y_range<-range(c(diff_fc))
  x_range<-range(c(data_vf$ELEV_SRTM,data_vc$ELEV_SRTM))
  plot(data_vf$ELEV_SRTM,diff_fc, ylab="diff_fc", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  text(data_vf$ELEV_SRTM,diff_fc,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("Testing stations residuals fusion vs Elevation",date_selected,sep=" "))
  
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
  
  dev.off()
}
      
##Check...difference at diff stations...


#Extract the training and testing information for the given date...
data_sf<-gam_fus$gam_fus_mod[[k]]$data_s #object for the first date...20100103                  
data_vf<-gam_fus$gam_fus_mod[[k]]$data_v #object for the first date...20100103                  
data_sc<-gam_cai$gam_CAI_mod[[k]]$data_s #object for the first date...20100103                  
data_vc<-gam_cai$gam_CAI_mod[[k]]$data_v #object for the first date...20100103                  

#Join information extracted using the function previously
id_selected<-intersect(data_v_resf$id,data_vf$id) #Match station using their official IDSs
pos<-match(id_selected,data_v_resf$id)
tmp<-as.data.frame(data_v_resf[pos,c("id","idx","mae","c1","c2","c3")])
tmp<-tmp[,c("id","idx","mae","c1","c2","c3")]
data_vf<-merge(data_vf,tmp,by="id")
id_selected<-intersect(data_v_resc$id,data_vc$id) #Match station using their official IDSs
pos<-match(id_selected,data_v_resc$id)
tmp<-as.data.frame(data_v_resc[pos,c("id","idx","mae","c1","c2","c3")])
tmp<-tmp[,c("id","idx","mae","c1","c2","c3")]
data_vc<-merge(data_vc,tmp,by="id")
#Turn the data frame back into a spdf
coords<- data_vf[,c('x_OR83M','y_OR83M')]
coordinates(data_vf)<-coords
proj4string(data_vf)<-proj_str  #Need to assign coordinates...
coords<- data_vc[,c('x_OR83M','y_OR83M')]
coordinates(data_vc)<-coords
proj4string(data_vc)<-proj_str  #Need to assign coordinates...

###################################################
############## RESIDUALS BY TIME!!! ##############

#tab_resmod9
for (i in 1:nrow(tab_resmod9f)){
  date<-strptime(tab_resmod9f$date[i], "%Y%m%d")   # interpolation date being processed, converting the string using specific format
  month<-as.integer(strftime(date, "%m"))
  tab_resmod9f$month[i]<-month
}

mae_function<-function(res){mae<-mean(abs(res),na.rm=T)}
tab_test<-melt(tab_resmod9f,
               measure=c("res_mod7","x_OR83M","y_OR83M"),
               id=c("id","month"),
               na.rm=F)
tab_test_cast<-cast(data=tab_test,formula=id~month~variable,mean)
table_id_month<-tab_test_cast[,,1]
tab_month_cast<-cast(data=tab_test,formula=month~variable,mean)
tab_month_cast<-cast(data=tab_test,formula=month~variable,mae_function)
X11()
barplot(tab_month_cast$res_mod7,
        names.arg=1:12,cex.names=0.8,   #names of the teleconnections indices and size of fonts of axis labes
        xlab="Month",       # font.lab is 2 to make the font bold
        ylab="MAE",font.lab=2)

savePlot(paste("fig9_mae_fusion_per_season_",date_selected,out_prefix,".png", sep=""), type="png")

plot(table_id_month[55,])
plot(table_id_month[5,])

#tab_resmod9c

for (i in 1:nrow(tab_resmod9c)){
  date<-strptime(tab_resmod9c$date[i], "%Y%m%d")   # interpolation date being processed, converting the string using specific format
  month<-as.integer(strftime(date, "%m"))
  tab_resmod9c$month[i]<-month
}

mae_function<-function(res){mae<-mean(abs(res),na.rm=T)}
tab_test<-melt(tab_resmod9c,
               measure=c("res_mod9","x_OR83M","y_OR83M"),
               id=c("id","month"),
               na.rm=F)
tab_test_cast<-cast(data=tab_test,formula=id~month~variable,mean)
table_id_month_cai<-tab_test_cast[,,1]
tab_month_cast_cai<-cast(data=tab_test,formula=month~variable,mean)
tab_month_cast_cai<-cast(data=tab_test,formula=month~variable,mae_function)

barplot(tab_month_cast_cai$res_mod9,
    names.arg=1:12,cex.names=0.8,   #names of the teleconnections indices and size of fonts of axis labes
    xlab="Month",       # font.lab is 2 to make the font bold
    ylab="MAE",font.lab=2)
grid(nx=12,ny=10)    

savePlot(paste("fig10_mae_cai_per_season_",date_selected,out_prefix,".png", sep=""), type="png")

plot(table_id_month_cai[55,])
plot(table_id_month_cai[5,])

###### 
overlay(data_v_resf,diff_r)
plot(avg_LC_rec)
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

### Wihtout setting range
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
                 
############### NOW USE RESHAPE TO CREATE TABLE....##########
# DO A PCA HERE...also look at the difference between prediction at stations...
                 
var_pat<-glob2rx("*2010*") #Search for files in relation to fusion
var<-match("*2010*",names(data_v_resf)) #Search for files in relation to fusion                  
xp<-data_v_resf[,names(data_v_resf)[10:374]]      # subsetting the original training data, note that that this is a "SpatialPointsDataFrame"
                 
#xp<-data_s[,var]      # subsetting the original training data, note that that this is a "SpatialPointsDataFrame"
xp<-as.data.frame(xp)
dropsc<-c("x_OR83M","y_OR83M")  #columns to be removed
xp<-xp[,!(names(xp) %in% dropsc)]  # dropping columns
                 
X<-as.matrix(na.omit(xp))
                 
PCA9var<-prcomp(~.,data=xp, retx = TRUE, center= TRUE, scale = TRUE, na.action=na.omit)
E<-matrix()
E<-PCA9var$rotation    #E are the eigenvectors of the standardized PCA base on xp data.frame
Xs<-scale(X)           #Use scale to standardize the original data matrix (center on mean and divide by standard deviation)
Xpc<- Xs %*% E          #Rotate the original standardized matrix Xs by the eigenvectors from the standardized PCA
plot(PCA9var)
#plot(PCA9var$scores)
loadings<-cor(X, Xpc)   #Correlation between the original variables and the principal components...
loadings<-as.data.frame(loadings)
#quartz(width=6, height=6)
plot(PC2~PC1, xlab= "PC1", ylab= "PC2", xlim=c(-1, 1), ylim=c(-1,1), pch =16, width=1, 
              main= paste("This is PCA for ",date_selected, sep=""), height=1,asp=1,data=loadings) #Add aspect options to get 
axis(1,pos=0)
axis(2,pos=0)
text(loadings$PC1,loadings$PC2,rownames(loadings), adj = c(0,0),offset=2, col="red",cex=1.2)
draw.circle(0,0,radius=1)
                 
#DO PCA ON SELECTED DATE...


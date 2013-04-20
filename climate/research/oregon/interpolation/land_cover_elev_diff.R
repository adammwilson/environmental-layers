#####################################  METHODS COMPARISON part 5 ##########################################
#################################### Spatial Analysis ############################################
#This script utilizes the R ojbects created during the interpolation phase.                       #
#This scripts focuses on a detailed study of differences in the predictions of CAI_kr and FUsion_Kr  
#Differences are examined through:
#1) per land cover classes
#2) per elevation classes
#3) through spiatial transects
#
#Note this code is for exploratory analyses so some sections are not succinct and
#can be improve for repeatability and clarity.
#AUTHOR: Benoit Parmentier                                                                        #
#DATE: 12/04/2012                                                                                 #
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
######### Functions used in the script
#loading R objects that might have similar names
load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

plot_transect<-function(list_trans,r_stack,title_plot,disp=TRUE){
  #This function creates plot of transects for stack of raster images.
  #The parameters are:
  #list_trans: list of files containing the transects lines in shapefile format
  #r_stack: raster stack of files
  #title_plot: plot title
  #disp: dispaly and save from X11 if TRUE
  nb<-length(list_trans)
  t_col<-rainbow(nb)
  list_trans_data<-vector("list",nb)
  for (i in 1:nb){
    trans_file<-list_trans[[i]][1]
    filename<-sub(".shp","",trans_file)             #Removing the extension from file.
    transect<-readOGR(".", filename)                 #reading shapefile 
    trans_data<-extract(r_stack, transect)
    if (disp==FALSE){
      png(file=paste(list_trans[[i]]),".png",sep="")
    }
    for (k in 1:ncol(trans_data[[1]])){
      y<-trans_data[[1]][,k]
      x<-1:length(y)
      if (k!=1){
        lines(x,y,col=t_col[k])
      }
      if (k==1){
        plot(x,y,type="l",xlab="Position index", ylab="temperature",col=rainbow(k)) 
      }
    }
    title(title_plot[i])
    legend("topright",legend=layerNames(r_stack), 
           cex=1.2, col=t_col,
           lty=1)
    
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

plot_transect_m<-function(list_trans,r_stack,title_plot,disp=TRUE,m_layers){
  #This function creates plot of transects for stack of raster images.
  #Arguments:
  #list_trans: list of files containing the transects lines in shapefile format
  #r_stack: raster stack containing the information to extect
  #title_plot: plot title
  #disp: display and save from X11 if TRUE or plot to png file if FALSE
  #m_layers: index for layerers containing alternate units to be drawned on a differnt scale
  #RETURN:
  #list containing transect information
  
  nb<-length(list_trans)
  t_col<-rainbow(nb)
  list_trans_data<-vector("list",nb)
  
  #For scale 1
  for (i in 1:nb){
    trans_file<-list_trans[[i]][1]
    filename<-sub(".shp","",trans_file)             #Removing the extension from file.
    transect<-readOGR(".", filename)                 #reading shapefile 
    trans_data<-extract(r_stack, transect)
    if (disp==FALSE){
      png(file=paste(list_trans[[i]]),".png",sep="")
    }
    #Plot layer values for specific transect
    for (k in 1:ncol(trans_data[[1]])){
      y<-trans_data[[1]][,k]
      x<-1:length(y)
      m<-match(k,m_layers)

      if (k==1 & is.na(m)){
        plot(x,y,type="l",xlab="Position index", ylab="temperature",col=t_col[k])
        axis(2,xlab="",ylab="tmax (in degree C)")
      }
      if (k==1 & !is.na(m)){
        plot(x,y,type="l",col=t_col[k],axes=F) #plotting fusion profile
        axis(4,xlab="",ylab="tmax (in degree C)")  
        
      }
      if (k!=1 & is.na(m)){
        #par(new=TRUE)              # new plot without erasing old
        lines(x,y,type="l",col=t_col[k],axes=F) #plotting fusion profile
        #axis(2,xlab="",ylab="tmax (in degree C)")
      }
      if (k!=1 & !is.na(m)){
        par(new=TRUE)              # key: ask for new plot without erasing old
        plot(x,y,type="l",col=t_col[k],axes=F) #plotting fusion profile
        #axis(4,xlab="",ylab="tmax (in degree C)")  
      }
      
    }
    title(title_plot[i])
    legend("topright",legend=layerNames(r_stack), 
           cex=1.2, col=t_col,
           lty=1)
    
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

transect_from_spdf<-function (spdf,selected_features){
  #This function produces a transect from a set of selected points in a point layer
  # Arguments:
  # spdf: SpatialPointDataFrame
  # selected_features: index of ssubset points used in the transect line
  # Return: SpatialLinesDataframe object corresponding to the transect
  # Author: Benoit Parmentier
  # Date: 11-29-2012
  
  dat_id<-spdf[selected_features,]  #creating new subset from spdf
  spdf_proj<-proj4string(dat_id)
  matrix_point_coords<-coordinates(dat_id)
  #Add possibility of keeping attributes?
  #Transform a sequence of points with coords into Spatial Lines
  #Note that X is the ID, modify for dataframe?
  trans4<-SpatialLines(list(Lines(list(Line(coordinates(matrix_point_coords))),"X")))   
  tmp<-as.data.frame(dat_id[1,])
  row.names(tmp)<-rep("X",1)
  trans4<-SpatialLinesDataFrame(trans4,data=tmp)
  proj4string(trans4)<-spdf_proj
  return(trans4)
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

out_prefix<-"methods_comp5_12142012_"
nb_transect<-4
##### LOAD USEFUL DATA

#obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path_wd<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison_10242012" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
#path<-"/Users/benoitparmentier/Dropbox/Data/NCEAS/Oregon_covariates/"            #Local dropbox folder on Benoit's laptop
setwd(path_wd) 
path_data_cai<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_CAI"  #Change to constant
path_data_fus<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations_10242012_GAM"
#list files that contain model objects and ratingin-testing information for CAI and Fusion
obj_mod_fus_name<-"results_mod_obj__365d_GAM_fusion_const_all_lstd_11022012.RData"
#obj_mod_cai_name<-"results_mod_obj__365d_GAM_CAI2_const_all_10312012.RData"
obj_mod_cai_name<-"results_mod_obj__365d_GAM_CAI3_const_all_12072012.RData"
gam_cai_mod<-load_obj(file.path(path_data_cai,obj_mod_cai_name))
lf_raster_cai<-"*_365d_GAM_CAI3_const_all_12072012.rst"
lf_raster_fus<-"*_365d_GAM_fusion_const_all_lstd_11022012.rst"
  
gam_fus<-load_obj(file.path(path_data_fus,obj_mod_fus_name))
gam_cai<-load_obj(file.path(path_data_cai,obj_mod_cai_name))  #This contains all the info
sampling_date_list<-gam_fus$sampling_obj$sampling_dat$date

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

################ VISUALIZATION !!!!!!!! ############
#updated the analysis

dates<-c("20100901")
i=2

for(i in 1:length(dates)){
  
  date_selected<-dates[i]
  
  #Selecting model from stack
  mf_selected<-"FSS_kr" #model to be used in the assessment...fusion+kr
  mc_selected<-"CAI_mod6" #model to be used in the assessment...CAI+kr...
  ###Note that the order of the layer is not the same as in the dataframe..this part can be modified later
  
  m_fus<-3 #model image to use in fus: mod7 is 1 in raster stack: 
  m_CAI<-2 #model image to use in CAI: mod+1
  rast_fus_pred<-raster(predictions,m_fus)  # Select the first model from the stack i.e fusion with kriging for both steps
  rast_cai_pred<-raster(predictions,m_CAI)  
  layerNames(rast_cai_pred)<-paste("cai",date_selected,sep="_")
  layerNames(rast_fus_pred)<-paste("fus",date_selected,sep="_")
  
  ## Start difference analysis
  #Calculate difference image for the date selected 
  rast_diff<-rast_fus_pred-rast_cai_pred
  layerNames(rast_diff)<-paste("diff",date_selected,sep="_")
  mean_val<-cellStats(rast_diff,mean)
  sd_val<-cellStats(rast_diff,sd)
  
  #Summarize results using plot
  #LC1 and LC3 and LC4
  avl<-c(0,10,1,10,20,2,20,30,3,30,40,4,40,50,5,50,60,6,60,70,7,70,80,8,80,90,9,90,100,10)#Note that category 1 does not include 0!!
  rclmat<-matrix(avl,ncol=3,byrow=TRUE)
  LC1_rec<-reclass(LC1,rclmat)  #Loss of layer names when using reclass
  LC2_rec<-reclass(LC2,rclmat)  #Loss of layer names when using reclass
  LC3_rec<-reclass(LC3,rclmat)  #Loss of layer names when using reclass
  LC4_rec<-reclass(LC4,rclmat)  #Loss of layer names when using reclass
  LC6_rec<-reclass(LC6,rclmat)  #Loss of layer names when using reclass
  
  #LC_s<-stack(LC1,LC3,LC4,LC6)
  LC_s<-stack(LC1,LC2,LC3,LC4,LC6)
  layerNames(LC_s)<-c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban")
  LC_s<-mask(LC_s,mask_ELEV_SRTM)
  LC_rec_s<-reclass(LC_s,rclmat)
  
  #plot average difference per class of forest and LC2
  rast_stack_zones<-LC_rec_s
  
  avg_LC1_rec<-zonal(rast_diff,zones=LC1_rec,stat="mean",na.rm=TRUE)
  avg_LC2_rec<-zonal(rast_diff,zones=LC2_rec,stat="mean",na.rm=TRUE)
  avg_LC3_rec<-zonal(rast_diff,zones=LC3_rec,stat="mean",na.rm=TRUE)
  avg_LC4_rec<-zonal(rast_diff,zones=LC4_rec,stat="mean",na.rm=TRUE)
  avg_LC6_rec<-zonal(rast_diff,zones=LC6_rec,stat="mean",na.rm=TRUE)
  
  std_LC1_rec<-zonal(rast_diff,zones=LC1_rec,stat="sd",na.rm=TRUE)
  std_LC2_rec<-zonal(rast_diff,zones=LC2_rec,stat="sd",na.rm=TRUE)
  std_LC3_rec<-zonal(rast_diff,zones=LC3_rec,stat="sd",na.rm=TRUE)
  std_LC4_rec<-zonal(rast_diff,zones=LC4_rec,stat="sd",na.rm=TRUE)
  std_LC6_rec<-zonal(rast_diff,zones=LC6_rec,stat="sd",na.rm=TRUE)
  
  avg_LC_rec<-zonal(rast_diff,zones=LC_rec_s,stat="mean",na.rm=TRUE)
  std_LC_rec<-zonal(rast_diff,zones=LC_rec_s,stat="sd",na.rm=TRUE)
  
  zones_stat_std<-as.data.frame(cbind(std_LC1_rec,std_LC2_rec[,2],std_LC3_rec[,2],std_LC4_rec[,2],std_LC6_rec[,2]))
  zones_stat<-as.data.frame(cbind(avg_LC1_rec,avg_LC2_rec[,2],avg_LC3_rec[,2],avg_LC4_rec[,2],avg_LC6_rec[,2]))
  names(zones_stat)<-c("zones","LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban")
  names(zones_stat_std)<-c("zones","LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban")
  
  X11()
  #Selecting model from stack
  #mf_selected<-"FSS_kr" #model to be used in the assessment...fusion+kr
  #mc_selected<-"CAI_mod7" #model to be used in the assessment...CAI+kr...
  plot(zones_stat$zones,zones_stat$LC1_forest,type="b",ylim=c(-4.5,4.5),
       ylab="",xlab="",axes=FALSE)
  mtext("difference between FSS and CAI (degree C)",line=3,side=2,cex=1.2,font=2) #Add ylab with distance 3 from box
  mtext("land cover percent classes",side=1,cex=1.2,line=3,font=2)
  lines(zones_stat$zones,zones_stat[,3],col="red",type="b")
  lines(zones_stat$zones,zones_stat[,4],col="blue",type="b")
  lines(zones_stat$zones,zones_stat[,5],col="darkgreen",type="b")
  lines(zones_stat$zones,zones_stat[,6],col="purple",type="b")
  breaks_lab<-zones_stat$zones
  tick_lab<-c("0","1-10","","20-30","","40-50","","60-70","","80-90","90-100") #Not enough space for  
  #tick_lab<-c("0","10-20","30-40","60-70","80-90","90-100")
  axis(side=1,las=1,tick=TRUE,
       at=breaks_lab,labels=tick_lab, cex.axis=1.2,font=2) #reduce number of labels to Jan and June
  #text(tick_lab, par(“usr”)[3], labels = tick_lab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
  axis(2,cex.axis=1.2,font=2)
  box()
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1, col=c("black","red","blue","darkgreen","purple"),bty="n",
         lty=1)
  title(paste("Prediction tmax difference (",mf_selected,"-",mc_selected,") and land cover ",sep=""),cex=1.4,font=2)
  savePlot(paste("fig6_diff_prediction_tmax_difference_land cover",mf_selected,mc_selected,date_selected,out_prefix,".png", sep="_"), type="png")
  dev.off()
  
  LC1<-mask(LC1,mask_ELEV_SRTM)
  cellStats(LC1,"countNA")        #Check that NA have been assigned to water and areas below 0 m
  
  LC1_50_m<- LC1>50
  LC1_100_m<- LC1>=100
  LC1_50_m[LC1_50_m==0]<-NA
  LC1_100_m[LC1_100_m==0]<-NA
  LC1_50<-LC1_50_m*LC1
  LC1_100<-LC1_100_m*LC1
  avl<-c(0,500,1,500,1000,2,1000,1500,3,1500,2000,4,2000,4000,5)
  rclmat<-matrix(avl,ncol=3,byrow=TRUE)
  elev_rec<-reclass(ELEV_SRTM,rclmat)  #Loss of layer names when using reclass
  
  elev_rec_forest<-elev_rec*LC1_100_m
  avg_elev_rec<-zonal(rast_diff,zones=elev_rec,stat="mean",na.rm=TRUE)
  std_elev_rec<-zonal(rast_diff,zones=elev_rec,stat="sd",na.rm=TRUE)
  avg_elev_rec_forest<-zonal(rast_diff,zones=elev_rec_forest,stat="mean",na.rm=TRUE)
  std_elev_rec_forest<-zonal(rast_diff,zones=elev_rec_forest,stat="sd",na.rm=TRUE)
  
  ## CREATE plots
  X11()
  plot(avg_elev_rec[,1],avg_elev_rec[,2],type="b",ylim=c(-10,1),
       ylab="",xlab="",axes=FALSE)
  mtext("tmax difference between FSS and CAI (degree C)",side=2,cex=1.2,line=3,font=2)
  mtext("elevation classes (m)",side=1,cex=1.2,line=3,font=2)
  lines(avg_elev_rec_forest[,1],avg_elev_rec_forest[,2],col="green",type="b") #Elevation and 100% forest...
  breaks_lab<-avg_elev_rec[,1]
  elev_lab<-c("0-500","500-1000","1000-1500","1500-2000","2000-4000")
  axis(side=1,las=1,
       at=breaks_lab,labels=elev_lab, cex=1.5,font=2) #reduce number of labels to Jan and June
  axis(2,cex.axis=1.2,font=2)
  legend("bottomleft",legend=c("Elevation", "elev_forest"), 
         cex=1, lwd=1.3,col=c("black","green"),bty="n",
         lty=1)
  box()
  title(paste("Prediction tmax difference (",mf_selected,"-",mc_selected,") and elevation ",sep=""),cex=1.4,font=2)
  savePlot(paste("fig7_diff_prediction_tmax_difference_elevation",mf_selected,mc_selected,date_selected,out_prefix,".png", sep="_"), type="png")
  dev.off()
  #Add plots with std as CI
   
}

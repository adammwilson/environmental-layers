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

out_prefix<-"methods_comp5_12042012_"
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
obj_mod_cai_name<-"results_mod_obj__365d_GAM_CAI2_const_all_10312012.RData"

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

################ VISUALIZATION !!!!!!!! ############
#updated the analysis

dates<-c("20100103","20100901")
i=2

for(i in 1:length(dates)){
  
  date_selected<-dates[i]
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
  
  s_range<-c(minValue(rast_fus1c),maxValue(rast_fus1c)) #stack min and max
  s_range<-c(min(s_range),max(s_range))
  col_breaks <- pretty(s_range, n=50)
  lab_breaks <- pretty(s_range, n=5)
  temp_colors <- colorRampPalette(c('blue', 'white', 'red'))
  X11(width=18,height=12)
  par(mfrow=c(3,3))
  for (k in 1:length(lf_fus1c)){
    fus1c_r<-raster(rast_fus1c,k)
    plot(fus1c_r, breaks=col_breaks, col=temp_colors(length(col_breaks)-1),   
         axis=list(at=lab_breaks, labels=lab_breaks))
  }
  plot(rast_fus1c,col=temp_colors(49))
  savePlot(paste("fig1_diff_models_fusion_",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  s_range<-c(minValue(rast_cai2c),maxValue(rast_cai2c)) #stack min and max
  s_range<-c(min(s_range),max(s_range))
  col_breaks <- pretty(s_range, n=50)
  lab_breaks <- pretty(s_range, n=5)
  temp_colors <- colorRampPalette(c('blue', 'white', 'red'))
  X11(width=18,height=12)
  par(mfrow=c(3,3))
  for (k in 1:length(lf_fus1c)){
    cai2c_r<-raster(rast_cai2c,k)
    plot(cai2c_r, breaks=col_breaks, col=temp_colors(length(col_breaks)-1),   
         axis=list(at=lab_breaks, labels=lab_breaks))
  }
  plot(rast_cai2c,col=temp_colors(49))
  savePlot(paste("fig2_diff_models_cai_",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  #PLOT CAI_Kr and Fusion_Kr
  
  rast_fus_pred<-raster(rast_fus1c,1)  # Select the first model from the stack i.e fusion with kriging for both steps
  rast_cai_pred<-raster(rast_cai2c,1)  
  layerNames(rast_cai_pred)<-paste("cai",date_selected,sep="_")
  layerNames(rast_fus_pred)<-paste("fus",date_selected,sep="_")
  #Plot side by side
  X11(width=16,height=9)
  rast_pred<-stack(rast_cai_pred,rast_fus_pred)
  layerNames(rast_pred)<-c(paste('CAI_kr',date_selected,sep=" "),paste('Fusion_kr',date_selected,sep=" "))
  s.range <- c(min(minValue(rast_pred)), max(maxValue(rast_pred)))
  col.breaks <- pretty(s.range, n=50)
  lab.breaks <- pretty(s.range, n=5)
  temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
  plot(rast_pred, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
       axis=list(at=lab.breaks, labels=lab.breaks))
  savePlot(paste("fig3_diff_CAI_fusion_",date_selected,out_prefix,".png", sep=""), type="png")
  
  #Scatter plot of fus vs cai
  plot(values(rast_fus_pred),values(rast_cai_pred),ylab="CAI",xlab="Fusion",axis=FALSE)
  title(paste("CAI and fusion scatterplot on ",date_selected,sep=""))
  savePlot(paste("fig4_diff_image_scatterplot_CAI_fusion_",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  
  ## Start difference analysis
  #Calculate difference image for the date selected 
  rast_diff<-rast_fus_pred-rast_cai_pred
  layerNames(rast_diff)<-paste("diff",date_selected,sep="_")
  mean_val<-cellStats(rast_diff,mean)
  sd_val<-cellStats(rast_diff,sd)
  
  #View classified diff and outliers... 
  diff_n_outlier<-rast_diff< (2*-sd_val) #Create negative and positive outliers...
  diff_p_outlier<-rast_diff> (2*sd_val) #Create negative and positive outliers...
  diff_outlier<-stack(diff_n_outlier,diff_p_outlier)
  layerNames(diff_outlier)<-c("Negative_diff_outliers","Positive_diff_outliers")
  bool_ramp<-colorRampPalette(c("black","red"))
  X11()
  plot(diff_outlier,col=bool_ramp(2))
  savePlot(paste("fig5_diff_image_outliers_CAI_fusion_",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  tmp<-overlay(diff_n_outlier,ELEV_SRTM,fun=function(x,y){return(x*y)})
  #could use mask
  tmp[tmp==0]<-NA
  mean_Elev_n_outliers<-cellStats(tmp,mean)
  mean_Elev<-cellStats(ELEV_SRTM,mean)
  print(c(mean_Elev_n_outliers,mean_Elev),digits=7) #This shows that outliers are in higher areas
  # on average: 1691m compared to 1044m
  ##postive outliers and land cover
  #LC2 (shrub), LC1(forest),LC3(grass),LC4(crop)
  tmp<-overlay(diff_p_outlier,LC2,fun=function(x,y){return(x*y)})
  tmp[tmp==0]<-NA
  mean_LC2_p_outliers<-cellStats(tmp,mean)  #There is more shrub (44.84% than on average 22.32)
  mean_LC2<-cellStats(LC2,mean)
  print(c(mean_LC2_p_outliers,mean_LC2),digits=7) #This shows that outliers have in higher 
  #proportion of shurb (44% against 25%)
  tmp<-overlay(diff_p_outlier,LC3,fun=function(x,y){return(x*y)})
  tmp[tmp==0]<-NA
  mean_LC3_p_outliers<-cellStats(tmp,mean)  #There is more grass (42.73% than on average 14.47)
  mean_LC3<-cellStats(LC3,mean)
  print(c(mean_LC3_p_outliers,mean_LC3),digits=7) #This shows that outliers have in higher 
  #proportion of shurb (44% against 25%)
  tmp<-overlay(diff_p_outlier,LC4,fun=function(x,y){return(x*y)})
  tmp[tmp==0]<-NA
  mean_LC4_p_outliers<-cellStats(tmp,mean)  #There is more grass (42.73% than on average 14.47)
  mean_LC4<-cellStats(LC4,mean)
  print(c(mean_LC4_p_outliers,mean_LC4),digits=7) #This shows that outliers have in higher 
  
  #CREATE A TABLE
  
  ####
  #View histogram
  hist(rast_diff)
  
  ### More Land cover analysis related to references...
  
  LC2<-mask(LC2,mask_ELEV_SRTM)
  cellStats(LC2,"countNA")        #Check that NA have been assigned to water and areas below 0 m
  
  LC2_50_m<- LC2>50
  
  LC2_50<-LC2_50_m*LC2
  diff_LC2_50<-LC2_50_m*rast_diff
  cellStats(diff_LC2_50,"mean")
  plot(LC2)
  plot(LC2_50)
  freq(LC2_50)
  
  #Forest NOW
  LC1<-mask(LC1,mask_ELEV_SRTM)
  cellStats(LC1,"countNA")        #Check that NA have been assigned to water and areas below 0 m
  
  LC1_50_m<- LC1>50
  LC1_100_m<- LC1>=100
  LC1_50_m[LC1_50_m==0]<-NA
  LC1_100_m[LC1_100_m==0]<-NA
  LC1_50<-LC1_50_m*LC1
  LC1_100<-LC1_100_m*LC1
  plot(LC1)
  plot(LC1_50_m)
  freq(LC1_50_m)
  diff_LC1_50<-LC1_50_m*rast_diff
  diff_LC1_100<-LC1_100_m*rast_diff
  
  plot(diff_LC1_50)
  cellStats(diff_LC1_50,"mean")
  cellStats(diff_LC1_100,"mean")
  plot(values(diff_LC1_50),values(LC1_50))
  plot(values(diff_LC1_100),values(LC1_100))
  x<-brick(LC1,rast_diff)
  
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
  plot(zones_stat$zones,zones_stat$LC1_forest,type="b",ylim=c(-4.5,4.5),
       ylab="difference between CAI and fusion",xlab="land cover percent class/10")
  lines(zones_stat$zones,zones_stat[,3],col="red",type="b")
  lines(zones_stat$zones,zones_stat[,4],col="blue",type="b")
  lines(zones_stat$zones,zones_stat[,5],col="darkgreen",type="b")
  lines(zones_stat$zones,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1)
  title(paste("Prediction tmax difference and land cover ",sep=""))
  
  savePlot(paste("fig6_diff_prediction_tmax_difference_land cover",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  
  avl<-c(0,500,1,500,1000,2,1000,1500,3,1500,2000,4,2000,2500,5,2500,4000,6)
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
       ylab="difference between CAI and fusion",xlab="elevation classes")
  lines(avg_elev_rec_forest[,1],avg_elev_rec_forest[,2],col="green",type="b") #Elevation and 100% forest...
  legend("topright",legend=c("Elevation", "elev_forest"), 
         cex=1.2, col=c("black","darkgreen"),
         lty=1)
  title(paste("Prediction tmax difference and elevation ",sep=""))
  savePlot(paste("fig7_diff_prediction_tmax_difference_elevation",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  #Add plots with std as CI
   
}

###################################################################
################   SPATIAL TRANSECT THROUGH THE IMAGE: ####################

#select date
dates<-c("20100103","20100901")
#j=2

for (j in 1:length(dates)){
  
  #Read predicted tmax raster surface and modeling information
  date_selected<-dates[j]
  
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
  
  setwd(path)
  rast_fus_pred<-raster(rast_fus1c,1)
  rast_cai_pred<-raster(rast_cai2c,1)
  rast_diff_fc<-rast_fus_pred-rast_cai_pred
  
  #Read in data_s and data_v

  k<-match(date_selected,sampling_date_list)
  names(gam_fus$gam_fus_mod[[k]])               #Show the name structure of the object/list
  
  #Extract the training and testing information for the given date...
  data_sf<-gam_fus$gam_fus_mod[[k]]$data_s #object for the first date...20100103    #Make this a function??              
  data_vf<-gam_fus$gam_fus_mod[[k]]$data_v #object for the first date...20100103                  
  data_sc<-gam_cai$gam_CAI_mod[[k]]$data_s #object for the first date...20100103                  
  data_vc<-gam_cai$gam_CAI_mod[[k]]$data_v #object for the first date...20100103
  
  ### CREATE A NEW TRANSECT BASED ON LOCATION OF SPECIFIED STATIONS
  
  selected_stations<-c("USW00024284","USC00354126","USC00358536","USC00354835",
                       "USC00356252","USC00359316","USC00358246","USC00350694",
                       "USC00350699","USW00024230","USC00353542")
  #add which one were training and testing
  data_vf$training<-rep(0,nrow(data_vf))
  data_sf$training<-rep(1,nrow(data_sf))
  
  data_stat<-rbind(data_vf[,c("id","training")],data_sf[,c("id","training")]) #bringing together data_v and data_s
  m<-match(selected_stations,data_stat$id)
  m<-as.integer(na.omit(m))
  trans4_stations<-transect_from_spdf(data_stat,m)
  point4_stations<-data_stat[m,]
  #tmp<-as.data.frame(data_stat[1,])
  #row.names(tmp)<-rep("X",1)
  #test<-SpatialLinesDataFrame(trans4_stations,data=tmp)
  writeOGR(obj=trans4_stations,layer="t4_line",dsn="t4_line.shp",driver="ESRI Shapefile", overwrite=T)
  ## Create list of transect
  
  list_transect<-vector("list",nb_transect)
  list_transect[[1]]<-c("t1_line.shp",paste("figure_9_tmax_transect1_OR",date_selected,out_prefix,sep="_"))
  list_transect[[2]]<-c("t2_line.shp",paste("figure_10_tmax_transect2_OR",date_selected,out_prefix,sep="_"))
  list_transect[[3]]<-c("t3_line.shp",paste("figure_11_tmax_transect3_OR",date_selected,out_prefix,sep="_"))
  list_transect[[4]]<-c("t4_line.shp",paste("figure_12_tmax_transect4_OR",date_selected,out_prefix,sep="_"))
  
  names(list_transect)<-c("transect_OR1","transect_OR2","transect_OR3","transect_OR4")
  
  #now add a transect for elevation
  list_transect2<-vector("list",nb_transect)
  list_transect2[[1]]<-c("t1_line.shp",paste("figure_13_tmax_elevation_transect1_OR",date_selected,out_prefix,sep="_"))
  list_transect2[[2]]<-c("t2_line.shp",paste("figure_14_tmax_elevation_transect2_OR",date_selected,out_prefix,sep="_"))
  list_transect2[[3]]<-c("t3_line.shp",paste("figure_15_tmax_elevation_transect3_OR",date_selected,out_prefix,sep="_"))
  list_transect2[[4]]<-c("t4_line.shp",paste("figure_16_tmax_elevation_transect4_OR",date_selected,out_prefix,sep="_"))
  
  names(list_transect2)<-c("transect_OR1","transect_OR2","transect_OR3","transect_OR4")
  
  rast_pred<-stack(rast_fus_pred,rast_cai_pred)
  rast_pred2<-stack(rast_fus_pred,rast_cai_pred,ELEV_SRTM)
  layerNames(rast_pred)<-c("fus","CAI")
  layerNames(rast_pred2)<-c("fus","CAI","elev")
  title_plot<-paste(names(list_transect),date_selected)
  title_plot2<-paste(names(list_transect2),date_selected)
  #r_stack<-rast_pred
  
  X11(width=9,height=9)
  nb_transect<-length(list_transect)
  s_range<-c(minValue(rast_diff_fc),maxValue(rast_diff_fc)) #stack min and max
  col_breaks <- pretty(s_range, n=50)
  lab_breaks <- pretty(s_range, n=7)
  temp_colors <- colorRampPalette(c('blue', 'white', 'red'))
  plot(rast_diff_fc, breaks=col_breaks, col=temp_colors(length(col_breaks)-1),   
         axis=list(at=lab_breaks, labels=lab_breaks))
  for (k in 1:nb_transect){
    trans_file<-list_transect[[k]]
    filename<-sub(".shp","",trans_file)             #Removing the extension from file.
    transect<-readOGR(".", filename)                 #reading shapefile 
    plot(transect,add=TRUE)
  }

  savePlot(paste("fig8_diff_transect_path_tmax_diff_CAI_fusion_",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  
  X11(width=18,height=9)
  m_layers_sc<-c(3)
  trans_data<-plot_transect(list_transect,rast_pred,title_plot,disp=TRUE)
  
  trans_data2<-plot_transect_m(list_transect2,rast_pred2,title_plot2,disp=TRUE,m_layers_sc)
  dev.off()
  
  ### PLOT LOCATIONS OF STATION ON FIGURES
  
  data_stat<-rbind(data_vf[,c("id","training")],data_sf[,c("id","training")]) #bringing together data_v and data_s
  m<-match(selected_stations,data_stat$id)
  m<-as.integer(na.omit(m))
  trans4_stations<-transect_from_spdf(data_stat,m)
  point4_stations<-data_stat[m,]

  pos<-match(c("x_OR83M","y_OR83M"),layerNames(s_raster)) #Find column with name "value"
  xy_stack<-subset(s_raster,pos)   #Select multiple layers from the stack
  r_stack<-stack(xy_stack, rast_pred2)
  trans4_data<-extract(r_stack,trans4_stations,cellnumbers=TRUE) #This extracts a list
  trans4_data<-as.data.frame(trans4_data[[1]])
  point4_cellID<-cellFromXY(r_stack,coordinates(point4_stations)) #This contains the cell ID the points
  pos<-match(point4_cellID,trans4_data$cell)
  
  #Plots lines where there are stations...
  X11(width=18,height=9)
  y<-trans4_data$fus
  x <- 1:length(y)
  plot(x,y,type="l",col="red", #plotting fusion profile
  ,xlab="",ylab="tmax (in degree C)")
  y<-trans4_data$CAI
  lines(x,y,col="green")
  abline(v=pos)#addlines whtere the stations area...
  #plot(elev)
  #title(title_plot[i]))
  legend("topleft",legend=c("fus","CAI"), 
  cex=1.2, col=c("red","green"),
  lty=1)
  savePlot(paste("fig17_transect_path_tmax_diff_CAI_fusion_",date_selected,out_prefix,".png", sep=""), type="png")
  
  
  y<-trans4_data$fus[1:150]
  x <- 1:150
  plot(x,y,type="l",col="red", #plotting fusion profile
       ,xlab="",ylab="tmax (in degree C)")
  y<-trans4_data$CAI[1:150]
  lines(x,y,col="green")
  abline(v=pos)#addlines whtere the stations area...
  #plot(elev)
  #title(title_plot[i]))
  legend("topleft",legend=c("fus","CAI"), 
         cex=1.2, col=c("red","green"),
         lty=1)
  savePlot(paste("fig18a_transect_path_tmax_diff_CAI_fusion_",date_selected,out_prefix,".png", sep=""), type="png")
  
  
  y<-trans4_data$fus[151:300]
  x <- 151:300
  plot(x,y,type="l",col="red", #plotting fusion profile
       ,xlab="",ylab="tmax (in degree C)")
  y<-trans4_data$CAI[151:300]
  lines(x,y,col="green")
  abline(v=pos)#addlines whtere the stations area...
  #plot(elev)
  #title(title_plot[i]))
  legend("topleft",legend=c("fus","CAI"), 
         cex=1.2, col=c("red","green"),
         lty=1)
  savePlot(paste("fig18b_transect_path_tmax_diff_CAI_fusion_",date_selected,out_prefix,".png", sep=""), type="png")
  
  dev.off()
  
  #cor(fus_y,elev_y)
  #cor(cai_y,elev_y)
  #cor(fus_y,cai_y)

}

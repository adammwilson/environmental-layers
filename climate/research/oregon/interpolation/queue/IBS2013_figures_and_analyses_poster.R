######################################## IBS 2013 POSTER #######################################
############################ Scripts for figures and analyses for the the IBS poster #####################################
#This script creates the figures used in the IBS 2013 poster.
#It uses inputs from interpolation objects created at earlier stages...                          #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 12/27/2012                                                                                #
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
#infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       #mean LST for day 244
inlistf<-"list_files_05032012.txt"                        #list of raster images containing the Covariates
infile6<-"OR83M_state_outline.shp"
#stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)

obj_list<-"list_obj_12272012.txt"                                  #Results of fusion from the run on ATLAS
#obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison_10242012" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
setwd(path) 
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
#Number of kriging model
out_prefix<-"methods_comp_12272012_"                                              #User defined output prefix

filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

### PREPARING RASTER COVARIATES STACK #######

#CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)
lines<-read.table(paste(path,"/",inlistf,sep=""), sep="")                      #Column 1 contains the names of raster files
inlistvar<-lines[,1]                                                           #column 3 the list of models to use...?

inlistvar<-paste(path,"/",as.character(inlistvar),sep="")
covar_names<-as.character(lines[,2])                                         #Column two contains short names for covaraites

s_raster<- stack(inlistvar)                                                  #Creating a stack of raster images from the list of variables.
layerNames(s_raster)<-covar_names                                            #Assigning names to the raster layers
projection(s_raster)<-proj_str

#Create mask
pos<-match("LC10",layerNames(s_raster))
LC10<-subset(s_raster,pos)
LC10[is.na(LC10)]<-0   #Since NA values are 0, we assign all zero to NA
mask_land<-LC10<100
mask_land_NA<-mask_land
mask_land_NA[mask_land_NA==0]<-NA

data_name<-"mask_land_OR"
raster_name<-paste(data_name,".rst", sep="")
writeRaster(mask_land, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
#writeRaster(r2, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)

pos<-match("ELEV_SRTM",layerNames(s_raster))
ELEV_SRTM<-raster(s_raster,pos)
elev<-ELEV_SRTM
elev[-0.050<elev]<-NA  #Remove all negative elevation lower than 50 meters...

mask_elev_NA<-elev

pos<-match("mm_01",layerNames(s_raster))
mm_01<-subset(s_raster,pos)
mm_01<-mm_01-273.15
mm_01<-mask(mm_01,mask_land_NA)
#mention this is the last... files

##################### METHODS COMPARISON  ###########################

######################################################################
# PART 1 : USING ACCURACY METRICS FOR FIVE METHODS COMPARISON
# Boxplots and histograms
#start function here...

lines<-read.table(paste(path,"/",obj_list,sep=""), sep=",")   #Column 1 contains the names RData objects
inlistobj<-lines[,1]
tinlistobj<-paste(path,"/",as.character(inlistobj),sep="")
obj_names<-as.character(lines[,2])                    #Column two contains short names for obj. model

tb_metrics_fun<-function(list_obj,path_data,names_obj){
  nel<-length(inlistobj)
  #method_mod <-vector("list",nel) #list of one row data.frame
  method_tb <-vector("list",nel) #list of one row data.frame
  for (k in 1:length(inlistobj)){
    #obj_tmp<-load_obj(as.character(inlistobj[i]))
    #method_mod[[i]]<-obj_tmp
    #names(method_mod[[i]])<-obj_names[i]
    mod_tmp<-load_obj(as.character(inlistobj[k]))
    tb<-mod_tmp[[1]][[3]][0,] #copy of the data.frame structure that holds the acuary metrics
    #mod_tmp<-method_mod[[k]]
    for (i in 1:365){                     # Assuming 365 days of prediction
      tmp<-mod_tmp[[i]][[3]]
      tb<-rbind(tb,tmp)
    }
    rm(mod_tmp)
    for(i in 4:(ncol(tb))){            # start of the for loop #1
      tb[,i]<-as.numeric(as.character(tb[,i]))  
    }
    method_tb[[k]]<-tb 
  }
  names(method_tb)<-names_obj
  return(method_tb)
}

tmp44<-tb_metrics_fun(as.character(inlistobj),path,obj_names)
#Condensed, and added other comparison, monthly comparison...:ok

plot_model_boxplot_combined_fun<-function(tb_list,path_data,obj_names,mod_selected,out_prefix,layout_m){
  
  method_stat<-vector("list",length(obj_names)) #This contains summary information based on accuracy metrics (MAE,RMSE)
  names_method<-obj_names
  metrics<-c("MAE","RMSE")
  tb_metric_list<-vector("list",length(metrics))
  tb_metric_list_na<-vector("list",length(metrics))  
  mean_list<-vector("list",length(metrics))
  sd_list<-vector("list",length(metrics))
  na_mod_list<-vector("list",length(metrics))
  
  for(i in 1:length(metrics)){            # Reorganizing information in terms of metrics
    #for(k in 1:length(tb_list)){            # start of the for main loop to all methods
    #tb<-tb_list[[k]]
    #metrics<-as.character(unique(tb$metric))            #Name of accuracy metrics (RMSE,MAE etc.)
    metric_name<-paste("tb_t_",metrics[i],sep="")
    png(paste("boxplot",metric_name,out_prefix,"_combined.png", sep="_"),height=480*layout_m[1],width=480*layout_m[2])
    par(mfrow=layout_m)
    for(k in 1:length(tb_list)){            # start of the for main loop to all methods
      #}#for(i in 1:length(metrics)){            # Reorganizing information in terms of metrics 
      tb<-tb_list[[k]]
      #metric_name<-paste("tb_t_",metrics[i],sep="")
      tb_metric<-subset(tb, metric==metrics[i])
      assign(metric_name,tb_metric)
      tb_metric_list[[i]]<-tb_metric
      tb_processed<-tb_metric     
      mod_pat<-glob2rx("mod*")   
      var_pat<-grep(mod_pat,names(tb_processed),value=FALSE) # using grep with "value" extracts the matching names  
      #mod_pat<-mod_selected
      #var_pat<-grep(mod_pat,names(tb_processed),value=FALSE) # using grep with "value" extracts the matching names
      na_mod<-colSums(!is.na(tb_processed[,var_pat]))
      for (j in 1:length(na_mod)){    
        if (na_mod[j]<183){
          tmp_name<-names(na_mod)[j]
          pos<-match(tmp_name,names(tb_processed))
          tb_processed<-tb_processed[,-pos]   #Remove columns that have too many missing values!!!
        }
      }
      tb_metric_list_na[[i]]<-tb_processed
      mod_pat<-glob2rx("mod*")
      var_pat<-grep(mod_pat,names(tb_processed),value=FALSE)
      #Plotting box plots
      
      #png(paste("boxplot",metric_name,names_methods[k],out_prefix,".png", sep="_"))
      boxplot(tb_processed[,var_pat],main=names_methods[k], ylim=c(1,5),
              ylab= metrics[i], outline=FALSE) #ADD TITLE RELATED TO METHODS...
      
      #Add assessment of missing prediction over the year.
      mean_metric<-sapply(tb_processed[,var_pat],mean,na.rm=T)
      sd_metric<-sapply(tb_processed[,var_pat],sd,na.rm=T)
      mean_list[[i]]<-mean_metric
      sd_list[[i]]<-sd_metric
      na_mod_list[[i]]<-na_mod_list          
      #Now calculate monthly averages and overall averages over full year
      method_stat<-list(mean_list,sd_list,na_mod_list)
      method_stat[[k]]<-list(mean_list,sd_list,na_mod_list)
      names(method_stat[[k]])<-c("mean_metrics","sd_metrics","na_metrics")
      names(mean_list)<-metrics
      method_mean[[k]]<-mean_list
      names_methods<-obj_names
      #names(method_stat)<-obj_names 
    }   
    dev.off() #Close file where figures are drawn
  }
  return(method_stat) 
}

tb_list<-tmp44
mod_selected<-""
layout_plot<-c(1,5)
mean_methods<-plot_model_boxplot_fun(tb_list,path,obj_names,mod_selected,out_prefix)
mean_methods_2<-plot_model_boxplot_combined_fun(tb_list,path,obj_names,mod_selected,out_prefix,layout_m=layout_plot)

#####################   PART II   #######################

##PLOTTING OF ONE DATE TO COMPARE METHODS!!!

lf_raster_fus<-"_365d_GAM_fusion_all_lstd_12272012.rst"
lf_raster_cai<-"_365d_GAM_CAI4_all_12272012.rst"
date_selected<-"20100103"
titles<-list(cai=c("cai mod1","cai mod4","cai mod7"),
             fusion=c("fusion mod1"," fusion mod4"," fusion mod7"))

mask_rast<-mask_elev_NA
mod_selected1<-c(1,4,7)
mod_selected2<-c(1,4,7)
#lf_raster_fus<-file_pat1
#lf_raster_cai<-file_pat2
file_pat1<-glob2rx(paste("*tmax_predicted*",date_selected,"*",lf_raster_cai,sep="")) #Search for files in relation to fusion                  
#lf_cai<-list.files(pattern=file_pat) #Search for files in relation to fusion                  
file_pat2<-glob2rx(paste("*tmax_predicted*",date_selected,"*",lf_raster_fus,sep="")) #Search for files in relation to fusion                  
#lf_fus<-list.files(pattern=file_pat) #Search for files in relation to fusion                  
layout_plot<-c(2,3)
raster_plots_interpolation_fun<-function(file_pat1,file_pat2,mod_selected1,mod_selected2,titles,mask_rast,
                                         layout_m,out_suffix){
  layout_m<-layout_plot
  lf_cai<-list.files(pattern=file_pat1) #Search for files in relation to fusion                  
  lf_fus<-list.files(pattern=file_pat2) #Search for files in relation to fusion                  
  
  r1<-stack(lf_cai[mod_selected1]) #CAI
  r2<-stack(lf_fus[mod_selected2])#FUS
  predictions<-stack(r1,r2)
  predictions<-mask(predictions,mask_rast)
  layerNames(predictions)<-unlist(titles)
  
  s.range <- c(min(minValue(predictions)), max(maxValue(predictions)))
  col.breaks <- pretty(s.range, n=50)
  lab.breaks <- pretty(s.range, n=5)
  temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
  X11(height=6,width=12)
  #plot(predictions, breaks=col.breaks, col=rev(heat.colors(length(col.breaks)-1)),
  #   axis=list(at=lab.breaks, labels=lab.breaks))
  plot(predictions, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
       axis=list(at=lab.breaks, labels=lab.breaks))
  #plot(reg_outline, add=TRUE)
  savePlot(paste("comparison_one_date_CAI_fusion_tmax_prediction_",date_selected,out_prefix,".png", sep=""),type="png")
  #png(paste("boxplot",metric_name,out_prefix,"_combined.png", sep="_"),height=480*layout_m[1],width=480*layout_m[2])
  #par(mfrow=layout_m)
  png(paste("comparison_one_date_CAI_fusion_tmax_prediction_levelplot_",date_selected,out_prefix,".png", sep=""),
      height=480*layout_m[1],width=480*layout_m[2])
  levelplot(predictions,main="comparison", ylab=NULL,xlab=NULL,par.settings = list(axis.text = list(font = 2, cex = 1.5),
                                                                                   par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
            #col.regions=temp.colors,at=seq(-1,1,by=0.02))
            col.regions=temp.colors(25))
  dev.off()
  #savePlot(paste("comparison_one_date_CAI_fusion_tmax_prediction_levelplot_",date_selected,out_prefix,".png", sep=""),type="png")
}

raster_plots_interpolation_fun(file_pat1,file_pat2,
                               mod_selected1,mod_selected2,titles,mask_rast,layout_plot,out_prefix)


#### FIGURE 3: Transect map

### FIGURE 4: transect plot



#### END OF THE SCRIPT #########


#This can be entered as textfile or option later...ok for running now on 12/07/2012


#Figure 1: Boxplots for all methods and models...

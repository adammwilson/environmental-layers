######################################## IBS 2013 POSTER #######################################
############################ Scripts for figures and analyses for the the IBS poster #####################################
#This script creates the figures used in the IBS 2013 poster.
#It uses inputs from interpolation objects created at earlier stages...                          #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 01/03/2013                                                                                #
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
  #s.range <- s.range+c(5,-5)
  col.breaks <- pretty(s.range, n=50)
  lab.breaks <- pretty(s.range, n=5)
  temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
  X11(height=6,width=36)
  X11(height=6,width=18)
  #plot(predictions, breaks=col.breaks, col=rev(heat.colors(length(col.breaks)-1)),
  #   axis=list(at=lab.breaks, labels=lab.breaks))
  plot(predictions, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
       axis=list(at=lab.breaks, labels=lab.breaks))
  #plot(reg_outline, add=TRUE)
  savePlot(paste("comparison_one_date_CAI_fusion_tmax_prediction_levelplot",date_selected,out_prefix,".png", sep=""),type="png")
  #png(paste("boxplot",metric_name,out_prefix,"_combined.png", sep="_"),height=480*layout_m[1],width=480*layout_m[2])
  #par(mfrow=layout_m)
  layerNames(predictions)<-titles
  plot_name_pan<-unlist(titles)
  #plot_name_pan<-c('cai_kr (RMSE=2.29)','cai_mod7 (RMSE=2.38)','fss_kr (RMSE=2.29')
  png(paste("comparison_one_date_CAI_fusion_tmax_prediction_levelplot_",date_selected,out_prefix,".png", sep=""),
      height=480*layout_m[1],width=480*layout_m[2])
  levelplot(predictions,main="Interpolated Surfaces Comparison", ylab=NULL,xlab=NULL,par.settings = list(axis.text = list(font = 2, cex = 1.3),
            par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
            names.attr=plot_name_pan,col.regions=temp.colors,at=seq(s.range[1],s.range[2],by=0.25))
            #col.regions=temp.colors(25))
  dev.off()
  #savePlot(paste("comparison_one_date_CAI_fusion_tmax_prediction_levelplot_",date_selected,out_prefix,".png", sep=""),type="png")
  return(predictions)
}

plot_transect_m2<-function(list_trans,r_stack,title_plot,disp=TRUE,m_layers){
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
  t_col<-c("red","green","black")
  lty_list<-c("dashed","solid","dotted")
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
        plot(x,y,type="l",xlab="transect distance from coastal origin (km)", ylab=" maximum temperature (degree C)",
             ,cex=1.2,col=t_col[k])
        #axis(2)
      }
      if (k==1 & !is.na(m)){
        plot(x,y,type="l",col=t_col[k],lty="dotted",axes=F) #plotting fusion profile
        #axis(4,xlab="",ylab="elevation(m)")  
        axis(4,cex=1.2)
      }
      if (k!=1 & is.na(m)){
        #par(new=TRUE)              # new plot without erasing old
        lines(x,y,type="l",xlab="",ylab="",col=t_col[k],axes=F) #plotting fusion profile
        #axis(2,xlab="",ylab="tmax (in degree C)")
      }
      if (k!=1 & !is.na(m)){
        par(new=TRUE)              # key: ask for new plot without erasing old
        plot(x,y,type="l",col=t_col[k],xlab="",ylab="",lty="dotted",axes=F) #plotting fusion profile
        #axis(4,xlab="",ylab="elevation(m)")  
        axis(4,cex=1.2)
      } 
    }
    title(title_plot[i])
    legend("topleft",legend=layerNames(r_stack)[1:2], 
           cex=1.2, col=t_col,lty=1,bty="n")
    legend("topright",legend=layerNames(r_stack)[3], 
           cex=1.2, col=t_col[3],lty="dotted",bty="n")
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

stat_moran_std_raster_fun<-function(i){
  list_var_stat<-vector("list",ncol(lf_list))
  for (k in 1:length(lf_list)){
    
    raster_pred<-raster(lf_list[i,k]) 
    tmp_rast<-mask(raster_pred,mask_rast)
    #tmp_rast<-raster_pred
    raster_pred2<-tmp_rast
    
    t1<-cellStats(raster_pred,na.rm=TRUE,stat=sd)    #Calculating the standard deviation for the 
    m1<-Moran(raster_pred,w=3) #Calculating Moran's I with window of 3 an default Queen's case
    stat<-as.data.frame(t(c(m1,t1)))
    names(stat)<-c("moranI","std")
    list_var_stat[[k]]<-stat
  }
  dat_var_stat<-do.call(rbind,list_var_stat)
  dat_var_stat$lf_names<-names(lf_list)
  dat_var_stat$dates<-dates[i]
  return(dat_var_stat)
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

obj_list<-"list_obj_01012013.txt"                                  #Results of fusion from the run on ATLAS
#obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison_01012013" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
setwd(path) 
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
#Number of kriging model
out_prefix<-"methods_comp_AAG2013_04082013_"                                              #User defined output prefix

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

#######FIGURE 1: Boxplot comparison

lines<-read.table(paste(path,"/",obj_list,sep=""), sep=",")   #Column 1 contains the names RData objects
inlistobj<-lines[,1]
tinlistobj<-paste(path,"/",as.character(inlistobj),sep="")
obj_names<-as.character(lines[,2])                    #Column two contains short names for obj. model

tmp44<-tb_metrics_fun(as.character(inlistobj),path,obj_names)
#Condensed, and added other comparison, monthly comparison...:ok

tb_list<-tmp44
mod_selected<-""
layout_plot<-c(1,5)
#out_prefix<-
#mean_methods<-plot_model_boxplot_fun(tb_list,path,obj_names,mod_selected,out_prefix)
mean_methods_2<-plot_model_boxplot_combined_fun(tb_list,path,obj_names,mod_selected,out_prefix,layout_m=layout_plot)

#######FIGURE 2: Map-Spatial patterns of interpolated surfaces

lf_raster_fus<-"_365d_GAM_fus5_all_lstd_12302012.rst"
#lf_raster_fus<-"_365d_GAM_fusion_all_lstd_12272012.rst"

lf_raster_cai<-"_365d_GAM_CAI4_all_12272012.rst"
date_selected<-"20100901"
#titles<-list(cai=c("cai_kr","cai_mod5","cai mod8"),
#             fusion=c("fusion_kr"," fusion_mod5"," fusion_mod8"))

mask_rast<-mask_elev_NA
mod_selected1<-c(1,2,3,4,5,6,7,8,9,10)
mod_selected2<-c(1)
#lf_raster_fus<-file_pat1
#lf_raster_cai<-file_pat2
file_pat1<-glob2rx(paste("*tmax_predicted*",date_selected,"*",lf_raster_cai,sep="")) #Search for files in relation to fusion                                   
file_pat2<-glob2rx(paste("*tmax_predicted*",date_selected,"*",lf_raster_fus,sep="")) #Search for files in relation to fusion         
lf_cai<-list.files(pattern=file_pat1) #Search for files in relation to fusion                  
lf_fus<-list.files(pattern=file_pat2) #Search for files in relation to fusion                  
titles<-list(cai=c("CAI_kr","CAI_mod6"),
             fusion=c("FSS_kr"))
titles<-list(c("cai_kr","cai_mod1","cai_mod2","cai_mod3","cai_mod4","cai_mod5","cai_mod6","cai_mod7","cai_mod8","cai_mod9","fss_kr"))
r1<-stack(lf_cai[mod_selected1]) #CAI
r2<-stack(lf_fus[mod_selected2])#FUS
             
predictions<-stack(r1,r2)
predictions<-mask(predictions,mask_rast)
layerNames(predictions)<-unlist(titles)
plot(predictions)
layout_plot<-c(1,3)
rast_pred<-raster_plots_interpolation_fun(file_pat1,file_pat2,
                               mod_selected1,mod_selected2,titles,mask_rast,layout_plot,out_prefix)

#######FIGURE 3: Map of transects

nb_transect<-4
list_transect2<-vector("list",nb_transect)
layers_names<-layerNames(rast_pred2)<-c("FSS_kr","fss_mod1","elev")
pos<-c(1,2) # postions in the layer prection
list_transect2[[1]]<-c("t1_line.shp",paste("figure_3_tmax_elevation_transect1_OR_",date_selected,
                                           paste(layers_names,collapse="_"),out_prefix,sep="_"))
list_transect2[[2]]<-c("t2_line.shp",paste("figure_3_tmax_elevation_transect2_OR_",date_selected,
                                           paste(layers_names,collapse="_"),out_prefix,sep="_"))
list_transect2[[3]]<-c("t3_line.shp",paste("figure_3_tmax_elevation_transect3_OR_",date_selected,
                                           paste(layers_names,collapse="_"),out_prefix,sep="_"))
list_transect2[[4]]<-c("t4_line.shp",paste("figure_3_tmax_elevation_transect4_OR_",date_selected,
                                           paste(layers_names,collapse="_"),out_prefix,sep="_"))

names(list_transect2)<-c("transect_OR1","transect_OR2","transect_OR3","transect_OR4")

#X11(width=9,height=9)
#png(paste("fig3_elevation_transect1_path_CAI_fusion_",date_selected,out_prefix,".png", sep=""))
#plot(elev)
#k<-1  #transect to plot
#trans_file<-list_transect2[[k]][[1]]
#filename<-sub(".shp","",trans_file)             #Removing the extension from file.
#transect<-readOGR(".", filename)                 #reading shapefile 
#plot(transect,add=TRUE)
#title("Transect Oregon")
#dev.off()

#######FIGURE 4: Spatial transects profiles

rast_pred<-predictions
rast_pred_selected<-subset(rast_pred,pos) #3 is referring to FSS, plot it first because it has the
                                             # the largest range.
rast_pred2<-stack(rast_pred_selected,elev)
layerNames(rast_pred2)<-layers_names
title_plot2<-paste(names(list_transect2),date_selected,sep=" ")
title_plot2<-paste(rep("Oregon transect on ",3), date_selected,sep="")
#r_stack<-rast_pred

X11(width=18,height=9)
m_layers_sc<-c(3)
#title_plot2
#rast_pred2
trans_data2<-plot_transect_m2(list_transect2,rast_pred2,title_plot2,disp=TRUE,m_layers_sc)
dev.off()

#######FIGURE 5: Moran's profile...

list_var_stat<-vector("list", 365)
lf_raster_fus<-"^fusion_tmax_predicted.*_365d_GAM_fusion_all_lstd_12272012.rst$"
lf_raster_cai<-"^CAI_tmax_predicted.*_365d_GAM_CAI4_all_12272012.rst$"
lf2<-list.files(pattern=lf_raster_fus)
lf1<-list.files(pattern=lf_raster_cai)

mask_rast<-mask_elev_NA

#out_prefix<-"test_01022013"
#lf1<-list.files(pattern=".*CAI.*.rst$")
#lf2<-list.files(pattern=".*fus5.*.rst$")
lf_list<-as.data.frame(cbind(lf1[1:365],lf2[1:365]))
lf_list[,1]<-as.character(lf1[1:365])
lf_list[,2]<-as.character(lf2[1:365])
names(lf_list)<-c("cai","fus")
dates<-1:365

#var_stat_rast<-lapply(1:nrow(lf_list),stat_moran_std_raster_fun)
var_stat_rast<-mclapply(1:nrow(lf_list),stat_moran_std_raster_fun,mc.preschedule=FALSE,mc.cores = 9)
#gam_fus_mod_s<-mclapply(1:length(ghcn.subsets), runGAMFusion,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement

var_stat<-do.call(rbind,var_stat_rast)

var_stat_cai<-subset(var_stat,lf_names=="cai")
var_stat_fus<-subset(var_stat,lf_names=="fus")

#### NOW plot the average statistic per date...

x1<-var_stat_fus$dates
y1<-var_stat_fus$moranI
x2<-var_stat_cai$dates
y2<-var_stat_cai$moranI

x_range<-range(c(x1,x2))
y_range<-range(c(y1,y2))

plot(x1,y1,type="l",col="black",ylim=y_range)
lines(x2,y2,type="l",col="red")
png(paste("fig5_IBS_moranI_",out_prefix,".png",sep=""))
plot(x1,y1,type="l",col="black",xlab="Day Of Year",
     ylab="Moran's I",ylim=y_range)
lines(x2,y2,type="l",col="red")
title("Moran's I for 365 dates in 2010")
t_col<-c("black","red")
legend("topleft",legend=c("FSS_kr","CAI_kr"), 
       cex=1.2, col=t_col,lty=1,bty="n")
dev.off()

## NOW PLOT STANDARD DEVIATION

x1<-var_stat_fus$dates
y1<-var_stat_fus$std
x2<-var_stat_cai$dates
y2<-var_stat_cai$std

x_range<-range(c(x1,x2))
y_range<-range(c(y1,y2))

png(paste("fig5_IBS_std_",out_prefix,".png",sep=""))
plot(x1,y1,type="l",col="black",xlab="Day Of Year",
  ylab=" Standard Deviation (degree C)",ylim=y_range)
lines(x2,y2,type="l",col="red")
title("Standard Deviation for 365 dates in 2010")
t_col<-c("black","red")
legend("topleft",legend=c("FSS_kr","CAI_kr"), 
       cex=1.2, col=t_col,lty=1,bty="n")
dev.off()

#######FIGURE 6: LAND COVER PROFILES

#######FIGURE 7: MULTISAMPLING

### PART I MULTISAMPLING COMPARISON ####

sampling_CAI<-load_obj("results2_CAI_sampling_obj_09132012_365d_GAM_CAI2_multisampling2.RData")
sampling_fus<-load_obj("results2_fusion_sampling_obj_10d_GAM_fusion_multisamp4_09192012.RData")
fus_CAI_mod<-load_obj("results2_CAI_Assessment_measure_all_09132012_365d_GAM_CAI2_multisampling2.RData")
gam_fus_mod1<-load_obj("results2_fusion_Assessment_measure_all_10d_GAM_fusion_multisamp4_09192012.RData")

tb_diagnostic2<-sampling_CAI$tb            #Extracting the accuracy metric information...
tb_diagnostic<-sampling_fus$tb

tb_diagnostic[["prop"]]<-as.factor(tb_diagnostic[["prop"]])
tb_diagnostic2[["prop"]]<-as.factor(tb_diagnostic2[["prop"]])

#Preparing the data for the plot
#fus data
t<-melt(tb_diagnostic,
        measure=c("mod1","mod2","mod3","mod4", "mod5", "mod6", "mod7", "mod8","mod9"), 
        id=c("dates","metric","prop"),
        na.rm=F)
avg_tb<-cast(t,metric+prop~variable,mean)
sd_tb<-cast(t,metric+prop~variable,sd)
n_tb<-cast(t,metric+prop~variable,length)
avg_tb[["prop"]]<-as.numeric(as.character(avg_tb[["prop"]]))
avg_RMSE<-subset(avg_tb,metric=="RMSE")

#CAI data
t2<-melt(tb_diagnostic2,
         measure=c("mod1","mod2","mod3","mod4", "mod5", "mod6", "mod7", "mod8","mod9"), 
         id=c("dates","metric","prop"),
         na.rm=F)
avg_tb2<-cast(t2,metric+prop~variable,mean)
sd_tb2<-cast(t2,metric+prop~variable,sd)
n_tb2<-cast(t2,metric+prop~variable,length)
avg_tb2[["prop"]]<-as.numeric(as.character(avg_tb2[["prop"]]))
avg_RMSE2<-subset(avg_tb2,metric=="RMSE")

#Select only information related to FUSION

x<-avg_RMSE[["prop"]]
i=9
mod_name<-paste("mod",i,sep="")
y<-avg_RMSE[[mod_name]]

sd_tb_RMSE <- subset(sd_tb, metric=="RMSE") 
x_sd<-sd_tb_RMSE[["prop"]]
i=9
mod_name<-paste("mod",i,sep="")
y_sd<-sd_tb_RMSE[[mod_name]]

#Select only information related to CAI

x2<-avg_RMSE2[["prop"]]
i=9
mod_name<-paste("mod",i,sep="")
y2<-avg_RMSE2[[mod_name]]

sd_tb_RMSE2 <- subset(sd_tb2, metric=="RMSE") 
x_sd2<-sd_tb_RMSE2[["prop"]]
i=9
mod_name<-paste("mod",i,sep="")
y_sd2<-sd_tb_RMSE2[[mod_name]]

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

#######FIGURE 8: SPATIAL ACCURACY AND DISTANCE TO CLOSEST STATIONS


#### END OF THE SCRIPT #########

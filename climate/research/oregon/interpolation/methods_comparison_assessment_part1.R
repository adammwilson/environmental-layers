#####################################  METHOD COMPARISON ##########################################
#################################### Spatial Analysis ########################################
#This script utilizes the R ojbects created during the interpolation phase.      
# R ojbects must be supplied in a text file with along with their names. 
# Five mehods are compared over set of year: Kriging, GWR, GAM, CAI and FUSION.
# At this stage the script produces figures of various accuracy metrics and compare x: #
#- boxplots for MAE, RMSE and other accuracy metrics   
#- MAE, RMSE plots per month
#- visualization of map results  for all predictions method  

#AUTHOR: Benoit Parmentier                                                                        
#DATE: 10/12/2012                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#491--  

###################################################################################################

###Loading R library and packages                                                      
#library(gtools)                                        # loading some useful tools 
library(mgcv)                                           # GAM package by Wood 2006 (version 2012)
library(sp)                                             # Spatial pacakge with class definition by Bivand et al. 2008
library(spdep)                                          # Spatial package with methods and spatial stat. by Bivand et al. 2012
library(rgdal)                                          # GDAL wrapper for R, spatial utilities (Keitt et al. 2012)
library(gstat)                                          # Kriging and co-kriging by Pebesma et al. 2004
library(automap)                                        # Automated Kriging based on gstat module by Hiemstra et al. 2008
library(spgwr)
library(maptools)
library(graphics)
library(parallel)                            # Urbanek S. and Ripley B., package for multi cores & parralel processing
library(raster)
library(rasterVis)
library(plotrix)      #Draw circle on graph and additional plotting options
library(reshape)      #Data format and type transformation
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

obj_list<-"list_obj_10172012.txt"                                  #Results of fusion from the run on ATLAS
#obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
setwd(path) 
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
                                                                                #Number of kriging model
out_prefix<-"methods_10172012_"                                              #User defined output prefix

filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

### PREPARING RASTER COVARIATES STACK #######

#CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)
lines<-read.table(paste(path,"/",inlistf,sep=""), sep="")                      #Column 1 contains the names of raster files
inlistvar<-lines[,1]
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

pos<-match("mm_01",layerNames(s_raster))
mm_01<-subset(s_raster,pos)
mm_01<-mm_01-273.15
mm_01<-mask(mm_01,mask_land_NA)
#mention this is the last... files

############# METHODS COMPARISON  ###########

######################################################################
# PART 1 : USING ACCURACY METRICS FOR FIVE METHODS COMPARISON
# Boxplots and histograms

lines<-read.table(paste(path,"/",obj_list,sep=""), sep=",")   #Column 1 contains the names RData objects
inlistobj<-lines[,1]
inlistobj<-paste(path,"/",as.character(inlistobj),sep="")
obj_names<-as.character(lines[,2])                    #Column two contains short names for obj. model

nel<-length(inlistobj)
method_mod <-vector("list",nel) #list of one row data.frame
method_tb <-vector("list",nel) #list of one row data.frame
method_mean<-vector("list",nel)

for (i in 1:length(inlistobj)){
  obj_tmp<-load_obj(inlistobj[i])
  method_mod[[i]]<-obj_tmp
  #names(method_mod[[i]])<-obj_names[i]
}
obj_tmp<-load_obj(inlistobj[i])

names(method_mod)<-obj_names

#Condense and add other comparison, transform in function??

for(k in 1:length(method_mod)){            # start of the for main loop to all methods
  
  tb<-method_mod[[k]][[1]][[3]][0,] #copy
  mod_tmp<-method_mod[[k]]
  
  for (i in 1:365){                     # Assuming 365 days of prediction
    tmp<-mod_tmp[[i]][[3]]
    tb<-rbind(tb,tmp)
  }
  
  rm(mod_tmp)
  
  for(i in 4:(ncol(tb))){            # start of the for loop #1
    tb[,i]<-as.numeric(as.character(tb[,i]))  
  }
  
  method_tb[[k]]<-tb
  tb_RMSE<-subset(tb, metric=="RMSE")
  tb_MAE<-subset(tb,metric=="MAE")
  tb_ME<-subset(tb,metric=="ME")
  tb_R2<-subset(tb,metric=="R2")
  tb_RMSE_f<-subset(tb, metric=="RMSE_f")
  tb_MAE_f<-subset(tb,metric=="MAE_f")
  
  tb_diagnostic1<-rbind(tb_RMSE,tb_MAE,tb_ME,tb_R2)
  
  na_mod<-colSums(!is.na(tb_RMSE[,4:ncol(tb)]))
  for (j in 4:ncol(tb)){
    
    if (na_mod[j-3]<183){
      tb_RMSE<-tb_RMSE[,-j]   #Remove columns that have too many missing values!!!
    }
  }
  
  na_mod<-colSums(!is.na(tb_MAE[,4:ncol(tb)]))
  for (j in 4:ncol(tb)){
    
    if (na_mod[j-3]<183){
      tb_MAE<-tb_MAE[,-j]   #Remove columns that have too many missing values!!!
    }
  }
  
  na_mod<-colSums(!is.na(tb_MAE_f[,4:ncol(tb)])) 
  for (j in 4:ncol(tb)){
    
    if (na_mod[j-3]<183){
      tb_MAE_f<-tb_MAE_f[,-j]   #Remove columns that have too many missing values!!!
    }
  }
  
  na_mod<-colSums(!is.na(tb_ME[,4:ncol(tb)]))
  for (j in 4:ncol(tb)){
    
    if (na_mod[j-3]<183){
      tb_ME<-tb_ME[,-j]   #Remove columns that have too many missing values!!!
    }
  }
  
  #Add assessment of missing prediction over the year.
  
  mean_RMSE<-sapply(tb_RMSE[,4:ncol(tb_RMSE)],mean,na.rm=T
  mean_MAE<-sapply(tb_MAE[,4:ncol(tb_MAE)],mean,na.rm=T)
  mean_R2<-sapply(tb_R2[,4:ncol(tb_R2)],mean, n.rm=T)
  mean_ME<-sapply(tb_ME[,4:ncol(tb_ME)],mean,na.rm=T)
  mean_MAE_f<-sapply(tb_MAE[,4:ncol(tb_MAE_f)],mean,na.rm=T)
  mean_RMSE_f<-sapply(tb_RMSE_f[,4:ncol(tb_RMSE_f)],mean,na.rm=T)
  mean_list<-list(mean_RMSE,mean_MAE,mean_R2,mean_ME,mean_MAE_f,mean_RMSE_f)
  names(mean_list)<-c("RMSE","MAE","R2","ME","MAE_f","RMSE_f")
  method_mean[[k]]<-mean_list
  names_methods<-obj_names
  
  sd_RMSE<-sapply(tb_RMSE[,4:ncol(tb_RMSE)],sd,na.rm=T)
  sd_MAE<-sapply(tb_MAE[,4:ncol(tb_MAE)],sd,na.rm=T)
  
  # Now create plots
  
  png(paste("RMSE_for_",names_methods[k],out_prefix,".png", sep=""))
  boxplot(tb_RMSE[,4:ncol(tb_RMSE)],main=names_methods[k],ylim=c(1,4.5),
          ylab= "RMSE", outline=FALSE) #ADD TITLE RELATED TO METHODS...
  dev.off()
  
  #boxplot(tb_RMSE[,4:ncol(tb_RMSE)],main=names_methods[k],outline=FALSE) #ADD TITLE RELATED TO METHODS...
  png(paste("MAE_for_",names_methods[k],out_prefix,".png", sep=""))
  boxplot(tb_MAE[,4:ncol(tb_MAE)],main=names_methods[k], ylim=c(1,3.5),
          ylab= "MAE", outline=FALSE) #ADD TITLE RELATED TO METHODS...
  dev.off()
  
  #boxplot(tb_RMSE[,4:ncol(tb_RMSE)],main=names_methods[k],outline=FALSE) #ADD TITLE RELATED TO METHODS...
  png(paste("ME_for_",names_methods[k],out_prefix,".png", sep=""))
  boxplot(tb_ME[,4:ncol(tb_MAE)],main=names_methods[k],
          ylab= "ME", outline=FALSE) #ADD TITLE RELATED TO METHODS...
  dev.off()

  # OVER THE YEAR
  #...
  for(i in 1:nrow(tb)){
    date<-tb$dates[i]
    date<-strptime(date, "%Y%m%d")
    tb$month[i]<-as.integer(strftime(date, "%m"))
  }
  # USE RESHAPE...
  mod_pat<-glob2rx("mod*")   
  var_pat<-grep(mod_pat,names(tb),value=TRUE) # using grep with "value" extracts the matching names
  tb_melt<-melt(tb,
                    measure=var_pat, 
                     id=c("metric","month"),
                   na.rm=F)
  tb_cast<-cast(tb_melt,metric+month~variable,mean)
  
                    metrics<-as.character(unique(tb$metric))            #Name of accuracy metrics (RMSE,MAE etc.)
                    tb_metric_list<-vector("list",length(metrics))
               
                    
  png(paste("MAE_for_",names_methods[k],out_prefix,".png", sep=""))
  boxplot(tb__m_MAE[,4:ncol(tb_MAE)],main=names_methods[k], ylim=c(1,3.5),
          ylab= "MAE", outline=FALSE) #ADD TITLE RELATED TO METHODS...
  dev.off()
  metrics<-as.character(unique(tb$metric))            #Name of accuracy metrics (RMSE,MAE etc.)
  tb_metric_list<-vector("list",length(metrics))
                                        
  for(i in 1:length(metrics)){            # Reorganizing information in terms of metrics 
        metric_name<-paste("tb_t_",metrics[i],sep="")
         tb_metric<-subset(tb, metric==metrics[i])
         assign(metric_name,tb_metric)
        tb_metric_list[[i]]<-tb_metric
  }     
  
   tb_processed<-tb_metric_list[[i]]     
   mod_pat<-glob2rx("mod*")   
   var_pat<-grep(mod_pat,names(tb_processed),value=FALSE) # using grep with "value" extracts the matching names         
   na_mod<-colSums(!is.na(tb_processed[,var_pat]))
   for (j in 4:ncol(tb)){    
      if (na_mod[j-3]<183){
      tb_ME<-tb_ME[,-j]   #Remove columns that have too many missing values!!!
   }
   
}

mod_formulas<-vector("list",length(method_mod))
for(k in 1:length(method_mod)){            # start of the for main loop to all methods
  models_tmp<-method_mod[[k]][[1]][[5]]  #day 1 for model k
  list_formulas<-vector("list",length(models_tmp))
  for (j in 1:length(models_tmp)){  #
    formula<-try(formula(models_tmp[[j]]))
    list_formulas[[j]]<-formula
  }
  names(list_formulas)<-names(models_tmp)
  mod_formulas[[k]]<-list_formulas
}

names(method_mean)<-obj_names
#Add summary mean graphs!! HERE

write.table(as.data.frame(method_mean$gam_fus_mod1$MAE), "methods_mean_gam_MAE_test1.txt", sep=",")
write.table(as.data.frame(method_mean$fus_CAI$MAE), "methods_mean_fus_CAI_MAE_test1.txt", sep=",")

######### Average per month

#Add code here...
gam_fus_mod1<-method_mod[[1]]


#####################   PART II   #######################
# VISUALIZATION OF RESULTS PLOTS ACROSS MODELS FOR METHODS

date_selected<-"20100103"

lf_krig<-list.files(pattern=paste("*",date_selected,"_07312012_365d_Kriging_autokrig2.rst$",sep=""))
lf_gwr<-list.files(pattern=paste("*",date_selected,".*08152012_1d_gwr4.rst$",sep=""))
lf_gam1<-list.files(pattern=paste("^GAM.*",date_selected,"_07242012_365d_GAM_fusion5.rst$",sep=""))
lf_fus1<-list.files(pattern=paste("*.tmax_predicted.*.",date_selected,".*._365d_GAM_fusion_lstd_10062012.rst$",sep=""))
lf_cai1<-list.files(pattern=paste("*CAI_tmax_pred.*",date_selected,"*.08072012_365d_GAM_CAI2.rst$",sep="")) #Search for files in relation to fusion
lf_gam2<-list.files(pattern=paste("^GAM.*",date_selected,"_08122012_365d_GAM_fusion6.rst$",sep=""))

#lf2_fus<-list.files(pattern=paste("*",date_selected,"*._365d_GAM_fusion_lstd_10062012.rst$",sep=""))
#lf2_fus<-list.files(pattern=paste("*.20100103.*._365d_GAM_fusion_lstd_10062012.rst$",sep=""))
lf2_fus<-list.files(pattern=paste("^fusion_tmax.*",date_selected,"_07242012_365d_GAM_fusion5.rst$",sep="")) #Search for files in relation to fusion


d_krig_rast<-stack(lf_krig)
d_gwr_rast<-stack(lf_gwr)
d_gam1_rast<-stack(lf_gam1)
d_fus1_rast<-stack(lf_fus1)
d_cai1_rast<-stack(lf_cai1)
d_gam2_rast<-stack(lf_gam2)

list_day_method<-list(d_krig_rast,d_gwr_rast,d_gam1_rast,d_fus1_rast,d_cai1_rast,d_gam2_rast)
names(list_day_method)<-paste(c("krig_","gwr_","gam1_","fus1_","cai1_","gam2_"),date_selected,sep="")
out_prefix2<-"_10172012"

for (k in 1:length(list_day_method)){
  
  predictions<-list_day_method[[k]]
  projection(predictions)<-proj_str
  predictions<-mask(predictions,mask_elev_NA)
  #layerNames(predictions)<-c(paste('fusion',date_selected,sep=" "),paste('CAI',date_list2[[k]],sep=" "))
  # use overall min and max values to generate an nice, consistent set
  # of breaks for both colors (50 values) and legend labels (5 values)
  #s.range <- c(min(minValue(predictions)), max(maxValue(predictions)))
  s.range<-c(-12,18)
  col.breaks <- pretty(s.range, n=60)
  lab.breaks <- pretty(s.range, n=6)
  temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
  
  # plot using these (common) breaks; note use of _reverse_ heat.colors,
  # making it so that larger numbers are redder
  X11(6,12)
  plot(predictions, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
       axis=list(at=lab.breaks, labels=lab.breaks))
  
  savePlot(paste(names(list_day_method)[[k]],"_method_prediction_",out_prefix2,".png", sep=""), type="png")
  dev.off()  
}

                
##PLOTING OF ONE DATE TO COMPARE METHODS!!!

pos<-match("ELEV_SRTM",layerNames(s_raster))
ELEV_SRTM<-raster(s_raster,pos)
elev<-ELEV_SRTM
elev[-0.050<elev]<-NA  #Remove all negative elevation lower than 50 meters...

mask_elev_NA<-elev> (-0.050)
  
date_selected<-"20100103"
lf_fus<-list.files(pattern=paste("^fusion_tmax.*",date_selected,"_07242012_365d_GAM_fusion5.rst$",sep="")) #Search for files in relation to fusion
lf_cai<-list.files(pattern=paste("*CAI_tmax_pred.*",date_selected,"*.08072012_365d_GAM_CAI2.rst$",sep="")) #Search for files in relation to fusion

r11<-raster(lf_fus) #Fusion
r12<-raster(lf_cai[1]) #CAI
predictions<-stack(r11,r12)
predictions<-mask(predictions,mask_land_elev_NA)
layerNames(predictions)<-c(paste('fusion',"20100103",sep=" "),paste('CAI',"20100103",sep=" "))

s.range <- c(min(minValue(predictions)), max(maxValue(predictions)))
col.breaks <- pretty(s.range, n=50)
lab.breaks <- pretty(s.range, n=5)
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))

# plot using these (common) breaks; note use of _reverse_ heat.colors,
# making it so that larger numbers are redder
X11(6,12)
#plot(predictions, breaks=col.breaks, col=rev(heat.colors(length(col.breaks)-1)),
#   axis=list(at=lab.breaks, labels=lab.breaks))
plot(predictions, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
     axis=list(at=lab.breaks, labels=lab.breaks))
#plot(reg_outline, add=TRUE)
savePlot(paste("comparison_one_date_CAI_fusion_tmax_prediction_",date_selected,out_prefix,".png", sep=""),type="png")

#results2_fusion_Assessment_measure_all_365d_GAM_fusion_lstd_10062012.RData
#### END OF THE SCRIPT

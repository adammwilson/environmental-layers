#####################################  METHOD COMPARISON ##########################################
#################################### CLIMATE INTERPOLATION ########################################
#This script utilizes the R ojbects created during the interpolation phase.                       #
#At this stage the stcrip produce figures of various accuracy metrics.                            #
#AUTHOR: Benoit Parmentier                                                                        #
#DATE: 08/26/2012                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#??--                                   #
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
library(gpclib)
library(maptools)
library(graphics)
library(parallel)                            # Urbanek S. and Ripley B., package for multi cores & parralel processing
library(raster)
library(rasterVis)
library(plotrix)   #Draw circle on graph

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

obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
#path<-"/Users/benoitparmentier/Dropbox/Data/NCEAS/Oregon_covariates/"            #Local dropbox folder on Benoit's laptop
setwd(path) 
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
                                                                                #Number of kriging model
out_prefix<-"methods_08262012_"                                              #User defined output prefix

lines<-read.table(paste(path,"/",inlistf,sep=""), sep="")                      #Column 1 contains the names of raster files
inlistvar<-lines[,1]
inlistvar<-paste(path,"/",as.character(inlistvar),sep="")

#mention this is the last... files

### RESULTS COMPARISON

# PART 1 : using R object created during the interpolation phase

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

#Condense and add other comparison 

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
    tb_RMSE<-tb_RMSE[,-j]   #Remove columns that has too many missing values!!!
  }
}

na_mod<-colSums(!is.na(tb_MAE[,4:ncol(tb)]))
for (j in 4:ncol(tb)){
  
  if (na_mod[j-3]<183){
    tb_MAE<-tb_MAE[,-j]   #Remove columns that has too many missing values!!!
  }
}

na_mod<-colSums(!is.na(tb_MAE_f[,4:ncol(tb)])) 
for (j in 4:ncol(tb)){
  
  if (na_mod[j-3]<183){
    tb_MAE_f<-tb_MAE_f[,-j]   #Remove columns that has too many missing values!!!
  }
}

na_mod<-colSums(!is.na(tb_ME[,4:ncol(tb)]))
for (j in 4:ncol(tb)){
  
  if (na_mod[j-3]<183){
    tb_ME<-tb_ME[,-j]   #Remove columns that has too many missing values!!!
  }
}

#Add assessment of missing prediction over the year.

mean_RMSE<-sapply(tb_RMSE[,4:ncol(tb_RMSE)],mean,na.rm=T)
mean_MAE<-sapply(tb_MAE[,4:ncol(tb_MAE)],mean,na.rm=T)
mean_R2<-sapply(tb_R2[,4:ncol(tb_R2)],mean, n.rm=T)
mean_ME<-sapply(tb_ME[,4:ncol(tb_ME)],mean,na.rm=T)
mean_MAE_f<-sapply(tb_MAE[,4:ncol(tb_MAE_f)],mean,na.rm=T)
mean_RMSE_f<-sapply(tb_RMSE_f[,4:ncol(tb_RMSE_f)],mean,na.rm=T)
mean_list<-list(mean_RMSE,mean_MAE,mean_R2,mean_ME,mean_MAE_f,mean_RMSE_f)
names(mean_list)<-c("RMSE","MAE","R2","ME","MAE_f","RMSE_f")
method_mean[[k]]<-mean_list
names_methods<-obj_names

# Now create plots

png(paste("RMSE_for_",names_methods[k],out_prefix,".png", sep=""))
boxplot(tb_RMSE[,4:ncol(tb_RMSE)],main=names_methods[k],
        ylab= "RMSE", outline=FALSE) #ADD TITLE RELATED TO METHODS...
dev.off()

#boxplot(tb_RMSE[,4:ncol(tb_RMSE)],main=names_methods[k],outline=FALSE) #ADD TITLE RELATED TO METHODS...
png(paste("MAE_for_",names_methods[k],out_prefix,".png", sep=""))
boxplot(tb_MAE[,4:ncol(tb_MAE)],main=names_methods[k],
        ylab= "MAE", outline=FALSE) #ADD TITLE RELATED TO METHODS...
dev.off()

#boxplot(tb_RMSE[,4:ncol(tb_RMSE)],main=names_methods[k],outline=FALSE) #ADD TITLE RELATED TO METHODS...
png(paste("ME_for_",names_methods[k],out_prefix,".png", sep=""))
boxplot(tb_ME[,4:ncol(tb_MAE)],main=names_methods[k],
        ylab= "ME", outline=FALSE) #ADD TITLE RELATED TO METHODS...
dev.off()

}

names(method_mean)<-obj_names
#Add summary mean graphs!! HERE

write.table(as.data.frame(method_mean$gam_fus_mod1$MAE), "methods_mean_gam_MAE_test1.txt", sep=",")
write.table(as.data.frame(method_mean$fus_CAI$MAE), "methods_mean_fus_CAI_MAE_test1.txt", sep=",")

#### END OF THE SCRIPT

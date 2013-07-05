######################################## Paper Methods_comparison_FSS #######################################
############################ Scripts for figures and analyses for the the IBS poster #####################################
#This script performs analyses and create figures for the FSS paper.
#It uses inputs from interpolation objects created at earlier stages...                          #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 06/27/2013                                                                                #
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

script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/"
source(file.path(script_path,"interpolation_method_day_function_multisampling_06082013.R")) #Include GAM_day

## Parmeters  

in_dir<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_GAM_fus_all_lst_05312013"
setwd(in_dir)
y_var_name <- "dailyTmax"
y_var_month <- "TMax"
#y_var_month <- "LSTD_bias"

infile_covariates<-list.files(pattern="covariates.*.tif")
out_prefix<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_GAM_fus_all_lst_05312013"
method_interpolation <- "gam_fusion"
raster_obj_file <- "raster_prediction_obj_gam_fusion_dailyTmax_365d_GAM_fus_all_lst_05312013.RData"
#Load objects containing training, testing, models objects 

#The names of covariates can be changed...these names should be output/input from covar script!!!
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev_s","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
#lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10") #use older version for continuity check to be changed
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
covar_names<-c(rnames,lc_names,lst_names)

#formulas<-

list_models<-c("y_var ~ s(elev_s)",
                 "y_var ~ s(LST)",
                 "y_var ~ s(elev_s,LST)",
                 "y_var ~ s(lat) + s(lon)+ s(elev_s)",
                 "y_var ~ s(lat,lon,elev_s)",
                 "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST)", 
                 "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(LC2)",  
                 "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(LC6)", 
                 "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(DISTOC)")

raster_prediction_obj <-load_obj(raster_obj_file)
#gam_cai_mod <-load_obj("results_mod_obj__365d_GAM_CAI4_all_12272012.RData")

names(raster_prediction_obj) #list of two objects

clim_method_mod_obj<-raster_prediction_obj$clim_method_mod_obj
raster_prediction_obj$summary_metrics_v

j<-1 #selected month for climatology 
i<-1
data_s <- raster_prediction_obj$method_mod_obj[[i]]$data_s #training data
data_v <- raster_prediction_obj$method_mod_obj[[i]]$data_v #testing data

data_month1 <- clim_method_mod_obj[[1]]$data_month #monthly data
data_month <- clim_method_mod_obj[[j]]$data_month #monthly data
clim_mod_obj_month <- clim_method_mod_obj[[j]]
names(clim_mod_obj_month)

#####
s_raster <- brick(infile_covariates)
names(s_raster)<-covar_names
data_s$y_var <- data_s[[y_var_name]]
formula<-"y_var ~ s(lat,lon,elev_s)"


### MONTH MODELS

data_month$y_var<-data_month[[y_var_month]]
mod_mgam1_s <- gam(y_var ~ s(lat,lon,elev_s),data=data_month) 
mod_mgam1_s 

raster_pred_s<- predict(object=s_raster,model=mod_mgam1_s,na.rm=FALSE)
names(raster_pred_s)<-"raster_pred_s"
plot(raster_pred_s)
plot(data_month, add=TRUE)
levelplot(raster_pred_s)

mod_mgam1_te <- gam(y_var ~ te(lat,lon,elev_s),data=data_month)
mod_mgam1_te

raster_pred_te<- predict(object=s_raster,model=mod_mgam1_te,na.rm=FALSE)
names(raster_pred_te)<- "raster_pred_te"

plot(raster_pred_te)
plot(data_month, add=TRUE)

raster_comp <- stack(raster_pred_te,raster_pred_s)
plot(raster_comp)
levelplot(raster_comp)

diff<- raster_pred_s -raster_pred_te
freq(diff)
plot(diff)

vis.gam(mod_mgam1_s,view=c("elev_s","lon"))
vis.gam(mod_mgam1_te,view=c("elev_s","lon"))

vis.gam(mod_mgam1_te,view=c("elev_s","lon"),theta=210,phi=40)
vis.gam(mod_mgam1_s,view=c("elev_s","lon"),theta=210,phi=40)

vis.gam(mod_mgam1_s,view=c("elev_s","lon"),theta=210,phi=40,plot.type="contour")
vis.gam(mod_mgam1_te,view=c("elev_s","lon"),theta=210,phi=40,plot.type="contour")

### USING LST and elev_s

data_month<-data_month[data_month$month==j,] #Subsetting dataset for the relevant month of the date being processed
LST_name<-lst_avg[j] # name of LST month to be matched
data_month$LST<-data_month[[LST_name]]
pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
LST<-subset(s_raster,LST_name)
names(LST)<-"LST"
s_raster<-addLayer(s_raster,LST)            #Adding current month

mod_mgam1_s <- gam(y_var ~ s(LST,elev_s),data=data_month) 
mod_mgam1_s 

raster_pred_s <- predict(object=s_raster,model=mod_mgam1_s,na.rm=FALSE)
names(raster_pred_s)<-"raster_pred_s"
plot(raster_pred_s)
plot(data_month, add=TRUE)
levelplot(raster_pred_s)

mod_mgam1_te <- gam(y_var ~ te(LST,elev_s),data=data_month)
mod_mgam1_te

raster_pred_te<- predict(object=s_raster,model=mod_mgam1_te,na.rm=FALSE)
names(raster_pred_te)<- "raster_pred_te"

plot(raster_pred_te)
plot(data_month, add=TRUE)

raster_comp <- stack(raster_pred_te,raster_pred_s)
plot(raster_comp)
levelplot(raster_comp)

diff<- raster_pred_s -raster_pred_te
freq(diff)
plot(diff)

vis.gam(mod_mgam1_s,view=c("LST","elev_s"))
vis.gam(mod_mgam1_te,view=c("LST","elev_s"))

vis.gam(mod_mgam1_te,view=c("LST","elev_s"),theta=210,phi=40)
vis.gam(mod_mgam1_s,view=c("LST","elev_s"),theta=210,phi=40)

vis.gam(mod_mgam1_s,view=c("LST","elev_s"),theta=210,phi=40,plot.type="contour")
vis.gam(mod_mgam1_te,view=c("LST","elev_s"),theta=210,phi=40,plot.type="contour")

### TEST USING TI() bases

mod_mgam1_ti <-gam(y_var~ ti(LST, elev_s),data=data_month)
mod_mgam1_ti

raster_pred_ti<- predict(object=s_raster,model=mod_mgam1_ti,na.rm=FALSE)
names(raster_pred_ti)<- "raster_pred_ti"

plot(raster_pred_ti)

vis.gam(mod_mgam1_ti,view=c("LST","elev_s"))

vis.gam(mod_mgam1_ti,view=c("LST","elev_s"),theta=210,phi=40)

### USE DIFFERENT BASE

mod_mgam1_s <- gam(y_var ~ s(LST,elev_s,bs="cr"),data=data_month) #DOES NOT WORK SINCE CR ONLY HANDLES ONE VARIABLE
mod_mgam1_s <- gam(y_var ~ s(LST,elev_s,bs="cc"),data=data_month) #DOES NOT WORK SINCE CR ONLY HANDLES ONE VARIABLE

mod_mgam1_s <- gam(y_var ~ s(LST,elev_s,bs="ps"),data=data_month) #DOES NOT WORK SINCE CR ONLY HANDLES ONE VARIABLE
mod_mgam1_s <- gam(y_var ~ s(LST,bs="ps"),data=data_month) #DOES NOT WORK SINCE CR ONLY HANDLES ONE VARIABLE

mod_mgam1_s 
summary(mod_mgam1_s)
mod_mgam1_s <- gam(y_var ~ s(LST,bs="tp"),data=data_month)
summary(mod_mgam1_s)



raster_pred_s <- predict(object=s_raster,model=mod_mgam1_s,na.rm=FALSE)
names(raster_pred_s)<-"raster_pred_s"
plot(raster_pred_s)
plot(data_month, add=TRUE)
levelplot(raster_pred_s)

mod_mgam1_te <- gam(y_var ~ te(LST,elev_s),data=data_month)
mod_mgam1_te

raster_pred_te<- predict(object=s_raster,model=mod_mgam1_te,na.rm=FALSE)
names(raster_pred_te)<- "raster_pred_te"

plot(raster_pred_te)
plot(data_month, add=TRUE)

raster_comp <- stack(raster_pred_te,raster_pred_s)
plot(raster_comp)
levelplot(raster_comp)



###############################
## DAILY MODELS USING GAM

k_dim <- 20
#y_var_min<-range(data_s$y_var)[1]
#y_var_max<-range(data_s$y_var)[2]
mod_dgam1 <- gam(y_var ~ s(lat,lon,elev_s),data=data_s)  #not working

#gamFit2 <- gam(y_var ~ s(lat,lon,elev_s,k=k_dim),knots=list( x=seq(from=y_var_min,to=y_var_max, len=k_dim),data=data_s))  
k_dim <- 20
mod_dgam2 <- gam(y_var ~ s(lat,lon,elev_s,k=k_dim),
                 knots=list(lat=seq(from=range(data_s$lat,na.rm=TRUE)[1],to=range(data_s$lat,na.rm=TRUE)[2], len=k_dim),
                            lon=seq(from=range(data_s$lon,na.rm=TRUE)[1],to=range(data_s$lon,na.rm=TRUE)[2], len=k_dim),
                            elev_s=seq(from=range(data_s$elev_s,na.rm=TRUE)[1],to=range(data_s$elev_s,na.rm=TRUE)[2], len=k_dim)),
                 data=data_s)  
mod_dgam2 
#raster_pred<- predict(object=r_stack,model=mod,na.rm=FALSE)
raster_pred<- predict(object=s_raster,model=mod_gam2,na.rm=FALSE)

mod_gam3 <-gam(y_var~ te(lat,lon,elev_s),data=data_s)

mod_dgam4 <- gam(y_var ~ te(lat,lon,elev_s,k=k_dim),
                knots=list(lat=seq(from=range(data_s$lat,na.rm=TRUE)[1],to=range(data_s$lat,na.rm=TRUE)[2], len=k_dim),
                           lon=seq(from=range(data_s$lon,na.rm=TRUE)[1],to=range(data_s$lon,na.rm=TRUE)[2], len=k_dim),
                           elev_s=seq(from=range(data_s$elev_s,na.rm=TRUE)[1],to=range(data_s$elev_s,na.rm=TRUE)[2], len=k_dim)),
                data=data_s)  

mod_dgam1_ti <-gam(y_var~ ti(lat,lon,elev_s),data=data_s)
mod_dgam1_ti

raster_pred_ti<- predict(object=s_raster,model=mod_mgam1_ti,na.rm=FALSE)
names(raster_pred_ti)<- "raster_pred_ti"

plot(raster_pred_ti)

test_gam <- gam(y_var ~ ti(lat)+ti(lon)+ti(elev_s)+ti(lat,lon)+ti(lat,elev_s)+ti(lon,elev_s)+ti(lat,lon,elev_s),data=data_s)
test_gam <- gam(y_var ~ ti(lat)+ti(lon)+ti(elev_s)+ti(lat,lon)+ti(lat,elev_s)+ti(lon,elev_s),data=data_s)
test_gam <- gam(y_var ~ ti(lat)+ti(lon)+ti(elev_s)+ti(lat,lon)+ti(lat,elev_s),data=data_s)
test_gam <- gam(y_var ~ ti(lat)+ti(lon)+ti(elev_s)+ti(lat,lon)+ti(lat,lon,elev_s),data=data_s)

test_gam <- gam(y_var ~ ti(lat)+ti(lon)+ti(elev_s)+ti(lat,lon,elev_s),data=data_s)
test_gam
raster_pred_ti<- predict(object=s_raster,model=test_gam,na.rm=FALSE)
names(raster_pred_ti)<- "raster_pred_ti"
plot(raster_pred_ti)


k_dim <- 35 #takes more than 2hours and does not fit a model

mod_gam5 <- gam(y_var ~ te(lat,lon,elev_s,k=k_dim),
                knots=list(lat=seq(from=range(data_s$lat,na.rm=TRUE)[1],to=range(data_s$lat,na.rm=TRUE)[2], len=k_dim),
                           lon=seq(from=range(data_s$lon,na.rm=TRUE)[1],to=range(data_s$lon,na.rm=TRUE)[2], len=k_dim),
                           elev_s=seq(from=range(data_s$elev_s,na.rm=TRUE)[1],to=range(data_s$elev_s,na.rm=TRUE)[2], len=k_dim)),
                data=data_s)  



#CONTRIBUTION OF VARIABLES...

#myModels<-clim_mod_obj_month$mod[1:6]
myModels<-method_mod_obj$mod

summary_list <- lapply(myModels, summary)
s.table_list <- lapply(summary_list, `[[`, 's.table')
p.table_list <- lapply(summary_list, `[[`, 'p.table')
#now put in one table
name_col<-function(i,x){
  x_mat<-x[[i]]
  x_df<-as.data.frame(x_mat)
  x_df$mod_name<-rep(names(x)[i],nrow(x_df))
  x_df$term_name <-row.names(x_df)
  return(x_df)
}

s.table_list2<-lapply(1:6,name_col,s.table_list)
p.table_list2<-lapply(1:6,name_col,p.table_list)
s.table_term <-do.call(rbind,s.table_list2)
#p.table_term <-do.call(rbind,p.table_list2)

table_term <- rbind(p.table_term,s.table_term)
test

#Testing one variable at a time:

#Models to run...this can be change for each run

list_models<-c("y_var ~ s(elev_s)",
               "y_var ~ s(LST)",
               "y_var ~ s(lat)",
               "y_var ~ s(DISTOC)",
               "y_var ~ s(lon)",
               "y_var ~ s(LC2)", 
               "y_var ~ s(LC6)",  
               "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(LC6)", 
               "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(DISTOC)")




t<-clim_mod_obj_month$mod[!sapply(clim_mod_obj_month$mod,is.null)] #remove NULL elements in list
#t<-method_mod_obj[[1]]$mod[!sapply(method_mod_obj$mod[[1]],is.null)] #remove NULL elements in list
t<-method_mod_obj[[1]]$mod[!sapply(method_mod_obj[[1]]$mod,inherits,"try_errors")] #remove NULL elements in list
inherits(clim_mod_obj_month$mod[[7]],"try-error")

myModels <- clim_mod_obj_month$mod[]
if(inherits(myModel,"gam")){
  list_mod[[j]] <- myModel
}

########### GET MOD OBJECT
remove_errors_list<-function(list_items){

  list_tmp<-list_items
  for(i in 1:length(list_items)){
    
    if(inherits(list_items[[i]],"try-error")){
      list_tmp[[i]]<-0
    }else{
      list_tmp[[i]]<-1
    }
  }
  cnames<-names(list_tmp[list_tmp>0])
  x<-list_items[match(cnames,names(list_items))]
  return(x)
}

myModels<-remove_errors_list(t$mod)
#could add AIC, GCV to the table as well as ME, RMSE...+dates...

#mean contribution for terms in dffiernt models over full year...

myParameters = lapply(1:length(myModels),function(x){ if(x==1)
paste("myModels[[",x,"]]",sep = "") 
paste("force(myModels[[",x,"]])",sep = "")})
myParStr = toString(paste( myParameters  ))
eval(parse(text = paste("anova(",myParStr,")")))
eval(parse(text = paste("aov(",myParStr,")")))

#TO DEAL WITH NA
#1) all.vars(formula) #get all the terms for all formulas...
#2) do na.omit...
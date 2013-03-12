##################    Master script for temperature predictions  #######################################
############################ TMIN AND TMAX predictions ##########################################
#                           
##This script produces intperpolated surface of TMIN and TMAX for specified processing region given sets 
#of inputs and parameters.
#STAGE 1: LST climatology calculation
#STAGE 2: Covariates preparation: aspect, land cover, distance to coast etc.
#STAGE 3: Data preparation: database query, extraction of covariates from stack
#STAGE 4: Raster prediction: run interpolation method -- gam fusion, gam CAI, ...
#STAGE 5: Output analyses-visualization of results for specific dates...
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE: 03/12/2013                                                                                 

#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363, TASK$568--   

##Comments and TODO:
# Modify code for stage 1 and call python script from R
# Modify code for stage 2, make it a function and fully automated (distoc var)
# Add options to run only specific stage + additional out_suffix?
# Make master script a function?
# Add log file for master script,add function to collect inputs and outputs
##################################################################################################

###Loading R library and packages   
library(RPostgreSQL)
library(maps)
library(maptools)
library(parallel)
library(gtools)                              # loading some useful tools 
library(mgcv)                                # GAM package by Simon Wood
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                              # NCAR Spatial Interpolation methods such as kriging, splines
library(raster)                              # Hijmans et al. package for raster processing
library(rasterVis)
library(reshape)
library(plotrix)

### Parameters and arguments

script_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/"
#list_script_files<-
stages_to_run<-c(1,2,3,4,5) #May decide on antoher strategy later on...

#####SCRIPT USED FOR THE PREDICTIONS

#master_script_temp_03052013.R

#IN MASTER SCRIPT:

#/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/climatology_01252013b.py
#/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/covariates_production_temperatures_02062013.R

#source(file.path(script_path,"Database_stations_covariates_processing_function_03052013.R"))
#source(file.path(script_path,"GAM_fusion_analysis_raster_prediction_multisampling_03052013.R"))
#source(file.path(script_path,"results_interpolation_date_output_analyses_03052013.R"))

#CALLED FROM GAM FUSION ANALYSIS RASTER PREDICTION

#source(file.path(script_path,"sampling_script_functions_03052013.R"))
#source(file.path(script_path,"GAM_fusion_function_multisampling_03052013.R")) #Include GAM_CAI
#source(file.path(script_path,"GAM_fusion_function_multisampling_validation_metrics_02262013.R"))


############ STAGE 1: LST Climatology ###############

if (stages_to_run[1]==1){
  #Call run through python
  #/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/climatology_01252013b.py
}

############ STAGE 2: Covariate production ################

if (stages_to_run[2]==2){
  #Transform into function...
  #/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/covariates_production_temperatures_02062013.R
}

############# STAGE 3: Data preparation ###############

source(file.path(script_path,"Database_stations_covariates_processing_function_03052013.R"))

#Setting up input argurments for script function...

db.name <- "ghcn"       # name of the Postgres database
var <- "TMAX"           # name of the variables to keep: TMIN, TMAX or PRCP
range_years<-c("2010","2011") #right bound not included in the range!!
range_years_clim<-c("2000","2011") #right bound not included in the range!!
infile1<- "outline_venezuela_region__VE_01292013.shp"      #This is the shape file of outline of the study area                                                      #It is an input/output of the covariate script
infile2<-"/home/layers/data/climate/ghcn/v2.92-upd-2012052822/ghcnd-stations.txt"                              #This is the textfile of station locations from GHCND
infile_covariates<-"covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script and used in stage 3 and stage 4
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84: same as earlier
in_path <- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data/"
out_prefix<-"_365d_GAM_fus5_all_lstd_03052013"                #User defined output prefix
#qc_flags<-    flags allowe for the query from the GHCND??

#The names of covariates can be changed...these names should be output/input from covar script!!!
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
covar_names<-c(rnames,lc_names,lst_names)

#list of 11 parameters for input in the function...

list_param_prep<-list(db.name,var,range_years,range_years_clim,infile1,infile2,infile_covariates,CRS_locs_WGS84,in_path,covar_names,out_prefix)
cnames<-c("db.name","var","range_years","range_years_clim","infile1","infile2","infile_covariates","CRS_locs_WGS84","in_path","covar_names","out_prefix")
names(list_param_prep)<-cnames

##### RUN SCRIPT TO GET STATION DATA WITH COVARIATES #####
list_outfiles<-database_covaratiates_preparation(list_param_prep)

############### STAGE 4: RASTER PREDICTION #################

#Prepare parameters for for raster prediction... 

#Collect parameters from the previous stage: data preparation stage

#3 parameters from output
infile_monthly<-list_outfiles$monthly_covar_ghcn_data #outile4 from database_covar script
infile_daily<-list_outfiles$daily_covar_ghcn_data  #outfile3 from database_covar script
infile_locs<- list_outfiles$loc_stations_ghcn #outfile2? from database covar script

list_param_data_prep<-list(infile_monthly,infile_daily,infile_locs,infile_covariates,covar_names,var,out_prefix,CRS_locs_WGS84)

names(list_param_data_prep)<-c("infile_monthly","infile_daily","infile_locs","infile_covariates","covar_names","var","out_prefix","CRS_locs_WGS84")

#Set additional parameters
#Input for sampling function...
seed_number<- 100  #if seed zero then no seed?     
nb_sample<-1           #number of time random sampling must be repeated for every hold out proportion
step<-0         
constant<-0             #if value 1 then use the same samples as date one for the all set of dates
prop_minmax<-c(0.3,0.3)  #if prop_min=prop_max and step=0 then predicitons are done for the number of dates...
infile_dates<-"list_365_dates_04212012.txt"

#Models to run...this can be change for each run
list_models<-c("y_var ~ s(elev_1)",
               "y_var ~ s(LST)",
               "y_var ~ s(elev_1,LST)",
               "y_var ~ s(lat) + s(lon)+ s(elev_1)",
               "y_var ~ s(lat,lon,elev_1)",
               "y_var ~ s(lat,lon) + s(elev_1) + s(N_w,E_w) + s(LST)", 
               "y_var ~ s(lat,lon) + s(elev_1) + s(N_w,E_w) + s(LST) + s(LC2)",
               "y_var ~ s(lat,lon) + s(elev_1) + s(N_w,E_w) + s(LST) + s(LC6)", 
               "y_var ~ s(lat,lon) + s(elev_1) + s(N_w,E_w) + s(LST) + s(DISTOC)")

#Choose interpolation method...
interpolation_method<-c("gam_fusion","gam_CAI") #other otpions to be added later

#Default name of LST avg to be matched               
lst_avg<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")  
in_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data"
#Create on the fly output folder...
out_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data"
script_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/"

#Collect all parameters in a list
list_param_raster_prediction<-list(list_param_data_prep,
                                seed_number,nb_sample,step,constant,prop_minmax,infile_dates,
                                list_models,lst_avg,in_path,out_path,script_path,
                                interpolation_method)

names(list_param_raster_prediction)<-c("list_param_data_prep",
                                "seed_number","nb_sample","step","constant","prop_minmax","infile_dates",
                                "list_models","lst_avg","in_path","out_path","script_path",
                                "interpolation_method")

#Source file
source(file.path(script_path,"GAM_fusion_analysis_raster_prediction_multisampling_03122013b.R"))

#Make the function call
raster_prediction_gam_fus_obj <-raster_prediction_gam_fusion(list_param_raster_prediction)


############## STAGE 5: OUTPUT ANALYSES ##################

source(file.path(script_path,"results_interpolation_date_output_analyses_03052013.R"))


###############   END OF SCRIPT   ###################
#####################################################


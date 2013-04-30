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
#DATE: 05/01/2013                                                                                 

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
script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/"
#script_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/"
#list_script_files<-
#stages_to_run<-c(1,2,3,4,5) #May decide on antoher strategy later on...
stages_to_run<-c(0,0,3,4,5) #May decide on antoher strategy later on...

#####SCRIPT USED FOR THE PREDICTIONS: Source all scripts here to avoid confusion on versions being run!!!!

#source(file.path(script_path,"master_script_temp_04022013.R")) #Master script can be run directly...

#CALLED FROM MASTER SCRIPT:

#/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/climatology_03192013.py
source(file.path(script_path,"covariates_production_temperatures_03212013.R"))
source(file.path(script_path,"Database_stations_covariates_processing_function_04042013.R"))
source(file.path(script_path,"GAM_fusion_analysis_raster_prediction_multisampling_04302013.R"))
source(file.path(script_path,"results_interpolation_date_output_analyses_04302013.R"))
#source(file.path(script_path,"results_covariates_database_stations_output_analyses_04012013.R"))

#FUNCTIONS CALLED FROM GAM ANALYSIS RASTER PREDICTION ARE FOUND IN...

source(file.path(script_path,"sampling_script_functions_03122013.R"))
source(file.path(script_path,"GAM_fusion_function_multisampling_04302013.R")) #Include GAM_CAI
source(file.path(script_path,"GAM_fusion_function_multisampling_validation_metrics_04302013.R"))

############ STAGE 1: LST Climatology ###############

if (stages_to_run[1]==1){
  #Call run through python
  #/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/climatology_03182013.py
}

############ STAGE 2: Covariate production ################

##Paths to inputs and output
var<-"TMAX"
in_path  <- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data/"
out_path <- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data/"
#in_path <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/input"
#out_path <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output"


lc_path<-"/home/layers/data/land-cover/lc-consensus-global"
infile_modis_grid<-"modis_sinusoidal_grid_world.shp" #Give path!!! NEED TO CHANGE THIS...

infile_elev<-"/home/layers/data/terrain/dem-cgiar-srtm-1km-tif/srtm_1km.tif"  #this is the global file: replace later with the input produced by the DEM team
infile_canheight<-"/home/layers/data/land-cover/treeheight-simard2011/Simard_Pinto_3DGlobalVeg_JGR.tif"              #Canopy height
list_tiles_modis <- c('h11v08','h11v07','h12v07','h12v08','h10v07','h10v08') #tile for Venezuel and surrounding area
#list_tiles_modis <- c("h08v04","h09v04") #tiles for Oregon

infile_reg_outline=""  #input region outline defined by polygon: none for Venezuel
#infile_reg_outline <- "OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon

CRS_interp<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs";
#CRS_interp <-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";

CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
out_region_name<-"_venezuela_region" #generated on the fly
out_suffix<-"_VE_04162013"
ref_rast_name<-""  #local raster name defining resolution, exent, local projection--. set on the fly??
#ref_rast_name<-"mean_day244_rescaled.rst"  #local raster name defining resolution, exent: oregon

#The names of covariates can be changed...these names should be output/input from covar script!!!
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev_s","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
#lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10") #use older version for continuity check to be changed
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
covar_names<-c(rnames,lc_names,lst_names)

list_param_covar_production<-list(var,in_path,out_path,lc_path,infile_modis_grid,infile_elev,infile_canheight,
                                  list_tiles_modis,infile_reg_outline,CRS_interp,CRS_locs_WGS84,out_region_name,
                                  out_suffix,ref_rast_name,covar_names) 

names(list_param_covar_production)<-c("var","in_path","out_path","lc_path","infile_modis_grid","infile_elev","infile_canheight",
                                      "list_tiles_modis","infile_reg_outline","CRS_interp","CRS_locs_WGS84","out_region_name",
                                      "out_suffix","ref_rast_name","covar_names") 

if (stages_to_run[2]==2){
  #Transform into function...
  #/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/covariates_production_temperatures_03212013.R
  infile_covar_brick<-covariates_production_temperature(list_param_covar_production)
}

############# STAGE 3: Data preparation ###############

#Setting up input argurments for script function...
#set up earlier
#var <- "TMIN"           # name of the variables to keep: TMIN, TMAX or PRCP --already set up earlier

infile_covariates<-"covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script and used in stage 3 and stage 4
#infile_covariates<-"covariates__venezuela_region_TMIN__VE_03192013.tif" #covariates stack for TMIN
#infile_covariates<- "covariates_Oregon_region_TMAX__OR_04052013.tif" #Oregon covar TMAX from earlier codes...for continuity

#CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84: same as earlier
infile1<- "outline_venezuela_region__VE_01292013.shp"      #This is the shape file of outline of the study area                                                      #It is an input/output of the covariate script
#infile_reg_outline <- "OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
#infile1 <-"OR83M_state_outline.shp" #remove this parameter!!!

#covar_names see stage 2

#specific to this stage
db.name <- "ghcn"       # name of the Postgres database
range_years<-c("2010","2011") #right bound not included in the range!!
range_years_clim<-c("2000","2011") #right bound not included in the range!!
infile2<-"/home/layers/data/climate/ghcn/v2.92-upd-2012052822/ghcnd-stations.txt"                              #This is the textfile of station locations from GHCND
#in_path <- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data/"
out_prefix<-"_365d_GAM_CAI_all_lst_04162013"                #User defined output prefix
qc_flags_stations<-c("0","S")    #flags allowed for screening after the query from the GHCND??
#qc_flags_stations<-c("0")   #flags allowed for screening after the query from the GHCND??

#list of 12 parameters for input in the function...

list_param_prep<-list(db.name,var,range_years,range_years_clim,infile1,infile2,infile_covariates,CRS_locs_WGS84,in_path,covar_names,qc_flags_stations,out_prefix)
cnames<-c("db.name","var","range_years","range_years_clim","infile1","infile2","infile_covariates","CRS_locs_WGS84","in_path","covar_names","qc_flags_stations","out_prefix")
names(list_param_prep)<-cnames

##### RUN SCRIPT TO GET STATION DATA WITH COVARIATES #####

list_outfiles<-database_covariates_preparation(list_param_prep)

############### STAGE 4: RASTER PREDICTION #################

#Prepare parameters for for raster prediction... 

#Collect parameters from the previous stage: data preparation stage

#3 parameters from output
infile_monthly<-list_outfiles$monthly_covar_ghcn_data #outile4 from database_covar script
infile_daily<-list_outfiles$daily_covar_ghcn_data  #outfile3 from database_covar script
infile_locs<- list_outfiles$loc_stations_ghcn #outfile2? from database covar script

#names(outfiles_obj)<- c("loc_stations","loc_stations_ghcn","daily_covar_ghcn_data","monthly_covar_ghcn_data")

list_param_data_prep<-list(infile_monthly,infile_daily,infile_locs,infile_covariates,covar_names,var,out_prefix,CRS_locs_WGS84)
names(list_param_data_prep)<-c("infile_monthly","infile_daily","infile_locs","infile_covariates","covar_names","var","out_prefix","CRS_locs_WGS84")

#Set additional parameters
#Input for sampling function...
seed_number<- 100  #if seed zero then no seed?     
nb_sample<-1           #number of time random sampling must be repeated for every hold out proportion
step<-0         
constant<-0             #if value 1 then use the same samples as date one for the all set of dates
prop_minmax<-c(0.3,0.3)  #if prop_min=prop_max and step=0 then predicitons are done for the number of dates...
#dates_selected<-c("20100101","20100102","20100103","20100901") # Note that the dates set must have a specific format: yyymmdd
dates_selected<-"" # if empty string then predict for the full year specified earlier

#Models to run...this can be change for each run
list_models<-c("y_var ~ s(elev_s)",
               "y_var ~ s(LST)",
               "y_var ~ s(elev_s,LST)",
               "y_var ~ s(lat) + s(lon)+ s(elev_s)",
               "y_var ~ s(lat,lon,elev_s)",
               "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST)", 
               "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(LC2)",
               "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(LC6)", 
               "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(DISTOC)")

#Choose interpolation method...
#interpolation_method<-c("gam_fusion","gam_CAI") #other otpions to be added later
interpolation_method<-c("gam_CAI") #other otpions to be added later


#Default name of LST avg to be matched               
lst_avg<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")  
#in_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data"
#Create on the fly output folder...
#out_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data"
#script_path<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/"


#Collect all parameters in a list
list_param_raster_prediction<-list(list_param_data_prep,
                                seed_number,nb_sample,step,constant,prop_minmax,dates_selected,
                                list_models,lst_avg,in_path,out_path,script_path,
                                interpolation_method)

names(list_param_raster_prediction)<-c("list_param_data_prep",
                                "seed_number","nb_sample","step","constant","prop_minmax","dates_selected",
                                "list_models","lst_avg","in_path","out_path","script_path",
                                "interpolation_method")


raster_prediction_obj <-raster_prediction_fun(list_param_raster_prediction)


############## STAGE 5: OUTPUT ANALYSES ##################

date_selected<-c("20100101") ##This is for year 2000!!!
#raster_prediction_obj<-load_obj("raster_prediction_obj_dailyTmin_365d_GAM_fus5_all_lstd_03292013.RData")
#raster_prediction_obj<-load_obj("raster_prediction_obj_gam_CAI_dailyTmax_365d_GAM_CAI_all_lst_04162013.RData")
#raster_prediciton_obj<-load_obj(paste("raster_prediction_obj","_","interpolation_method,
#                                y_var_name,out_prefix,sep="")
list_param_results_analyses<-list(in_path,out_path,script_path,raster_prediction_obj,interpolation_method,infile_covar,covar_names,date_selected,var,out_prefix)
names(list_param_results_analyses)<-c("in_path","out_path","script_path","raster_prediction_obj","interpolation_method",
                     "infile_covar","covar_names","date_selected","var","out_prefix")
#plots_assessment_by_date<-function(j,list_param){

plots_assessment_by_date(1,list_param_results_analyses)
#source(file.path(script_path,"results_interpolation_date_output_analyses_04302013.R"))
#Call as function...

###############   END OF SCRIPT   ###################
#####################################################


##################    Master script for temperature predictions  #######################################
############################ TMIN AND TMAX predictions ##########################################
#                           
##This script produces intperpolated surface of TMIN and TMAX for specified processing region(s) given sets 
#of inputs and parameters.
#STAGE 1: LST climatology downloading and/or calculation
#STAGE 2: Covariates preparation for study/processing area: calculation of covariates (spect,land cover,etc.) and reprojection
#STAGE 3: Data preparation: meteorological station database query and extraction of covariates values from raster brick
#STAGE 4: Raster prediction: run interpolation method (-- gam fusion, gam CAI, ...) and perform validation 
#STAGE 5: Output analyses: assessment of results for specific dates...
#
#AUTHOR: Benoit Parmentier                                                                       
#DATE: 05/31/2013                                                                                 

#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363, TASK$568--   

## TODO:
# Modify code for stage 1 and call python script from R
# Modify code for stage 2, make it a function and fully automated (distoc var)
# Add options to run only specific stage + additional out_suffix?
# Make master script a function?
# Add log file for master script,add function to collect inputs and outputs
# Comments for run:
#Testing full code for 5 stages (no downloading) for Oregon region.
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

######## PARAMETERS FOR WORK FLOW #########################
### Need to add documentation ###

script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/"

##SCRIPT USED FOR THE PREDICTIONS: Source or list all scripts here to avoid confusion on versions being run!!!!

#source(file.path(script_path,"master_script_temp_05272013.R")) #Master script can be run directly...

#CALLED FROM MASTER SCRIPT:

modis_download_script <- file.path(script_path,"modis_download_05142013.py") # LST modis download python script
clim_script <- file.path(script_path,"climatology_05142013.py") # LST climatology python script
grass_setting_script <- file.path(script_path,"grass-setup.R") #Set up system shell environment for python+GRASS
source(file.path(script_path,"download_and_produce_MODIS_LST_climatology_05302013.R"))
source(file.path(script_path,"covariates_production_temperatures_05302013.R"))
source(file.path(script_path,"Database_stations_covariates_processing_function_05212013.R"))
source(file.path(script_path,"GAM_fusion_analysis_raster_prediction_multisampling_05212013.R"))
source(file.path(script_path,"results_interpolation_date_output_analyses_05062013.R"))
#source(file.path(script_path,"results_covariates_database_stations_output_analyses_04012013.R"))

#FUNCTIONS CALLED FROM GAM ANALYSIS RASTER PREDICTION ARE FOUND IN...

source(file.path(script_path,"sampling_script_functions_03122013.R"))
source(file.path(script_path,"GAM_fusion_function_multisampling_05212013.R")) #Include GAM_CAI
source(file.path(script_path,"GAM_fusion_function_multisampling_validation_metrics_05062013.R"))

#stages_to_run<-c(1,2,3,4,5) #May decide on antoher strategy later on...
stages_to_run<-c(0,2,3,4,5) #May decide on antoher strategy later on...

var<-"TMAX" # variable being interpolated
out_prefix<-"_365d_GAM_fus_all_lst_05312013"                #User defined output prefix
out_suffix<-"_OR_05312013"
out_suffix_modis <-"_05302013" #use tiles produce previously

#interpolation_method<-c("gam_fusion","gam_CAI") #other otpions to be added later
#interpolation_method<-c("gam_CAI") #other otpions to be added later
interpolation_method<-c("gam_fusion") #other otpions to be added later

#out_path <- paste("/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data",
#                  out_prefix,"/",sep="")
out_path<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data"
out_path <-paste(out_path,out_prefix,sep="")

if (!file.exists(out_path)){
  dir.create(out_path)
  #} else{
  #  out_path <-paste(out_path..)
}
  
lc_path<-"/home/layers/data/land-cover/lc-consensus-global"
infile_modis_grid<-"/data/project/layers/commons/modis/modis_sinusoidal/modis_sinusoidal_grid_world.shp" #modis grid tiling system, global
infile_elev<-"/home/layers/data/terrain/dem-cgiar-srtm-1km-tif/srtm_1km.tif"  #elevation at 1km, global extent to be replaced by the new fused product 
infile_canheight<-"/home/layers/data/land-cover/treeheight-simard2011/Simard_Pinto_3DGlobalVeg_JGR.tif"         #Canopy height, global extent
infile_distoc <- "/data/project/layers/commons/distance_to_coast/GMT_intermediate_coast_distance_01d_rev.tif" #distance to coast, global extent at 0.01 deg
#infile_covariates<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script and used in stage 3 and stage 4
#infile_reg_outline<- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/outline_venezuela_region__VE_01292013.shp" 
#infile_covariates<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/covariates__venezuela_region_TMIN__VE_03192013.tif" #covariates stack for TMIN
#infile_covariates<- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/covariates_Oregon_region_TMAX__OR_04052013.tif" #Oregon covar TMAX from earlier codes...for continuity
#infile_reg_outline=""  #input region outline defined by polygon: none for Venezuela
#This is the shape file of outline of the study area                                                      #It is an input/output of the covariate script
infile_reg_outline <- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
#infile_reg_outline <-"OR83M_state_outline.shp" #remove this parameter!!!
#ref_rast_name<-""  #local raster name defining resolution, exent, local projection--. set on the fly?? 
#this may be redundant with infile_reg_outline
ref_rast_name<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/mean_day244_rescaled.rst"  #local raster name defining resolution, exent: oregon

#covar_names see stage 2

#list_tiles_modis <- c("h11v08,h11v07,h12v07,h12v08,h10v07,h10v08") #tile for Venezuela and surrounding area
list_tiles_modis <- c("h08v04,h09v04") #tiles for Oregon
  
#CRS_interp<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs";
CRS_interp <-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
#out_region_name<-"_venezuela_region" #generated on the fly
out_region_name<-"_oregon_region" #generated on the fly
  
#The names of covariates can be changed...these names should be output/input from covar script!!!
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev_s","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
#lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10") #use older version for continuity check to be changed
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
               "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
               "nobs_09","nobs_10","nobs_11","nobs_12")
covar_names<-c(rnames,lc_names,lst_names)
  
############ STAGE 1: LST Climatology ###############

#Parameters,Inputs from R to Python??
#list_tiles_modis <- c("h08v04,h09v04") #tiles for Oregon #defined above...
start_year = "2001"
end_year = "2010"
#hdfdir =  '/home/layers/commons/modis/MOD11A1_tiles' #destination file where hdf files are stored locally after download.
hdfdir <- "/data/project/layers/commons/modis/MOD11A1_tiles"
download=0
clim_calc=1

list_param_download_clim_LST_script <- list(list_tiles_modis,start_year,end_year,hdfdir,
                                            var,grass_setting_script,modis_download_script, clim_script,
                                            download,clim_calc,out_suffix_modis)
names(list_param_download_clim_LST_script)<-c("list_tiles_modis","start_year","end_year","hdfdir",
                                              "var","grass_setting_script","modis_download_script","clim_script",
                                              "download","clim_calc","out_suffix_modis")
no_tiles <- length(unlist(strsplit(list_tiles_modis,",")))  # transform string into separate element in char vector

if (stages_to_run[1]==1){
  #clim_production_obj <-mclapply(1:2, list_param=list_param_download_clim_LST_script, download_calculate_MODIS_LST_climatology,mc.preschedule=FALSE,mc.cores = 2) #This is the end bracket from mclapply(...) statement
  clim_production_obj <-lapply(1:no_tiles, list_param=list_param_download_clim_LST_script, download_calculate_MODIS_LST_climatology) #,mc.preschedule=FALSE,mc.cores = 2) #This is the end bracket from mclapply(...) statement
  
}
#Collect LST climatology list as output???

############ STAGE 2: Covariate production ################

#list of 17 parameters
list_param_covar_production<-list(var,out_path,lc_path,infile_modis_grid,infile_elev,infile_canheight,
                                  infile_distoc,list_tiles_modis,infile_reg_outline,CRS_interp,CRS_locs_WGS84,out_region_name,
                                  out_suffix,out_suffix_modis,ref_rast_name,hdfdir,covar_names) 

names(list_param_covar_production)<-c("var","out_path","lc_path","infile_modis_grid","infile_elev","infile_canheight",
                                      "infile_distoc","list_tiles_modis","infile_reg_outline","CRS_interp","CRS_locs_WGS84","out_region_name",
                                      "out_suffix","out_suffix_modis","ref_rast_name","hdfdir","covar_names") 

## Modify to store infile_covar_brick in output folder!!!
if (stages_to_run[2]==2){
  covar_obj <- covariates_production_temperature(list_param_covar_production)
  infile_covariates <- covar_obj$infile_covariates
  infile_reg_outline <- covar_obj$infile_reg_outline
}

#Note that if stages_to_run[2]!=2, then use values defined at the beginning of the script for infile_covariates and infile_reg_outline

############# STAGE 3: Data preparation ###############


#specific to this stage
db.name <- "ghcn"       # name of the Postgres database
range_years<-c("2010","2011") #right bound not included in the range!!
range_years_clim<-c("2000","2011") #right bound not included in the range!!
infile_ghncd_data <-"/home/layers/data/climate/ghcn/v2.92-upd-2012052822/ghcnd-stations.txt"                              #This is the textfile of station locations from GHCND
#qc_flags_stations<-c("0","S")    #flags allowed for screening after the query from the GHCND??
qc_flags_stations<-c("0")    #flags allowed for screening after the query from the GHCND??

#infile_covariates and infile_reg_outline defined in stage 2 or at the start of script...

#list of 12 parameters for input in the function...

list_param_prep<-list(db.name,var,range_years,range_years_clim,infile_reg_outline,infile_ghncd_data,infile_covariates,CRS_locs_WGS84,out_path,covar_names,qc_flags_stations,out_prefix)
cnames<-c("db.name","var","range_years","range_years_clim","infile_reg_outline","infile_ghncd_data","infile_covariates","CRS_locs_WGS84","out_path","covar_names","qc_flags_stations","out_prefix")
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

list_param_data_prep <- list(infile_monthly,infile_daily,infile_locs,infile_covariates,covar_names,var,out_prefix,CRS_locs_WGS84)
names(list_param_data_prep) <- c("infile_monthly","infile_daily","infile_locs","infile_covariates","covar_names","var","out_prefix","CRS_locs_WGS84")

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
#Default name of LST avg to be matched               
lst_avg<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")  

#Collect all parameters in a list
list_param_raster_prediction<-list(list_param_data_prep,
                                seed_number,nb_sample,step,constant,prop_minmax,dates_selected,
                                list_models,lst_avg,out_path,script_path,
                                interpolation_method)
names(list_param_raster_prediction)<-c("list_param_data_prep",
                                "seed_number","nb_sample","step","constant","prop_minmax","dates_selected",
                                "list_models","lst_avg","out_path","script_path",
                                "interpolation_method")

raster_prediction_obj <-raster_prediction_fun(list_param_raster_prediction)

############## STAGE 5: OUTPUT ANALYSES ##################

date_selected_results<-c("20100101") 

list_param_results_analyses<-list(out_path,script_path,raster_prediction_obj,interpolation_method,
                                  infile_covariates,covar_names,date_selected_results,var,out_prefix)
names(list_param_results_analyses)<-c("out_path","script_path","raster_prediction_obj","interpolation_method",
                     "infile_covariates","covar_names","date_selected_results","var","out_prefix")
#plots_assessment_by_date<-function(j,list_param){
if (stages_to_run[5]==5){
  #source(file.path(script_path,"results_interpolation_date_output_analyses_05062013.R"))
  #Use lapply or mclapply
  summary_v_day <-plots_assessment_by_date(1,list_param_results_analyses)
  #Call as function...
}
  
###############   END OF SCRIPT   ###################
#####################################################


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
#DATE: 07/18/2013                                                                                 

#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363, TASK$568--   

## TODO:
# Modify code for stage 1 and call python script from R in parallel
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
library(spgwr)
library(reshape)
library(plotrix)

######## PARAMETERS FOR WORK FLOW #########################
### Need to add documentation ###

#Adding command line arguments to use mpiexec
args<-commandArgs(TRUE)
script_path<-"/nobackup/aguzman4/climateLayers/climateCode/environmental-layers/climate/research/oregon/interpolation"
dataHome<-"/nobackup/aguzman4/climateLayers/interp/testdata/"
script_path2<-"/nobackup/aguzman4/climateLayers/climateCode/environmental-layers/climate/research/world/interpolation"


#CALLED FROM MASTER SCRIPT:

modis_download_script <- file.path(script_path,"modis_download_05142013.py") # LST modis download python script
clim_script <- file.path(script_path,"climatology_05312013.py") # LST climatology python script
grass_setting_script <- file.path(script_path,"grass-setup.R") #Set up system shell environment for python+GRASS
#source(file.path(script_path,"download_and_produce_MODIS_LST_climatology_06112013.R"))
source(file.path(script_path,"covariates_production_temperatures.R"))
source(file.path(script_path,"Database_stations_covariates_processing_function.R"))
source(file.path(script_path2,"GAM_fusion_analysis_raster_prediction_multisampling.R"))
source(file.path(script_path,"results_interpolation_date_output_analyses.R"))
#source(file.path(script_path,"results_covariates_database_stations_output_analyses_04012013.R")) #to be completed

#FUNCTIONS CALLED FROM GAM ANALYSIS RASTER PREDICTION ARE FOUND IN...

source(file.path(script_path,"sampling_script_functions.R"))
source(file.path(script_path2,"GAM_fusion_function_multisampling.R")) #Include GAM_CAI
source(file.path(script_path,"interpolation_method_day_function_multisampling.R")) #Include GAM_day
source(file.path(script_path,"GAM_fusion_function_multisampling_validation_metrics.R"))

#stages_to_run<-c(1,2,3,4,5) #May decide on antoher strategy later on...
stages_to_run<-c(0,0,0,4,0) #MRun only raster fitting, prediction and assessemnt (providing lst averages, covar brick and met stations)
#If stage 2 is skipped then use previous covar object
covar_obj_file<-args[6]
#If stage 3 is skipped then use previous met_stations object
met_stations_outfiles_obj_file<-args[7]

var<-"TMAX"    
out_prefix<-args[1] #User defined output prefix
out_suffix<-args[2] #Regional suffix
out_suffix_modis<-args[3] #pattern to find tiles produced previously

interpolation_method<-c("gam_fusion")#,"gam_CAI") #Testing mem usage

out_path<-"/nobackup/aguzman4/climateLayers/output/"
out_path<-paste(out_path,out_prefix,sep="")

if(!file.exists(out_path)){
   dir.create(out_path)
}
  
lc_path<-"/nobackup/aguzman4/climateLayers/interp/testdata/data_workflow/inputs/lc-consensus-global"
infile_modis_grid<-"/nobackup/aguzman4/climateLayers/interp/testdata/data_workflow/inputs/modis_grid/modis_sinusoidal_grid_world.shp" #modis grid tiling system, global
infile_elev<-"/nobackup/aguzman4/climateLayers/interp/testdata/data_workflow/inputs/dem-cgiar-srtm-1km-tif/srtm_1km.tif"  #elevation at 1km, global extent to be replaced by the new fused product 
infile_canheight<-"/nobackup/aguzman4/climateLayers/interp/testdata/data_workflow/inputs/treeheight-simard2011/Simard_Pinto_3DGlobalVeg_JGR.tif"         #Canopy height, global extent
infile_distoc <- "/nobackup/aguzman4/climateLayers/interp/testdata/data_workflow/inputs/distance_to_coast/GMT_intermediate_coast_distance_01d_rev.tif" #distance to coast, global extent at 0.01 deg


#infile_reg_outline <- "/nobackup/aguzman4/climateLayers/subsetWGS84/shapefiles/shp_40.00_-115.00_1929.shp"
infile_reg_outline <- args[4]

ref_rast_name<-args[5]

buffer_dist<-0 #not in use yet, must change climatology step to make sure additional tiles are downloaded and LST averages
               #must also be calculated for neighbouring tiles.

list_tiles_modis <- c("h08v04,h09v04") #tiles for Oregon
  
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
CRS_interp <-CRS_locs_WGS84 


out_region_name<-""
 
#The names of covariates can be changed...these names should be output/input from covar script!!!
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev_s","slope","aspect","CANHGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
               "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
               "nobs_09","nobs_10","nobs_11","nobs_12")
covar_names<-c(rnames,lc_names,lst_names)
  
list_val_range <-c("lon,-180,180","lat,-90,90","N,-1,1","E,-1,1","N_w,-1,1","E_w,-1,1","elev_s,0,6000","slope,0,90",
                   "aspect,0,360","DISTOC,-0,10000000","CANHGHT,0,255","LC1,0,100","LC5,0,100","mm_01,-15,50",
                   "mm_02,-15,50","mm_03,-15,50","mm_04,-15,50","mm_05,-15,50","mm_06,-15,50","mm_07,-15,50",
                   "mm_08,-15,50","mm_09,-15,50","mm_10,-15,50","mm_11,-15,50","mm_12,-15,50")

############ STAGE 1: LST Climatology ###############
#print("Starting stage 1")

#Parameters,Inputs from R to Python??
start_year = "2001"
end_year = "2010"
hdfdir <- "/nobackup/aguzman4/climate/interp/testdata/data_workflow/inputs/MOD11A1_tiles" #path directory to MODIS data
download=0 #download MODIS product if 1
clim_calc=1 #calculate lst averages/climatology if 1

list_param_download_clim_LST_script <- list(list_tiles_modis,start_year,end_year,hdfdir,
                                            var,grass_setting_script,modis_download_script, clim_script,
                                            download,clim_calc,out_suffix_modis)
names(list_param_download_clim_LST_script)<-c("list_tiles_modis","start_year","end_year","hdfdir",
                                              "var","grass_setting_script","modis_download_script","clim_script",
                                              "download","clim_calc","out_suffix_modis")
no_tiles <- length(unlist(strsplit(list_tiles_modis,",")))  # transform string into separate element in char vector

if (stages_to_run[1]==1){
  clim_production_obj <-lapply(1:no_tiles, list_param=list_param_download_clim_LST_script, download_calculate_MODIS_LST_climatology)
}
#Collect LST climatology list as output???

############ STAGE 2: Covariate production ################

#list of 18 parameters
list_param_covar_production<-list(var,out_path,lc_path,infile_modis_grid,infile_elev,infile_canheight,
                                  infile_distoc,list_tiles_modis,infile_reg_outline,CRS_interp,CRS_locs_WGS84,out_region_name,
                                  buffer_dist,list_val_range,out_suffix,out_suffix_modis,ref_rast_name,hdfdir,covar_names) 

names(list_param_covar_production)<-c("var","out_path","lc_path","infile_modis_grid","infile_elev","infile_canheight",
                                      "infile_distoc","list_tiles_modis","infile_reg_outline","CRS_interp","CRS_locs_WGS84","out_region_name",
                                      "buffer_dist","list_val_range","out_suffix","out_suffix_modis","ref_rast_name","hdfdir","covar_names") 

## Modify to store infile_covar_brick in output folder!!!
if (stages_to_run[2]==2){
  covar_obj <- covariates_production_temperature(list_param_covar_production)
  infile_covariates <- covar_obj$infile_covariates
  infile_reg_outline <- covar_obj$infile_reg_outline
  covar_names<- covar_obj$covar_names
}else{
  covar_obj <-load_obj(covar_obj_file)
  infile_covariates <- covar_obj$infile_covariates
  #Region passed as input from command line
  #infile_reg_outline <- covar_obj$infile_reg_outline
  covar_names<- covar_obj$covar_names
}

#Note that if stages_to_run[2]!=2, then use values defined at the beginning of the script for infile_covariates and infile_reg_outline

############# STAGE 3: Data preparation ###############
#specific to this stage
db.name <- "ghcn" #       # name of the Postgres database
range_years<-c("2010","2011") #right bound not included in the range!!
range_years_clim<-c("2001","2011") #right bound not included in the range!!
infile_ghncd_data <-"/nobackup/aguzman4/climateLayers/stations/ghcnd-stations.txt"                              #This is the textfile of station locations from GHCND
#qc_flags_stations<-c("0","S")    #flags allowed for screening after the query from the GHCND??
qc_flags_stations<-c("0")    #flags allowed for screening after the query from the GHCND??

#infile_covariates and infile_reg_outline defined in stage 2 or at the start of script...

#list of 12 parameters for input in the function...

list_param_prep<-list(db.name,var,range_years,range_years_clim,infile_reg_outline,infile_ghncd_data,infile_covariates,CRS_locs_WGS84,out_path,covar_names,qc_flags_stations,out_prefix)
cnames<-c("db.name","var","range_years","range_years_clim","infile_reg_outline","infile_ghncd_data","infile_covariates","CRS_locs_WGS84","out_path","covar_names","qc_flags_stations","out_prefix")
names(list_param_prep)<-cnames

##### RUN SCRIPT TO GET STATION DATA WITH COVARIATES #####

if (stages_to_run[3]==3){
  list_outfiles<-database_covariates_preparation(list_param_prep)
}else{
  list_outfiles <-load_obj(met_stations_outfiles_obj_file) 
}

############### STAGE 4: RASTER PREDICTION #################
#Collect parameters from the previous stage: data preparation stage

#3 parameters from output
infile_monthly<-list_outfiles$monthly_covar_ghcn_data #outile4 from database_covar script
infile_daily<-list_outfiles$daily_covar_ghcn_data  #outfile3 from database_covar script
infile_locs<- list_outfiles$loc_stations_ghcn #outfile2? from database covar script

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
screen_data_training<-FALSE #screen training data for NA and use same input training for all models fitted

#Models to run...this can be changed for each run
#LC1: Evergreen/deciduous needleleaf trees

#Combination for test run:
#list_models<-c("y_var ~ s(elev_s)",
#                "y_var ~ s(LST)",
#                "y_var ~ s(lat,lon)+ s(elev_s)",
#                "y_var ~ te(lat,lon,elev_s)",
#                "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST)", 
#                "y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(LC1)") 

list_models<-c("y_var ~ s(lat,lon) + s(elev_s)",
               "y_var ~ s(lat,lon) + s(elev_s) + s(LST)")#,
               #"y_var ~ s(lat,lon) + s(elev_s) + s(N_w,E_w) + s(LST) + s(LC1)")

#Default name of LST avg to be matched               
lst_avg<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")  

#Collect all parameters in a list
list_param_raster_prediction<-list(list_param_data_prep,screen_data_training,
                                seed_number,nb_sample,step,constant,prop_minmax,dates_selected,
                                list_models,lst_avg,out_path,script_path,
                                interpolation_method)
names(list_param_raster_prediction)<-c("list_param_data_prep","screen_data_training",
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

# #LAND COVER INFORMATION
# LC1: Evergreen/deciduous needleleaf trees
# LC2: Evergreen broadleaf trees
# LC3: Deciduous broadleaf trees
# LC4: Mixed/other trees
# LC5: Shrubs
# LC6: Herbaceous vegetation
# LC7: Cultivated and managed vegetation
# LC8: Regularly flooded shrub/herbaceous vegetation
# LC9: Urban/built-up
# LC10: Snow/ice
# LC11: Barren lands/sparse vegetation
# LC12: Open water


################################ DOWNLOAD AND PRODUCE LST CLIMATOLOGY  #######################################
############################ Production of spatial average from MODIS ##########################################
#This script downloads modis tiles and computes monthly average over a given time period.
#Part 1: Download given list of tiles...
#Part 2: Compute climatology using monhtly average method over 10 years 
#
# This R script calls python scripts. 
#
#AUTHORS: Benoit Parmentier    
#         based on modified earlier python scripts from J. Regetz
#DATE: 05/20/2013                                                                                 

#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#???--   

##################################################################################################

download_calculate_MODIS_LST_climatology <-function(j,list_param){
  
  ###Function to download and calculate LST climatology from MODIS tiles
  #AUTHOR: Benoit Parmentier                                                                       
  #DATE: 05/20/2013                                                                                 
  #PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--   
  
  #1) var 
  #2) list_tiles_modis 
  #3) start_year 
  #4) end_year 
  #5) hdfdir: destination file where hdf files are stored locally after download.
  #6) grass_setting_script 
  #7) modis_download_script 
  #8) clim_script
  #9) download  if 1 then download modis files before calculating averages
  #10) clim_calc
  #11) out_suffix_modis
  
  ###Loading R library and packages   
  #...
   
  #PARSING OUT input parameters
  
  var <- list_param$var
  list_tiles_modis <- list_param$list_tiles_modis
  start_year <- list_param$start_year 
  end_year <- list_param$end_year 
  hdfdir <- list_param$hdfdir #destination file where hdf files are stored locally after download.
  grass_setting_script <- list_param$grass_setting_script
  modis_download_script <- list_param$modis_download_script
  clim_script <- list_param$clim_script
  download <- list_param$download #"1"  # if 1 then download modis files before calculating averages
  clim_calc <- list_param$clim_calc #"1"  # if 1 then download modis files before calculating averages
  out_suffix_modis <- list_param$out_suffix_modis
  
  #j to be used later...parallelization??
  
  #Additional parameters
  end_month= "12"
  start_month= "1"
  
  ### Create output directory if it does not exist
  if (!file.exists(hdfdir)){
    dir.create(hdfdir)
    #} else{
    #  out_path <-paste(out_path..)
  }
  
  ### CALCULATE NIGHT OR DAY CLIMATOLOGY?
  
  if (var=="TMIN") {
    night="1" # if 1 then produce night climatology
  } else{
    night="0" # if 0 then produce day climatology
  }  
  
  ## Prepare list of arguments for python script
  
  list_param_python_script <- list(list_tiles_modis,start_year,end_year,start_month,end_month,hdfdir,
                                   night,download,out_suffix_modis)
  names(list_param_python_script)<-c("list_tiles_modis","start_year","end_year","start_month","end_month","hdfdir",
                                     "night","download","out_suffix_modis")
  list_param_python_script_str <- paste(unlist(list_param_python_script), collapse=" ")
  
  #command_str<-"python /home/parmentier/Data/test5.py h01v05,h02v05 2001 2005 12 1 /benoit 1 test_out 1"
  #paste(toString(list_param_python_script),collapse=",",sep=" ")
  
  ##Downownload files if necessary...
  if (download==1){
    command_download_str <- paste("python",modis_download_script, list_param_python_script_str,sep=" ")
    #command_str <- paste("python","/home/parmentier/Data/benoit_test/test5.py", list_param_python_script_str,sep=" ")
    system(command_download_str)
  }
  
  if (clim_calc==1){
    ##Now run climatology: Can add other climatology scripts or method to produce climatology later on...
    source(grass_setting_script) #Setting to access GRASS (current setting valid for Atlas at nceas)
    command_clim_str <- paste("python",clim_script, list_param_python_script_str,sep=" ") #preparing shell command
    system(command_clim_str)
    #/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/climatology_05132013.py
  }
  
  ## return list of files???, may be modified later to return clim list and list of downloaded files + missing.
  if (clim_calc==1){
    list_output_obj <-list(command_download_str,command_clim_str)
    names(list_output_obj) <- c("command_download_str", "command_clim_str")
  } else{
    list_output_obj <-list(command_download_str)
    names(list_output_obj) <- c("command_download_str")
  }
    #Add function to list files created or modify in python...
  
  return(list_output_obj)
}

## Run function:

#list_tiles_modis <- c("h11v08,h11v07,h12v07,h12v08,h10v07,h10v08") #tile for Venezuela and surrounding area
#list_tiles_modis <- c("h08v04,h09v04") #tiles for Oregon #defined above...
#list_tiles_modis <- c("h09v09,h10v09,h11v09,h12v09,h13v09,h14v09")
#list_tiles_modis <-c("h30v10,h31v10,h32v10,h30v11,h31v11") #list("Queensland")

#list_tiles_modis <- c("h08v04")
script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/"
modis_download_script <- file.path(script_path,"modis_download_05142013.py") # LST modis download python script
clim_script <- file.path(script_path,"climatology_05142013.py") # LST climatology python script
grass_setting_script <- file.path(script_path,"grass-setup.R")
var="TMIN"
list_tiles_modis <- c("h11v11,h12v11,h13v11,h14v11,h11v12,h12v12,h13v12,h12v13,h13v13,h13v14,h14v14")

start_year = "2001"
end_year = "2010"
hdfdir =  '/home/layers/commons/modis/MOD11A1_tiles' #destination file where hdf files are stored locally after download.
#hdfdir =  '/home/parmentier/Data/IPLANT_project/MOD11A1_tiles'
download=1
clim_calc=0
out_suffix_modis="_05202013"

list_param_download_clim_LST_script <- list(list_tiles_modis,start_year,end_year,hdfdir,
                                 var,grass_setting_script,modis_download_script, clim_script,
                                 download,clim_calc,out_suffix_modis)
names(list_param_download_clim_LST_script)<-c("list_tiles_modis","start_year","end_year","hdfdir",
                                   "var","grass_setting_script","modis_download_script","clim_script",
                                   "download","clim_calc","out_suffix_modis")
debug(download_calculate_MODIS_LST_climatology)

download_calculate_MODIS_LST_climatology(1,list_param_download_clim_LST_script)

### END OF SCRIPT

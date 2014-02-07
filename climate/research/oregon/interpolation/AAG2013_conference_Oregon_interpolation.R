####################################  INTERPOLATION TEMPERATURES  #######################################
############################  AAG 2013 and OREGON transitition script #######################################
#This script reads information concerning the Oregon case study to adapt data for the revised 
# interpolation code.
#Figures and data for the AAG conference are also produced.
#AUTHOR: Benoit Parmentier                                                                      #
#DATE: 04/08/2013            
#Version: 1
#PROJECT: Environmental Layers project                                       #
#################################################################################################

###Loading R library and packages                                                      
library(gtools)                              # loading some useful tools 
library(mgcv)                                # GAM package by Simon Wood
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                              # NCAR Spatial Interpolation methods such as kriging, splines
library(raster)                              # Hijmans et al. package for raster processing
library(gdata)                               # various tools with xls reading
library(rasterVis)
library(parallel)
library(maptools)
library(maps)
library(reshape)
library(plotrix)

#### UNCTION USED IN SCRIPT

create_modis_tiles_region<-function(tiles,modis_sp){
  #This functions returns a subset of tiles from the modis grdi.
  #Arguments: modies grid tile,list of tiles
  #Output: spatial grid data frame of the subset of tiles
  
  h_list<-lapply(tiles,substr,start=2,stop=3) #passing multiple arguments
  v_list<-lapply(tiles,substr,start=5,stop=6) #passing multiple arguments
  selected_tiles<-subset(subset(modis_sp,subset = h %in% as.numeric (h_list) ),
                         subset = v %in% as.numeric(v_list)) 
  return(selected_tiles)
}

### Parameters and argument

lc_path<-"/home/layers/data/land-cover/lc-consensus-global"
infile_modis_grid<-"modis_sinusoidal_grid_world.shp"
infile_elev<-"/home/layers/data/terrain/dem-cgiar-srtm-1km-tif/srtm_1km.tif"  #this is the global file: replace later with the input produced by the DEM team
infile_canheight<-"Simard_Pinto_3DGlobalVeg_JGR.tif"              #Canopy height
#list_tiles_modis = c('h11v08','h11v07','h12v07','h12v08','h10v07','h10v08') #tile for Venezuel and surrounding area
list_tiles_modis = c('h08v04','h09v04')

#infile_reg_outline=""  #input region outline defined by polygon
infile_reg_outline= "OR83M_state_outline.shp"
infile_countries_sinusoidal<-"countries_sinusoidal_world.shp"

#CRS_interp<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs";
CRS_interp <-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
out_region_name<-"Oregon_region" #generated on the fly
out_prefix<-"_OR_04052013"
ref_rast_name<- "mean_day244_rescaled.rst"                     #This is the shape file of outline of the study area. #local raster name defining resolution, exent, local projection--. set on the fly??
infile_covariates<-"covariates__venezuela_region__VE_01292013.tif" #this is an output from covariate script and used in stage 3 and stage 4

#The names of covariates can be changed...these names should be output/input from covar script!!!
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
covar_names<-c(rnames,lc_names,lst_names)
infile2<-"/home/layers/data/climate/ghcn/v2.92-upd-2012052822/ghcnd-stations.txt"                              #This is the textfile of station 

in_path<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_covariates"

#c("Oregon", c("h08v04","h09v04","h08v05","h09v05"))
study_area_list_tiles <- vector("list",6)
study_area_list_tiles[[1]] <-list("Oregon", c("h08v04","h09v04"))
study_area_list_tiles[[2]] <-list("Venezuela",c("h10v07", "h10v08", "h11v7", "h11v08", "h12v07", "h12v08"))
study_area_list_tiles[[3]] <-list("Norway",c("h18v02","h18v03", "h19v02", "h19v03"))
study_area_list_tiles[[4]] <-list("East_Africa",c("h20v08", "h21v08", "h22v08", "h20v09", "h21v09", "h22v09", "h21v10"))
study_area_list_tiles[[5]] <-list("South_Africa",c("h19v11", "h20v11", "h19v12", "h20v12"))
study_area_list_tiles[[6]] <-list("Queensland",c("h31v10", "h31v10", "h32v10", "h30v11", "h31v11"))

#######################################################################################
###########################      BEGIN SCRIPT    ######################################


setwd(in_path)


####### PART I: Prepare data for figures and for Oregon interpolation ##########

### Read in Venezuela covariate stack

s_raster_Ven<-brick(infile_covariates) #read brick 
names(s_raster_Ven)<-covar_names #assign names
mm_01_Ven<-subset(s_raster_ven,"mm_01") #select LST January month average

### Read in world map to show stuy areas. 

world_sp <- getData("countries")  # different resolutions available
outfile2<-file.path(in_path,paste("word_countries.shp",sep=""))  #Name of the file
writeOGR(world_sp,dsn= dirname(outfile2),layer= sub(".shp","",basename(outfile2)), driver="ESRI Shapefile",overwrite_layer=TRUE)

### Read in sinusoidal grid and world countries
filename<-sub(".shp","",infile_modis_grid)       #Removing the extension from file.
modis_grid<-readOGR(".", filename)     #Reading shape file using rgdal library

### Create list ofALL STUDY AREAS/TEST SITES

## Create list of study area regions:
list_tiles<-lapply(1:length(study_area_list_tiles),function(k) study_area_list_tiles[[k]][[2]])
modis_reg_outlines<-lapply(list_tiles,FUN=create_modis_tiles_region,modis_sp=modis_grid) #problem...this does not 

#writeOGR(modis_reg_outline,dsn= ".",layer= paste("outline",out_region_name,"_",out_suffix,sep=""), 
#         driver="ESRI Shapefile",overwrite_layer="TRUE")

####################################################
#Read in GHCND database station locations

dat_stat <- read.fwf(infile2, 
                     widths = c(11,9,10,7,3,31,4,4,6),fill=TRUE)  
colnames(dat_stat)<-c("STAT_ID","lat","lon","elev","state","name","GSNF","HCNF","WMOID")
coords<- dat_stat[,c('lon','lat')]
coordinates(dat_stat)<-coords
proj4string(dat_stat)<-CRS_locs_WGS84 #this is the WGS84 projection
#Save shapefile for later
outfile1<-file.path(in_path,paste("ghcnd_stations.shp",sep=""))  #Name of the file
writeOGR(dat_stat,dsn= dirname(outfile1),layer= sub(".shp","",basename(outfile1)), driver="ESRI Shapefile",overwrite_layer=TRUE)

interp_area <- readOGR(dsn=in_path,sub(".shp","",infile_reg_outline))
interp_area_WGS84 <-spTransform(interp_area,CRS_locs_WGS84)         # Project from WGS84 to new coord. system

# Spatial query to find relevant stations

inside <- !is.na(over(dat_stat, as(interp_area_WGS84, "SpatialPolygons")))  #Finding stations contained in the current interpolation area
stat_reg_OR<-dat_stat[inside,]              #Selecting stations contained in the current interpolation area

stat_reg_OR <-spTransform(stat_reg_OR,CRS(proj4string(interp_area)))         # Project from WGS84 to new coord. system

#Now Venezuela
interp_area_Ven_WGS84 <-spTransform(modis_reg_outlines[[2]],CRS_locs_WGS84)         # Project from WGS84 to new coord. system
inside <- !is.na(over(dat_stat, as(interp_area_Ven_WGS84, "SpatialPolygons")))  #Finding stations contained in the current interpolation area
stat_reg_Ven <-dat_stat[inside,]              #Selecting stations contained in the current interpolation area

## Get the data in the local projection
stat_reg_Ven <-spTransform(stat_reg_Ven,CRS(proj4string(modis_reg_outlines[[2]])))         # Project from WGS84 to new coord. system

### READ IN COVARIATES FILES FOR OREGON AND MAKE IT A MULTI-BAND FILE

inlistf<-"list_files_covariates_04032013.txt"
lines<-read.table(paste(inlistf,sep=""), sep=" ")                  #Column 1 contains the names of raster files
inlistvar<-lines[,1]
inlistvar<-as.character(inlistvar)
covar_names_OR<-as.character(lines[,2])                                         #Column two contains short names for covaraites

s_raster_OR<- stack(inlistvar)                                                  #Creating a stack of raster images from the list of variables.
layerNames(s_raster_OR)<-covar_names_OR                                            #Assigning names to the raster layers

aspect<-subset(s_raster_OR,"aspect")             #Select layer from stack
slope<-subset(s_raster_OR,"slope")             #Select layer from stack
distoc<-subset(s_raster_OR,"DISTOC")  

N<-cos(aspect*pi/180)
E<-sin(aspect*pi/180)
Nw<-sin(slope*pi/180)*cos(aspect*pi/180)   #Adding a variable to the dataframe
Ew<-sin(slope*pi/180)*sin(aspect*pi/180)   #Adding variable to the dataframe.

xy <-coordinates(slope)  #get x and y projected coordinates...
xy_latlon<-project(xy, CRS_interp, inv=TRUE) # find lat long for projected coordinats (or pixels...)

x <-init(slope,v="x")
y <-init(slope,v="y")
lon<-x
lat<-y
lon <-setValues(lon,xy_latlon[,1]) #longitude for every pixel in the processing tile/region
lat <-setValues(lat,xy_latlon[,2]) #latitude for every pixel in the processing tile/region
CANHEIGHT<-subset(s_raster_OR,"CANHEIGHT")
CANHEIGHT[is.na(CANHEIGHT)]<-0

elev<-subset(s_raster_OR,"elev")
elev[elev==-9999] <- NA

r<-stack(x,y,lon,lat,N,E,Nw,Ew,elev,slope,aspect,CANHEIGHT,distoc)
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
s_raster<-r
#Add landcover layers
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10")
lc_reg_s<-subset(s_raster_OR,lc_names)
#Should be a function...ok for now??
test<-vector("list",nlayers(lc_reg_s))
for (k in 1:nlayers(lc_reg_s)){
  LC<-raster(lc_reg_s,layer=k)             #Select layer from stack
  LC[is.na(LC)]<-0
  test[[k]]<-LC
}
#tmp_df<-freq(lc_reg_s,merge=TRUE) #check to see if it worked
#head(tmp_df)
lc_reg_s<-stack(test)
s_raster<-addLayer(s_raster, lc_reg_s)
  
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
lst_mm_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")
lst_mm_s<-subset(s_raster_OR,lst_mm_names)
lst_mm_s <- lst_mm_s - 273.16 
lst_nobs_names<-c("nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
                "nobs_09","nobs_10","nobs_11","nobs_12")
lst_nobs_s<-subset(s_raster_OR,lst_nobs_names)

s_raster<-addLayer(s_raster, lst_mm_s)
s_raster<-addLayer(s_raster, lst_nobs_s)
covar_names<-c(rnames,lc_names,lst_names)
names(s_raster)<-covar_names

# create mask!!! Should combine with mask of elev
LC10<-subset(s_raster,"LC10")

LC10_mask<-LC10
LC10_mask[is.na(LC10_mask)]<- 0
LC10_mask[LC10==100]<- NA
LC10_mask[LC10_mask<100]<- 1
LC10_mask[is.na(LC10_mask)]<- 0
mask_land_NA<-LC10_mask
mask_land_NA[mask_land_NA==0]<-NA


##### SAVE AS MULTIBAND...make this a function...

#list of files...
file_format<-".tif"
NA_val<- -9999
band_order<- "BSQ"
var<-"TMAX"
data_name<-paste("covariates_",out_region_name,"_",sep="")
raster_name<-paste(data_name,var,"_",out_prefix,file_format, sep="")
#writeRaster(s_raster, filename=raster_name,NAflag=-999,bylayer=FALSE,bandorder="BSQ",overwrite=TRUE)  #Writing the data in a raster file format...
#if mask
#stat_val<- extract(s_raster, ghcn3)                                          #Extracting values from the raster stack for every point location in coords data frame.
s_raster_m<-mask(s_raster,mask_land_NA,filename=raster_name,
                 overwrite=TRUE,NAflag=NA_val,bylayer=FALSE,bandorder=band_order)
#if no mask
#writeRaster(s_raster, filename=raster_name,NAflag=-999,bylayer=FALSE,bandorder="BSQ",overwrite=TRUE)  #Writing the data in a raster file format...

############# PART II: PRODUCE FIGURES #######

### CREATE FIGURE TEST SITES

dat_stat_sinusoidal <- spTransform(dat_stat,CRS(proj4string(modis_grid)))
world_sinusoidal <- readOGR(dsn=".",sub(".shp","",infile_countries_sinusoidal))

png(paste("Study_area_modis_grid",out_prefix,".png",sep=""))
plot(world_sinusoidal)
plot(dat_stat_sinusoidal,cex=0.2,pch=16,col=c("blue"),add=TRUE)
plot(modis_grid,add=TRUE)
for (k in 1:length(modis_reg_outlines)){
  plot(modis_reg_outlines[[k]],border=c("red"),lwd=2.5,add=TRUE)
}
title("Study area for temperature and precipitation predictions")
#legend
dev.off()

### CREATE FIGURE MEAN DAILY AND MEAN MONTHLY: AAG 2013  ####

lst_md<-raster(ref_rast_name)
lst_mm_09<-subset(s_raster,"mm_09")
plot(stack(lst_md,lst_mm_09))

lst_md<-raster("mean_day001_rescaled.rst")
lst_md<- lst_md - 273.16
lst_mm_01<-subset(s_raster,"mm_01")

png(filename=paste("Comparison_daily_monthly_mean_lst",out_prefix,".png",sep=""),width=960,height=480)
par(mfrow=c(1,2))
plot(lst_md)
plot(interp_area,add=TRUE)
title("Mean January 1")
plot(lst_mm_01)
plot(interp_area,add=TRUE)
title("Mean for monht of January")
dev.off()

### CREATE FIGURE NUMBER OF STATIONS PER SITE 

png(paste("stations_for_Venezuela_Oregon_areas",out_prefix,".png",sep=""),,width=960,height=480)
par(mfrow=c(1,2))
#Oregon data
plot(lst_mm_01)
plot(interp_area,add=TRUE)
plot(stat_reg_OR,add=TRUE)
title("Stations located in Oregon from GHNCD")
plot(mm_01_Ven)
plot(modis_reg_outlines[[2]],add=TRUE)
plot(stat_reg_Ven,add=TRUE)
title("Stations located in Venezuela from GHNCD")
dev.off()

### CREATE FIGURE NUMBER OF STATIONS PER SITE AND SPECIFIC MONTH... 

#png(paste("stations_for_Venezuela_Oregon_areas_per_month",out_prefix,".png",sep=""))
#par(mfrow=c(1,2))
#plot(interp_area_WGS84)
#plot(stat_reg_OR,add=TRUE)
#plot(modis_reg_outlines[[2]])
#plot(stat_reg_Ven,add=TRUE)
#dev.off()

############ PART III: SCREENING OF COVARIATES #############

### SCREENING FUNCTION for covariate stack and GHNCD data base to add later in the functions

#Screen for extreme values": this needs more thought, min and max val vary with regions
#min_val<-(-15+273.16) #if values less than -15C then screen out (note the Kelvin units that will need to be changed later in all datasets)
#r1[r1 < (min_val)]<-NA
#s_raster<-addLayer(s_raster,LST)            #Adding current month

nel<-12
#tab_range<-data.frame(varname=character(nel),varterm=character(nel),vmin=numeric(nel),vmax=numeric(nel))
val_range<-vector("list",nel) #list of one row data.frame
val_rst<-vector("list",nel) #list of one row data.frame
val_range[[1]]<-data.frame(varname="lon",vmin=-180,vmax=180)
val_range[[2]]<-data.frame(varname="lat",vmin=-90,vmax=90)
val_range[[3]]<-data.frame(varname="N",vmin=-1,vmax=1)
val_range[[4]]<-data.frame(varname="E",vmin=-1,vmax=1)
val_range[[5]]<-data.frame(varname="N_w",vmin=-1,vmax=1)
val_range[[6]]<-data.frame(varname="E_w",vmin=-1,vmax=1)
val_range[[7]]<-data.frame(varname="elev",vmin=0,vmax=6000)
val_range[[8]]<-data.frame(varname="slope",varterm="slope",vmin=0,vmax=90)
val_range[[9]]<-data.frame(varname="aspect",varterm="aspect",vmin=0,vmax=360)
val_range[[10]]<-data.frame(varname="DISTOC",vmin=-0,vmax=10000000)
val_range[[11]]<-data.frame(varname="CANHEIGHT",vmin=0,vmax=255)
val_range[[12]]<-data.frame(varname="LC1",vmin=0,vmax=100)
val_range[[13]]<-data.frame(varname="LC3",vmin=0,vmax=100)
val_range[[14]]<-data.frame(varname="mm_01",vmin=-15,vmax=50)
val_range[[15]]<-data.frame(varname="mm_02",vmin=-15,vmax=50)
val_range[[16]]<-data.frame(varname="mm_03",vmin=-15,vmax=50)
val_range[[17]]<-data.frame(varname="mm_04",vmin=-15,vmax=50)
val_range[[18]]<-data.frame(varname="mm_05",vmin=-15,vmax=50)
val_range[[19]]<-data.frame(varname="mm_06",vmin=-15,vmax=50)
val_range[[20]]<-data.frame(varname="mm_07",vmin=-15,vmax=50)
val_range[[21]]<-data.frame(varname="mm_08",vmin=-15,vmax=50)
val_range[[22]]<-data.frame(varname="mm_09",vmin=-15,vmax=50)
val_range[[23]]<-data.frame(varname="mm_10",vmin=-15,vmax=50)
val_range[[24]]<-data.frame(varname="mm_11",vmin=-15,vmax=50)
val_range[[25]]<-data.frame(varname="mm_12",vmin=-15,vmax=50)

tab_range<-do.call(rbind,val_range)

#pos<-match("ELEV_SRTM",layerNames(s_raster)) #Find column with the current month for instance mm12
#ELEV_SRTM<-raster(s_raster,pos)

screening_val_covariates_fun<-function(tab_range,r_stack){
  #Screening for raster stack
  #
  for (k in 1:nrow(tab_range)){
    avl<-c(-Inf,tab_range$vmin[k],NA, tab_range$vmax[k],+Inf,NA)   #This creates a input vector...val 1 are -9999, 2 neg, 3 positive
    rclmat<-matrix(avl,ncol=3,byrow=TRUE)
    #s_raster_r<-raster(r_stack,match(tab_range$varterm[k],names(r_stack))) #select relevant layer from stack
    s_raster_r<-raster(r_stack,match(tab_range$varname[k],names(r_stack)))
    s_raster_r<-reclass(s_raster_r,rclmat)  #now reclass values 
    r_stack<-dropLayer(r_stack,"N")
    names(s_raster_r)<-tab_range$varname[k] #Loss of layer names when using reclass
    val_rst[[k]]<-s_raster_r
  }
  s_rst_m<-stack(val_rst) #This a raster stack with valid range of values
  r_stack<-addLayer(r_stack,s_rst_m) #add back layers that were screened out
  return(r_stack)
}

#### ADD SCREENING FUNCTION FOR GHCND extracted!!!

#Remove NA for LC and CANHEIGHT
#ghcn$LC1[is.na(ghcn$LC1)]<-0
#ghcn$LC3[is.na(ghcn$LC3)]<-0
#ghcn$CANHEIGHT[is.na(ghcn$CANHEIGHT)]<-0
#ghcn$LC4[is.na(ghcn$LC4)]<-0
#ghcn$LC6[is.na(ghcn$LC6)]<-0
##ghcn$ELEV_SRTM[ghcn$ELEV_SRTM==-9999]<-NA
#dst<-subset(dst,dst$TMax>-15 & dst$TMax<40)
#dst<-subset(dst,dst$ELEV_SRTM>0) #This will drop two stations...or 24 rows

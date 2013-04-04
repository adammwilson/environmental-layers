####################################  INTERPOLATION TEMPERATURES  #######################################
############################      SCRIPT 1- MEOT          #######################################
#This script reads information concerning the Oregon case study to adapt data for the revised 
# interpolation code.
#Figures and data for the AAG conference are also produced.
#AUTHOR: Benoit Parmentier                                                                      #
#DATE: 04/03/2013            
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

### Parameters and argument

lc_path<-"/home/layers/data/land-cover/lc-consensus-global"
infile_modis_grid<-"modis_sinusoidal_grid_world.shp"
infile_elev<-"/home/layers/data/terrain/dem-cgiar-srtm-1km-tif/srtm_1km.tif"  #this is the global file: replace later with the input produced by the DEM team
infile_canheight<-"Simard_Pinto_3DGlobalVeg_JGR.tif"              #Canopy height
#list_tiles_modis = c('h11v08','h11v07','h12v07','h12v08','h10v07','h10v08') #tile for Venezuel and surrounding area
list_tiles_modis = c('h08v04','h09v04')

#infile_reg_outline=""  #input region outline defined by polygon
infile_reg_outline= "OR83M_state_outline.shp"

#CRS_interp<-"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs";
CRS_interp <-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
out_region_name<-"Oregon_region" #generated on the fly
out_suffix<-"_OR_04032013"
ref_rast_name<- "mean_day244_rescaled.rst"                     #This is the shape file of outline of the study area. #local raster name defining resolution, exent, local projection--. set on the fly??

#The names of covariates can be changed...these names should be output/input from covar script!!!
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
covar_names<-c(rnames,lc_names,lst_names)
infile2<-"/home/layers/data/climate/ghcn/v2.92-upd-2012052822/ghcnd-stations.txt"                              #This is the textfile of station 

in_path<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_covariates"
setwd(in_path)


####################################################
#Read in GHCND database station locations

dat_stat <- read.fwf(infile2, 
                     widths = c(11,9,10,7,3,31,4,4,6),fill=TRUE)  
colnames(dat_stat)<-c("STAT_ID","lat","lon","elev","state","name","GSNF","HCNF","WMOID")
coords<- dat_stat[,c('lon','lat')]
coordinates(dat_stat)<-coords
proj4string(dat_stat)<-CRS_locs_WGS84 #this is the WGS84 projection
#Save shapefile for later


# Spatial query to find relevant stations

dat_stat2<-spTransform(dat_stat,CRS(CRS_interp))         # Project from WGS84 to new coord. system

#dat_stat2<-spTransform(dat_stat,CRS(CRS_interp))         # Project from WGS84 to new coord. system
interp_area <- readOGR(dsn=in_path,sub(".shp","",infile_reg_outline))

inside <- !is.na(over(dat_stat2, as(interp_area, "SpatialPolygons")))  #Finding stations contained in the current interpolation area
stat_reg<-dat_stat2[inside,]              #Selecting stations contained in the current interpolation area

#Read in world map 

#Get Natural Earth Data using rworldmap package:
#Derived from http://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/
#world_sp <- getData("GADM", country="FRA",level=0)  # different resolutions available
world_sp <- getData("countries")  # different resolutions available
#proj4string(world_sp)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#proj4string(world_sp)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(world_sp)
plot(dat_stat,cex=0.2,pch=16,col=c("red"),add=TRUE)
#Read in sinusoidal grid and world countries

setwd(in_path)	
filename<-sub(".shp","",infile_modis_grid)       #Removing the extension from file.
modis_grid<-readOGR(".", filename)     #Reading shape file using rgdal library
if (infile_reg_outline!=""){
  filename<-sub(".shp","",infile_reg_outline)   #Removing the extension from file.
  reg_outline<-readOGR(".", filename)
}
if (infile_reg_outline==""){
  reg_outline<-create_modis_tiles_region(modis_grid,list_tiles_modis) #problem...this does not 
  #align with extent of modis LST!!!
  writeOGR(reg_outline,dsn= ".",layer= paste("outline",out_region_name,"_",out_suffix,sep=""), 
           driver="ESRI Shapefile",overwrite_layer="TRUE")
}

create_modis_tiles_region<-function(modis_grid,tiles){
  #This functions returns a subset of tiles from the modis grdi.
  #Arguments: modies grid tile,list of tiles
  #Output: spatial grid data frame of the subset of tiles
  h_list<-lapply(tiles,substr,start=2,stop=3) #passing multiple arguments
  v_list<-lapply(tiles,substr,start=5,stop=6) #passing multiple arguments
  selected_tiles<-subset(subset(modis_grid,subset = h %in% as.numeric (h_list) ),
                         subset = v %in% as.numeric(v_list)) 
  return(selected_tiles)
}

### READ IN COVARIATES FILES FOR OREGON AND MAKE IT A MULTI-BAND FILE

inlistf<-"list_files_05032012.txt"
lines<-read.table(paste(path,"/",inlistf,sep=""), sep=" ")                  #Column 1 contains the names of raster files
inlistvar<-lines[,1]
inlistvar<-paste(path,"/",as.character(inlistvar),sep="")
covar_names<-as.character(lines[,2])                                         #Column two contains short names for covaraites

s_raster<- stack(inlistvar)                                                  #Creating a stack of raster images from the list of variables.
layerNames(s_raster)<-covar_names                                            #Assigning names to the raster layers
file_format<-".tif"
NA_val<- -999
band_order<- "BSQ"
data_name<-paste("covariates_",out_region_name,"_",sep="")
raster_name<-paste(data_name,var,"_",out_suffix,file_format, sep="")
#writeRaster(s_raster, filename=raster_name,NAflag=-999,bylayer=FALSE,bandorder="BSQ",overwrite=TRUE)  #Writing the data in a raster file format...
#if mask
s_raster_m<-mask(s_raster,LC_mask,filename=raster_name,
                 overwrite=TRUE,NAflag=NA_val,bylayer=FALSE,bandorder=band_order)
#if no mask
#writeRaster(s_raster, filename=raster_name,NAflag=-999,bylayer=FALSE,bandorder="BSQ",overwrite=TRUE)  #Writing the data in a raster file format...

#return




### SCREENING FUNCTION

#Screen for extreme values": this needs more thought, min and max val vary with regions
#min_val<-(-15+273.16) #if values less than -15C then screen out (note the Kelvin units that will need to be changed later in all datasets)
#r1[r1 < (min_val)]<-NA
s_raster<-addLayer(s_raster,LST)            #Adding current month

nel<-12
#tab_range<-data.frame(varname=character(nel),varterm=character(nel),vmin=numeric(nel),vmax=numeric(nel))
val_range<-vector("list",nel) #list of one row data.frame
val_rst<-vector("list",nel) #list of one row data.frame
val_range[[1]]<-data.frame(varname="lon",varterm="lon",vmin=-180,vmax=180)
val_range[[2]]<-data.frame(varname="lat",varterm="lat",vmin=-90,vmax=90)
val_range[[3]]<-data.frame(varname="ELEV_SRTM",varterm="ELEV_SRTM",vmin=0,vmax=6000)
val_range[[4]]<-data.frame(varname="Eastness",varterm="Eastness",vmin=-1,vmax=1)
val_range[[5]]<-data.frame(varname="Northness",varterm="Northness",vmin=-1,vmax=1)
val_range[[6]]<-data.frame(varname="Northness_w",varterm="Northness_w",vmin=-1,vmax=1)
val_range[[7]]<-data.frame(varname="Eastness_w",varterm="Eastness_w",vmin=-1,vmax=1)
val_range[[8]]<-data.frame(varname="mm_01",varterm="LST",vmin=-258.16,vmax=313.16)
val_range[[9]]<-data.frame(varname="DISTOC",varterm="DISTOC",vmin=-0,vmax=10000000)
val_range[[10]]<-data.frame(varname="LC1",varterm="LC1",vmin=0,vmax=100)
val_range[[11]]<-data.frame(varname="LC3",varterm="LC3",vmin=0,vmax=100)
val_range[[12]]<-data.frame(varname="slope",varterm="slope",vmin=0,vmax=90)
tab_range<-do.call(rbind,val_range)
#pos<-match("ELEV_SRTM",layerNames(s_raster)) #Find column with the current month for instance mm12
#ELEV_SRTM<-raster(s_raster,pos)
for (k in 1:length(val_range)){
  avl<-c(-Inf,tab_range$vmin[k],NA, tab_range$vmax[k],+Inf,NA)   #This creates a input vector...val 1 are -9999, 2 neg, 3 positive
  rclmat<-matrix(avl,ncol=3,byrow=TRUE)
  s_raster_r<-raster(s_raster_f,match(tab_range$varterm[k],layerNames(s_raster_f)))
  s_raster_r<-reclass(s_raster_r,rclmat)  #Loss of layer names when using reclass
  layerNames(s_raster_r)<-tab_range$varterm[k]
  val_rst[[k]]<-s_raster_r
}
s_rst_m<-stack(val_rst) #This a raster stack with valid range of values



##################    Data preparation for interpolation   #######################################
############################ Covariate production for a given tile/region ##########################################
#This script produces covariates raster for a a specfied study area.                             
# It requires the following inputs:                                                              
# 1)list of modis tiles or shape file with region outline            
# 2)input names for global covariates:
# -SRTM CGIAR 1 km
# -Canopy heihgt (Simard)
# -land cover concensus (Jetz lab)
# -MODIS LST: mean and obs
#3) The output is a multiband file in tif format with projected covariates for the processing region/tile.             
#AUTHOR: Benoit Parmentier                                                                       
#DATE: 01/28/2013                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--   

##Comments and TODO:
#This script is meant to be for general processing tile by tile or region by region.
#We must decide on a local projection. This can best set up from the tile/region extent: for now use
#- lcc with two standard paralells and one central meridian in the middle of the region.
#- produce a distance to ocean layer that is global.
#- do not keep output in memory??

##################################################################################################

###Loading R library and packages   
library(RPostgreSQL)
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                                          # GDAL wrapper for R, spatial utilities
library(raster)
library(gtools)
library(rasterVis)
library(graphics)
library(grid)
library(lattice)

### Parameters and arguments

##Paths to inputs and output
in_path <- "/home/parmentier/Data/benoit_test"
in_path <- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data/"
out_path<- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/output_data/"
lc_path<-"/home/layers/data/land-cover/lc-consensus-global"
elev_path<-"/home/layers/data/terrain/dem-cgiar-srtm-1km-tif"

infile1<-"worldborder_sinusoidal.shp"
infile2<-"modis_sinusoidal_grid_world.shp"
infile3<-"countries_sinusoidal_world.shp"
infile4<-"srtm_1km.tif"  #this is the global file: replace later with the input produced by the DEM team
infile5<-"Simard_Pinto_3DGlobalVeg_JGR.tif"              #Canopy height
list_tiles_modis = c('h11v08','h11v07','h12v07','h12v08','h10v07','h10v08') #tile for Venezuel and surrounding area
infile_reg_outline=""  #input region outline defined by polygon
CRS_interp<-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
out_region_name<-"_venezuela_region"
out_suffix<-"_VE_01292013"
ref_rast_name<-""  #local raster name defining resolution, exent, local projection--. set on the fly??
                   #for the processing tile/region? This is a group fo six tiles for now.

#### Functions used in the script  ###

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

#This function is very very slow not be used most likely
create_polygon_from_extent<-function(reg_ref_rast){
  #This functions returns polygon sp from input rast
  #Arguments: input ref rast
  #Output: spatial polygon
  set1f <- function(x){rep(1, x)}
  tmp_rast <- init(reg_ref_rast, fun=set1f, overwrite=TRUE)
  reg_outline_poly<-rasterToPolygons(tmp_rast)
  return(reg_outline_poly)
}

create_raster_region <-function(raster_name,reg_ref_rast){
  #This functions returns a subset of tiles from the modis grdi.
  #Arguments: raster name of the file,reference file with
  #Output: spatial grid data frame of the subset of tiles
  
  layer_rast<-raster(raster_name)
  new_proj<-proj4string(layer_rast)                  #Extract coordinates reference system in PROJ4 format
  region_temp_projected<-projectExtent(reg_ref_rast,CRS(new_proj))     #Project from current to region coord. system
  layer_crop_rast<-crop(layer_rast, region_temp_projected) #crop using the extent from teh region tile
  #layer_projected_rast<-projectRaster(from=layer_crop_rast,crs=proj4string(reg_outline),method="ngb")
  layer_projected_rast<-projectRaster(from=layer_crop_rast,to=reg_ref_rast,method="ngb")
  return(layer_projected_rast)
}

mosaic_raster_list<-function(mosaic_list,out_names,out_path){
  #This functions returns a subset of tiles from the modis grid.
  #Arguments: modies grid tile,list of tiles
  #Output: spatial grid data frame of the subset of tiles
  #Note that rasters are assumed to be in the same projection system!!
  
  rast_list<-vector("list",length(mosaic_list))
  for (i in 1:length(mosaic_list)){  
    # read the individual rasters into a list of RasterLayer objects
    # this may be changed so that it is not read in the memory!!!
    input.rasters <- lapply(as.character(mosaic_list[[i]]), raster)
    mosaiced_rast<-input.rasters[[1]]
    
    for (k in 2:length(input.rasters)){
      mosaiced_rast<-mosaic(mosaiced_rast,input.rasters[[k]], fun=mean)
      #mosaiced_rast<-mosaic(mosaiced_rast,raster(input.rasters[[k]]), fun=mean)
    }
    data_name<-paste("mosaiced_",sep="") #can add more later...
    raster_name<-paste(data_name,out_names[i],".tif", sep="")
    writeRaster(mosaiced_rast, filename=file.path(out_path,raster_name),overwrite=TRUE)  
    #Writing the data in a raster file format...  
    rast_list[[i]]<-file.path(out_path,raster_name)
  }
  return(rast_list)
}

###########################################################
############ Main body: BEGIN--START OF THE SCRIPT ###################

##### STEP 1: Reading region or tile information to set the study or processing region

setwd(in_path)

filename<-sub(".shp","",infile2)       #Removing the extension from file.
modis_grid<-readOGR(".", filename)     #Reading shape file using rgdal library
filename<-sub(".shp","",infile1)       #Removing the extension from file.
world_countries<-readOGR(".", filename)     #Reading shape file using rgdal library

if (infile_reg_outline!=""){
  filename<-sub(".shp","",infile_reg_outline)   #Removing the extension from file.
  reg_outline<-readOGR(".", filename)
}

if (infile_reg_outline==""){
  reg_outline<-create_modis_tiles_region(modis_grid,list_tiles_modis) #problem...this does not 
                                                                      #align with extent of modis LST!!!
  writeOGR(reg_outline,dsn= ".",layer= paste("outline",out_region_name,"_",out_suffix,sep=""), driver="ESRI Shapefile")
}

tmp<-extent(ref_rast)

#modis_tiles<-create_modis_tiles_region(modis_grid,list_tiles_modis)
##Create covariates for the stuy area: pull everything from the same folder?

#### STEP 2: process and/or produce covariates for the tile/region

################################
#1) LST climatology: project, mosaic

tile<-list_tiles_modis[i]
pat_str2 <- glob2rx(paste("nobs","*.tif",sep=""))
tmp_str2<- mixedsort(list.files(pattern=pat_str2))
pat_str1 <- glob2rx(paste("mean","*.tif",sep=""))
tmp_str1<- mixedsort(list.files(pattern=pat_str1))
#add lines using grep to select tiles...

#list_date_names<-as.character(0:11)
#lsit_date_names<-month.abb
out_rastnames<-paste("_lst_","nobs",out_suffix,sep="")
list_date_names<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
mosaic_list<-split(tmp_str2,list_date_names)
new_list<-vector("list",length(mosaic_list))
for (i in 1:length(list_date_names)){
  j<-grep(list_date_names[i],mosaic_list,value=FALSE)
  names(mosaic_list)[j]<-list_date_names[i]
  new_list[i]<-mosaic_list[j]
}
mosaic_list<-new_list
out_rastnames<-paste(list_date_names,out_rastnames,sep="")
#reproject and crop if necessary
nobs_m_list<-mosaic_raster_list(mosaic_list,out_rastnames,out_path)
plot(stack(nobs_m_list))

##Now mosaic for mean: should reorder files!!
pat_str1 <- glob2rx(paste("mean","*.tif",sep=""))
tmp_str1<- mixedsort(list.files(pattern=pat_str1))
out_rastnames<-paste("_lst_","mean",out_suffix,sep="")
list_date_names<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
mosaic_list<-split(tmp_str1,list_date_names)
new_list<-vector("list",length(mosaic_list))
for (i in 1:length(list_date_names)){
  j<-grep(list_date_names[i],mosaic_list,value=FALSE)
  names(mosaic_list)[j]<-list_date_names[i]
  new_list[i]<-mosaic_list[j]
}
mosaic_list<-new_list
out_rastnames<-paste(list_date_names,out_rastnames,sep="")

mean_m_list<-mosaic_raster_list(mosaic_list,out_rastnames,out_path)
plot(stack(mean_m_list))
#Use this as ref file for now?? Ok for the time being: this will need to change to be a processing tile.
ref_rast<-raster(mean_m_list[[1]]) 
#Modis shapefile tile is slighly shifted:
# +proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs for ref_rast
#"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" ??
#reassign proj from modis tile to raster? there is a 10m diff in semi-axes...(a and b)


#########################################
##2) Crop and reproject Canopy height data

#Make it a function?
#canopy_rast<-raster("Simard_Pinto_3DGlobalVeg_JGR.tif")
canopy_name<-file.path(in_path,infile5)
#new_proj<-proj4string(canopy)                  #Assign coordinates reference system in PROJ4 format
#region_temp_projected<-spTransform(modis_tiles,CRS(new_proj))     #Project from WGS84 to new coord. system
#canopy_crop_rast<-crop(canopy, region_temp_projected) #crop using the extent from teh region tile
#canopy_projected_rast<-projectRaster(from=canopy_crop_rast,crs=proj4string(modis_tiles),method="ngb")
#Use GDAL instead?? system( 'gdalwarp...')

#CANHEIGHT<-raster(s_raster,layer=pos)             #Select layer from stack
#s_raster<-dropLayer(s_raster,pos)
#CANHEIGHT[is.na(CANHEIGHT)]<-0

CANHEIGHT<-create_raster_region(canopy_name,ref_rast)

##########################################
#3) Creating elev, aspect, slope from STRM

SRTM_name<-file.path(elev_path,infile4)
SRTM_reg<-create_raster_region(SRTM_name,ref_rast)

#new_proj<-proj4string(SRTM_rast)                  #Assign coordinates reference system in PROJ4 format
#region_temp_projected<-spTransform(modis_tiles,CRS(new_proj))     #Project from WGS84 to new coord. system
#SRTM_crop_rast<-crop(SRTM, region_temp_projected) #crop using the extent from teh region tile
#SRTM_projected_rast<-projectRaster(from=SRTM,crs=proj4string(modis_tiles),method="ngb")

#pos<-match("ELEV_SRTM",layerNames(s_raster)) #Find column with name "ELEV_SRTM"
#ELEV_SRTM<-raster(s_raster,layer=pos)             #Select layer from stack on 10/30
#s_raster<-dropLayer(s_raster,pos)
#ELEV_SRTM[ELEV_SRTM <0]<-NA

#Call a function to reproject the data in a local projection defined on the fly using the processing tile
#extent...For the time being just use sinusoidal projection.
###calculate slope and aspect

terrain_rast<-terrain(SRTM_reg, opt=c("slope","aspect"),unit="degrees", neighbors=8) #, filename=\u2019\u2019, ...)
pos<-match("aspect",names(terrain_rast)) #Find column with name "value"
r1<-raster(terrain_rast,layer=pos)             #Select layer from stack
pos<-match("slope",names(terrain_rast)) #Find column with name "value"
r2<-raster(terrain_rast,layer=pos)             #Select layer from stack
N<-cos(r1)
E<-sin(r1)
Nw<-sin(r2)*cos(r1)   #Adding a variable to the dataframe
Ew<-sin(r2)*sin(r1)   #Adding variable to the dataframe.

#topo_rast<-stack(STRM_reg,N,E,Nw,Ew)

######################################
#4) LCC land cover

oldpath<-getwd()
setwd(lc_path)
#lc_name<-"con_1km_class_1.tif"
lc_list<-list.files(pattern="con_1km_class_.*.tif")
#lc<-raster(file.path(lc_path,lc_names))

lc_reg_list<-vector("list",length(lc_list))
for (i in 1:length(lc_list)){
  
  lc_name<-lc_list[[i]]
  lc_reg<-create_raster_region(lc_name,ref_rast)
  data_name<-paste("reg_",sub(".tif","",lc_name),"_",sep="") #can add more later...
  raster_name<-paste(data_name,out_suffix,".tif", sep="")
  writeRaster(lc_reg, filename=file.path(out_path,raster_name),overwrite=TRUE)  
  lc_reg_list[[i]]<-file.path(out_path,raster_name)
}
setwd(out_path)
lc_reg_list<-mixedsort(list.files(pattern="reg_con.*.tif"))
lc_reg_s<-stack(lc_reg_list)
#lc_reg_s<-as.character(lc_reg_list)
#Now combine forest classes...in LC1 forest, LC2, LC3, LC4 and LC6-urban...??

#create a local mask for the tile/processing region

LC12<-raster(paste("reg_con_1km_class_12_",out_suffix,".tif",sep="")) #this is open water
LC_mask<-LC12
LC_mask[LC_mask==100]<-NA
LC_mask <- LC_mask > 100
lc_reg_s<-mask(lc_reg_s,LC_mask,filename=paste("reg_con_1km_classes_",out_suffix,".tif",sep=""))

###############################
#5) DISTOC, distance to coast: Would be useful to have a distance to coast layer ready...

#This does not work...clump needs igraph. I'll look into this later...for now I used IDRISI to clump pixels.
#rc<-clump(LC12)
#tab_freq<-freq(rc)
#Modify at a later stage:
#raster<-"DISTOC_VE_01292013.rst"  
#ocean_rast<-raster(file.path(in_path,"lc12_tmp_grouped_rec.rst"))
#ocean_rast[ocean_rast==0]<-NA
#Distance calculated in a global layer??
#distoc_reg<-distance(ocean_rast,doEdge=TRUE) #this is very slow: more than 35 min use GRASS instead??

#load DISTOC produced from IDRISI: automate the process later
distoc_reg<-raster(file.path(in_path,"DISTOC_VE_01292013.rst"))

################################
#6) X-Y coordinates and LAT-LONG: do not keep in memory?
r1 <-ref_rast
xy <-coordinates(r1)  #get x and y projected coordinates...
CRS_interp<-proj4string(r1)
xy_latlon<-project(xy, CRS_interp, inv=TRUE) # find lat long for projected coordinats (or pixels...)
x <-init(r1,v="x")
y <-init(r1,v="y")
lon <-x
lat <-lon
lon <-setValues(lon,xy_latlon[,1]) #longitude for every pixel in the processing tile/region
lat <-setValues(lat,xy_latlon[,2]) #latitude for every pixel in the processing tile/region
rm(r1)
#coord_s<-stack(x,y,lat,lon)

################################
##Step 3: combine covariates in one stack for the next work flow stage
#Create a stack in tif format...

#? output name??
r<-stack(x,y,lon,lat,N,E,Nw,Ew,SRTM_reg,terrain_rast,CANHEIGHT,distoc_reg)
rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
names(r)<-rnames
s_raster<-r
#Add landcover layers
lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
names(lc_reg_s)<-lc_names #assign land cover names
s_raster<-addLayer(s_raster, lc_reg_s)

lst_s<-stack(c(as.character(mean_m_list),as.character(nobs_m_list)))
lst_names<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12",
             "nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
names(lst_s)<-lst_names
s_raster<-addLayer(s_raster, lst_s)
s_raster<-mask(s_raster,LC_mask,filename="test.tif")

covar_names<-c(rnames,lc_names,lst_names)
names(s_raster)<-covar_names
#Write out stack of number of change 
data_name<-paste("covariates_",out_region_name,"_",sep="")
raster_name<-paste(data_name,out_suffix,".tif", sep="")
writeRaster(s_raster, filename=raster_name,NAflag=-999,bylayer=FALSE,bandorder="BSQ",overwrite=TRUE)  #Writing the data in a raster file format...
#using bil format more efficient??

#######################################################
################### END OF SCRIPT #####################

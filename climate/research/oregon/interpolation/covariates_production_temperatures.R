library(raster)
library(gtools)
library(rasterVis)
library(graphics)
library(grid)
library(lattice)
library(rgdal)
##checking results from GRASS climatology calculation...
in_path <- "/home/parmentier/Data/benoit_test"
in_path <- "/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data/"
setwd(in_path)
#mean_h12v08_dec_11
tile="h12v08"
pat_str <- glob2rx(paste("mean","*", tile,"*01232013.tif",sep=""))
tmp_str<- mixedsort(list.files(pattern=pat_str))
infile1<-"worldborder_sinusoidal.shp"
infile2<-"modis_sinusoidal_grid_world.shp"
infile3<-"countries_sinusoidal_world.shp"
tiles = c('h11v08','h11v07','h12v07','h12v08','h10v07','h10v08')

###Reading the station data
filename<-sub(".shp","",infile2)                                             #Removing the extension from file.
modis_grid<-readOGR(".", filename)                                                #Reading shape file using rgdal library

##Function select tiles:
out_file_name<-"modis_venezuela_region"
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

modis_tiles<-create_modis_tiles_region(modis_grid,tiles)

##Create covariates for the stuy area: pull everything from the same folder?

##Extracting the variables values from the raster files                                             

lines<-read.table(paste(path,"/",inlistf,sep=""), sep=" ")                  #Column 1 contains the names of raster files
inlistvar<-lines[,1]
inlistvar<-paste(path,"/",as.character(inlistvar),sep="")
covar_names<-as.character(lines[,2])                                         #Column two contains short names for covaraites

s_raster<- stack(inlistvar)                                                  #Creating a stack of raster images from the list of variables.
layerNames(s_raster)<-covar_names                                            #Assigning names to the raster layers
projection(s_raster)<-CRS

## Crop and reproject Canopy height

canopy_rast<-raster("Simard_Pinto_3DGlobalVeg_JGR.tif")
new_proj<-proj4string(canopy)                  #Assign coordinates reference system in PROJ4 format
region_temp_projected<-spTransform(modis_tiles,CRS(new_proj))     #Project from WGS84 to new coord. system
canopy_crop_rast<-crop(canopy, region_temp_projected) #crop using the extent from teh region tile
canopy_projected_rast<-projectRaster(from=canopy_crop_rast,crs=proj4string(modis_tiles),method="ngb")
#system( 'gdalwarp...')
#1) Creating elev, aspect, slope from STRM

pos<-match("ASPECT",layerNames(s_raster)) #Find column with name "value"
r1<-raster(s_raster,layer=pos)             #Select layer from stack
pos<-match("slope",layerNames(s_raster)) #Find column with name "value"
r2<-raster(s_raster,layer=pos)             #Select layer from stack
N<-cos(r1*pi/180)
E<-sin(r1*pi/180)
Nw<-sin(r2*pi/180)*cos(r1*pi/180)   #Adding a variable to the dataframe
Ew<-sin(r2*pi/180)*sin(r1*pi/180)   #Adding variable to the dataframe.

#2) DISTOOC, distance to coast: Would be useful to have a distance to coast layer ready...

#3) LST clim?

#4) LCC land cover
path_data<-"/home/parmentier/Data/IPLANT_project/Venezuela_interpolation/Venezuela_01142013/input_data/LandCover"
setwd(path_data)
pos<-match("LC1",layerNames(s_raster)) #Find column with name "value"
LC1<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC1[is.na(LC1)]<-0                      #NA must be set to zero.
pos<-match("LC3",layerNames(s_raster)) #Find column with name "value"
LC3<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC3[is.na(LC3)]<-0

#Modification added to account for other land cover
pos<-match("LC4",layerNames(s_raster)) #Find column with name "value"
LC4<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC4[is.na(LC4)]<-0

pos<-match("LC6",layerNames(s_raster)) #Find column with name "value"
LC6<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
LC6[is.na(LC6)]<-0

LC_s<-stack(LC1,LC3,LC4,LC6)
layerNames(LC_s)<-c("LC1_forest","LC3_grass","LC4_crop","LC6_urban")
#plot(LC_s)

pos<-match("CANHEIGHT",layerNames(s_raster)) #Find column with name "value"
CANHEIGHT<-raster(s_raster,layer=pos)             #Select layer from stack
s_raster<-dropLayer(s_raster,pos)
CANHEIGHT[is.na(CANHEIGHT)]<-0
pos<-match("ELEV_SRTM",layerNames(s_raster)) #Find column with name "ELEV_SRTM"
ELEV_SRTM<-raster(s_raster,layer=pos)             #Select layer from stack on 10/30
s_raster<-dropLayer(s_raster,pos)
ELEV_SRTM[ELEV_SRTM <0]<-NA

xy<-coordinates(r1)  #get x and y projected coordinates...
xy_latlon<-project(xy, CRS, inv=TRUE) # find lat long for projected coordinats (or pixels...)
lon<-raster(xy_latlon) #Transform a matrix into a raster object ncol=ncol(r1), nrow=nrow(r1))
ncol(lon)<-ncol(r1)
nrow(lon)<-nrow(r1)
extent(lon)<-extent(r1)
projection(lon)<-CRS  #At this stage this is still an empty raster with 536 nrow and 745 ncell 
lat<-lon
values(lon)<-xy_latlon[,1]
values(lat)<-xy_latlon[,2]

r<-stack(N,E,Nw,Ew,lon,lat,LC1,LC3,LC4,LC6, CANHEIGHT,ELEV_SRTM)
rnames<-c("Northness","Eastness","Northness_w","Eastness_w", "lon","lat","LC1","LC3","LC4","LC6","CANHEIGHT","ELEV_SRTM")
layerNames(r)<-rnames
s_raster<-addLayer(s_raster, r)















#tmp<- readGDAL(tmp_str)
#r<-GDAL.open(tmp_str)
#rt<-create2GDAL(tmp_str, type="Float32")
tmp_rast<-stack(tmp_str)
X11(height=18,width=18)
plot(tmp_rast)
plot(subset(tmp_rast,12))
#levelplot(subset(tmp_rast,12))
png
pat_str2 <- glob2rx(paste("nobs","*", tile,"*.tif",sep=""))
tmp_str2<- mixedsort(list.files(pattern=pat_str2))
tmp_rast2<-stack(tmp_str2)
plot(tmp_rast2)
plot(subset(tmp_rast2,12))
levelplot(subset(tmp_rast2,12))
levelplot(tmp_rast2)
png(filename="test.png",width=2400,height=2400)
levelplot(tmp_rast2)
dev.off()





library(ncdf)
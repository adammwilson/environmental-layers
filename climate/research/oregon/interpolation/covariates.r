### script to assemble, reproject and align covariate raster datasets.

###Loading R library and packages   
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                                          # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                                          # GDAL wrapper for R, spatial utilities
library(raster)                                         # Raster package for image processing by Hijmans et al. 

###Parameters and arguments
path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"             #Path to all datasets on Atlas
path<-"/home/adamw/acrobates/projects/interp"                                 #Path to all datasets on Atlas

region="oregon"
datapath=paste(path,"/data/regions/",region,sep="")

setwd(path)                                                                   #Setting the working directory

## define working projection
dproj="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs" #define projection

## load roi polygon

### use MODIS tile as ROI instead
modt=readOGR("/home/adamw/acrobates/Global/modis_sinusoidal","modis_sinusoidal_grid_world",)
tiles=c("H9V4")
roi=modt[modt$HV%in%tiles,]

## Bounding box of region in lat/lon
roi_ll=spTransform(roi,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
roi_bb=bbox(roi_ll)

## Bounding box of region in lcc
roi_lcc=bbox(spTransform(roi,CRS(dproj)))

##  Get DEM data
## mount atlas to subset elevation data
system("sshfs -o idmap='user' wilson@atlas.nceas.ucsb.edu:/ /media/data/atlas")
## run subset on atlas
system(paste("ssh atlas \"gdalwarp -r cubic -multi -t_srs \'",dproj,"\' -te ",paste(as.numeric(roi_lcc),collapse=" ")," /home/layers/data/terrain/dem-fused/SRTM_West_S60_N55.tif /home/wilson/data/oregon/topo/dem.tif \"",sep=""))
## copy to litoria
file.copy("/media/data/atlas/home/wilson/data/oregon/topo/dem.tif",paste(datapath,"/topo/dem.tif",sep=""),overwrite=T)


### Resample fine resolution DEM to generate 1km version
#GDALinfo(paste(datapath,"/topo/dem.tif",sep=""))
#dem2=aggregate(raster(paste(datapath,"/topo/dem.tif",sep="")),fact=7)


dest=raster("data/lulc/W_Layer1_ClippedTo_OR83M.rst")  # choose one to match (projection, resolution, extent)
projection(dest)=CRS()
tifs=list.files(summarydatadir,pattern="*.tif$",full=T) #get list of files to process
mclapply(tifs,function(f) projectRaster(raster(f),dest,filename=paste(dirname(f),"/OR03M_",basename(f),sep=""),overwrite=T))  # warp them


#### Topography

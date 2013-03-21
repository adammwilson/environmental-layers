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
#DATE: 03/21/2013                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--   

##Comments and TODO:
#This script is meant to be for general processing tile by tile or region by region.
#We must decide on a local projection. This can best set up from the tile/region extent: for now use
#- lcc with two standard paralells and one central meridian in the middle of the region.
#- produce a distance to ocean layer that is global.
#- do not keep output in memory??

##################################################################################################


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

covariates_production_temperature<-function(list_param){
  #This functions produce covariates used in the interpolation of temperature.
  #It requires 15 arguments:
  #
  #
  #
  
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
  
  ###########################################################
  ############ Main body: BEGIN--START OF THE SCRIPT ###################
  
  ##### STEP 1: PARSING ARGUMENTS
  
  var<-list_param$var
  in_path <-list_param$in_path
  out_path<- list_param$out_path
  lc_path<-list_param$lc_path 
  infile_modis_grid<-list_param$infile_modis_grid
  infile_elev<-list_param$infile_elev #this is the global file: replace later with the input produced by the DEM team
  infile_canheight<-list_param$infile_canheight #Canopy height
  list_tiles_modis<-list_param$list_tiles_modis #tile for Venezuela and surrounding area
  infile_reg_outline<-list_param$infile_reg_outline   #input region outline defined by polygon
  CRS_interp<-list_param$CRS_interp #local projection system
  CRS_locs_WGS84<-list_param$CRS_locs_WGS84 #
  out_region_name<-list_param$out_region_name  #generated on the fly
  out_suffix<-list_param$out_suffix 
  ref_rast_name<-list_param$ref_rast_name #local raster name defining resolution, exent, local projection--. set on the fly??
  covar_names<-list_param$covar_names 
  
  ##### SET UP STUDY AREA ####
  
  setwd(in_path)
  
  filename<-sub(".shp","",infile_modis_grid)       #Removing the extension from file.
  modis_grid<-readOGR(".", filename)     #Reading shape file using rgdal library
  #filename<-sub(".shp","",infile1)       #Removing the extension from file.
  #world_countries<-readOGR(".", filename)     #Reading shape file using rgdal library
  
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
  
  #Should add option for a reference file here...
  #tmp<-extent(ref_rast)
  
  #modis_tiles<-create_modis_tiles_region(modis_grid,list_tiles_modis)
  ##Create covariates for the stuy area: pull everything from the same folder?
  
  #### STEP 2: process and/or produce covariates for the tile/region
  
  ################################
  #1) LST climatology: project, mosaic
  i<-1
  tile<-list_tiles_modis[i]
  if (var=="TMIN"){
    lst_pat<-"LST_Night_1km"
  }
  if (var=="TMAX"){
    lst_pat<-"" #for the time being change at later stage...
    #day_pat<-"LST_Day_1km"
  }
  
  #Get list of files containing the LST averages
  pat_str2 <- glob2rx(paste("nobs","*",lst_pat,"*.tif",sep=""))
  tmp_str2<- mixedsort(list.files(pattern=pat_str2))
  pat_str1 <- glob2rx(paste("mean","*",lst_pat,"*.tif",sep=""))
  tmp_str1<- mixedsort(list.files(pattern=pat_str1))
  #add lines using grep to select tiles...
 
  #Format list for mosaicing: mosaic for every month the relevant number of files
  #out_rastnames<-paste("_lst_","nobs",out_suffix,sep="")
  out_rastnames<-paste("_",lst_pat,"_","nobs",out_suffix,sep="")
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
  #nobs_m_list<-list.files(pattern='mosaiced_.*._lst_nobs_VE_03182013.tif')
  nobs_m_list<-mosaic_raster_list(mosaic_list,out_rastnames,out_path)
  
  ##Now mosaic for mean: should reorder files!!
  pat_str1 <- glob2rx(paste("mean","*.tif",sep=""))
  tmp_str1<- mixedsort(list.files(pattern=pat_str1))
  out_rastnames<-paste("_",lst_pat,"_","mean",out_suffix,sep="")
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
  #mean_m_list<-list.files(pattern='mosaiced_.*._lst_mean_VE_03182013.tif')
  mean_m_list<-mosaic_raster_list(mosaic_list,out_rastnames,out_path)

  #Use this as ref file for now?? Ok for the time being: this will need to change to be a processing tile.
  ref_rast<-raster(mean_m_list[[1]])
  #ref_rast <-raster("mosaiced_dec_lst_mean_VE_03182013.tif")
  #Modis shapefile tile is slighly shifted:
  # +proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs for ref_rast
  #"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" ??
  #reassign proj from modis tile to raster? there is a 10m diff in semi-axes...(a and b)
  
  ##Write function to screen data values...
  
  #Screen LST for extreme values?
  #min_val<-(-15+273.16) #if values less than -15C then screen out (note the Kelvin units that will need to be changed later in all datasets)
  #LST[LST < (min_val)]<-NA
  
  #########################################
  ##2) Crop and reproject Canopy height data
  
  #Make it a function?
  #canopy_rast<-raster("Simard_Pinto_3DGlobalVeg_JGR.tif")
  canopy_name<-file.path(in_path,infile_canheight)
  
  CANHEIGHT<-create_raster_region(canopy_name,ref_rast)
  
  ##########################################
  #3) Creating elev, aspect, slope from STRM
  
  SRTM_reg<-create_raster_region(infile_elev,ref_rast)
  
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
  lc_reg_list<-mixedsort(list.files(pattern=paste("^reg_con_1km_class_.*.",out_suffix,".tif",sep="")))
  lc_reg_s<-stack(lc_reg_list)
  
  #Now combine forest classes...in LC1 forest, LC2, LC3, LC4 and LC6-urban...??
  
  #create a local mask for the tile/processing region
  
  #LC12<-raster(paste("reg_con_1km_class_12_",out_suffix,".tif",sep="")) #this is open water
  LC12<-raster(lc_reg_s,layer=nlayers(lc_reg_s)) #this is open water
  LC_mask<-LC12
  LC_mask[LC_mask==100]<-NA
  LC_mask <- LC_mask > 100
  
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
  #ref_rast <-raster("mosaiced_dec_lst_mean_VE_03182013.tif")
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

  r<-stack(x,y,lon,lat,N,E,Nw,Ew,SRTM_reg,terrain_rast,CANHEIGHT,distoc_reg)
  #rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
  s_raster<-r
  #Add landcover layers
  #lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
  s_raster<-addLayer(s_raster, lc_reg_s)
  
  lst_s<-stack(c(as.character(mean_m_list),as.character(nobs_m_list)))

  s_raster<-addLayer(s_raster, lst_s)
  
  #covar_names<-c(rnames,lc_names,lst_names)
  names(s_raster)<-covar_names
  #Write out stack of number of change 
  data_name<-paste("covariates_",out_region_name,"_",sep="")
  raster_name<-paste(data_name,var,"_",out_suffix,".tif", sep="")
  #writeRaster(s_raster, filename=raster_name,NAflag=-999,bylayer=FALSE,bandorder="BSQ",overwrite=TRUE)  #Writing the data in a raster file format...
  s_raster_m<-mask(s_raster,LC_mask,filename=raster_name,
                 overwrite=TRUE,NAflag=-999,bylayer=FALSE,bandorder="BSQ")
  #using bil format more efficient??
  return(raster_name)
}

#######################################################
################### END OF SCRIPT/FUNCTION #####################
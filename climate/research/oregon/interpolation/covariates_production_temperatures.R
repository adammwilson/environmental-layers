##################    Data preparation for interpolation   #######################################
############################ Covariate production for a given tile/region ##########################################
#This script produces covariates raster for a a specfied study area.                             
# It requires the following inputs:                                                              
# 1)list of modis tiles or shape file with region outline            
# 2)input names for global covariates:
# -elevation: SRTM CGIAR 1 km (replace by global DEM?)
# -Canopy heihgt (Simard)
# -12 layers from land cover concensus (Jetz lab)
# -distance to coast NASA/NOOA (http://oceancolor.gsfc.nasa.gov/DOCS/DistFromCoast/)
# -24 LST layers: "climatology" produced from MOD11A1, LST (mean and obs) using script in step 1 of workflow
# 3) The output is a multiband file in tif format with projected covariates for the processing region/tile.             
#AUTHOR: Benoit Parmentier                                                                       
#DATE: 05/27/2013                                                                                 
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--   

##Comments and TODO:
#This script is meant to be for general processing tile by tile or region by region.
#We must decide on a local projection. This can best set up from the tile/region extent: for now use
#- lcc with two standard paralells and one central meridian in the middle of the region.
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
  #This functions returns a subset of tiles from the modis grdid.
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

create__m_raster_region <-function(j,list_param){
  #This functions returns a subset of tiles from the modis grdid.
  #Arguments: raster name of the file,reference file with
  #Output: spatial grid data frame of the subset of tiles
  
  ## Parse input arguments
  raster_name <- list_param$raster_name[[j]] #list of raster ot project and crop, this is a list!!
  reg_ref_rast <- list_param$reg_ref_rast #This must have a coordinate system defined!!
  out_rast_name <- list_param$out_rast_name[j]
  
  ## Start #
  layer_rast<-raster(raster_name)
  new_proj<-proj4string(layer_rast)                  #Extract current coordinates reference system in PROJ4 format
  region_temp_projected<-projectExtent(reg_ref_rast,CRS(new_proj))     #Project from ref to current region coord. system
  layer_crop_rast<-crop(layer_rast, region_temp_projected) #crop using the extent from the region tile
  #layer_projected_rast<-projectRaster(from=layer_crop_rast,crs=proj4string(reg_outline),method="ngb")
  layer_projected_rast<-projectRaster(from=layer_crop_rast,to=reg_ref_rast,method="ngb")
  
  writeRaster(layer_projected_rast, filename=out_rast_name,overwrite=TRUE)  
  
  return(out_rast_name)
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

mosaic_m_raster_list<-function(j,list_param){
  #This functions returns a subset of tiles from the modis grid.
  #Arguments: modies grid tile,list of tiles
  #Output: spatial grid data frame of the subset of tiles
  #Note that rasters are assumed to be in the same projection system!!
  
  #rast_list<-vector("list",length(mosaic_list))
  #for (i in 1:length(mosaic_list)){  
    # read the individual rasters into a list of RasterLayer objects
    # this may be changed so that it is not read in the memory!!!
  
  #parse output...
  
  #j<-list_param$j
  mosaic_list<-list_param$mosaic_list
  out_path<-list_param$out_path
  out_names<-list_param$out_rastnames
  ## Start
  
  input.rasters <- lapply(as.character(mosaic_list[[j]]), raster)
  mosaiced_rast<-input.rasters[[1]]
    
  for (k in 2:length(input.rasters)){
    mosaiced_rast<-mosaic(mosaiced_rast,input.rasters[[k]], fun=mean)
    #mosaiced_rast<-mosaic(mosaiced_rast,raster(input.rasters[[k]]), fun=mean)
  }
  
  data_name<-paste("mosaiced_",sep="") #can add more later...
  raster_name<-paste(data_name,out_names[j],".tif", sep="")
  writeRaster(mosaiced_rast, filename=file.path(out_path,raster_name),overwrite=TRUE)  
  #Writing the data in a raster file format...  
  rast_list<-file.path(out_path,raster_name)
  
  return(rast_list)
}

#Works for now improve later?    
change_names_file_list<-function(list_name,out_suffix,out_prefix,extension,out_path=""){
  #Function to add suffix and prefix to list of file names
  lf_new_names_list<-vector("list",length(list_name)) #this will contain new names for files
  for (i in 1:length(list_name)){
    
    lf_name<-basename(list_name[[i]])
    lf_out_path<-dirname(list_name[[i]])
    data_name<-paste(out_prefix,sub(extension,"",lf_name),"_",sep="") #can add more later...
    raster_name<-paste(data_name,out_suffix, sep="") #out_suffix must include extension!!!
    if((lf_out_path!="") & (out_path=="")){
      lf_new_names_list[[i]]<-file.path(lf_out_path,raster_name)
    }else{
      lf_new_names_list[[i]]<-file.path(out_path,raster_name)
    }
    
  }
  return(unlist(lf_new_names_list))
}

covariates_production_temperature<-function(list_param){
  #This functions produce covariates used in the interpolation of temperature.
  #It requires 16 arguments:
  #1)  var : interpolated variable: TMIN, TMAX, (PRCP?)
  #2)  out_path : output directory 
  #3)  lc_path : path to land cover consensus product
  #4)  infile_modis_grid : modis grid litles 
  #5)  infile_elev : this is the global file: replace later with the input produced by the DEM team
  #6)  infile_canheight : Canopy height
  #7)  infile_distoc : global distance to coast
  #8)  list_tiles_modis : tile for Venezuela and surrounding area
  #9)  infile_reg_outline : input region outline defined by polygon
  #10)  CRS_interp : local projection system
  #11) CRS_locs_WGS84 : CRS_locs_WGS84 #
  #12) out_region_name : generated on the fly?
  #13) out_suffix : added to the covariates stack/brick 
  #14) out_suffix_modis : suffix used in the LST averages used for the covariates (may vary to use older LST averages)
  #15) ref_rast_name: local raster name defining resolution, exent, local projection--. set on the fly??
  #16) hdfdir: directory where the LST averages are stored...
  #17) out_suffix_modis : suffix used in producing LST climatology 
  #18) covar_names : names of covariates
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
  out_path<- list_param$out_path
  lc_path<-list_param$lc_path 
  infile_modis_grid<-list_param$infile_modis_grid
  infile_elev<-list_param$infile_elev #this is the global file: replace later with the input produced by the DEM team
  infile_canheight<-list_param$infile_canheight #Canopy height
  infile_distoc<-list_param$infile_distoc #Canopy height
  list_tiles_modis<-list_param$list_tiles_modis #tile for Venezuela and surrounding area
  infile_reg_outline <-list_param$infile_reg_outline   #input region outline defined by polygon
  CRS_interp<-list_param$CRS_interp #local projection system
  CRS_locs_WGS84<-list_param$CRS_locs_WGS84 #
  out_region_name<-list_param$out_region_name  #generated on the fly
  out_suffix<-list_param$out_suffix 
  ref_rast_name<-list_param$ref_rast_name #local raster name defining resolution, exent, local projection--. set on the fly??
  hdfdir <- list_param$hdfdir
  out_suffix_modis <- list_param$out_suffix_modis
  covar_names<-list_param$covar_names 
  
  ##### SET UP STUDY AREA ####
  
  setwd(out_path)
  
  list_tiles_modis <- unlist(strsplit(list_tiles_modis,","))  # transform string into separate element in char vector
  
  filename<-sub(".shp","",basename(infile_modis_grid))       #Removing path and the extension from file name.
  #modis_grid<-readOGR(".", filename)     #Reading shape file using rgdal library
  modis_grid<-readOGR(dsn=dirname(infile_modis_grid), filename)     #Reading shape file using rgdal library
  
  #filename<-sub(".shp","",infile1)       #Removing the extension from file.
  #world_countries<-readOGR(".", filename)     #Reading shape file using rgdal library
  #outfile1<-file.path(out_path,paste("stations","_",out_prefix,".shp",sep=""))
  #writeOGR(stat_reg,dsn= dirname(outfile1),layer= sub(".shp","",basename(outfile1)), driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  if (infile_reg_outline!=""){
    filename<-sub(".shp","",basename(infile_reg_outline))   #Removing path and the extension from file name.
    reg_outline<-readOGR(dsn=dirname(infile_reg_outline), filename) # Read in the region outline
  }
  
  #if no shapefile defining the study/processing area then create one using modis grid tiles
  if (infile_reg_outline==""){
    reg_outline<-create_modis_tiles_region(modis_grid,list_tiles_modis) #problem...this does not 
    #align with extent of modis LST!!!
    infile_reg_outline <-paste("outline",out_region_name,"_",out_suffix,".shp",sep="")
    writeOGR(reg_outline,dsn= out_path,layer= sub(".shp","",infile_reg_outline), 
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
    lst_pat<-"LST_Day_1km" #for the time being change at later stage...
  }
  
  #Need to clean up this section, later on, this should be written in a function to avoid repetition
  #Get list of files containing the LST averages
  hdfdir_lst_avg<-file.path(hdfdir,"LST_averages")
  pat_str2 <- glob2rx(paste("nobs","*",lst_pat,"*",out_suffix_modis,"*.tif",sep=""))
  tmp_str2 <- mixedsort(list.files(path=hdfdir_lst_avg,pattern=pat_str2,full.names=TRUE)) #note that this assumes the LST averages are stored in hdfdir
  pat_str1 <- glob2rx(paste("mean","*",lst_pat,"*",out_suffix_modis,"*.tif",sep=""))
  tmp_str1 <- mixedsort(list.files(path=hdfdir_lst_avg,pattern=pat_str1,full.names=TRUE))
  #add lines using grep to select tiles...
 
  #Format list for mosaicing: mosaic for every month the relevant number of files
  out_rastnames<-paste("_",lst_pat,"_","nobs",out_suffix,sep="")
  list_date_names<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  mosaic_list<-split(tmp_str2,list_date_names) #split list of files into groups using lst_date_name_pattern
  
  new_list<-vector("list",length(mosaic_list))
  for (i in 1:length(list_date_names)){
    j<-grep(list_date_names[i],mosaic_list,value=FALSE)
    names(mosaic_list)[j]<-list_date_names[i]
    new_list[i]<-mosaic_list[j]
  }
  mosaic_list_nobs<-new_list
  out_rastnames_nobs<-paste(list_date_names,out_rastnames,sep="")
  
  ##Now mosaic for mean: should reorder files!!
  out_rastnames_mean<-paste("_",lst_pat,"_","mean",out_suffix,sep="")
  list_date_names<-c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  mosaic_list<-split(tmp_str1,list_date_names)
  new_list<-vector("list",length(mosaic_list))
  for (i in 1:length(list_date_names)){
    j<-grep(list_date_names[i],mosaic_list,value=FALSE)
    names(mosaic_list)[j]<-list_date_names[i]
    new_list[i]<-mosaic_list[j]
  }
  mosaic_list_mean <-new_list #list ready for mosaicing
  out_rastnames_mean<-paste(list_date_names,out_rastnames_mean,sep="")
  
  ### Now mosaic tiles...Note that function could be improved to use less memory
  list_param_mosaic<-list(j,mosaic_list_mean,out_rastnames_mean,out_path)
  names(list_param_mosaic)<-c("j","mosaic_list","out_rastnames","out_path")
  mean_m_list <-mclapply(1:12, list_param=list_param_mosaic, mosaic_m_raster_list,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
  
  list_param_mosaic<-list(j,mosaic_list_nobs,out_rastnames_nobs,out_path)
  names(list_param_mosaic)<-c("j","mosaic_list","out_rastnames","out_path")
  nobs_m_list <-mclapply(1:12, list_param=list_param_mosaic, mosaic_m_raster_list,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
  
  #Use this as ref file for now?? Ok for the time being: this will need to change to be a processing tile.
  if (ref_rast_name==""){
    ref_rast<-raster(mean_m_list[[1]]) 
    #Use one mosaiced modis tile as reference image...We will need to add a function 
    #to define a local reference system and reproject later!!
    #Assign new projection system here in the argument CRS_interp (!it is used later)
  }else{
    ref_rast<-raster(ref_rast_name) #This is the reference image used to define the study/processing area
    proj4string(ref_rast) <- CRS_interp #Assign given reference system
  }
  
  ## Project mosaiced tiles if local projection is provided...

  out_suffix_lst <-paste(out_suffix,".tif",sep="")          
  mean_lst_list_outnames<-change_names_file_list(mean_m_list,out_suffix_lst,"reg_",".tif",out_path=out_path)     
  nobs_lst_list_outnames<-change_names_file_list(nobs_m_list,out_suffix_lst,"reg_",".tif",out_path=out_path)   
  
  # if ref_name!="" need to reproject and clip!!! do this in mclapply for all the list of covar!!!
  if (ref_rast_name!=""){
    #list(mean_m_list)
    list_param_create_region<-list(j,raster_name=mean_m_list,reg_ref_rast=ref_rast,out_rast_name=mean_lst_list_outnames)
    #test<-create__m_raster_region(1,list_param_create_region)
    mean_m_list <-mclapply(1:12, list_param=list_param_create_region, create__m_raster_region,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
    list_param_create_region<-list(j,raster_name=nobs_m_list,reg_ref_rast=ref_rast,out_rast_name=nobs_lst_list_outnames)
    nobs_m_list <-mclapply(1:12, list_param=list_param_create_region, create__m_raster_region,mc.preschedule=FALSE,mc.cores = 6) #This is the end bracket from mclapply(...) statement
  }

  
  #ref_rast <-raster("mosaiced_dec_lst_mean_VE_03182013.tif")
  #Modis shapefile tile is slighly shifted:
  # +proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs for ref_rast
  #"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" ??
  #reassign proj from modis tile to raster? there is a 10m diff in semi-axes...(a and b)
  
  ######################################
  #4) LCC land cover
  
  lc_list<-mixedsort(list.files(path=lc_path,pattern="con_1km_class_.*.tif",full.names=TRUE))
  
  out_suffix_lc<-paste(out_suffix,".tif",sep="")          
  lc_list_outnames<-change_names_file_list(lc_list,out_suffix_lc,"reg_",".tif",out_path=out_path)    
  list_param_create_region<-list(j,raster_name=lc_list,reg_ref_rast=ref_rast,out_rast_name=lc_list_outnames)
  lc_reg_list <-mclapply(1:length(lc_list), list_param=list_param_create_region, create__m_raster_region,mc.preschedule=FALSE,mc.cores = 9) #This is the end bracket from mclapply(...) statement
  lc_reg_s <- stack(lc_reg_list)
  
  #Now create mask based on water areas 

  LC12<-raster(lc_reg_s,layer=nlayers(lc_reg_s)) #this is open water
  LC_mask<-LC12
  LC_mask[LC_mask==100]<-NA
  LC_mask <- LC_mask > 100
  
  ###############################
  #5) DISTOC, distance to coast: Would be useful to have a distance to coast layer ready...
  #Crop and reproject Canopy height data

  lc_list <- c(infile_elev,infile_canheight,infile_distoc) #, lc_list #15 layers to reproject...
  out_suffix_l <-paste(out_suffix,".tif",sep="")          
  lc_list_outnames<-change_names_file_list(lc_list,out_suffix_l,"reg_",".tif",out_path=out_path)    
  list_param_create_region<-list(j,raster_name=lc_list,reg_ref_rast=ref_rast,out_rast_name=lc_list_outnames)
  list_covar_layers <- mclapply(1:3, list_param=list_param_create_region, create__m_raster_region,mc.preschedule=FALSE,mc.cores = 3) #This is the end bracket from mclapply(...) statement
  
  SRTM_reg <- raster(list_covar_layers[[1]])
  CANHEIGHT <- raster(list_covar_layers[[2]])
  distoc <- raster(list_covar_layers[[3]])
    
  ##########################################
  #3) aspect, slope from STRM
    
  terrain_rast<-terrain(SRTM_reg, opt=c("slope","aspect"),unit="degrees", neighbors=8) #, filename=\u2019\u2019, ...)
  pos<-match("aspect",names(terrain_rast)) #Find column with name "value"
  r1<-raster(terrain_rast,layer=pos)             #Select layer from stack
  pos<-match("slope",names(terrain_rast)) #Find column with name "value"
  r2<-raster(terrain_rast,layer=pos)             #Select layer from stack
  N<-cos(r1)
  E<-sin(r1)
  Nw<-sin(r2)*cos(r1)   #Adding a variable to the dataframe
  Ew<-sin(r2)*sin(r1)   #Adding variable to the dataframe.
  
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

  r<-stack(x,y,lon,lat,N,E,Nw,Ew,SRTM_reg,terrain_rast,CANHEIGHT,distoc)
  #rnames<-c("x","y","lon","lat","N","E","N_w","E_w","elev","slope","aspect","CANHEIGHT","DISTOC")
  s_raster<-r
  #Add landcover layers
  #lc_names<-c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12")
  s_raster<-addLayer(s_raster, lc_reg_s)
  
  lst_s<-stack(c(as.character(mean_m_list),as.character(nobs_m_list)))

  s_raster<-addLayer(s_raster, lst_s)
  
  #covar_names<-c(rnames,lc_names,lst_names)
  names(s_raster)<-covar_names
  
  ##Write function to screen data values...
  
  #Screen LST for extreme values?
  #min_val<-(-15+273.16) #if values less than -15C then screen out (note the Kelvin units that will need to be changed later in all datasets)
  #LST[LST < (min_val)]<-NA
  
  #add screening here...
  
  #Write out stack of number of change 
  data_name<-paste("covariates_",out_region_name,"_",sep="")
  raster_name<-paste(data_name,var,"_",out_suffix,".tif", sep="")
  #writeRaster(s_raster, filename=raster_name,NAflag=-999,bylayer=FALSE,bandorder="BSQ",overwrite=TRUE)  #Writing the data in a raster file format...
  #Mask and save layers
  s_raster_m<-mask(s_raster,LC_mask,filename=raster_name,
                 overwrite=TRUE,NAflag=-999,bylayer=FALSE,bandorder="BSQ")
  #using bil format more efficient??
  
  #return reg_outline!!!
  covar_obj <-list(raster_name,infile_reg_outline)
  names(covar_obj) <-c("infile_covariates","infile_reg_outline")
  return(covar_obj)
}

#######################################################
################### END OF SCRIPT/FUNCTION #####################
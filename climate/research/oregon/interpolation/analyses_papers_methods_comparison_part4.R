### Analyses and exploration of results for single time scale methods

### Loading R library and packages        
#library used in the workflow production:
library(gtools)                              # loading some useful tools 
library(mgcv)                                # GAM package by Simon Wood
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                              # NCAR Spatial Interpolation methods such as kriging, splines
library(raster)                              # Hijmans et al. package for raster processing
library(gdata)                               # various tools with xls reading, cbindX
library(rasterVis)                           # Raster plotting functions
library(parallel)                            # Parallelization of processes with multiple cores
library(maptools)                            # Tools and functions for sp and other spatial objects e.g. spCbind
library(maps)                                # Tools and data for spatial/geographic objects
library(reshape)                             # Change shape of object, summarize results 
library(plotrix)                             # Additional plotting functions
library(plyr)                                # Various tools including rbind.fill
library(spgwr)                               # GWR method
library(automap)                             # Kriging automatic fitting of variogram using gstat
library(rgeos)                               # Geometric, topologic library of functions
#RPostgreSQL                                 # Interface R and Postgres, not used in this script

#Additional libraries not used in workflow
library(pgirmess)                            # Krusall Wallis test with mulitple options, Kruskalmc {pgirmess}  
library(ncf)

#### FUNCTION USED IN SCRIPT

function_analyses_paper <-"contribution_of_covariates_paper_interpolation_functions_10152013.R"

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}


##############################
#### Parameters and constants  

script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/" #path to script
source(file.path(script_path,function_analyses_paper)) #source all functions used in this script.

in_dir1 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb3_08132013"
in_dir2 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb4_08152013"
#kriging results:
in_dir3 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_kriging_day_lst_comb3_07112013"
#gwr results:
in_dir4 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gwr_day_lst_comb3_part1_07122013"
#multisampling results (gam)
#in_dir5<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_daily_mults10_lst_comb3_08082013"
#in_dir6<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_kriging_daily_mults10_lst_comb3_08062013"
#in_dir7<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gwr_daily_mults10_lst_comb3_08072013"
#Hold-out every two days over 365 days
in_dir5 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_daily_mults1_lst_comb3_10122013"
in_dir6 <- "/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_daily_mults1_lst_comb3_10112013"
in_dir7 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_daily_mults1_lst_comb3_10132013"

out_dir<-"/home/parmentier/Data/IPLANT_project/paper_analyses_tables_fig_08032013"
setwd(out_dir)

infile_reg_outline <- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
met_stations_outfiles_obj_file<-"/data/project/layers/commons/data_workflow/output_data_365d_gam_fus_lst_test_run_07172013/met_stations_outfiles_obj_gam_fusion__365d_gam_fus_lst_test_run_07172013.RData"
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
y_var_name <- "dailyTmax"
out_prefix<-"analyses_10152013"

#method_interpolation <- "gam_daily"
covar_obj_file_1 <- "covar_obj__365d_gam_day_lst_comb3_08132013.RData"
met_obj_file_1 <- "met_stations_outfiles_obj_gam_daily__365d_gam_day_lst_comb3_08132013.RData"
#met_stations_outfiles_obj_gam_daily__365d_gam_day_lst_comb3_08132013.RData

#raster_prediciton object for baseline 1 () s(lat,lon) + s(elev)) and baseline 2 (slat,lon))
raster_obj_file_1 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_lst_comb3_08132013.RData" 
raster_obj_file_2 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_lst_comb4_08152013.RData"

raster_obj_file_3 <- "raster_prediction_obj_kriging_daily_dailyTmax_365d_kriging_day_lst_comb3_07112013.RData"
raster_obj_file_4 <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_day_lst_comb3_part1_07122013.RData"
#multisampling using baseline lat,lon + elev
#raster_obj_file_5 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_daily_mults10_lst_comb3_08082013.RData"
#raster_obj_file_6 <- "raster_prediction_obj_kriging_daily_dailyTmax_365d_kriging_daily_mults10_lst_comb3_08062013.RData"
#raster_obj_file_7 <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_daily_mults10_lst_comb3_08072013.RData"

raster_obj_file_5 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_daily_mults1_lst_comb3_10122013.RData"
raster_obj_file_6 <- "raster_prediction_obj_kriging_daily_dailyTmax_365d_kriging_daily_mults1_lst_comb3_10112013.RData"
raster_obj_file_7 <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_daily_mults1_lst_comb3_10132013.RData"

#Load objects containing training, testing, models objects 

met_stations_obj <- load_obj(met_stations_outfiles_obj_file)
covar_obj <-load_obj(file.path(in_dir1,covar_obj_file_1)) #Reading covariates object for GAM daily method
infile_covariates <- covar_obj$infile_covariates
infile_reg_outline <- covar_obj$infile_reg_outline
covar_names<- covar_obj$covar_names
#####
s_raster <- brick(infile_covariates)
names(s_raster)<-covar_names

raster_prediction_obj_1 <-load_obj(file.path(in_dir1,raster_obj_file_1)) #comb3 (baseline 2)
raster_prediction_obj_2 <-load_obj(file.path(in_dir2,raster_obj_file_2)) #comb4 (baseline 1)
raster_prediction_obj_3 <-load_obj(file.path(in_dir3,raster_obj_file_3)) #comb3/mod1 baseline 2, kriging
raster_prediction_obj_4 <-load_obj(file.path(in_dir4,raster_obj_file_4)) #comb3/mod1 baseline 2, gwr
raster_prediction_obj_5 <-load_obj(file.path(in_dir5,raster_obj_file_5)) #gam daily multisampling 10 to 70%
raster_prediction_obj_6 <-load_obj(file.path(in_dir6,raster_obj_file_6)) #kriging daily multisampling 10 to 70% 
raster_prediction_obj_7 <-load_obj(file.path(in_dir7,raster_obj_file_7)) #gwr daily multisampling 10 to 70%


############### BEGIN SCRIPT #################

############ PART 1: Exploration of surfaces bias, delta and climatology surfaces ###########

#"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb4_07152013/"
in_dir<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_kriging_cai_lst_comb3_07312013"

out_dir<-""
setwd(in_dir)
y_var_name <- "dailyTmax"
y_var_month <- "TMax"
#y_var_month <- "LSTD_bias"

out_prefix<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_GAM_fus_all_lst_05312013"
method_interpolation <- "kriging_CAI"
covar_obj_file <- "covar_obj__365d_kriging_cai_lst_comb3_07312013.RData"
raster_obj_file <- "raster_prediction_obj_kriging_CAI_dailyTmax_365d_kriging_cai_lst_comb3_07312013.RData"
script_path<-"/data/project/layers/commons/data_workflow/env_layers_scripts/"

source(file.path(script_path,"interpolation_method_day_function_multisampling_07052013.R")) #Include GAM_day

#Load objects containing training, testing, models objects 
covar_obj <-load_obj(covar_obj_file)
infile_covariates <- covar_obj$infile_covariates
infile_reg_outline <- covar_obj$infile_reg_outline
covar_names<- covar_obj$covar_names

raster_prediction_obj <-load_obj(raster_obj_file)

names(raster_prediction_obj) #list of two objects

raster_prediction_obj$summary_metrics_v

j<-1 #selected month for climatology 
i<-1
data_s <- raster_prediction_obj$method_mod_obj[[i]]$data_s #training data
data_v <- raster_prediction_obj$method_mod_obj[[i]]$data_v #testing data

method_mod_obj <- raster_prediction_obj$method_mod_obj #this object contains daily information, training, testing and images

clim_method_mod_obj <- raster_prediction_obj$clim_method_mod_obj
data_month1 <- clim_method_mod_obj[[1]]$data_month #monthly data
data_month <- clim_method_mod_obj[[j]]$data_month #monthly data
clim_mod_obj_month <- clim_method_mod_obj[[j]]
names(clim_mod_obj_month)

#####
s_raster <- brick(infile_covariates)
names(s_raster)<-covar_names
#data_s$y_var <- data_s[[y_var_name]]
#formula<-"y_var ~ s(lat,lon,elev_s)"
#date<-strptime(sampling_dat$date[i], "%Y%m%d")   # interpolation date being processed
month<-strftime(date, "%m")          # current month of the date being processed
month<-"01"
LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
pos<-match("LST",names(s_raster)) #Find the position of the layer with name "LST", if not present pos=NA
s_raster<-dropLayer(s_raster,pos)      # If it exists drop layer
LST<-subset(s_raster,LST_month)
names(LST)<-"LST"
s_raster<-addLayer(s_raster,LST)            #Adding current month


### MONTH MODELS
index<-1
data_month$y_var<-data_month[[y_var_month]]
mod1 <- clim_method_mod_obj[[1]]$mod[[1]] 


clim_method_mod_obj[[1]]$clim #list of files containing model predictions...
clim_rast<-stack(clim_method_mod_obj[[index]]$clim)  
delta_rast<-raster(method_mod_obj[[index]]$delta) #only one delta image!!!
pred_temp<-as.character(method_mod_obj[[index]][[y_var_name]]) #list of files with path included
rast_pred_temp_s <-stack(pred_temp) #stack of temperature predictions from models (daily)

names(delta_rast)<-"delta"
rast_temp_date<-stack(clim_rast,delta_rast)
#rast_temp_date<-mask(rast_temp_date,LC_mask,file=file.path(out_path,"test.tif"),overwrite=TRUE)
#bias_d_rast<-raster("fusion_bias_LST_20100103_30_1_10d_GAM_fus5_all_lstd_02082013.rst")

plot(rast_temp_date)

month_m_rast<-subset(clim_rast,"mod1")
day_m_rast<-subset(rast_pred_temp_s,1)

test<- month_m_rast + delta_rast
diff <- test - day_m_rast  #this is equal to zeor roughly...

s_sgdf<-as(day_m_rast,"SpatialGridDataFrame") #Conversion to spatial grid data frame, only convert the necessary layers!! 
#s_spdf<-as(day_m_rast,"SpatialPointsDataFrame") #Conversion to spatial grid data frame, only convert the necessary layers!!

names(s_sgdf)<-"var1.pred"

mod1$krige_output<-s_sgdf

plot(mod1) #does not work because we set krige_output to null!!!
formula_mod<-formula("y_var~lat*lon + elev_s")
col_names<-all.vars(formula_mod) #extract terms names from formula object
if (length(col_names)==1){
  data_fit <-data_month
}else{
  data_fit <- remove_na_spdf(col_names,data_month)
}
ref_rast<-as(subset(s_raster,1),"SpatialGridDataFrame")
s_spdf<-select_var_stack(s_raster,formula_mod,spdf=TRUE) #This only works if s_raster is in memory!!! need to be modified

proj4string(data_fit)<-proj4string(s_spdf)
test_mod <- autoKrige(formula_mod, input_data=data_fit,new_data=s_spdf,data_variogram=data_fit)
plot(test_mod)
prediction_spdf = test_mod$krige_output
sample_variogram = test_mod$exp_var
variogram_model = test_mod$var_model

#### CHECKING THE INPUTS FROM COVARIATES
LC1 <- subset(s_raster,"LC1")
plot(LC1,colNA=c("red"))

LC2 <- subset(s_raster,"LC2")
plot(LC2,colNA=c("red"))

LC_names<- paste("LC",1:10,sep="")
lc_reg_s <-subset(s_raster,LC_names)
plot(lc_reg_s,colNA="red")

plot(subset(s_raster,"CANHGHT"),colNA="red")

#Now create mask based on water areas 

LC12<-raster(lc_reg_s,layer=nlayers(lc_reg_s)) #this is open water
LC_mask<-LC12
LC_mask[LC_mask==100]<-NA
LC_mask <- LC_mask > 100

CANHGHT <- subset(s_raster,"CANHGHT")

lc_path<-"/data/project/layers/commons/data_workflow/inputs/lc-consensus-global"
infile_modis_grid<-"/data/project/layers/commons/data_workflow/inputs/modis_grid/modis_sinusoidal_grid_world.shp" #modis grid tiling system, global
infile_elev<-"/data/project/layers/commons/data_workflow/inputs/dem-cgiar-srtm-1km-tif/srtm_1km.tif"  #elevation at 1km, global extent to be replaced by the new fused product 
infile_canheight<-"/data/project/layers/commons/data_workflow/inputs/treeheight-simard2011/Simard_Pinto_3DGlobalVeg_JGR.tif"         #Canopy height, global extent
infile_distoc <- "/data/project/layers/commons/data_workflow/inputs/distance_to_coast/GMT_intermediate_coast_distance_01d_rev.tif" #distance to coast, global extent at 0.01 deg

CANH<-raster(infile_canheight)

LC1_W<-raster(list.files(path=lc_path,full.names=T)[4])

#Correlation matrix for a subset
r<-subset(s_raster,5:8)
t44<-layerStats(r,"pearson",na.rm=T)
image(t44[[1]])

###########################################################################################
############ PART 2: Granularity-Autocorrelation analyses of predicted surfaces ###########
##### 

lf2 <- raster_prediction_obj_2$method_mod_obj[[index]][[y_var_name]]
lf2 #contains the models for gam

pred_temp_s <-stack(lf2)
date_selected <- "20109101"
#names_layers <-c("mod1=s(lat,long)+s(elev)","mod4=s(lat,long)+s(LST)","diff=mod1-mod4")
names_layers <-c("mod1 = s(lat,long)","mod2 = s(lat,long)+s(elev)","mod3 = s(lat,long)+s(N_w)","mod4 = s(lat,long)+s(E_w)",
                 "mod5 = s(lat,long)+s(LST)","mod6 = s(lat,long)+s(DISTOC)","mod7 = s(lat,long)+s(LC1)",
                 "mod8 = s(lat,long)+s(LC1,LST)","mod9 = s(lat,long)+s(CANHGHT)","mod10 = s(lat,long)+s(LST,CANHGHT)")

#names_layers<-names(pred_temp_s)
#names(pred_temp_s)<-names_layers

s.range <- c(min(minValue(pred_temp_s)), max(maxValue(pred_temp_s)))
#s.range <- s.range+c(5,-5)
col.breaks <- pretty(s.range, n=200)
lab.breaks <- pretty(s.range, n=100)
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
max_val<-s.range[2]
min_val <-s.range[1]
#max_val<- -10
#min_val <- 0
layout_m<-c(4,3) #one row two columns

p<-levelplot(pred_temp_s,main="Interpolated Surfaces Model Comparison baseline 1", ylab=NULL,xlab=NULL,
          par.settings = list(axis.text = list(font = 2, cex = 1.3),layout=layout_m,
                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
          names.attr=names_layers,col.regions=temp.colors,at=seq(max_val,min_val,by=0.01))
#col.regions=temp.colors(25))
print(p)

#####################################
### Create spatial correlogram ####

r_mod5 <- subset(pred_temp_s,"mod5") #wiht LST
r_mod1 <- subset(pred_temp_s,"mod1") #wiht lat,long
r_mod2 <- subset(pred_temp_s,"mod2") #wiht elev

df_mod5 <- as(r_mod5,"SpatialPointsDataFrame")
df_mod1 <- as(r_mod1,"SpatialPointsDataFrame")
df_mod2 <- as(r_mod2,"SpatialPointsDataFrame")

r_stack <-stack(subset(s_raster,"mm_01"),pred_temp_s)
df_rs <- as(r_stack,"SpatialPointsDataFrame")


correg_t<-correlog(coordinates(df_mod5),df_mod5$mod5)
correg_t<-correlog(coordinates(df_mod5)[,1],coordinates(df_mod5)[,2],df_mod5$mod5)

data_s<-(as.data.frame((list_data_s[[1]])))
data_v<-(list_data_v[[1]])

data_s <-na.omit(data_s[,c("x","y","LST",y_var_name,"elev")])
correg_t1 <- correlog(as.matrix(cbind(data_s$x,data_s$y)),z=data_s$LST)
correg_t2 <- correlog(as.matrix(cbind(data_s$x,data_s$y)),z=data_s$elev)
correg_t3 <- correlog(as.matrix(cbind(data_s$x,data_s$y)),z=data_s[[y_var_name]])
correg_t4 <- correlog(as.matrix(cbind(data_s$x,data_s$y)),z=data_s[,c("LST",y_var_name,"elev")])

plot(correg_t1)
plot(correg_t2,add=T)

correg_t1[,1]
correg_t2[,1]

df_x <- as.data.frame(cbind(correg_t1[,1],correg_t1[,2],correg_t2[,2],correg_t3[,2]))
names(df_x)<-c("dist","LST",y_var_name,"elev")

xyplot(LST+dailyTmax+elev~dist,df_x,type="b",
       auto.key=list(title="Var", space = "right", cex=1.0), 
       par.settings = list(superpose.symbol=list(pch = 0:3, cex=1)), 
)

### For the whole image:

sp_correlogram_fun <- function(i,list_param){
  
  df <- list_param$list_df[[i]]
  var_zname <- list_param$var_zname[i]
  order_lag <- list_param$order_lag[i]
  method_cor <- list_param$method_cor
  nb_obj <- list_param$nb_obj 
  randomisation_par <- list_param$randomisation_par
  sp.cor <- sp.correlogram(nb_obj, df[[var_zname]], order=order_lag,
                           method=method_cor, randomisation=randomisation_par)
  return(sp.cor)
  
}

# 'nb' - neighbourhood of each cell
#r.nb <- dnearneigh(as.matrix(xy), d1=0.5, d2=1.5)
# 'nb' - an alternative way to specify the neighbourhood
# r.nb <- cell2nb(nrow=side, ncol=side, type="queen")
#sp.cor <- sp.correlogram(r.nb, df_mod5$mod5, order=15,
#                         method="I", randomisation=FALSE)

r_stack <-stack(subset(s_raster,c("mm_01","mm_07")),pred_temp_s)
names(r_stack)[1:2]<-c("mm01","mm_07")
df_rs <- as(r_stack,"SpatialPointsDataFrame")
r.nb <- dnearneigh(coordinates(df_rs), d1=res(s_raster)[1]/2, d2=1.5*res(s_raster)[1]) #lag1

#Do not run... slow
rk.nb14 <- knearneigh(coordinates(df_rs), k=14) #lag1
#rk_nb14 <- knearneigh(coordinates(df_rs), k=14) #lag1
#save(rk_nb14, file = "rk_nb14.RData")


#rk_nb7 <- knearneigh(coordinates(df_rs), k=7) #lag1
#save(rk_nb7, file = "rk_nb7.RData")

#lrk_nb7 <- knn2nb(rk_nb7)

#m_LST1 <- moran.test(df_rs$mm01,nb2listw(lrk_nb7),na.action=na.omit,zero.policy=TRUE)
#sp.cor <- sp.correlogram(lrk_nb7, df_rs$mm01, order=7,
#                         method="I", randomisation=FALSE)


list_df <- list(df_rs,df_rs,df_rs,df_rs,df_rs)
var_zname <- c("mm01","mm_07","mod1","mod2","mod5")
order_lag <- c(14,14,14,14,14)
method_cor <- "I"
nb_obj <- r.nb 
randomisation_par <- "FALSE"

list_param_spat_correlog <- list(list_df,var_zname,order_lag,method_cor,nb_obj,randomisation_par)
names(list_param_spat_correlog) <- c("list_df","var_zname","order_lag","method_cor","nb_obj","randomisation_par")

#debug(sp_correlogram_fun)
#list_sp_correlog  <-sp_correlogram_fun(2,list_param_spat_correlog)
#r_qc_s <- lapply(1:length(infile_var),FUN=import_list_modis_layers_fun,list_param=list_param_import_modis)  
#r_qc_s <-mclapply(1:11,FUN=import_list_modis_layers_fun,list_param=list_param_import_modis,mc.preschedule=FALSE,mc.cores = 11) #This is the end bracket from mclapply(...) statement    
list_sp_correlog <-mclapply(1:length(list_df),FUN=sp_correlogram_fun,list_param=list_param_spat_correlog ,mc.preschedule=FALSE,mc.cores = 5) #This is the end bracket from mclapply(...) statement
#does not work...
print(list_sp_correlog[[1]])
plot(list_sp_correlog[[1]])
print(list_sp_correlog[[2]])
plot(list_sp_correlog[[2]])

##### Use filter option to compute lag Moran's I

#Queen's case for 5 lags...: should do this in a function to generate filters...
#lag 1: 2*1+1 rows
f1 <- matrix(c(1,1,1,
               1,0,1,
               1,1,1), nrow=3)
#lag 2: 2*2+1 rows
f2 <- matrix(c(1,1,1,1,1,             #filter for lag 2
               1,0,0,0,1, 
               1,0,0,0,1,
               1,0,0,0,1,
               1,1,1,1,1),nrow=5)
f3 <- matrix(c(1,1,1,1,1,1,1,
               1,0,0,0,0,0,1, 
               1,0,0,0,0,0,1,
               1,0,0,0,0,0,1,
               1,0,0,0,0,0,1,
               1,0,0,0,0,0,1,
               1,1,1,1,1,1,1),nrow=7)
f4 <- matrix(c(1,1,1,1,1,1,1,1,1,
               1,0,0,0,0,0,0,0,1, 
               1,0,0,0,0,0,0,0,1,
               1,0,0,0,0,0,0,0,1,
               1,0,0,0,0,0,0,0,1,
               1,0,0,0,0,0,0,0,1,
               1,0,0,0,0,0,0,0,1,
               1,0,0,0,0,0,0,0,1,
               1,1,1,1,1,1,1,1,1),nrow=9)
r<- subset(s_raster,"mm_07")
Moran(r,f1)
Moran(r,f2)
Moran(r,f3)

#generate automatically filters for MORAN's I in the image...

autocor_filter_fun <-function(no_lag=1,f_type="queen"){
  if(f_type=="queen"){
    no_rows <- 2*no_lag +1
    border_row <-rep(1,no_rows)
    other_row <- c(1,rep(0,no_rows-2),1)
    other_rows <- rep(other_row,no_rows-2)
    mat_data<- c(border_row,other_rows,border_row)
    autocor_filter<-matrix(mat_data,nrow=no_rows)
  }
  #if(f_type=="rook){} #add later
  return(autocor_filter)
}

#moran_multipe_fun<-function(i,list_param)
#  lapply(list_filters,FUN=Moran,x=r)

r<- subset(r_stack,"mod1")
Moran(r,f1)
Moran(r,f2)
list_filters<-lapply(1:5,FUN=autocor_filter_fun,f_type="queen")

Moran(r,list_filters[[1]])
Moran(r,list_filters[[2]])

plot(subset(s_raster,"mm_09"))

r_stack <-stack(subset(s_raster,c("mm_09")),pred_temp_s)
names(r_stack)[1]<-c("mm_09")

r<- subset(r_stack,"mod1")
Moran(r) #with lag 1 and default rooks lag correlation

list_filters<-lapply(1:5,FUN=autocor_filter_fun,f_type="queen")


#cacluate Moran's I for 5 lags for one layer
moran_list <- lapply(list_filters,FUN=Moran,x=r)

moran_multiple_fun<-function(i,list_param){
  #un
  list_filters <-list_param$list_filters
  r <- subset(list_param$r_stack,i)
  moran_list <- lapply(list_filters,FUN=Moran,x=r)
  moran_v <-as.data.frame(unlist(moran_list))
  names(moran_v)<-names(r)
  return(moran_v)
}

list_filters<-lapply(1:10,FUN=autocor_filter_fun,f_type="queen")

list_param_moran <- list(list_filters=list_filters,r_stack=r_stack)
#moran_r <-moran_multiple_fun(1,list_param=list_param_moran)
nlayers(r_stack)
moran_I_df <-mclapply(1:nlayers(r_stack), list_param=list_param_moran, FUN=moran_multiple_fun,mc.preschedule=FALSE,mc.cores = 11) #This is the end bracket from mclapply(...) statement

moran_df <- do.call(cbind,moran_I_df)
moran_df$lag <-1:nrow(moran_df)

#melt(moran_df,id=names(moran_df))
#moran_df <- do.call(rbind,moran_I_df)
mydata<-moran_df
dd <- do.call(make.groups, mydata[,-ncol(mydata)]) 
dd$lag <- mydata$lag 
#names(dd)[2]<-"models"
names_layers <-c("LST",names_layers)

xyplot(data ~ lag | which, dd,type="b",strip=strip.custom(factor.levels=names_layers)) 

#solve problem wiht name

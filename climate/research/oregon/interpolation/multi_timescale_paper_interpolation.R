####################################  INTERPOLATION OF TEMPERATURES  #######################################
############################  Script for manuscript analyses,tables and figures: MULTITIME SCALE  ##############################
#This script uses the worklfow code applied to the Oregon case study. Multitime scale methods (GAM,GWR, Kriging) are tested with
#different covariates using FUSION and CAI. Accuracy methods are added in the the function script to evaluate results.
#Figures, tables and data for the  paper are also produced in the script.
#AUTHOR: Benoit Parmentier 
#CREATED ON: 10/31/2013  
#MODIFIED ON: 12/23/2013            
#Version: 2
#PROJECT: Environmental Layers project                                     
#################################################################################################

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
library(gridExtra)
#Additional libraries not used in workflow
library(pgirmess)                            # Krusall Wallis test with mulitple options, Kruskalmc {pgirmess}  

#### FUNCTION USED IN SCRIPT

function_analyses_paper1 <-"contribution_of_covariates_paper_interpolation_functions_10222013.R"
function_analyses_paper2 <-"multi_timescales_paper_interpolation_functions_12232013.R"

##############################
#### Parameters and constants  

script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/" #path to script
source(file.path(script_path,function_analyses_paper1)) #source all functions used in this script.
source(file.path(script_path,function_analyses_paper2)) #source all functions used in this script.

#direct methods: gam, kriging, gwr
in_dir1 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_daily_lst_comb5_11012013"
in_dir2 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_daily_lst_comb5_11022013"
in_dir3a <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_daily_lst_comb5p1_3_11062013"
in_dir3b <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_daily_lst_comb5p4_7_11292013"

#CAI: gam, kriging, gwr
in_dir4 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_cai_lst_comb5_11032013"
in_dir5 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_cai_lst_comb5_11032013"
in_dir6 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_cai_lst_comb5_11042013"
#FSS: gam, kriging, gwr
in_dir7 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_fss_lst_comb5_11062013"
in_dir8 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_fss_lst_comb5_11052013"
in_dir9 <- "/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_fss_lst_comb5_11052013"
###

### hold out 0-70

in_dir10 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_fss_lst_mults_0_70_comb5_11082013"
in_dir11 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_fss_lst_mults_0_70_comb5_11132013"
in_dir12 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_fss_lst_mults_0_70_comb5_11162013"
in_dir13 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_cai_lst_mults_0_70_comb5_11192013"
in_dir14 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_cai_lst_mults_0_70_comb5_11272013"
in_dir15 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_cai_lst_mults_0_70_comb5_11222013"

##raster_prediction object for comb5
#direct methods
raster_obj_file_1 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_daily_lst_comb5_11012013.RData" 
raster_obj_file_2 <- "raster_prediction_obj_kriging_daily_dailyTmax_365d_kriging_daily_lst_comb5_11022013.RData"
raster_obj_file_3a <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_daily_lst_comb5p1_3_11062013.RData"
raster_obj_file_3b <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_daily_lst_comb5p4_7_11292013.RData"
#CAI
raster_obj_file_4 <- "raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_cai_lst_comb5_11032013.RData"
raster_obj_file_5 <- "raster_prediction_obj_kriging_CAI_dailyTmax_365d_kriging_cai_lst_comb5_11032013.RData"
raster_obj_file_6 <- "raster_prediction_obj_gwr_CAI_dailyTmax_365d_gwr_cai_lst_comb5_11042013.RData"
#FSS
raster_obj_file_7 <- "raster_prediction_obj_gam_fusion_dailyTmax_365d_gam_fss_lst_comb5_11062013.RData"
raster_obj_file_8 <- "raster_prediction_obj_kriging_fusion_dailyTmax_365d_kriging_fss_lst_comb5_11052013.RData"
raster_obj_file_9 <- "raster_prediction_obj_gwr_fusion_dailyTmax_365d_gwr_fss_lst_comb5_11052013.RData"

## Results from monthly holdout 0 to 70%

raster_obj_file_10 <- "raster_prediction_obj_gam_fusion_dailyTmax_365d_gam_fss_lst_mults_0_70_comb5_11082013.RData"
raster_obj_file_11 <- "raster_prediction_obj_kriging_fusion_dailyTmax_365d_kriging_fss_lst_mults_0_70_comb5_11132013.RData"
raster_obj_file_12 <- "raster_prediction_obj_gwr_fusion_dailyTmax_365d_gwr_fss_lst_mults_0_70_comb5_11162013.RData"
raster_obj_file_13 <- "raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_cai_lst_mults_0_70_comb5_11192013.RData"
raster_obj_file_14 <- "raster_prediction_obj_kriging_CAI_dailyTmax_365d_kriging_cai_lst_mults_0_70_comb5_11272013.RData"
raster_obj_file_15 <- "raster_prediction_obj_gwr_CAI_dailyTmax_365d_gwr_cai_lst_mults_0_70_comb5_11222013.RData"

#List of object with 0% monthly hold out
list_raster_obj_files  <- list(file.path(in_dir1,raster_obj_file_1),file.path(in_dir2,raster_obj_file_2),
                               file.path(in_dir3a,raster_obj_file_3a),file.path(in_dir3b,raster_obj_file_3b),
                               file.path(in_dir4,raster_obj_file_4),file.path(in_dir5,raster_obj_file_5),
                               file.path(in_dir6,raster_obj_file_6),file.path(in_dir7,raster_obj_file_7),
                               file.path(in_dir8,raster_obj_file_8),file.path(in_dir9,raster_obj_file_9))
 
names(list_raster_obj_files)<- c("gam_daily","kriging_daily","gwr_daily_a","gwr_daily_b",
                                 "gam_CAI","kriging_CAI","gwr_CAI",
                                 "gam_fss","kriging_fss","gwr_fss")

y_var_name <- "dailyTmax"
out_prefix<-"analyses_12232013"
out_dir<-"/home/parmentier/Data/IPLANT_project/paper_multitime_scale__analyses_tables"
out_dir <-paste(out_dir,"_",out_prefix,sep="")

if (!file.exists(out_dir)){
  dir.create(out_dir)
  #} else{
  #  out_path <-paste(out_path..)
}
setwd(out_dir)

infile_reg_outline <- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
met_stations_outfiles_obj_file<-"/data/project/layers/commons/data_workflow/output_data_365d_gam_fus_lst_test_run_07172013/met_stations_outfiles_obj_gam_fusion__365d_gam_fus_lst_test_run_07172013.RData"
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
ref_rast_name<- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/mean_day244_rescaled.rst"                     #This is the shape file of outline of the study area. #local raster name defining resolution, exent, local projection--. set on the fly??
infile_reg_outline <- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
ref_rast_name <-"/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/mean_day244_rescaled.rst"  #local raster name defining resolution, exent: oregon
ref_rast_d001 <-"/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/mean_day001_rescaled.rst"
transect_list <-c("/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/transect_OR_1.shp",
                  "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/transect_OR_2.shp",
                  "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/transect_OR_3.shp")

#method_interpolation <- "gam_daily"
#covar_obj_file_1<- list.files(path=in_dir1,pattern="covar_obj.*.RData")
covar_obj_file_1 <- "covar_obj__365d_gam_daily_lst_comb5_11012013.RData"
met_obj_file_1 <- "met_stations_outfiles_obj_gam_daily__365d_gam_daily_lst_comb5_11012013.RData"

#Load objects containing training, testing, models objects 
met_stations_obj <- load_obj(file.path(in_dir1,met_obj_file_1))
covar_obj <-load_obj(file.path(in_dir1,covar_obj_file_1)) #Reading covariates object for GAM daily method
infile_covariates <- covar_obj$infile_covariates
infile_reg_outline <- covar_obj$infile_reg_outline
covar_names<- covar_obj$covar_names
#####
s_raster <- brick(infile_covariates)
names(s_raster)<-covar_names

###################################################################
############### BEGIN SCRIPT ##################################

#############################PART I: Generate tables for paper ##########
#Table 1: Covariates used (not produced in R)
#Table 2: Covariates models and functional form (not produced in R)
#Table 3: Combinations of models tested (not produced in R)
#Table 4. Average RMSE for all models acroos year 2010 wiht 30% daily hold out
#Table 5: Average monhtly RMSE for CAI model 7, climatology surfaces,  CAI and single time scale methods 

##### Table 4: Contribution of covariates using validation accuracy metrics
## This is a table of accuracy  

summary_metrics_v_list<-lapply(list_raster_obj_files,FUN=function(x){x<-load_obj(x);x[["summary_metrics_v"]]$avg$rmse})                           
names(summary_metrics_v_list)

#for gam models
table_gam<- summary_metrics_v_list[grep("gam",names(summary_metrics_v_list))]
table_gam <-do.call(cbind,table_gam)
table_gam<-table_gam[1:7,]                           
#for kriging models
table_kriging <- summary_metrics_v_list[grep("kriging",names(summary_metrics_v_list))]
table_kriging <- do.call(cbind,table_kriging)
table_kriging <- table_kriging[1:7,]                         

#for gwr models
table_gwr <- summary_metrics_v_list[grep("gwr",names(summary_metrics_v_list))] #select models related to gwr
table_gwr$gwr_daily <- c(table_gwr[[1]],table_gwr[[2]]) #combine both gwr object results
table_gwr <- table_gwr[c(5,3,4)] #reorder list and drop first two objects
table_gwr <- do.call(cbind,table_gwr)
table_gwr <- table_gwr[1:7,]                         

table4_paper <- as.data.frame(do.call(rbind,list(table_gam,table_kriging,table_gwr)))    
table4_paper <- round(table4_paper,digit=3) #roundto three digits teh differences
table4_paper$Methods <- c(rep("gam",7),
                          rep("kriging",7),
                          rep("gwr",7))    
                             
#Check input covariates and model formula:
#list_formulas <-raster_prediction_obj_2$method_mod_obj[[1]]$formulas #formulas for models run comb5
list_formulas <-unlist(lapply(list_raster_obj_files[[1]],FUN=function(x){x<-load_obj(x);x$method_mod_obj[[1]]$formulas}))
#strsplit(list_formulas,"~")
                             
table4_paper$Formulas<-rep(list_formulas,3)                             
table4_paper<-table4_paper[(c(5,4,1,2,3))]  #reordering columns                           

#Testing siginificance between models
                             
#mod_compk1 <-kruskal.test(tb1$rmse ~ as.factor(tb1$pred_mod)) #Kruskal Wallis test
#mod_compk2 <-kruskal.test(tb2$rmse ~ as.factor(tb2$pred_mod))
#print(mod_compk1) #not significant

###Table 4, writeout
names_table_col <-c("Models","Method","Single time scale","Multiple time scale CAI","Multiple time scale FSS") # 

names(table4_paper)<- names_table_col
print(table4_paper) #show resulting table

#Now write out table 4

file_name<-paste("table4_multi_timescale_paper","_",out_prefix,".txt",sep="")
write.table(table4_paper,file=file_name,sep=",")

################ Table 5 ################
#### Monthly RMSE for GAM CAI Climatology, GAM CAI daily and Single  time scale
## This is a table of accuracy  

#raster_prediction_obj_4 <-load_obj(file.path(in_dir4,raster_obj_file_4)) #comb5 gam_dir

tb_m_gam_CAI <- load_obj(file.path(in_dir13,raster_obj_file_13))$tb_month_diagnostic_v #getting objet
tb_gam_CAI <- load_obj(file.path(in_dir13,raster_obj_file_13))$tb_diagnostic_v #getting objet
tb_gam_daily <- load_obj(file.path(in_dir1,raster_obj_file_1))$tb_diagnostic_v #getting objet

names_mod <- paste("mod",1:7,sep="")
list_stat_tb_gam_CAI <-calc_stat_by_month_tb(names_mod,tb_gam_CAI,month_holdout=T)
list_stat_tb_gam_daily <-calc_stat_by_month_tb(names_mod,tb_gam_daily,month_holdout=F)

list_stat_tb_gam_CAI$avg

clim_rmse <- subset(tb_m_gam_CAI,prop==30 & pred_mod== "mod7",select="rmse")
CAI_rmse <- subset(list_stat_tb_gam_CAI$avg,prop_month==30 & pred_mod== "mod7",select="rmse")
daily_rmse <- subset(list_stat_tb_gam_daily$avg, pred_mod== "mod7",select="rmse")

table5 <- data.frame(clim=clim_rmse,CAI_month=CAI_rmse,daily_month=daily_rmse)
table5 <- round(table5,digit=3) #roundto three digits teh differences

table5$month <- month.abb
table5 <- table5[,c(4,1,2,3)]

###Table 5, writeout
names_table_col <-c("Month","Long term Climatology RMSE",
                    "Average Monthly RMSE from Daily pred (CAI)"
                    ,"Average Monthly RMSE from Daily pred (Single time scale)") # 

names(table5)<- names_table_col
print(table5) #show resulting table

#Now write out table 5

file_name<-paste("table5_multi_timescale_paper","_",out_prefix,".txt",sep="")
write.table(table5,file=file_name,sep=",")

#######################################################
####### PART 2: generate figures for paper #############

#figure 1: Study area OR
#Figure 2: LST climatology production: daily mean compared to monthly mean
#figure 3: Prediction procedures: direct, CAI and FSS (figure created outside R)
#Figure 4. Accuracy and monthly hold out for FSS and CAI procedures and GWR, Kriging and GAM methods.
#Figure 5. Overtraining tendency, difference between training and testing
#Figure 6: Spatial pattern of prediction for one day (3 maps)
#Figure 7: Transect location (transect map)
#Figure 8: Transect profiles for one day 
#Figure 9: Image differencing and land cover: spatial patterns   
#Figure 10: LST and Tmax at stations, elevation and land cover.
#Figure 11: Spatial lag profiles for predicted surfaces 
#Figure 12: Daily deviation
#Figure 13: Spatial correlogram at stations, LST and elevation every 5 lags

################################################
######### Figure 1: Oregon study area, add labeling names to  Willamette Valley an Mountain Ranges?
#3 parameters from output
#infile_monthly<-list_outfiles$monthly_covar_ghcn_data #outile4 from database_covar script
#infile_daily<-list_outfiles$daily_covar_ghcn_data  #outfile3 from database_covar script
#infile_locs<- list_outfiles$loc_stations_ghcn #outfile2? from database covar script

ghcn_dat <- readOGR(dsn=dirname(met_stations_obj$monthly_covar_ghcn_data),
        sub(".shp","",basename(met_stations_obj$monthly_covar_ghcn_data)))
ghcn_dat_WGS84 <-spTransform(ghcn_dat,CRS_locs_WGS84)         # Project from WGS84 to new coord. system

interp_area <- readOGR(dsn=dirname(infile_reg_outline),sub(".shp","",basename(infile_reg_outline)))
interp_area_WGS84 <-spTransform(interp_area,CRS_locs_WGS84)         # Project from WGS84 to new coord. system

usa_map <- getData('GADM', country='USA', level=1) #Get US map
usa_map_2 <- usa_map[usa_map$NAME_1!="Alaska",] #remove Alaska
usa_map_2 <- usa_map_2[usa_map_2$NAME_1!="Hawaii",] #remove Hawai 
usa_map_OR <- usa_map_2[usa_map_2$NAME_1=="Oregon",] #get OR

elev <- subset(s_raster,"elev_s")
elev_WGS84 <- projectRaster(from=elev,crs=CRS_locs_WGS84,method="ngb")

#set up the output file to plot
res_pix<-960
col_mfrow<-1
row_mfrow<-1
png(filename=paste("Figure1_paper_study_area_",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
par(mfrow=c(1,1))

#plot(elev_WGS84,cex.axis=2.1,cex.z=2.1)
plot(elev_WGS84,cex.axis=1.4,axis.args=list(at=seq(0,3500,500),
               labels=seq(0, 3500, 500), 
               cex.axis=1.4))
plot(interp_area_WGS84,add=T)
plot(ghcn_dat_WGS84,add=T)
title_text <-c("Elevation (m) and meteorological"," stations in Oregon")
legend("topleft",legend=title_text,cex=2.1,bty="n")
#Add region label
par(mar = c(0,0,0,0)) # remove margin
#opar <- par(fig=c(0.9,0.95,0.8, 0.85), new=TRUE)
#opar <- par(fig=c(0.85,0.95,0.8, 0.9), new=TRUE)
#opar <- par(fig=c(0.8,0.95,0.75, 0.9), new=TRUE)
#opar <- par(fig=c(0.75,0.95,0.7, 0.9), new=TRUE)
opar <- par(fig=c(0.65,0.9,0.8, 0.92), new=TRUE)

plot(usa_map_2,border="black") #border and lwd are options of graphics package polygon object
plot(usa_map_OR,col="dark grey",add=T)
box()
dev.off()

################################################
######### Figure 2: LST averaging: daily mean compared to monthly mean

lst_md <- raster(ref_rast_name)
projection(lst_md)<-projection(s_raster)
lst_mm_09<-subset(s_raster,"mm_09")

lst_md<-raster(ref_rast_d001)
lst_md<- lst_md - 273.16
lst_mm_01<-subset(s_raster,"mm_01")

png(filename=paste("Figure2_paper_Comparison_daily_monthly_mean_lst",out_prefix,".png",sep=""),width=960,height=480)
par(mfrow=c(1,2))
plot(lst_md)
plot(interp_area,add=TRUE)
title("Mean for January 1")
plot(lst_mm_01)
plot(interp_area,add=TRUE)
title("Mean for month of January")
dev.off()

################################################
######### Figure 3: Prediction procedures: direct, CAI and FSS 
#(figure created outside R)


################################################
######### Figure 4. RMSE multi-timescale mulitple hold out for FSS and CAI

raster_prediction_obj_9 <-load_obj(file.path(in_dir9,raster_obj_file_9)) #comb5 gwr_fss

list_raster_obj_files_holdout  <- list(file.path(in_dir10,raster_obj_file_10),file.path(in_dir11,raster_obj_file_11),
                                  file.path(in_dir12,raster_obj_file_12),file.path(in_dir13,raster_obj_file_13),
                                  file.path(in_dir14,raster_obj_file_14),file.path(in_dir15,raster_obj_file_15))
 
names(list_raster_obj_files_holdout)<- c("gam_fss","kriging_fss","gwr_fss",
                                         "gam_CAI","kriging_CAI","gwr_CAI")
tb_v_list<-lapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_diagnostic_v"]]})                           
tb_s_list<-lapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_diagnostic_s"]]})                           
#tb_s_list<-mclapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_diagnostic_s"]]},mc.preschedule=FALSE,mc.cores = 6)                           
#tb_v_list<-mclapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_diagnostic_v"]]},mc.preschedule=FALSE,mc.cores = 6)                           
names(tb_s_list) <- paste("tb_s_",names(tb_s_list),sep="")
names(tb_v_list) <- paste("tb_v_",names(tb_v_list),sep="")

#Extract object element...
tb_mv_list<-lapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_month_diagnostic_v"]]})                           
tb_ms_list<-lapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_month_diagnostic_s"]]})                           
#tb_mv_list<-mclapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_month_diagnostic_v"]]},mc.preschedule=FALSE,mc.cores = 6)                           
#tb_ms_list<-mclapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_month_diagnostic_s"]]},mc.preschedule=FALSE,mc.cores = 6)                           
names(tb_ms_list) <- paste("tb_ms_",names(tb_ms_list),sep="") #monthly training accuracy
names(tb_mv_list) <- paste("tb_mv_",names(tb_mv_list),sep="") #monthly testing accuracy

list_tb <- c(tb_s_list,tb_v_list,tb_ms_list,tb_mv_list) #combined in one list
ac_metric <- "rmse"
#plot_names <- c("RMSE for gam_FSS","RMSE for kriging_FSS","RMSE for gwr_FSS",
#               "RMSE for gam_CAI","RMSE for kriging_CAI","RMSE for gwr_CAI")
names_mod <- paste("mod",1:7,sep="")
plot_names <- names(list_tb) #this is the default names for the plots
#now replace names for relevant figure used later on.
plot_names[7:12] <- c("RMSE for gam_FSS","RMSE for kriging_FSS","RMSE for gwr_FSS",
               "RMSE for gam_CAI","RMSE for kriging_CAI","RMSE for gwr_CAI")
#Quick function to explore accuracy make this a function to create solo figure...and run only a subset...

list_plots <- plot_accuracy_by_holdout_fun(list_tb,ac_metric,plot_names,names_mod)

##For paper...combine figures... tb_v for GWR, Kriging and GAM for both FSS and CAI

layout_m<-c(2,3) #one row two columns
#par(mfrow=layout_m)
    
##add option for plot title? 
png(paste("Figure4_paper_accuracy_",ac_metric,"_prop_month","_",out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
p1<-list_plots[[7]]
p2<-list_plots[[8]]
p3<-list_plots[[9]]
p4<-list_plots[[10]]
p5<-list_plots[[11]]
p6<-list_plots[[12]]

grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)
dev.off()

################################################
######### Figure 5. RMSE multi-timescale mulitple hold out Overtraining tendency

#x<-tb_ms_list[[1]]
tb_ms_list_diff <- lapply(tb_ms_list,FUN=function(x){x[x$prop!=0,]})                           
tb_mv_list_diff <- lapply(tb_mv_list,FUN=function(x){x[x$prop!=0,]})                           

tb_s_list_diff <- lapply(tb_s_list,FUN=function(x){x[x$prop_month!=0,]})                           
tb_v_list_diff <- lapply(tb_v_list,FUN=function(x){x[x$prop_month!=0,]}) 

#For paper...
#Combine figures... tb_v for GWR, Kriging and GAM for both FSS and CAI

metric_names <- c("mae","rmse","me","r")
list_metric_names <- vector("list", length=6) #list(metric_names)
list_metric_names[[1]] <- metric_names
list_metric_names <-lapply(1:6,FUN=function(i,list_metric_names,metric_names){list_metric_names[[i]]<-metric_names},
       list_metric_names,metric_names)

##### Calculate differences between training and testing for all multi-timescale mehtods
list_diff <-lapply(1:6,FUN=list_diff_df_fun,list_tb_s=tb_s_list, list_tb_v=tb_v_list,list_of_list_metric_names=list_metric_names)  
names(list_diff) <- c("gam_fss","kriging_fss","gwr_fss",
                                         "gam_CAI","kriging_CAI","gwr_CAI")

#Check results
#metric_names <- c("mae","rmse","me","r")
#diff_kriging_CAI <- diff_df(list_tb[["tb_s_kriging_CAI"]],list_tb[["tb_v_kriging_CAI"]],metric_names)
#diff_gwr_CAI <- diff_df(list_tb[["tb_s_gwr_CAI"]],list_tb[["tb_v_gwr_CAI"]],metric_names)
#head(diff_gwr_CAI)
#head(list_diff[[6]]

list_m_diff <-lapply(1:6,FUN=list_diff_df_fun,list_tb_s=tb_ms_list_diff, list_tb_v=tb_mv_list_diff,list_of_list_metric_names=list_metric_names)  
names(list_m_diff) <- c("gam_fss","kriging_fss","gwr_fss",
                                         "gam_CAI","kriging_CAI","gwr_CAI")

## Now create boxplots...
layout_m<-c(2,2) #one row two columns

png(paste("Figure5_paper_boxplot_overtraining_",out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
#boxplot(diff_kriging_CAI$rmse,diff_gam_CAI$rmse,diff_gwr_CAI$rmse,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
#        main="Difference between training and testing daily rmse")
par(mfrow=layout_m)

#monthly CAI
boxplot(list_m_diff$kriging_CAI$rmse,list_m_diff$gam_CAI$rmse,list_m_diff$gwr_CAI$rmse,
        names=c("kriging_CAI","gam_CAI","gwr_CAI"),ylab="RMSE (°C)",xlab="Interpolation Method",
        main="Difference between training and testing for monhtly rmse")
legend("topleft",legend=c("a"),bty="n") #bty="n", don't put box around legend

#daily CAI
boxplot(list_diff$kriging_CAI$rmse,list_diff$gam_CAI$rmse,list_diff$gwr_CAI$rmse,
        names=c("kriging_CAI","gam_CAI","gwr_CAI"),ylab="RMSE (°C)",xlab="Interpolation Method",
        main="Difference between training and testing daily rmse")
legend("topleft",legend=c("b"),bty="n")

#monthly fss
boxplot(list_m_diff$kriging_fss$rmse,list_m_diff$gam_fss$rmse,list_diff$gwr_fss$rmse,
        names=c("kriging_FSS","gam_FSS","gwr_FSS"),ylab="RMSE (°C)",xlab="Interpolation Method",
        main="Difference between training and testing for monhtly rmse")
legend("topleft",legend=c("c"),bty="n")

#daily fss
boxplot(list_diff$kriging_fss$rmse,list_diff$gam_fss$rmse,list_diff$gwr_fss$rmse,
        names=c("kriging_FSS","gam_FSS","gwr_FSS"),ylab="RMSE (°C)",xlab="Interpolation Method",
        main="Difference between training and testing daily rmse")
legend("topleft",legend=c("d"),bty="n")

dev.off()


################################################
######### Figure 6: Spatial pattern of prediction for one day (maps)

y_var_name <-"dailyTmax"
index<-244 #index corresponding to Sept 1

lf_list<-lapply(list_raster_obj_files[c("gam_daily","gam_CAI","gam_fss")],
                               FUN=function(x){x<-load_obj(x);x$method_mod_obj[[index]][[y_var_name]]})                           

date_selected <- "20109101"
#methods_names <-c("gam","kriging","gwr")
methods_names <-c("gam_daily","gam_CAI","gam_FSS")

names_layers<-methods_names
#lf <- (list(lf1,lf4[1:7],lf7[1:7]))
lf<-list(lf_list[[1]],lf_list[[2]][1:7],lf_list[[3]][1:7])

names_layers <-c("mod1 = lat*long","mod2 = lat*long + LST","mod3 = lat*long + elev","mod4 = lat*long + N_w*E_w",
                 "mod5 = lat*long + elev + DISTOC","mod6 = lat*long + elev + LST","mod7 = lat*long + elev + LST*FOREST")
nb_fig<- c("6a","6b","6c")
list_plots_spt <- vector("list",length=length(lf))
for (i in 1:length(lf)){
  pred_temp_s <-stack(lf[[i]])

  #lf2 <- raster_prediction_obj_2$method_mod_obj[[index]][[y_var_name]]
  #lf2 #contains the models for gam

  #s.range <- c(min(minValue(pred_temp_s)), max(maxValue(pred_temp_s)))
  #s.range <- s.range+c(5,-5)
  #col.breaks <- pretty(s.range, n=200)
  #lab.breaks <- pretty(s.range, n=100)
  temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
  #max_val<-s.range[2]
  #min_val <-s.range[1]
  max_val <- 40
  min_val <- -10
  layout_m<-c(2,4) #one row two columns

  png(paste("Figure_",nb_fig[i],"_spatial_pattern_tmax_prediction_models_gam_levelplot_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])

  p <- levelplot(pred_temp_s,main=methods_names[i], ylab=NULL,xlab=NULL,
          par.settings = list(axis.text = list(font = 2, cex = 1.3),layout=layout_m,
                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
          names.attr=names_layers,col.regions=temp.colors,at=seq(min_val,max_val,by=0.25))
  #col.regions=temp.colors(25))
  print(p)
  dev.off()
  list_plots_spt[[i]] <- p
}

layout_m<-c(2,4) # works if set to this?? ok set the resolution...
#layout_m<-c(2*3,4) # works if set to this?? ok set the resolution...

png(paste("Figure6_paper_","_spatial_pattern_tmax_prediction_models_gam_levelplot_",date_selected,out_prefix,".png", sep=""),
    height=960*layout_m[1],width=960*layout_m[2])
    #height=480*6,width=480*4)

p1 <- list_plots_spt[[1]]
p2 <- list_plots_spt[[2]]
p3 <- list_plots_spt[[3]]

grid.arrange(p1,p2,p3,ncol=1)
dev.off()

################################################
#Figure 7: Spatial transects for one day (maps)

#######Figure 7a: Map of transects

## Transects image location in OR             
png(paste("Figrue7_paper_elevation_transect_paths_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])

plot(elev)
for(i in 1:length(transect_list)){
  filename<-sub(".shp","",transect_list[i])             #Removing the extension from file.
  transect<-readOGR(dirname(filename), basename(filename))                 #reading shapefile 
  plot(transect,add=TRUE)
}
title("Transect Oregon")
dev.off()

#### TRANSECT PROFILES
nb_transect <- 3
list_transect2<-vector("list",nb_transect)
list_transect3<-vector("list",nb_transect)
list_transect4<-vector("list",nb_transect)

#names_layers <-c("mod1 = lat*long","mod2 = lat*long + LST","mod3 = lat*long + elev","mod4 = lat*long + N_w*E_w",
#                 "mod5 = lat*long + elev + DISTOC","mod6 = lat*long + elev + LST","mod7 = lat*long + elev + LST*FOREST")

rast_pred<-stack(lf[[2]]) #GAM_CAI
rast_pred_selected2<-subset(rast_pred,c(1,2,6)) #3 is referring to FSS, plot it first because it has the
rast_pred_selected3<-subset(rast_pred,c(1,3,6)) #3 is referring to FSS, plot it first because it has the
                                          # the largest range.
rast_pred2 <- stack(rast_pred_selected2,subset(s_raster,"elev_s"))
rast_pred3 <- stack(rast_pred_selected3,subset(s_raster,"elev_s"))

#layers_names<-layerNames(rast_pred2)<-c("lat*lon","lat*lon + elev + LST","elev")
layers_names2 <- names(rast_pred2)<-c("mod1","mod2","mod6","elev")
layers_names3 <- names(rast_pred3)<-c("mod1","mod3","mod6","elev")
#pos<-c(1,2) # postions in the layer prection
#transect_list
list_transect2[[1]]<-c(transect_list[1],paste("Figure8a_paper_tmax_elevation_transect1_OR_",date_selected,
                                           paste("mod1_2_6",collapse="_"),out_prefix,sep="_"))
list_transect2[[2]]<-c(transect_list[2],paste("Figure8b_tmax_elevation_transect2_OR_",date_selected,
                                           paste("mod1_2_6",collapse="_"),out_prefix,sep="_"))
list_transect2[[3]]<-c(transect_list[3],paste("Figure8c_paper_tmax_elevation_transect3_OR_",date_selected,
                                           paste("mod1_2_6",collapse="_"),out_prefix,sep="_"))

list_transect3[[1]]<-c(transect_list[1],paste("Figure8a_tmax_elevation_transect1_OR_",date_selected,
                                           paste("mod1_3_6",collapse="_"),out_prefix,sep="_"))
list_transect3[[2]]<-c(transect_list[2],paste("Figure8b_tmax_elevation_transect2_OR_",date_selected,
                                           paste("mod1_3_6",collapse="_"),out_prefix,sep="_"))
list_transect3[[3]]<-c(transect_list[3],paste("Figure8c_tmax_elevation_transect3_OR_",date_selected,
                                           paste("mod1_3_6",collapse="_"),out_prefix,sep="_"))

names(list_transect2)<-c("Oregon Transect 1","Oregon Transect 2","Oregon Transect 3")
names(list_transect3)<-c("Oregon Transect 1","Oregon Transect 2","Oregon Transect 3")

names(rast_pred2)<-layers_names2
names(rast_pred3)<-layers_names3

title_plot2<-paste(names(list_transect2),"on",date_selected,sep=" ")
#title_plot2<-paste(names(list_transect2),"on",date_selected,sep=" ")

#title_plot2<-paste(rep("Oregon transect on ",3), date_selected,sep="")
#title_plot3<-paste(names(list_transect3),date_selected,sep=" ")
#title_plot3<-paste(rep("Oregon transect on ",3), date_selected,sep="")

#r_stack<-rast_pred
#m_layers_sc<-c(3) #elevation in the third layer
m_layers_sc<-c(4) #elevation in the third layer

#title_plot2
#rast_pred2
#debug(plot_transect_m2)
trans_data2 <-plot_transect_m2(list_transect2,rast_pred2,title_plot2,disp=FALSE,m_layers_sc)
trans_data3 <-plot_transect_m2(list_transect3,rast_pred3,title_plot2,disp=FALSE,m_layers_sc)

################################################
#Figure 9: Spatial pattern: Image differencing  
#Do for january and September...?

#names_layers <-c("mod1 = lat*long","mod2 = lat*long + LST","mod3 = lat*long + elev","mod4 = lat*long + N_w*E_w",
#                 "mod5 = lat*long + elev + DISTOC","mod6 = lat*long + elev + LST","mod7 = lat*long + elev + LST*FOREST")

methods_name <-c("gam_daily","gam_CAI","gam_fss")
index<-244 #index corresponding to Sept 1
y_var_name <-"dailyTmax"
ref_mod <- 3 #mod3
alt_mod <- 6 #mod6
file_format <- ".rst"
NA_flag_val <- -9999

list_param_diff <- list(index,list_raster_obj_files,methods_name,y_var_name,ref_mod,alt_mod,NA_flag_val,file_format,out_dir,out_prefix)
names(list_param_diff) <- c("index","list_raster_obj_files","methods_name","y_var_name","ref_mod","alt_mod","NA_flag_val","file_format","out_dir","out_prefix")

#diff_list <- mclapply(1:365, list_param=list_param_diff, FUN=diff_date_rast_pred_fun,mc.preschedule=FALSE,mc.cores = 11) #This is the end bracket from mclapply(...) statement

diff_pred_date1_list<- diff_date_rast_pred_fun(1,list_param_diff)
diff_pred_date2_list<- diff_date_rast_pred_fun(244,list_param_diff)
r_stack_diff <-stack(c(diff_pred_date1_list,diff_pred_date2_list))
names(r_stack_diff) <- c("Jan_Daily","Jan_CAI","Jan_FSS","Sept_Daily","Sept_CAI","Sept_FSS")
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))

layout_m<-c(1,1) #one row two columns
png(paste("Figure9_paper_difference_image_",out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
plot(r_stack_diff,col=temp.colors(25))
#levelplot(r_stack_diff)
dev.off()
###


####################################################################
#Figure 10: LST and Tmax at stations, elevation and land cover.

LC_subset <- c("LC1","LC5","LC6","LC7","LC9","LC11")  
LC_names <- c("LC1_forest", "LC5_shrub", "LC6_grass", "LC7_crop", "LC9_urban","LC11_barren")
LC_s <- subset(s_raster,LC_subset)
names(LC_s) <- LC_names
plot(LC_s)

avl<-c(0,10,1,10,20,2,20,30,3,30,40,4,40,50,5,50,60,6,60,70,7,70,80,8,80,90,9,90,100,10)#Note that category 1 does not include 0!!

stat_list <- extract_diff_by_landcover(r_stack_diff,s_raster,LC_subset,LC_names,avl)

#r_subset_name <- "elev_s"
#r_names <- c("elev_s")
#stat_list_elev <- extract_diff_by_landcover(r_stack_diff,s_raster,LC_subset,LC_names,avl)
#write_out_raster_fun(s_raster,out_suffix=out_prefix,out_dir=out_dir,NA_flag_val=-9999,file_format=".rst")

#show correlation with LST by day over the year, ok writeout s_raster of coveriate??

title_plots_list <-c("Jan_Daily","Jan_CAI","Jan_FSS","Sept_Daily","Sept_CAI","Sept_FSS")

## Now create plots
layout_m<-c(2,3) #one row two columns
#savePlot(paste("fig6_diff_prediction_tmax_difference_land cover",mf_selected,mc_selected,date_selected,out_prefix,".png", sep="_"), type="png")

png(paste("Figure10_paper_diff_prediction_tmax_difference_land cover,ac_metric","_",out_prefix,".png", sep=""),
      height=480*layout_m[1],width=480*layout_m[2])
par(mfrow=layout_m)    
#funciton plot
for (i in 1:length(stat_list$avg)){
  #i=i+1
  zones_stat <- as.data.frame(stat_list$avg[[i]])
  zones_stat$zones <- 0:10

  plot(zones_stat$zones,zones_stat[,1],type="b",ylim=c(-4.5,6),
       ylab="",xlab="",axes=FALSE)
  #mtext("difference between mod3 and mod6 (degree C)",line=3,side=2,cex=1.2,font=2) #Add ylab with distance 3 from box
  #mtext("land cover percent classes",side=1,cex=1.2,line=3,font=2)
  lines(zones_stat$zones,zones_stat[,2],col="red",lty="dashed",pch=2) #shrub
  points(zones_stat$zones,zones_stat[,2],col="red",lty="dashed",pch=2) #shrub
  lines(zones_stat$zones,zones_stat[,3],col="green",lty="dotted",pch=3) #grass
  points(zones_stat$zones,zones_stat[,3],col="green",lty="dotted",pch=3) #grass
  lines(zones_stat$zones,zones_stat[,4],col="blue",lty="dashed",pch=4) #crop
  points(zones_stat$zones,zones_stat[,4],col="blue",lty="dashed",pch=4) #crop
  lines(zones_stat$zones,zones_stat[,5],col="darkgreen",lty="dashed",pch=5)
  points(zones_stat$zones,zones_stat[,5],col="darkgreen",lty="dashed",pch=5)
  lines(zones_stat$zones,zones_stat[,6],col="purple",lty="dashed",pch=6)
  points(zones_stat$zones,zones_stat[,6],col="purple",lty="dashed",pch=6)

  breaks_lab<-zones_stat$zones
  #make it slanted...
  tick_lab<-c("0","1-10","","20-30","","40-50","","60-70","","80-90","90-100") #Not enough space for  
  #tick_lab<-c("0","10-20","30-40","60-70","80-90","90-100")
  axis(side=1,las=1,tick=TRUE,
       at=breaks_lab,labels=tick_lab, cex.axis=1.2,font=2) #reduce number of labels to Jan and June
  #text(tick_lab, par(\u201cusr\u201d)[3], labels = tick_lab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
  axis(2,cex.axis=1.4,font=2)
  box()
  legend("topleft",legend=names(zones_stat)[-7], 
        cex=1.4, col=c("black","red","green","blue","darkgreen","purple"),bty="n",
        lty=1,pch=1:7)
  title(paste(title_plots_list[i],sep=""),cex=1.6, font=2)
  #title(paste("Prediction tmax difference (",mf_selected,"-",mc_selected,") and land cover ",sep=""),cex=1.4,font=2) 
}
dev.off()


################################################
#### Figure 11: Spatial lag profiles  
#This figure is generated to show the spatial Moran'I for 10 spatial 
#for Jan 1 and Sept 1 in 2010 for all models (1 to 7) and methods

index<-1 #index corresponding to Jan 1 #For now create Moran's I for only one date...
lf_moran_list_date1 <-lapply(list_raster_obj_files[c("gam_daily","gam_CAI","gam_fss")],
                               FUN=function(x){x<-load_obj(x);x$method_mod_obj[[index]][[y_var_name]]})                           
index<-244 #index corresponding to Sept 1 #For now create Moran's I for only one date...
lf_moran_list_date2 <-lapply(list_raster_obj_files[c("gam_daily","gam_CAI","gam_fss")],
                               FUN=function(x){x<-load_obj(x);x$method_mod_obj[[index]][[y_var_name]]})                           

#date_selected <- "20100901"
#date_selected <- "20100101"
#methods_names <-c("gam","kriging","gwr")
methods_names <-c("gam_daily","gam_CAI","gam_FSS")

names_layers<-methods_names
#Subset images to eliminate mod_kr
lf1 <- list(lf_moran_list_date1[[1]],lf_moran_list_date1[[2]][1:7],lf_moran_list_date1[[3]][1:7])
lf2 <- list(lf_moran_list_date2[[1]],lf_moran_list_date2[[2]][1:7],lf_moran_list_date2[[3]][1:7])
names(lf1)<-c("gam_daily","gam_CAI","gam_FSS")
names(lf2)<-c("gam_daily","gam_CAI","gam_FSS")

### Now extract Moran's I for a range of lag using a list of images

#set maximum lag range
nb_lag <-10
#Provide list of raster images:
list_lf <- list(lf1,lf2)

list_moran_df1 <- calculate_moranI_profile(list_lf[[1]],nb_lag) #for January 1
list_moran_df2 <- calculate_moranI_profile(list_lf[[2]],nb_lag) #for September 1
names(list_moran_df1)<-c("gam_daily","gam_CAI","gam_FSS")
names(list_moran_df2)<-c("gam_daily","gam_CAI","gam_FSS")

#Run accross two dates...
#list_moran lapply(list_lf,FUN=calculate_moranI_profile,nb_lag=nb_lag)

### Prepare to plot lag Moran's I profiles

#generate automatic title for exploration if necessary!!
list_title_plot<- list(c("Spatial lag profile on January 1, 2010"),
                  c("Spatial lag profile on September 1, 2010"))
#name used in the panel!!!
names_panel_plot <-c("mod1 = lat*long","mod2 = lat*long + LST","mod3 = lat*long + elev","mod4 = lat*long + N_w*E_w",
                 "mod5 = lat*long + elev + DISTOC","mod6 = lat*long + elev + LST","mod7 = lat*long + elev + LST*FOREST")
layout_m<-c(2,4) # works if set to this?? ok set the resolution...
list_moran_df <- list(list_moran_df1,list_moran_df2)
list_param_plot_moranI_profile_fun <- list(list_moran_df,list_title_plot,names_panel_plot,layout_m)
names(list_param_plot_moranI_profile_fun) <- c("list_moran_df","list_title_plot","names_panel_plot","layout_m")

#debug(plot_moranI_profile_fun)
#p<- plot_moranI_profile_fun(1,list_param=list_param_plot_moranI_profile_fun)
  
list_moranI_plots <- lapply(1:2,FUN=plot_moranI_profile_fun,list_param=list_param_plot_moranI_profile_fun)


#This function uses list moran_df object from calculate_moranI_profile function!!

#layout_m<-c(2,4) # works if set to this?? ok set the resolution...
#layout_m<-c(2*3,4) # works if set to this?? ok set the resolution...

png(paste("Figure11_paper_spatial_correlogram_prediction_models_levelplot_",out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
    #height=3*480*layout_m[1],width=2*480*layout_m[2])
    #height=480*6,width=480*4)
#png(paste("Figure11_paper_spatial_correlogram_prediction_models_levelplot_",out_prefix,".png", sep=""),
#    height=480,width=480)
    #height=480*6,width=480*4)

p1 <- list_moranI_plots[[1]]
p2 <- list_moranI_plots[[2]]

grid.arrange(p1,p2,ncol=1)
dev.off()

################################################
#Figure 12: Monthly climatology, Daily deviation and bias

lf_delta_gam_fss <-extract_list_from_list_obj(load_obj(list_raster_obj_files[["gam_fss"]])$method_mod_obj,"delta") #getting objet

test01 <-stack(lf_delta_gam_fss[[287]]) #Ot.14

test0 <-stack(lf_delta_gam_fss[[288]]) #Ot.15
test1 <-stack(lf_delta_gam_fss[[289]]) #Ot.16
test2<-stack(lf_delta_gam_fss[[290]]) #Ot.17

plot(test01)

plot(test0)
plot(test1)
plot(test2)


################################################
#Figure 13: Tmax and LST averagees

names_tmp<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")
LST_s<-subset(s_raster,names_tmp)
names_tmp<-c("nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
LST_nobs<-subset(s_raster,names_tmp)
plot(LST_s)
plot(LST_nobs)
y_range_nobs <- c(0,385)
y_range_avg <- c(-15,60)
var_name <- "LST"

#It works for any variable with stack of monthly values...!!
LST_stat <- plot_mean_nobs_r_stack_by_month(var_s=LST_s,var_nobs=LST_nobs,y_range_nobs,y_range_avg,var_name,out_prefix)

##################################################
####### Figure 13 : disussion  

##################################################
####### Figure 13a: spatial variation -MoranI and  std  

#Use data from accuraccy with no monthly hold out
raster_obj <- load_obj(list_raster_obj_files$gam_CAI)
data_month <- (raster_obj$method_mod_obj[[1]]$data_month_s)
dates<-unique(raster_obj$tb_diagnostic_v$date)
#Extract monthly data frame used in fitting 
list_data_month_s <-extract_list_from_list_obj(raster_obj$validation_mod_month_obj,"data_s") 
year_nbs <- sapply(list_data_month_s,FUN=length) #number of observations per month
#Convert sp data.frame and combined them in one unique df, see function define earlier
data_month_all <-convert_spdf_to_df_from_list(list_data_month_s) #long rownames
#LSTD_bias_avgm<-aggregate(LSTD_bias~month,data=data_month_all,mean)
#LSTD_bias_sdm<-aggregate(LSTD_bias~month,data=data_month_all,sd)
LST_avgm<-aggregate(LST~month,data=data_month_all,mean)
TMax_avgm<-aggregate(TMax~month,data=data_month_all,mean)

#plot(LST_avgm,type="b")
#lines(TMax_avgm,col="blue",add=T)
plot(data_month_all$month,data_month_all$LST, xlab="month",ylab="LST at station")
title(paste("Monthly LST at stations in Oregon", "2010",sep=" "))
png(paste("LST_at_stations_per_month_",out_prefix,".png", sep=""), type="png")
                  

statistics_LST_s<- LST_stat$avg #extracted earlier!!!

png(paste("Monthly_mean_TMax_LST_at_Stations_",out_prefix,".png", sep=""), type="png")

plot(TMax_avgm,type="b",ylim=c(0,35),col="red",xlab="month",ylab="tmax (degree C)")
lines(1:12,LST_avgm$LST,type="b",col="blue")
#lines(1:12,statistics_LST_s$mean,type="b",col="darkgreen")
text(TMax_avgm[,1],TMax_avgm[,2],rownames(statistics_LST_s),cex=0.8,pos=2)
                  
legend("topleft",legend=c("TMax","LST"), cex=1, col=c("red","blue"),
              lty=1,bty="n")
title(paste("Monthly mean tmax and LST at stations in Oregon", "2010",sep=" "))           

##################################################
####### Figure 13b: spatial variation -MoranI and  std  
#LST for area with FOrest 50> and grass >50

#Account for forest
data_month_all_forest<-data_month_all[data_month_all$LC1>=50,]
data_month_all_grass<-data_month_all[data_month_all$LC6>=50,]
#data_month_all_grass<-data_month_all[data_month_all$LC6>=10,]
data_month_all_urban<-data_month_all[data_month_all$LC9>=50,]

LST_avgm_forest<-aggregate(LST~month,data=data_month_all_forest,mean)
LST_avgm_grass<-aggregate(LST~month,data=data_month_all_grass,mean)
LST_avgm_urban<-aggregate(LST~month,data=data_month_all_urban,mean)

plot(TMax_avgm,type="b",ylim=c(-7,42))
lines(LST_avgm$LST,col="blue",add=T)
lines(LST_avgm_forest,col="green",add=T)
lines(LST_avgm_grass,col="red",add=T)
lines(LST_avgm_urban,col="pink",add=T)
legend("topleft",legend=c("TMax","LST","LST_forest","LST_grass","LST_urban"), cex=0.8, col=c("black","blue","green","red","pink"),
       lty=1,bty="n")
title(paste("Monthly average tmax for stations in Oregon ", "2010",sep=""))

savePlot(paste("Temporal_profile_res",id[i],out_prefix,".png", sep=""), type="png")  

##################################################
####### Figure 13c: spatial variation -MoranI and  std  

#get the  list of all prediction for a specific method
#list_lf <-extract_list_from_list_obj(load_obj(list_raster_obj_files$gam_CAI)$method_mod_obj,"dailyTmax") #getting objet
list_lf_gam_CAI <-extract_list_from_list_obj(load_obj(list_raster_obj_files[["gam_CAI"]])$method_mod_obj,"dailyTmax") #getting objet
list_lf_gam_fss <-extract_list_from_list_obj(load_obj(list_raster_obj_files[["gam_fss"]])$method_mod_obj,"dailyTmax") #getting objet

#nb_lag <- 10
list_filters<-lapply(1:nb_lag,FUN=autocor_filter_fun,f_type="queen") #generate lag 10 filters
list_param_stat_moran <- list(filter=list_filters[[10]],lf_list=list_lf_gam_CAI)
tt <- stat_moran_std_raster_fun(1,list_param=list_param_stat_moran)
tt <- mclapply(1:11, list_param=list_param_stat_moran, FUN=stat_moran_std_raster_fun,mc.preschedule=FALSE,mc.cores = 11) #This is the end bracket from mclapply(...) statement
list_param_stat_moran <- list(filter=list_filters[[10]],lf_list=list_lf_gam_fss)

#Takes 1 hour to get the average moran's I for the whole year so load moran_std_tt_fss2.RData
#tt_fss <- mclapply(1:365, list_param=list_param_stat_moran, FUN=stat_moran_std_raster_fun,mc.preschedule=FALSE,mc.cores = 11) #This is the end bracket from mclapply(...) statement
#save(tt_fss,file=file.path(out_dir,"moran_std_tt_fss.RData"))
#tx<- do.call(rbind,tt_fss)

#dates <-strptime(dates, "%Y%m%d")   # interpolation date being processed
#mo<-as.integer(strftime(dates, "%m"))          # current month of the date being processed

dates<-unique(raster_obj$tb_diagnostic_v$date)
tt_fss2 <- load_obj("moran_std_tt_fss2.RData")
tx<- do.call(rbind,tt_fss2)
dates_proc<-strptime(dates, "%Y%m%d")   # interpolation date being processed

dates_proc <- as.data.frame(dates_proc)
dates_proc$index <- 1:365
tx2 <- merge(tx,dates_proc,by="index")
tx2$month <- as.integer(strftime(tx2$dates_proc, "%m"))          # current month of the date being processed
names(tx2) <- c("index","moranI","std","pred_mod","date","month")
t<-melt(tx2,
          #measure=mod_var, 
          id=c("pred_mod","month"),
          na.rm=F)
#t$value<-as.numeric(t$value) #problem with char!!!
tb_mod_m_avg <-cast(t,pred_mod+month~variable,mean) #monthly mean for every model

#mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
#day<-as.integer(strftime(date_proc, "%d"))
#year<-as.integer(strftime(date_proc, "%Y"))
# end of pasted
x<-subset(tb_mod_m_avg,pred_mod=="mod7")

plot(x$month,x$moranI,type="b",col="blue")
par(new=TRUE)              # key: ask for new plot without erasing old
plot(x$month,x$std,type="b",col="red",axes=F)
  #axis(4,xlab="",ylab="elevation(m)")  
axis(4,cex=1.2)

##################################################
####### Figure 13d: correlation between elevation and LST

LST_s <- subset(s_raster,c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12"))
LST1 <- subset(LST_s,1)

test<-stack(LST_s,elev)
names(test) <- c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12","elev")
x_cor <-layerStats(test,stat="pearson",na.rm=T)
corr(values(LST1),values(elev))
plot(1:12,x_cor[[1]][1:12,13],type="b",ylab="corelation",xlab="month",main="correlation between elevation and LST")

#now summarize by month...

#plot(data_month$TMax,add=TRUE)
#list_data_s <-extract_list_from_list_obj(load_obj(list_raster_obj_files$gam_CAI)$validation_mod_obj,"data_s")
#list_data_v <-extract_list_from_list_obj(load_obj(list_raster_obj_files$gam_CAI)$validation_mod_obj,"data_v")

#data_s <- list_data_s[[1]]
#res_m <- data_s$dailyTmax - data_s$mod3
#SSRES_m <- sum((res_m)^2,na.rm=T)
#res_dev <- data_s$dailyTmax - data_s$mod3_del
#SSRES_dev <- sum((res_dev)^2,na.rm=T)
#SST <- sum((data_s$dailyTmax - mean(data_s$dailyTmax))^2,na.rm=T)

#1- (SSRES_m/SST)
#1- (SSRES_dev/SST)



#### Now elev?
#LC1<-mask(LC1,mask_ELEV_SRTM)
#  cellStats(LC1,"countNA")        #Check that NA have been assigned to water and areas below 0 m
  
#LC1_50_m<- LC1>50
#LC1_100_m<- LC1>=100
#LC1_50_m[LC1_50_m==0]<-NA
#LC1_100_m[LC1_100_m==0]<-NA
#LC1_50<-LC1_50_m*LC1
#LC1_100<-LC1_100_m*LC1
#avl<-c(0,500,1,500,1000,2,1000,1500,3,1500,2000,4,2000,4000,5)
#rclmat<-matrix(avl,ncol=3,byrow=TRUE)
#elev_rec<-reclass(ELEV_SRTM,rclmat)  #Loss of layer names when using reclass
  
#elev_rec_forest<-elev_rec*LC1_100_m
#avg_elev_rec<-zonal(rast_diff,zones=elev_rec,stat="mean",na.rm=TRUE)
#std_elev_rec<-zonal(rast_diff,zones=elev_rec,stat="sd",na.rm=TRUE)
#avg_elev_rec_forest<-zonal(rast_diff,zones=elev_rec_forest,stat="mean",na.rm=TRUE)
#std_elev_rec_forest<-zonal(rast_diff,zones=elev_rec_forest,stat="sd",na.rm=TRUE)
  
## CREATE plots
#X11()
#plot(avg_elev_rec[,1],avg_elev_rec[,2],type="b",ylim=c(-10,1),
#       ylab="",xlab="",axes=FALSE)
#mtext("tmax difference between FSS and CAI (degree C)",side=2,cex=1.2,line=3,font=2)
#mtext("elevation classes (m)",side=1,cex=1.2,line=3,font=2)
#lines(avg_elev_rec_forest[,1],avg_elev_rec_forest[,2],col="green",type="b") #Elevation and 100% forest...
#breaks_lab<-avg_elev_rec[,1]
#elev_lab<-c("0-500","500-1000","1000-1500","1500-2000","2000-4000")
#axis(side=1,las=1,
#       at=breaks_lab,labels=elev_lab, cex=1.5,font=2) #reduce number of labels to Jan and June
#axis(2,cex.axis=1.2,font=2)
#legend("bottomleft",legend=c("Elevation", "elev_forest"), 
#         cex=1, lwd=1.3,col=c("black","green"),bty="n",
#         lty=1)
#box()
#title(paste("Prediction tmax difference (",mf_selected,"-",mc_selected,") and elevation ",sep=""),cex=1.4,font=2)
#savePlot(paste("fig7_diff_prediction_tmax_difference_elevation",mf_selected,mc_selected,date_selected,out_prefix,".png", sep="_"), type="png")
#dev.off()


###################### END OF SCRIPT #######################

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
#1,5,79,11
###



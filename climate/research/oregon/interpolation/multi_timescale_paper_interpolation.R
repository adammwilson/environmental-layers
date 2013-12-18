####################################  INTERPOLATION OF TEMPERATURES  #######################################
############################  Script for manuscript analyses,tables and figures: MULTITIME SCALE  ##############################
#This script uses the worklfow code applied to the Oregon case study. Multitime scale methods (GAM,GWR, Kriging) are tested with
#different covariates using FUSION and CAI. Accuracy methods are added in the the function script to evaluate results.
#Figures, tables and data for the  paper are also produced in the script.
#AUTHOR: Benoit Parmentier 
#CREATED ON: 10/31/2013  
#MODIFIED ON: 12/18/2013            
#Version: 1
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
function_analyses_paper2 <-"multi_timescales_paper_interpolation_functions_12182013.R"

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

## holdout

raster_obj_file_10 <- "raster_prediction_obj_gam_fusion_dailyTmax_365d_gam_fss_lst_mults_0_70_comb5_11082013.RData"
raster_obj_file_11 <- "raster_prediction_obj_kriging_fusion_dailyTmax_365d_kriging_fss_lst_mults_0_70_comb5_11132013.RData"
raster_obj_file_12 <- "raster_prediction_obj_gwr_fusion_dailyTmax_365d_gwr_fss_lst_mults_0_70_comb5_11162013.RData"
raster_obj_file_13 <- "raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_cai_lst_mults_0_70_comb5_11192013.RData"
raster_obj_file_14 <- "raster_prediction_obj_kriging_CAI_dailyTmax_365d_kriging_cai_lst_mults_0_70_comb5_11272013.RData"
raster_obj_file_15 <- "raster_prediction_obj_gwr_CAI_dailyTmax_365d_gwr_cai_lst_mults_0_70_comb5_11222013.RData"

out_dir<-"/home/parmentier/Data/IPLANT_project/paper_multitime_scale__analyses_tables_fig_09032013"
setwd(out_dir)

infile_reg_outline <- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
met_stations_outfiles_obj_file<-"/data/project/layers/commons/data_workflow/output_data_365d_gam_fus_lst_test_run_07172013/met_stations_outfiles_obj_gam_fusion__365d_gam_fus_lst_test_run_07172013.RData"
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
y_var_name <- "dailyTmax"
out_prefix<-"analyses_12022013"
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

#raster_prediction_obj_1 <-load_obj(file.path(in_dir1,raster_obj_file_1)) #comb5 gam_daily

############### BEGIN SCRIPT #################

############
##### 1) Generate: Table 4. Contribution of covariates using validation accuracy metrics
## This is a table of accuracy  

list_raster_obj_files  <- list(file.path(in_dir1,raster_obj_file_1),file.path(in_dir2,raster_obj_file_2),
                               file.path(in_dir3a,raster_obj_file_3a),file.path(in_dir3b,raster_obj_file_3b),
                               file.path(in_dir4,raster_obj_file_4),file.path(in_dir5,raster_obj_file_5),
                               file.path(in_dir6,raster_obj_file_6),file.path(in_dir7,raster_obj_file_7),
                               file.path(in_dir8,raster_obj_file_8),file.path(in_dir9,raster_obj_file_9))
 
names(list_raster_obj_files)<- c("gam_daily","kriging_daily","gwr_daily","gwr_daily",
                                 "gam_CAI","kriging_CAI","gwr_CAI",
                                 "gam_fss","kriging_fss","gwr_fss")

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
table_gwr <- summary_metrics_v_list[grep("gwr",names(summary_metrics_v_list))]
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
table4_paper<-table4_paper[(c(5,4,1,2,3))]                             

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

####################################
####### Now create figures #############

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
#Figure 11: Spatial lag profiles and stations data  
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
######### Figure 2:  Method comparison workflow 

# Workflow figure is not generated in R

################################################
######### Figure 3: LST averaging: daily mean compared to monthly mean

lst_md<-raster(ref_rast_name)
projection(lst_md)<-projection(s_raster)
lst_mm_09<-subset(s_raster,"mm_09")

lst_md<-raster(ref_rast_d001)
lst_md<- lst_md - 273.16
lst_mm_01<-subset(s_raster,"mm_01")

png(filename=paste("Comparison_daily_monthly_mean_lst",out_prefix,".png",sep=""),width=960,height=480)
par(mfrow=c(1,2))
plot(lst_md)
plot(interp_area,add=TRUE)
title("Mean January 1")
plot(lst_mm_01)
plot(interp_area,add=TRUE)
title("Mean for month of January")
dev.off()

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
#Quick function to explore accuracy make this a function to create solo figure...and run only a subset...

list_plots <- plot_accuracy_by_holdout_fun(list_tb,ac_metric)
names(list_tb)
tb_v_list 
#For paper...
#Combine figures... tb_v for GWR, Kriging and GAM for both FSS and CAI
#grid.arrange(p1,p2, ncol=2)

layout_m<-c(2,3) #one row two columns
#par(mfrow=layout_m)
    
##add option for plot title? 
png(paste("Figure4__accuracy_",ac_metric,"_prop_month","_",out_prefix,".png", sep=""),
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

png(paste("Figure_5_boxplot_overtraining_",out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
#boxplot(diff_kriging_CAI$rmse,diff_gam_CAI$rmse,diff_gwr_CAI$rmse,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
#        main="Difference between training and testing daily rmse")
par(mfrow=layout_m)

#monthly CAI
boxplot(list_m_diff$kriging_CAI$rmse,list_m_diff$gam_CAI$rmse,list_m_diff$gwr_CAI$rmse,
        names=c("kriging_CAI","gam_CAI","gwr_CAI"),
        main="Difference between training and testing for monhtly rmse")

#daily CAI
boxplot(list_diff$kriging_CAI$rmse,list_diff$gam_CAI$rmse,list_diff$gwr_CAI$rmse,
        names=c("kriging_CAI","gam_CAI","gwr_CAI"),
        main="Difference between training and testing daily rmse")
#monthly fss
boxplot(list_m_diff$kriging_fss$rmse,list_m_diff$gam_fss$rmse,list_diff$gwr_fss$rmse,
        names=c("kriging_fss","gam_fss","gwr_fss"),
        main="Difference between training and testing for monhtly rmse")

#daily fss
boxplot(list_diff$kriging_fss$rmse,list_diff$gam_fss$rmse,list_diff$gwr_fss$rmse,
        names=c("kriging_fss","gam_fss","gwr_fss"),
        main="Difference between training and testing daily rmse")

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
nb_fig<- c("7a","7b","7c")
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

png(paste("Figure7_","_spatial_pattern_tmax_prediction_models_gam_levelplot_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
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
png(paste("Fig7_elevation_transect_paths_",date_selected,out_prefix,".png", sep=""),
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
list_transect2[[1]]<-c(transect_list[1],paste("figure_3_tmax_elevation_transect1_OR_",date_selected,
                                           paste("mod1_2_6",collapse="_"),out_prefix,sep="_"))
list_transect2[[2]]<-c(transect_list[2],paste("figure_3_tmax_elevation_transect2_OR_",date_selected,
                                           paste("mod1_2_6",collapse="_"),out_prefix,sep="_"))
list_transect2[[3]]<-c(transect_list[3],paste("figure_3_tmax_elevation_transect3_OR_",date_selected,
                                           paste("mod1_2_6",collapse="_"),out_prefix,sep="_"))

list_transect3[[1]]<-c(transect_list[1],paste("figure_3_tmax_elevation_transect1_OR_",date_selected,
                                           paste("mod1_3_6",collapse="_"),out_prefix,sep="_"))
list_transect3[[2]]<-c(transect_list[2],paste("figure_3_tmax_elevation_transect2_OR_",date_selected,
                                           paste("mod1_3_6",collapse="_"),out_prefix,sep="_"))
list_transect3[[3]]<-c(transect_list[3],paste("figure_3_tmax_elevation_transect3_OR_",date_selected,
                                           paste("mod1_3_6",collapse="_"),out_prefix,sep="_"))

names(list_transect2)<-c("transect_OR1","transect_OR2","transect_OR3")
names(list_transect3)<-c("transect_OR1","transect_OR2","transect_OR3")

names(rast_pred2)<-layers_names2
names(rast_pred3)<-layers_names3

title_plot2<-paste(names(list_transect2),date_selected,sep=" ")
title_plot2<-paste(rep("Oregon transect on ",3), date_selected,sep="")
title_plot3<-paste(names(list_transect3),date_selected,sep=" ")
title_plot3<-paste(rep("Oregon transect on ",3), date_selected,sep="")

#r_stack<-rast_pred
#m_layers_sc<-c(3) #elevation in the third layer
m_layers_sc<-c(4) #elevation in the third layer

#title_plot2
#rast_pred2
#debug(plot_transect_m2)
trans_data2 <-plot_transect_m2(list_transect2,rast_pred2,title_plot2,disp=FALSE,m_layers_sc)
trans_data3 <-plot_transect_m2(list_transect3,rast_pred3,title_plot3,disp=FALSE,m_layers_sc)

################################################
#Figure 8: Spatial pattern: Image differencing and land cover  
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
png(paste("Figure_9_difference_image_",out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
plot(r_stack_diff,col=temp.colors(25))
#levelplot(r_stack_diff)
dev.off()
###

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

png(paste("Figure_9_diff_prediction_tmax_difference_land cover,ac_metric","_",out_prefix,".png", sep=""),
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
  tick_lab<-c("0","1-10","","20-30","","40-50","","60-70","","80-90","90-100") #Not enough space for  
  #tick_lab<-c("0","10-20","30-40","60-70","80-90","90-100")
  axis(side=1,las=1,tick=TRUE,
       at=breaks_lab,labels=tick_lab, cex.axis=1.2,font=2) #reduce number of labels to Jan and June
  #text(tick_lab, par(\u201cusr\u201d)[3], labels = tick_lab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
  axis(2,cex.axis=1.2,font=2)
  box()
  legend("topleft",legend=names(zones_stat)[-7], 
        cex=1, col=c("black","red","green","blue","darkgreen","purple"),bty="n",
        lty=1,pch=1:7)
  title(paste(title_plots_list[i],sep=""),cex=1.4, font=2)
  #title(paste("Prediction tmax difference (",mf_selected,"-",mc_selected,") and land cover ",sep=""),cex=1.4,font=2) 
}
dev.off()

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

################################################
#Figure 9: Tmax and LST averagees

names_tmp<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")
LST_s<-subset(s_raster,names_tmp)
names_tmp<-c("nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
LST_nobs<-subset(s_raster,names_tmp)

#LST_nobs<-mask(LST_nobs,LC_mask,filename="test2.tif")
#LST_s<-mask(LST_s,LC_mask,filename="test3.tif")
#c("Jan","Feb")
plot(LST_s)
plot(LST_nobs)

#Map 5: LST and TMax

#note differnces in patternin agricultural areas and 
min_values<-cellStats(LST_s,"min")
max_values<-cellStats(LST_s,"max")
mean_values<-cellStats(LST_s,"mean")
sd_values<-cellStats(LST_s,"sd")
#median_values<-cellStats(molst,"median") Does not extist
statistics_LST_s<-cbind(min_values,max_values,mean_values,sd_values) #This shows that some values are extremes...especially in October
LST_stat_data<-as.data.frame(statistics_LST_s)
rownames(LST_stat_data) <- month.abb
names(LST_stat_data)<-c("min","max","mean","sd")
# Statistics for number of valid observation stack
min_values<-cellStats(LST_nobs,"min")
max_values<-cellStats(LST_nobs,"max")
mean_values<-cellStats(LST_nobs,"mean")
sd_values<-cellStats(LST_nobs,"sd")
LST_nobs_stat_data<-cbind(min_values,max_values,mean_values,sd_values) #This shows that some values are extremes...especially in October
LST_nobs_stat_data <- as.data.frame(LST_nobs_stat_data)
rownames(LST_nobs_stat_data) <- month.abb

#X11(width=12,height=12)
#Plot statiscs (mean,min,max) for monthly LST images
plot(1:12,LST_stat_data$mean,type="b",ylim=c(-15,60),col="black",xlab="month",ylab="tmax (degree C)")
lines(1:12,LST_stat_data$min,type="b",col="blue")
lines(1:12,LST_stat_data$max,type="b",col="red")
text(1:12,LST_stat_data$mean,rownames(LST_stat_data),cex=1,pos=2)

legend("topleft",legend=c("min","mean","max"), cex=1.5, col=c("blue","black","red"),
       lty=1)
title(paste("LST statistics for Oregon", "2010",sep=" "))
#savePlot("lst_statistics_OR.png",type="png")

#Plot number of valid observations for LST
plot(1:12,LST_nobs_stat_data$mean,type="b",ylim=c(0,365),col="black",xlab="month",ylab="tmax (degree C)")
lines(1:12,LST_nobs_stat_data$min,type="b",col="blue")
lines(1:12,LST_nobs_stat_data$max,type="b",col="red")
text(1:12,LST_nobs_stat_data$mean,rownames(LST_stat_data),cex=1,pos=2)

legend("topleft",legend=c("min","mean","max"), cex=1.5, col=c("blue","black","red"),
       lty=1)
title(paste("LST number of valid observations for Oregon", "2010",sep=" "))
#savePlot("lst_nobs_OR.png",type="png")

#met_stations_obj <- load_obj(file.path(in_dir1,met_obj_file_1))
#met_stations_obj$monthly_query_ghcn_data
#

raster_obj <- load_obj(list_raster_obj_files$gam_CAI)
data_month <- (raster_obj$method_mod_obj[[1]]$data_month_s)
dates<-unique(raster_obj$tb_diagnostic_v$date)


#########################
#Figure
#LST for area with FOrest 50> and grass >50

########################
#Figure 

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
tt_fss <- mclapply(1:365, list_param=list_param_stat_moran, FUN=stat_moran_std_raster_fun,mc.preschedule=FALSE,mc.cores = 11) #This is the end bracket from mclapply(...) statement
save(tt_fss,file=file.path(out_dir,"moran_std_tt_fss.RData"))

tx<- do.call(rbind,tt_fss)

mo<-as.integer(strftime(dates, "%m"))          # current month of the date being processed

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

##############

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
#Figure 10: Spatial lag profiles and stations data  

index<-244 #index corresponding to Sept 1 #For now create Moran's I for only one date...
index<-1
lf_moran_list<-lapply(list_raster_obj_files[c("gam_daily","gam_CAI","gam_fss")],
                               FUN=function(x){x<-load_obj(x);x$method_mod_obj[[index]][[y_var_name]]})                           
#lf1 <- raster_prediction_obj_1$method_mod_obj[[index]][[y_var_name]] #select relevant raster images for the given dates

date_selected <- "20100901"
date_selected <- "20100101"
#methods_names <-c("gam","kriging","gwr")
methods_names <-c("gam_daily","gam_CAI","gam_FSS")

names_layers<-methods_names
#lf <- (list(lf1,lf4[1:7],lf7[1:7]))
lf<-list(lf_moran_list[[1]],lf_moran_list[[2]][1:7],lf_moran_list[[3]][1:7])

names_layers <-c("mod1 = lat*long","mod2 = lat*long + LST","mod3 = lat*long + elev","mod4 = lat*long + N_w*E_w",
                 "mod5 = lat*long + elev + DISTOC","mod6 = lat*long + elev + LST","mod7 = lat*long + elev + LST*FOREST")


nb_lag <-10
#lf

calculate_moranI_profile <- function(nb_lag,lf){
  list_filters<-lapply(1:nb_lag,FUN=autocor_filter_fun,f_type="queen") #generate lag 10 filters
  #moran_list <- lapply(list_filters,FUN=Moran,x=r)
  list_moran_df <- vector("list",length=length(lf))
  for (j in 1:length(lf)){
    r_stack <- stack(lf[[j]])
    list_param_moran <- list(list_filters=list_filters,r_stack=r_stack) #prepare parameters list for function
    #moran_r <-moran_multiple_fun(1,list_param=list_param_moran)
    nlayers(r_stack) 
    moran_I_df <-mclapply(1:nlayers(r_stack), list_param=list_param_moran, FUN=moran_multiple_fun,mc.preschedule=FALSE,mc.cores = 10) #This is the end bracket from mclapply(...) statement

    moran_df <- do.call(cbind,moran_I_df) #bind Moran's I value 10*nlayers data.frame
    moran_df$lag <-1:nrow(moran_df)
  
    list_moran_df[[j]] <- moran_df
  }
}

names(list_moran_df)<-c("gam_daily","gam_CAI","gam_fss")
list_dd <- vector("list",length=length(list_moran_df))

for(j in 1:length(lf)){
  method_name <- names(list_moran_df)[j]
  mydata <- list_moran_df[[j]]
  dd <- do.call(make.groups, mydata[,-ncol(mydata)]) 
  dd$lag <- mydata$lag
  dd$method_v <- method_name
  list_dd[[j]] <- dd
}

dd_combined<- do.call(rbind,list_dd)

layout_m<-c(2,4) #one row two columns

png(paste("Figure_9_spatial_correlogram_tmax_prediction_models_gam_levelplot_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
par(mfrow=layout_m)
p<-xyplot(data ~ lag | which , data=dd_combined,group=method_v,type="b", as.table=TRUE,
          pch=1:3,auto.key=list(columns=3,cex=1.5,font=2),
          par.settings = list(
          superpose.symbol = list(pch=1:3,col=1:3,pch.cex=1.4),
          axis.text = list(font = 2, cex = 1.3),layout=layout_m,
                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),
                              par.strip.text=list(font=2,cex=1.5),
          strip=strip.custom(factor.levels=names_layers),
          xlab=list(label="Spatial lag neighbor", cex=2,font=2),
          ylab=list(label="Moran's I", cex=2, font=2))

#Use as.table to reverse order of panel from top to bottom.

#p<-xyplot(data ~ lag | which , dd_combined,group=method_v,type="b",
#          par.settings = list(axis.text = list(font = 2, cex = 1.3),layout=layout_m,
#                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),
#                              par.strip.text=list(font=2,cex=1.5),
#          strip=strip.custom(factor.levels=names_layers),
#          xlab=list(label="Spatial lag neighbor", cex=2,font=2),
#          ylab=list(label="Moran's I", cex=2, font=2))

print(p)

dev.off()

LST_s <- subset(s_raster,c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12"))
LST1 <- subset(LST_s,1)
test<-stack(LST_s,elev)
names(test) <- c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12","elev")
x_cor <-layerStats(test,stat="pearson",na.rm=T)
corr(values(LST1),values(elev))
plot(1:12,x_cor[[1]][1:12,13],type="b",ylab="corelation",xlab="month",main="correlation between elevation and LST")

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


